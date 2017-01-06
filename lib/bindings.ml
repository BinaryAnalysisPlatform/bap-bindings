open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bap_plugins.Std
open Format

include Self()

let obj_addr x =
  Nativeint.shift_left (Nativeint.of_int (Obj.magic x)) 1


module Make( Internal : Cstubs_inverted.INTERNAL) =
struct
  module C = Ctypes

  let enum ?(first=0) name sexp cases =
    let pref = String.uppercase name in
    let t =
      C.view ~format_typ:(fun k ppf -> fprintf ppf "enum %s_tag%t" name k)
        ~read:ident ~write:ident C.int in
    let cases = List.mapi cases ~f:(fun i x ->
        let name = String.uppercase (Sexp.to_string (sexp x)) in
        pref ^ "_" ^ name, Int64.of_int (first + i)) in
    Internal.enum cases t;
    t

  type 'a opaque = {
    total  : 'a C.typ;
    nullable  : 'a option C.typ;
  }

  let def name = Internal.internal ("bap_" ^ name)

  module OString = struct
    module Array = C.CArray
    module Pool = Nativeint.Table

    let managed = Pool.create ~size:1024 ()

    let addr ptr =
      C.to_voidp ptr |>
      C.raw_address_of_ptr

    let is_managed ptr = Hashtbl.mem managed (addr ptr)

    let release ptr = Hashtbl.remove managed (addr ptr)

    let str_of_ptr ptr = C.string_of (C.ptr C.void) (C.to_voidp ptr)

    let expect_managed ptr =
      invalid_argf "Object at %s is not managed by ocaml"
        (str_of_ptr ptr) ()

    let size ptr =
      match Hashtbl.find managed (addr ptr) with
      | None -> expect_managed ptr
      | Some arr -> Array.length arr - 1

    let read ptr =
      match Hashtbl.find managed (addr ptr) with
      | None -> expect_managed ptr
      | Some arr ->
        String.init (Array.length arr) ~f:(Array.get arr)


    let write str =
      let len = String.length str + 1 in
      let arr = Array.make C.char len in
      for i = 0 to len - 2 do
        arr.(i) <- str.[i];
      done;
      arr.(len - 1) <- '\x00';
      let kind = Bigarray.char in
      let barr = C.bigarray_of_array C.array1 kind arr in
      let ptr = C.bigarray_start C.array1 barr in
      Hashtbl.set managed ~key:(addr ptr) ~data:arr;
      ptr

    let () = def "strlen" C.(ptr char @-> returning int) size

    let t : string C.typ = C.view ~read ~write (C.ptr C.char)
  end

  module Opaque = struct
    open Ctypes

    type 'a t = 'a opaque

    type 'a fat = {
      typeid : int;
      ovalue : 'a;
    }

    type typeinfo = {
      name : string;
    }

    type opaque_t = Opaque

    let registered = ref 0
    let typeinfo = Int.Table.create ~size:1024 ()

    let type_error name id =
      let got = match Hashtbl.find typeinfo id with
        | None -> "<unknown>"
        | Some {name} -> name in
      invalid_argf
        "Type error: expected a value of type %s, but got %s"
        name got ()

    let addr ptr = raw_address_of_ptr (to_voidp ptr)

    type cstruct = opaque_t structure

    let newtype (type t) name =
      incr registered;
      let name = "bap_" ^ name in
      let t : cstruct typ = structure name in
      let typeid = !registered in
      let null = from_voidp t null in
      Internal.typedef t name;
      Hashtbl.set typeinfo ~key:typeid ~data:{name};
      let read (opaque : cstruct ptr) : t =
        let {typeid=id; ovalue} = Root.get (to_voidp opaque) in
        if id <> typeid then type_error name id;
        ovalue in
      let write (ovalue : t) : cstruct ptr =
        from_voidp t (Root.create {typeid; ovalue}) in
      let read_opt ptr = if is_null ptr then None else Some (read ptr) in
      let write_opt = Option.value_map ~f:write ~default:null in
      let view read write = view ~read ~write (ptr t) in
      {total = view read write; nullable = view read_opt write_opt}
  end

  type 'a ctype = 'a Ctypes.typ

  let free ptr =
    if OString.is_managed ptr then OString.release ptr
    else C.Root.release ptr

  let () =
    def "free" C.(ptr void @-> returning void) free

  let standalone_init argc argv =
    try Plugins.run (); 0 with _ -> 1

  let version () = Config.version

  module Error = struct
    let current_error : Error.t option ref = ref None

    let set err = current_error := Some err
    let clear () = current_error := None

    let lift1 f x = match f x with
      | Ok x -> Some x
      | Error err -> set err; None

    let lift2 f x y = match f x y with
      | Ok x -> Some x
      | Error err -> set err; None

    let () =
      def "error_get" C.(void @-> returning OString.t)
        (fun () -> match current_error with
           | {contents=None} -> "unknown error (if any)"
           | {contents=Some err} -> Error.to_string_hum err);
  end

  let () =
    def "version" C.(void @-> returning OString.t) version;
    def "_standalone_init" C.(int @-> ptr string @-> returning int)
      standalone_init

  let regular (type t) (module T : Regular.S with type t = t) t name =
    let def fn = def (name ^ "_" ^ fn) in
    def "to_string" C.(t @-> returning OString.t) T.to_string;
    def "compare" C.(t @-> t @-> returning int) T.compare;
    def "equal"   C.(t @-> t @-> returning bool) T.equal;
    def "hash"    C.(t @-> returning int) T.hash;

  module Endian = struct
    module T = struct
      type t = Word.endian
    end

    let {total; nullable} : endian opaque = Opaque.newtype "endian_t"
  end

  module Size = struct
    let {total; nullable} : size opaque = Opaque.newtype "size_t"
    module Addr = struct
      let {total; nullable} : addr_size opaque =
        Opaque.newtype "addr_size_t"
    end
  end


  module Arch = struct
    let {total; nullable} : arch opaque = Opaque.newtype "arch_t"

    let def name typ impl =
      def ("arch_" ^ name) typ impl

    let name arch = Sexp.to_string (sexp_of_arch arch)

    let arches = Array.of_list Arch.all

    let get_tag arch =
      Array.findi_exn arches ~f:(fun i a -> Arch.equal a arch) |> fst

    let () =
      regular (module Arch) total "arch";
      let bap_arch_tag = enum "bap_arch" sexp_of_arch Arch.all in
      def "tag" C.(total @-> returning bap_arch_tag) get_tag;
      def "create" C.(bap_arch_tag @-> returning total) (fun i -> arches.(i));
      def "endian" C.(total @-> returning Endian.total) Arch.endian;
      def "addr_size" C.(total @-> returning Size.Addr.total) Arch.addr_size;
      def "of_string" C.(string @-> returning nullable) Arch.of_string;
      List.iter Arch.all ~f:(fun arch ->
          let name = Sexp.to_string (sexp_of_arch arch) in
          def name C.(void @-> returning total) (fun () -> arch));
  end

  module Tid = struct
    let {total=t; nullable} : tid opaque = Opaque.newtype "tid_t"

    let def x = def ("tid_" ^ x)

    let () =
      regular (module Tid) t "tid";
      def "create" C.(void @-> returning t) Tid.create;
      def "set_name" C.(t @-> string @-> returning void) Tid.set_name;
      def "name" C.(t @-> returning OString.t) Tid.name;
      def "from_string" C.(string @-> returning nullable)
        (Error.lift1 Tid.from_string)
  end

  module Term = struct
    type enum =
        Top | Sub | Arg | Blk | Def | Phi | Jmp
    [@@deriving sexp, enumerate]

    let term_tag = enum "bap_term" sexp_of_enum all_of_enum


    let tag = Term.switch
        ~program:(fun _ -> Top)
        ~sub:(fun _ -> Sub)
        ~arg:(fun _ -> Arg)
        ~blk:(fun _ -> Blk)
        ~phi:(fun _ -> Phi)
        ~def:(fun _ -> Def)
        ~jmp:(fun _ -> Jmp)

    let def ns name typ impl =
      def ("term_" ^ ns ^ "_" ^ name) typ impl

    let generic typ name =
      let def fn = def name fn in
      def "name" C.(typ @-> returning OString.t) Term.name;
      def "clone" C.(typ @-> returning typ) Term.clone;
      def "tid" C.(typ @-> returning Tid.t) Term.tid;
      ()

    let parent p name {total=t;nullable=t_opt} cls =
      let def fn = def name fn in
      def "length" C.(p @-> returning int) (Term.length cls);
      def "find" C.(p @-> Tid.t @-> returning t_opt) (Term.find cls);
      def "update" C.(p @-> t @-> returning p) (Term.update cls);
      def "remove" C.(p @-> Tid.t @-> returning p) (Term.remove cls);

      let changecb_t = C.(t_opt @-> returning t_opt) in

      def "change"
        C.(p @-> Tid.t @-> (Foreign.funptr changecb_t) @-> returning p)
        (Term.change cls);

  end

  module Program = struct
    let {total=t} : program term opaque = Opaque.newtype "program_t"

    let () =
      regular (module Program) t "program"
  end

  module Source = struct
    let rooter
      : rooter source opaque = Opaque.newtype "rooter_source_t"
    let {total=brancher; nullable=brancher_opt}
      : brancher source opaque = Opaque.newtype "brancher_source_t"
    let {total=symbolizer; nullable=symbolizer_opt}
      : symbolizer source opaque = Opaque.newtype "symbolizer_source_t"
    let {total=reconstructor; nullable=reconstructor_opt}
      : reconstructor source opaque =
      Opaque.newtype "reconstructor_source_t"

    let factory
        (type t) (module Factory : Source.Factory.S with type t = t)
        typ name =
      let def fn x = def (name ^ "_factory_" ^ fn) x in
      def "find" C.(string @-> returning typ.nullable) Factory.find
  end



  module Rooter = struct
    let {total; nullable} : rooter opaque = Opaque.newtype "rooter_t"
    let () =
      Source.factory (module Rooter.Factory) Source.rooter "rooter"
  end

  module Brancher = struct
    let {total; nullable} : brancher opaque = Opaque.newtype "brancher_t"
  end

  module Symbolizer = struct
    let {total; nullable} : symbolizer opaque = Opaque.newtype "symbolizer_t"
  end

  module Reconstructor = struct
    let {total; nullable} : reconstructor opaque = Opaque.newtype "reconstructor_t"
  end

  module Project = struct
    let {total; nullable} : project opaque = Opaque.newtype "project_t"
    let project_t = total
    let project_or_null = nullable

    let def name typ impl =
      def ("project_" ^ name) typ impl

    type params = Params

    let params : params C.structure ctype =
      C.structure "bap_project_parameters_t"

    let params_field name typ =
      C.field params ("bap_project_" ^ name) typ

    let rooter_p = params_field
        "rooter" Source.rooter.nullable
    let brancher_p = params_field
        "brancher" Source.brancher_opt
    let symbolizer_p = params_field
        "symbolizer" Source.symbolizer_opt
    let reconstructor_p = params_field
        "reconstructor" Source.reconstructor_opt
    let disassembler_p = params_field
        "disassember" C.string_opt

    let () = C.seal params
    let () = Internal.structure params

    module Input = struct
      let {total=t} : Project.input opaque = Opaque.newtype "project_input_t"

      let def name typ impl = def ("input_" ^ name) typ impl

      let () =
        def "file" C.(string @-> string_opt @-> returning t)
          (fun filename loader -> Project.Input.file ?loader ~filename);
    end

    let create input params =
      let getf fld =
        if C.is_null params then None
        else C.getf C.(!@params) fld in
      let disassembler,
          brancher,
          symbolizer,
          rooter,
          reconstructor =
        getf disassembler_p,
        getf brancher_p,
        getf symbolizer_p,
        getf rooter_p,
        getf reconstructor_p in
      Project.create ?disassembler ?brancher ?symbolizer ?rooter
        ?reconstructor input


    let () =
      def "create" C.(Input.t @-> ptr params @-> returning nullable)
        (Error.lift2 create);

      def "arch" C.(project_t @-> returning Arch.total)
        Project.arch;

      def "program" C.(project_t @-> returning Program.t)
        Project.program

  end
end
