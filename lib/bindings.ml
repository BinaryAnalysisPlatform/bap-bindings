open Core_kernel.Std
open Regular.Std
open Bap.Std
open Bap_plugins.Std
open Format

include Self()

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

    type c = C

    let registered = ref 0
    let typeinfo = Int.Table.create ~size:1024 ()

    let type_error name id =
      let got = match Hashtbl.find typeinfo id with
        | None -> "<unknown>"
        | Some {name} -> name in
      invalid_argf
        "Type error: expected a value of type %s, but got %s"
        name got ()




    let newtype name =
      let name = "bap_" ^ name in
      incr registered;
      let typeid = !registered in
      let t : c structure typ = structure name in
      Internal.typedef t name;
      Hashtbl.set typeinfo ~key:typeid ~data:{name};
      let read opaque =
        let {typeid=id; ovalue} = Root.get (to_voidp opaque) in
        if id <> typeid then type_error name id;
        ovalue in
      let write ovalue =
        from_voidp t (Root.create {typeid; ovalue}) in
      let read_opt ptr =
        if is_null ptr then None
        else read ptr in
      let write_opt = function
        | None -> from_voidp t null
        | Some oval -> write oval in
      let make_typ read write = view
          ~read ~write (ptr t) in
      {
        total = make_typ read write;
        nullable = make_typ read_opt write_opt;
      }
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

  module Term = struct
    type enum =
        Top | Sub | Arg | Blk | Def | Phi | Jmp
    [@@deriving sexp, enumerate]


    let def name typ impl =
      def ("term_" ^ name) typ impl

    let tag = Term.switch
        ~program:(fun _ -> Top)
        ~sub:(fun _ -> Sub)
        ~arg:(fun _ -> Arg)
        ~blk:(fun _ -> Blk)
        ~phi:(fun _ -> Phi)
        ~def:(fun _ -> Def)
        ~jmp:(fun _ -> Jmp)

    let () =
      let {total; nullable} : _ opaque = Opaque.newtype "term_t" in
      let _term_tag = enum "bap_term" sexp_of_enum all_of_enum in
      def "name" C.(total @-> returning OString.t) Term.name;
      def "clone" C.(total @-> returning total) Term.clone;
      ()

  end

  module Program = struct

  end

  module Project = struct
    let {total; nullable} : project opaque = Opaque.newtype "project_t"
    let project_t = total
    let project_or_null = nullable

    let def name typ impl =
      def ("project_" ^ name) typ impl

    module Input = struct
      let {total=t} : Project.input opaque = Opaque.newtype "project_input_t"

      let def name typ impl = def ("input_" ^ name) typ impl

      let () =
        def "file" C.(string @-> string_opt @-> returning t)
          (fun filename loader -> Project.Input.file ?loader ~filename);
    end

    let lift_error f x = match f x with
      | Ok x -> Some x
      | Error err -> Error.set err; None

    let () =
      def "create" C.(Input.t @-> returning nullable)
        (lift_error Project.create);

      def "arch" C.(project_t @-> returning Arch.total)
        Project.arch



  end
end
