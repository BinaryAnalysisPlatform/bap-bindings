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

  module Opaque = struct
    open Ctypes

    type 'a t = 'a opaque

    type c = C

    let t : c structure typ = structure "bap_opaque_t"
    let tag = field t "bap_opaque_tag" int
    let value = field t "bap_opaque_value" (ptr void)
    let () = seal t

    let opaque_ptr = ptr t

    let registered = ref 0
    let typenames = Int.Table.create ~size:1024 ()

    let type_error name id =
      let got = match Hashtbl.find typenames id with
        | None -> "<unknown>"
        | Some name -> name in
      invalid_argf
        "Type error: expected a value of type %s, but got %s"
        name got ()

    let newtype name =
      let name = "bap_" ^ name in
      incr registered;
      let typeid = !registered in
      Hashtbl.set typenames ~key:typeid ~data:name;
      let finalise repr_ptr =
        Root.release (getf !@repr_ptr value) in
      let read repr_ptr =
          let repr = !@repr_ptr in
          let id = getf repr tag in
          if id <> typeid then type_error name id;
          Root.get (getf repr value) in
      let write oval =
        let repr = make t in
          setf repr tag typeid;
          setf repr value (Root.create oval);
          allocate ~finalise t repr in
      let read_opt ptr =
        if is_null ptr then None
        else read ptr in
      let write_opt = function
        | None -> from_voidp t null
        | Some oval -> write oval in
      Internal.typedef opaque_ptr name;
      let make_typ read write = view
          ~format_typ:(fun k ppf -> fprintf ppf "%s%t" name k)
          ~read ~write opaque_ptr in
      {
        total = make_typ read write;
        nullable = make_typ read_opt write_opt;
      }
  end

  type 'a ctype = 'a Ctypes.typ

  let def name = Internal.internal ("bap_" ^ name)


  let standalone_init argc argv =
    try Plugins.run (); 0 with _ -> 1

  let version () = Config.version

  module Error = struct
    let current_error : Error.t option ref = ref None

    let set err = current_error := Some err
    let clear () = current_error := None

    let () =
      def "error_get" C.(void @-> returning string)
        (fun () -> match current_error with
           | {contents=None} -> "unknown error (if any)"
           | {contents=Some err} -> Error.to_string_hum err);
  end

  let () =
    def "version" C.(void @-> returning string) version;
    def "_standalone_init" C.(int @-> ptr string @-> returning int)
      standalone_init

  let regular (type t) (module T : Regular.S with type t = t) t name =
    let def fn = def (name ^ "_" ^ fn) in
    def "to_string" C.(t @-> returning string) T.to_string;
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
