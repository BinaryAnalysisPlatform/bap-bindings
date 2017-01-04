open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Format


include Self()

module Make( Internal : Cstubs_inverted.INTERNAL) =
struct
  module C = Ctypes

  module Opaque = struct
    open Ctypes

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

    let datatype name =
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
      Internal.typedef opaque_ptr name;
      view
        ~format_typ:(fun k ppf -> fprintf ppf "%s%t" name k)
        ~read ~write opaque_ptr

  end

  type 'a ctype = 'a Ctypes.typ

  let def name = Internal.internal ("bap_" ^ name)


  let standalone_init argc argv =
    try Plugins.run (); 0 with _ -> 1

  let version () = Config.version

  let () =
    def "version" C.(void @-> returning string) version;
    def "_standalone_init" C.(int @-> ptr string @-> returning int)
      standalone_init;

  module Endian = struct
    module T = struct
      type t = Word.endian
    end

    let t : endian ctype = Opaque.datatype "endian_t"
  end

  module Size = struct
    let t : size ctype = Opaque.datatype "size_t"
    let addr : addr_size ctype = Opaque.datatype "addr_size_t"
  end


  module Arch = struct
    let t : arch Ctypes.typ = Opaque.datatype "arch_t"
    let t_option : arch option Ctypes.typ = Opaque.datatype "arch_option_t"

    let def name typ impl =
      def ("arch_" ^ name) typ impl

    let () =
      def "endian" C.(t @-> returning Endian.t) Arch.endian;
      def "addr_size" C.(t @-> returning Size.addr) Arch.addr_size;
      def "create" C.(string @-> returning t_option) Arch.of_string;

      List.iter Arch.all ~f:(fun arch ->
          let name = Sexp.to_string (sexp_of_arch arch) in
          def name C.(void @-> returning t) (fun () -> arch))
  end

  module Project = struct
    let t : project ctype = Opaque.datatype "project_t"
    let project_t = t

    let def name typ impl =
      def ("project_" ^ name) typ impl

    module Input = struct
      let t : Project.input ctype = Opaque.datatype "project_input_t"

      let def name typ impl = def ("input_" ^ name) typ impl

      let () =
        def "file" C.(string @-> string_opt @-> returning t)
          (fun filename loader -> Project.Input.file ?loader ~filename)
    end
  end
end
