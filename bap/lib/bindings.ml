open Core_kernel.Std
open Bap.Std
open Regular.Std
open Graphlib.Std
open Bap_plugins.Std
open Format
module C = Ctypes

include Self()

let obj_addr x =
  Nativeint.shift_left (Nativeint.of_int (Obj.magic x)) 1

let name = None

let list_of_nullterminated_array p =
  let rec loop acc p =
    if C.is_null p then List.rev acc
    else match C.(!@p) with
      | None -> List.rev acc
      | Some s -> loop C.(s :: acc) C.(p +@ 1) in
  loop [] p


module Make( Internal : Cstubs_inverted.INTERNAL) =
struct

  module Enum : sig
    module type T = sig
      type t [@@deriving enumerate, compare, sexp]
    end
    type 'a t
    val define :
      ?invalid:string ->
      ?first:int -> (module T with type t = 'a) -> string -> 'a t
    val total : 'a t -> 'a C.typ
    val partial : 'a t -> 'a option C.typ

  end = struct
    module type T = sig
      type t [@@deriving enumerate, compare, sexp]
    end

    type 'a t = {
      total : 'a C.typ;
      partial : 'a option C.typ;
    }


    let define ?(invalid="invalid")
        ?(first=0) (type t) (module E: T with type t = t) name =
      assert (first >= 0);
      let compare x y = E.compare y x in
      let enum = Array.of_list E.all in
      Array.sort enum ~cmp:compare;
      let read i =
        Array.get enum (i - first) in
      let write t =
        match
          Array.binary_search ~compare enum `First_equal_to t
        with None -> assert false
           | Some x -> x + first in
      let read_opt i = if i = -1  then None else Some (read i) in
      let write_opt = function
        | None -> -1
        | Some x -> write x in
      let pref = String.uppercase ("bap_" ^ name) in
      let view read write =
        C.view ~format_typ:(fun k ppf -> fprintf ppf "enum bap_%s_t%t" name k)
          ~read ~write C.int in
      let cases = Array.to_list enum |> List.mapi  ~f:(fun i x ->
          let name = String.uppercase (Sexp.to_string (E.sexp_of_t x)) in
          pref ^ "_" ^ name, Int64.of_int (first + i)) in
      let invalid = String.uppercase invalid in
      let cases = (pref ^ "_" ^ invalid, Int64.of_int (~-1)) :: cases in
      let t = view read write in
      let partial = view read_opt write_opt in
      Internal.enum cases t;
      Internal.typedef t ("bap_" ^ name ^ "_t");
      {total = t; partial}

    let total t = t.total
    let partial t = t.partial
  end


  let def name = Internal.internal ("bap_" ^ name)

  module OString = struct
    (*
       Implementation details.
       =======================


       Value representation
       --------------------

       An OCaml string is copied to a CArray value, which is a
       custom block that contains a pointer to a malloced buffer.
       An explicit null character is added to the end of the array (so
       the CArray is one character longer than the original string).

       The CArray value is managed by the OCaml GC and is preserved
       with the hash table (called managed), that maps pointers
       (virtual memory addresses) to the CArrays.

       A pointer to the mallocated buffer is returned to the C
       side. When the `bap_release` function is called, a pointer
       passed to the function is used to remove a corresponding entry
       from the hashtable. Once it is removed, it should become
       unreachable and the memory will be collected as soon, as the GC
       comes to it.

       Although the type is mostly used to pass string data from OCaml
       to C, it maybe also used for the opposite side. We're not using
       this direction right now, but the Ctypes interfaces requires us
       to provide the implementation for the both sides. For
       simplicity we are requiring that a pointer passed from the
       C-side must be managed by the OString module. A runtime check
       ensures this. If the value is managed, then it will be copied
       to the string. In the general case we can remove this
       constraint and allow the C side to pass a data ownership to our
       side.


       Performance overhead
       --------------------

       Since we can't pass a pointer to data that is managed by OCaml,
       as the data can be moved by the GC, we have no other choices
       other than copying the data from the OCaml arena to the C
       (malloc) arena. So the performance cost is the cost of copying
       the data (plus an allocation of few custom blocks - the managed
       buffer itself, the nativeing pointer, and a few regular blocks,
       including the fat pointer, and an entry in the managed table).

       Although the performance implication will be also linear to the
       size of the buffer, the constant factor can be reduced by using
       Core's bigstrings instead of the CArray. Being bigarrays
       underneath the hood, they also store their data in the malloc
       arena, so we can safely pass a pointer to data. However, the
       copying is implemented more efficiently with the `memcpy`
       function, unlike a pure OCaml for-loop used for the
       CArray.
    *)
    module Pool = Nativeint.Table
    let managed = Pool.create ~size:1024 ()
    module Array = C.CArray

    let addr ptr =
      C.to_voidp ptr |>
      C.raw_address_of_ptr

    let is_managed ptr = Hashtbl.mem managed (addr ptr)

    let release ptr = Hashtbl.remove managed (addr ptr)

    let str_of_ptr ptr = C.string_of (C.ptr C.void) (C.to_voidp ptr)

    let expect_managed ptr =
      invalid_argf "Object at %s is not managed by OCaml"
        (str_of_ptr ptr) ()

    let size ptr =
      match Hashtbl.find managed (addr ptr) with
      | None -> expect_managed ptr
      | Some arr -> Array.length arr - 1

    let read ptr =
      match Hashtbl.find managed (addr ptr) with
      | None -> expect_managed ptr
      | Some arr ->
        String.init (Array.length arr - 1) ~f:(Array.get arr)

    let write str =
      let len = String.length str + 1 in
      let arr = Array.make C.char len in
      for i = 0 to len - 2 do
        arr.(i) <- str.[i];
      done;
      arr.(len - 1) <- '\x00';
      let ptr = Array.start arr in
      Hashtbl.add_exn managed ~key:(addr ptr) ~data:arr;
      ptr


    let read_opt ptr =
      if C.is_null ptr then None else Some (read ptr)
    let write_opt = function
      | None -> C.from_voidp C.char C.null
      | Some str -> write str

    let () = def "strlen" C.(ptr char @-> returning int) size

    let t : string C.typ = C.view ~read ~write (C.ptr C.char)
    let nullable : string option C.typ =
      C.view (C.ptr C.char) ~read:read_opt ~write:write_opt

    let spec = (t,nullable)
  end

  module Opaque : sig
    type 'a t

    val newtype : ?prefix:string -> ?suffix:string -> string -> 'a t
    val view : 'a t -> read:('a -> 'b) -> write:('b -> 'a) -> 'b t
    val total : 'a t -> 'a C.typ
    val nullable : 'a t -> 'a option C.typ
    val instanceof : base:'a t -> 'b t -> unit
    val typename : 'a t -> string
    val name : 'a t -> string
  end = struct
    open Ctypes

    type 'a t = {
      total  : 'a C.typ;
      nullable  : 'a option C.typ;
      instances : Int.Hash_set.t;
      id : int;
      base : string;
      prefix : string;
      suffix : string;
    }

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

    let nullpointer name =
      invalid_argf
        "Fatal error: an attempt to dereference a \
         null pointer of type %s" name ()

    let addr ptr = raw_address_of_ptr (to_voidp ptr)

    type cstruct = opaque_t structure

    let nullable t = t.nullable
    let total t = t.total
    let typename t = t.prefix ^ t.base ^ t.suffix
    let name t = t.base

    let newtype (type t) ?(prefix="bap_") ?(suffix="_t") base =
      incr registered;
      let name = prefix ^ base ^ suffix in
      let t : cstruct typ = structure name in
      let typeid = !registered in
      let null = from_voidp t null in
      Internal.typedef t name;
      Hashtbl.set typeinfo ~key:typeid ~data:{name};
      let instances = Int.Hash_set.create () in
      let is_instance id =
        id = typeid || Hash_set.mem instances id in
      let read (opaque : cstruct ptr) : t =
        if is_null opaque then nullpointer name;
        let {typeid=id; ovalue} = Root.get (to_voidp opaque) in
        if is_instance id then ovalue
        else type_error name id in
      let write (ovalue : t) : cstruct ptr =
        from_voidp t (Root.create {typeid; ovalue}) in
      let read_opt ptr = if is_null ptr then None else Some (read ptr) in
      let write_opt = Option.value_map ~f:write ~default:null in
      let view read write = view ~read ~write (ptr t) in
      {
        total = view read write;
        nullable = view read_opt write_opt;
        instances; id=typeid; base;
        prefix; suffix;
      }

    let instanceof ~base t =
      Hash_set.add base.instances t.id

    let view t ~read ~write = {
      t with
      total = C.view t.total ~read ~write;
      nullable = C.view t.nullable
          ~read:(Option.map ~f:read)
          ~write:(Option.map ~f:write);
    }

  end

  type 'a opaque = 'a Opaque.t

  let (!!) = Opaque.total
  let (!?) = Opaque.nullable

  type 'a ctype = 'a Ctypes.typ

  let release ptr =
    if not (C.is_null ptr) then
      if OString.is_managed ptr
      then OString.release ptr
      else C.Root.release ptr

  let () =
    def "release" C.(ptr void @-> returning void) release

  let load_plugins () =
    try Plugins.run (); 0 with _ -> 1

  let version () = Config.version

  module Error = struct
    let current_error : Error.t option ref = ref None

    let set err = current_error := Some err
    let clear () = current_error := None

    let failf fmt = Format.kfprintf (fun ppf ->
        let str = Format.flush_str_formatter () in
        let err = Error.of_string str in
        Error err) Format.str_formatter fmt

    let lift x = match x with
      | Ok x -> Some x
      | Error err -> set err; None

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
      def "error_clean" C.(void @-> returning void) clear;
  end

  let () =
    def "version" C.(void @-> returning OString.t) version;
    def "load_plugins" C.(void @-> returning int) load_plugins

  let fn = Foreign.funptr
  let fn_opt = Foreign.funptr_opt

  module Container = struct
    module type S0 = Container.S0
    module type S1 = Container.S1

    let apply hf seq f data = hf seq ~f:(fun elt -> f elt data)

    let instance
        (type t) (type elt)
        (module Seq : Container.S0 with type t = t and type elt = elt)
        seq elt =
      let visitor = C.(fn (!!elt @-> ptr void @-> returning void)) in
      let predicate = C.(fn (!!elt @-> ptr void @-> returning bool)) in
      let name = Opaque.name seq in
      let def fn = def (name ^ "_" ^ fn) in
      Internal.typedef visitor (name ^ "_visitor_t");
      Internal.typedef predicate (name ^ "_predicate_t");
      def "iter"
        C.(!!seq @-> visitor @-> ptr void @-> returning void)
        (apply Seq.iter);
      def "find"
        C.(!!seq @-> predicate @-> ptr void @-> returning !?elt)
        (apply Seq.find);
      def "exists"
        C.(!!seq @-> predicate @-> ptr void @-> returning bool)
        (apply Seq.exists);
      def "forall"
        C.(!!seq @-> predicate @-> ptr void @-> returning bool)
        (apply Seq.for_all);
      def "count"
        C.(!!seq @-> predicate @-> ptr void @-> returning int)
        (apply Seq.count);
      def "mem" C.(!!seq @-> !!elt @-> returning bool) Seq.mem;
  end

  module Seq = struct
    module Bap = Seq
    module Iter = struct
      type 'a t = {
        seq : 'a seq;
        mutable pos : ('a * 'a seq) option;
      }

      let create seq = {seq; pos = Seq.next seq}
      let value {pos} = Option.map ~f:fst pos
      let has_next {pos} = Option.is_some pos
      let reset iter = iter.pos <- Seq.next iter.seq
      let next iter =
        let res = value iter in
        Option.iter iter.pos ~f:(fun (_,rest) ->
            iter.pos <- Seq.next rest);
        res
    end

    type poly = Poly

    let t : poly seq opaque = Opaque.newtype "seq"

    let () =
      let def fn = def ("seq_" ^ fn) in
      def "is_empty" C.(!!t @-> returning bool) Seq.is_empty;
      def "length" C.(!!t @-> returning int) Seq.length;
      def "append" C.(!!t @-> !!t @-> returning !!t) Seq.append;
      def "sub" C.(!!t @-> int @-> int @-> returning !!t)
        (fun t pos len -> Seq.sub t ~pos ~len);
      def "take" C.(!!t @-> int @-> returning !!t) Seq.take;
      def "drop" C.(!!t @-> int @-> returning !!t) Seq.drop;
      ()

    let iterator name seq iter elt =
      let def fn = def (name ^ "_seq_iterator_" ^ fn) in
      def "create" C.(!!seq @-> returning !!iter) Iter.create;
      def "next" C.(!!iter @-> returning !?elt) Iter.next;
      def "value" C.(!!iter @-> returning !?elt) Iter.value;
      def "has_next" C.(!!iter @-> returning bool) Iter.has_next;
      def "reset" C.(!!iter @-> returning void) Iter.reset

    let instance (type t) (module T : T with type t = t) elt =
      let module Seq0 = struct
        type elt = t
        type t = elt seq
        include (Seq : Container.S1 with type 'a t := 'a seq)
      end in
      let name = Opaque.name elt in
      let seq : t seq opaque = Opaque.newtype (name ^ "_seq") in
      let iter :  t Iter.t opaque = Opaque.newtype (name ^ "_seq_iterator") in
      let def fn = def (name ^ "_seq_" ^ fn) in
      let mapper = fn C.(!!elt @-> ptr void @-> returning !!elt) in
      Internal.typedef mapper (name ^ "_seq_mapper");
      Opaque.instanceof ~base:t seq;
      Container.instance (module Seq0) seq elt;
      iterator name seq iter elt;
      def "map" C.(!!seq @-> mapper @-> ptr void @-> returning !!seq)
        (fun seq f data -> Seq.map seq ~f:(fun x -> f x data));
      def "view" C.(ptr !!elt @-> int @-> returning !!seq)
        begin fun ptr len ->
          let arr = C.CArray.from_ptr ptr len in
          Seq.init len ~f:(C.CArray.get arr)
        end;
      def "copy_from" C.(ptr !!elt @-> int @-> returning !!seq)
        (fun ptr len ->
           Seq.of_list C.CArray.(from_ptr ptr len |> to_list));
      seq
  end

  module Set = struct
    type a = A
    let t : a Hash_set.t opaque = Opaque.newtype "set"

    let () =
      let def fn = def ("set_" ^ fn) in
      def "length" C.(!!t @-> returning int) Hash_set.length;
      def "is_empty" C.(!!t @-> returning bool)
        (fun xs -> Hash_set.length xs = 0);
      def "copy" C.(!!t @-> returning !!t) Hash_set.copy;
      def "clear" C.(!!t @-> returning void) Hash_set.clear;
      def "equal" C.(!!t @-> !!t @-> returning bool) Hash_set.equal;
      def "diff" C.(!!t @-> !!t @-> returning !!t) Hash_set.diff


    let instance (type a) (module T : Regular.S with type t = a) elt seq =
      let module Set = struct
        type t = a Hash_set.t and elt = a
        include (struct
          include Hash_set
          let mem ?equal = mem
        end : Container.S1 with type 'a t := 'a Hash_set.t)
      end in
      let name = Opaque.name elt ^ "_set" in
      let base = t in
      let t = Opaque.newtype name in
      let def fn = def (name ^ "_" ^ fn) in
      let of_seq xs =
        let set = T.Hash_set.create () in
        Sequence.iter ~f:(Hash_set.add set) xs;
        set in
      Container.instance (module Set) t elt;
      Opaque.instanceof ~base t;
      def "create" C.(void @-> returning !!t) T.Hash_set.create;
      def "of_seq" C.(!!seq @-> returning !!t) of_seq;
      def "add" C.(!!t @-> !!elt @-> returning void) Hash_set.add;
      def "remove" C.(!!t @-> !!elt @-> returning void) Hash_set.remove;
      def "remove_if"
        C.(!!t @-> fn (!!elt @-> ptr void @-> returning bool) @->
           ptr void @-> returning void)
        (fun set f data -> Hash_set.filter_inplace set ~f:(fun x -> f x data));
      t
  end

  module Stdio = struct
    open Foreign
    type file = unit C.ptr

    type io = {
      stdin : file;
      stdout : file;
      stderr : file;
    } [@@deriving fields]

    let io = ref None

    let file : file C.typ = C.(ptr void)

    let get field = match !io with
      | None ->
        invalid_arg "BAP is not initialized.\n\
                     Please, call bap_init before any call to BAP functions"
      | Some io -> field io

    let stdin ()  : file = get stdin
    let stdout () : file = get stdout
    let stderr () : file = get stderr



    let fwrite = foreign "fwrite"
        C.(ptr void @-> size_t @-> size_t @-> file @-> returning size_t)

    let fputs = foreign "fputs"
        C.(string @-> file @-> returning int);;

    def "_stdio_init" C.(file @-> file @-> file @-> returning void)
      (fun stdin stdout stderr -> io := Some {stdin; stdout; stderr})
  end

  let size_t = Unsigned.Size_t.of_int

  module Printable = struct
    module Bap = Printable
    let instance (type t)
        (module T : Printable.S with type t = t) t =
      let name = Opaque.name t in
      let def fn = def (name ^ "_" ^ fn) in
      def "to_string"
        C.(!!t @-> returning OString.t) T.to_string;
      def "fprint"
        C.(!!t @-> Stdio.file @-> returning int)
        (fun t file -> Stdio.fputs (T.to_string t) file);
      def "print"
        C.(!!t @-> returning int)
        (fun t -> Stdio.fputs (T.to_string t) (Stdio.stdout ()));
      def "eprint"
        C.(!!t @-> returning int)
        (fun t -> Stdio.fputs (T.to_string t) (Stdio.stderr ()));
  end

  module Data = struct
    let buffer ptr len =
      C.bigarray_of_ptr C.array1 len Bigarray.char ptr

    let instance (type t) (module T : Data.S with type t = t) t =
      let name = Opaque.name t in
      let def fn = def (name ^ "_" ^ fn) in
      def "data_version" C.(void @-> returning OString.t) (fun () -> T.version);
      def "data_size" C.(!!t @-> returning size_t)
        (fun t -> size_t (T.size_in_bytes t));
      def "copy" C.(!!t @-> ptr char @-> int @-> returning void)
        (fun t ptr len -> T.blit_to_bigstring (buffer ptr len) t 0);
      def "of_bytes" C.(!!t @-> ptr char @-> int @-> returning !?t)
        (fun t ptr len ->
           try Some (T.of_bigstring (buffer ptr len)) with exn -> None);
      def "fwrite" C.(!!t @-> Stdio.file @-> returning size_t)
        (fun value file ->
           let buf = T.to_bigstring value in
           let len = Bigstring.length buf in
           let beg = C.(bigarray_start array1 buf) in
           Stdio.fwrite C.(to_voidp beg) (size_t 1) (size_t len) file);
      def "input" C.(string @-> returning !!t) (fun file -> T.Io.read file);
      def "output" C.(string @-> !!t @-> returning void)
        (fun file t -> T.Io.write file t);
      def "set_reader" C.(string @-> returning void)
        (fun name -> T.set_default_reader name);
      def "set_writer" C.(string @-> returning void)
        (fun name -> T.set_default_writer name);
      def "set_printer" C.(string @-> returning void)
        (fun name -> T.set_default_printer name);
  end

  module Regular = struct
    module Bap = Regular

    module Make(Spec : sig
        val name : string
        module T : Regular.S
      end) = struct
      open Spec

      let set_of_pset pset =
        let set = T.Hash_set.create () in
        T.Set.iter pset ~f:(Hash_set.add set);
        set

      let pset_of_set =
        Hash_set.fold ~init:T.Set.empty ~f:T.Set.add

      let t = Opaque.newtype name
      let seq = Seq.instance (module T) t
      let set = Set.instance (module T) t seq
      let list = Opaque.view seq
          ~write:Seq.Bap.of_list
          ~read:Seq.Bap.to_list

      let pset = Opaque.view set
          ~write:set_of_pset
          ~read:pset_of_set

      let def fn  = def (name ^ "_" ^ fn);;

      Data.instance (module T) t;
      Printable.instance (module T) t;
      def "compare" C.(!!t @-> !!t @-> returning int) T.compare;
      def "equal"   C.(!!t @-> !!t @-> returning bool) T.equal;
      def "hash"    C.(!!t @-> returning int) T.hash
    end
  end

  module Value = struct
    module Bap = Value
    type t = value
    include Regular.Make(struct
        let name = "value"
        module T = Value
      end)

    let () =
      def "tagname" C.(!!t @-> returning OString.t) Value.tagname


    module Dict = struct
      type t = Dict.t
      let t : t opaque = Opaque.newtype "value_dict"

      let get t = Fn.flip Dict.find t
      let set t = Fn.flip Dict.set t
      let add t = Fn.flip Dict.add t
      let rem t = Fn.flip Dict.remove t
      let has t = Fn.flip Dict.mem t
      let def fn = def ("dict_" ^ fn)

      let () =
        def "empty" C.(void @-> returning !!t) (fun () -> Dict.empty);
        def "is_empty" C.(!!t @-> returning bool) Dict.is_empty;
    end

  end

  module Color = struct
    module T = struct
      type t = [
        | `black
        | `red
        | `green
        | `yellow
        | `blue
        | `magenta
        | `cyan
        | `white
        | `gray
      ] [@@deriving enumerate, compare, sexp]
    end

    let t : color Enum.t = Enum.define (module T) "color"

  end

  module Endian = struct
    module T = struct
      type t = Word.endian =
        | LittleEndian
        | BigEndian
      [@@deriving compare, enumerate]
      let sexp_of_t = function
        | LittleEndian -> Sexp.Atom "little"
        | BigEndian -> Sexp.Atom "big"

      let t_of_sexp = function
        | Sexp.Atom "little" -> LittleEndian
        | Sexp.Atom "big" -> BigEndian
        | _ -> invalid_arg "endian_of_sexp: expected big | little"
    end
    let enum = Enum.define (module T) "endian"
    let t = Enum.total enum
    let partial = Enum.partial enum
  end

  (* small positive int *)
  module Unsigned = struct
    let t = C.int
    (* use minus one as a sentinel *)
    let partial = C.view t
        ~read:(fun x -> Option.some_if (x >= 0) x)
        ~write:(function None -> -1 | Some x -> x)
  end

  module Size = struct
    module T = Size
    let t = C.view C.int ~read:Size.of_int_exn ~write:Size.in_bits
    let partial = C.view C.int
        ~read:Size.of_int_opt
        ~write:(function
            | None -> -1
            | Some x -> Size.in_bits x)
  end

  module Word = struct
    include Regular.Make(struct
        type t = word
        let name = "word"
        module T = Word
      end)

    let fits_into_int64 x = Result.is_ok (Word.to_int64 x)

    let to_int64 x = match Word.to_int64 x with
      | Error _ -> Int64.minus_one
      | Ok x -> x

    let () =
      def "of_string" C.(string @-> returning !!t) Word.of_string;
      def "of_bool" C.(bool @-> returning !!t) Word.of_bool;
      def "of_int" C.(int @-> int @-> returning !!t)
        (fun width x -> Word.of_int ~width x);
      def "of_int32" C.(int @-> int32_t @-> returning !!t)
        (fun width x -> Word.of_int32 ~width x);
      def "of_int64" C.(int @-> int64_t @-> returning !!t)
        (fun width x -> Word.of_int64 ~width x);
      def "of_binary" C.(int @-> Endian.t @-> string @-> returning !!t)
        (fun width endian data -> Word.of_binary ~width endian data);
      def "fits_into_int64" C.(!!t @-> returning bool) fits_into_int64;
      def "to_int64" C.(!!t @-> returning int64_t) to_int64;
      def "signed" C.(!!t @-> returning !!t) Word.signed;
      def "is_zero" C.(!!t @-> returning bool) Word.is_zero;
      def "is_one" C.(!!t @-> returning bool) Word.is_one;
      def "bitwidth" C.(!!t @-> returning int) Word.bitwidth;
      def "extract" C.(int @-> int @-> !!t @-> returning !?t)
        (fun hi lo t -> Error.lift1 (Word.extract ~hi ~lo) t);
      def "concat" C.(!!t @-> !!t @-> returning !!t) Word.concat;
      def "succ" C.(!!t @-> returning !!t) Word.succ;
      def "pred" C.(!!t @-> returning !!t) Word.pred;
      def "nsucc" C.(!!t @-> int @-> returning !!t) Word.nsucc;
      def "npred" C.(!!t @-> int @-> returning !!t) Word.npred;
      let bop name = def name C.(!!t @-> !!t @-> returning !!t) in
      bop "add" Word.add;
      bop "sub" Word.sub;
      bop "mul" Word.mul;
      bop "div" Word.div;
      bop "modulo" Word.modulo;
      bop "logand" Word.logand;
      bop "logor" Word.logor;
      bop "logxor" Word.logxor;
      bop "lshift" Word.lshift;
      bop "rshift" Word.rshift;
      bop "arshift" Word.arshift;
      let uop name = def name C.(!!t @-> returning !!t) in
      uop "abs" Word.abs;
      uop "neg" Word.neg;
      uop "lnot" Word.lnot;
  end


  module Arch = struct
    let t = Enum.define (module Arch) "arch"
    let total = Enum.total t
    let partial = Enum.partial t

    let size arch = (Arch.addr_size arch :> Size.T.all)

    let () =
      let def fn = def ("arch_" ^ fn) in
      def "endian" C.(total @-> returning Endian.t) Arch.endian;
      def "addr_size" C.(total @-> returning Size.t) size ;
      def "to_string" C.(total @-> returning OString.t) Arch.to_string ;
      def "of_string" C.(string @-> returning partial) Arch.of_string;
  end

  module Type = struct
    include Regular.Make(struct
        type t = Type.t
        let name = "type"
        module T = Type
      end)
    module Tag = struct
      type t = [`Imm | `Mem] [@@deriving enumerate, sexp, compare]
    end

    let enum = Enum.define (module Tag) "type_tag"
    let tag_t = Enum.total enum

    let to_tag = function
      | Type.Imm _ -> `Imm
      | Type.Mem _ -> `Mem

    let imm_size = function
      | Type.Imm x -> Some x
      | _ -> None

    let addr_size = function
      | Type.Mem (x,_) -> Some (Size.T.in_bits x)
      | _ -> None

    let value_size = function
      | Type.Mem (_,x) -> Some (Size.T.in_bits x)
      | _ -> None

    let make_mem addr_size size =
      let open Option in
      Size.T.addr_of_int_opt addr_size >>= fun addr_size ->
      Size.T.of_int_opt size >>| fun size ->
      Type.Mem (addr_size, size)

    let make_imm n = Type.Imm n

    let () =
      def "tag" C.(!!t @-> returning tag_t) to_tag;
      def "imm_size" C.(!!t @-> returning Unsigned.partial) imm_size;
      def "addr_size" C.(!!t @-> returning Unsigned.partial) addr_size;
      def "value_size" C.(!!t @-> returning Unsigned.partial) value_size;
      def "create_mem" C.(int @-> int @-> returning !?t) make_mem;
      def "create_imm" C.(int @-> returning !!t) make_imm;
  end

  module Var = struct
    module Bap = Var
    include Regular.Make(struct
        type t = var
        let name = "var"
        module T = Var
      end)

    let () =
      def "name" C.(!!t @-> returning OString.t) Var.name;
      def "type" C.(!!t @-> returning !!Type.t) Var.typ;
      def "is_physical" C.(!!t @-> returning bool) Var.is_physical;
      def "is_virtual" C.(!!t @-> returning bool) Var.is_virtual;
      def "base" C.(!!t @-> returning !!t) Var.base;
      def "same" C.(!!t @-> !!t @-> returning bool) Var.same;
      def "index" C.(!!t @-> returning int) Var.index;
      def "with_index" C.(!!t @-> int @-> returning !!t) Var.with_index;
      def "create" C.(string @-> !!Type.t @-> bool @-> bool @-> returning !!t)
        (fun name typ is_virtual fresh ->
           Var.create ~is_virtual ~fresh name typ)
  end

  module Exp = struct
    include Regular.Make(struct
        type t = exp
        let name = "exp"
        module T = Exp
      end)

    module Tag = struct
      type t = [
        | `Load | `Store | `BinOp | `UnOp | `Var | `Int
        | `Cast | `Let | `Unknown | `Ite | `Extract | `Concat
      ] [@@deriving sexp, enumerate, compare]
    end

    (* XXX: the following will be removed after we will
       add enumerate to corresponding definitions in bap.mli *)

    module Cast = struct
      type t = Bil.cast =
        | UNSIGNED
        | SIGNED
        | HIGH
        | LOW
      [@@deriving enumerate, compare, sexp]
    end

    module Binop = struct
      type t = Bil.binop =
        | PLUS
        | MINUS
        | TIMES
        | DIVIDE
        | SDIVIDE
        | MOD
        | SMOD
        | LSHIFT
        | RSHIFT
        | ARSHIFT
        | AND
        | OR
        | XOR
        | EQ
        | NEQ
        | LT
        | LE
        | SLT
        | SLE
      [@@deriving enumerate, compare, sexp]
    end

    module Unop = struct
      type t = Bil.unop =
        | NEG
        | NOT
      [@@deriving enumerate, compare, sexp]
    end

    let tag = Enum.define (module Tag) "exp_tag"
    let cast = Enum.define (module Cast) "cast"
    let binop = Enum.define (module Binop) "binop"
    let unop = Enum.define (module Unop) "unop"

    let decompose t = Enum.total t, Enum.partial t

    let tag_t,tag_p = decompose tag
    let cast_t,cast_p = decompose cast
    let binop_t,binop_p = decompose binop
    let unop_t,unop_p = decompose unop

    let to_tag = function
      | Bil.Load _ -> `Load
      | Bil.Store _ -> `Store
      | Bil.BinOp _ -> `BinOp
      | Bil.UnOp _ -> `UnOp
      | Bil.Var _ -> `Var
      | Bil.Int _ -> `Int
      | Bil.Cast _ -> `Cast
      | Bil.Let _ -> `Let
      | Bil.Unknown _ -> `Unknown
      | Bil.Ite _ -> `Ite
      | Bil.Extract _ -> `Extract
      | Bil.Concat _ -> `Concat

    let mem = function
      | Bil.Load (mem,_,_,_)
      | Bil.Store (mem,_,_,_,_) -> Some mem
      | _ -> None

    let addr = function
      | Bil.Load (_,x,_,_)
      | Bil.Store (_,x,_,_,_) -> Some x
      | _ -> None

    let endian = function
      | Bil.Load (_,_,x,_)
      | Bil.Store (_,_,_,x,_) -> Some x
      | _ -> None

    let size = function
      | Bil.Load (_,_,_,x)
      | Bil.Store (_,_,_,_,x) -> Some x
      | _ -> None

    let exp = function
      | Bil.UnOp (_,x)
      | Bil.Cast (_,_,x)
      | Bil.Let (_,_,x)
      | Bil.Extract (_,_,x) -> Some x
      | _ -> None

    let var = function
      | Bil.Var x | Bil.Let (x,_,_) -> Some x
      | _ -> None

    let lhs = function
      | Bil.BinOp (_,x,_)
      | Bil.Ite (_,x,_)
      | Bil.Concat (x,_) -> Some x
      | _ -> None

    let rhs = function
      | Bil.BinOp (_,_,x)
      | Bil.Ite (_,_,x)
      | Bil.Concat (_,x)
      | Bil.Let (_,x,_)
      | Bil.Store (_,_,x,_,_) -> Some x
      | _ -> None

    let unop = function
      | Bil.UnOp (op,_) -> Some op
      | _ -> None

    let binop = function
      | Bil.BinOp (op,_,_) -> Some op
      | _ -> None

    let cast = function
      | Bil.Cast (ct,_,_) -> Some ct
      | _ -> None

    let cast_size = function
      | Bil.Cast (_,x,_) -> Some x
      | _ -> None

    let extract_lobit = function
      | Bil.Extract (x,_,_) -> Some x
      | _ -> None

    let extract_hibit = function
      | Bil.Extract (_,x,_) -> Some x
      | _ -> None

    let value = function
      | Bil.Int x -> Some x
      | _ -> None

    let unknown_msg = function
      | Bil.Unknown (x,_) -> x
      | _ -> ""

    let unknown_type = function
      | Bil.Unknown (_,x) -> Some x
      | _ -> None

    let create_load mem addr endian size =
      Bil.load ~mem ~addr endian size

    let create_store mem addr exp endian size =
      Bil.store ~mem ~addr exp endian size


    let () =
      let proj p = C.(!!t @-> returning !?p) in
      def "tag" C.(!!t @-> returning tag_t) to_tag;
      def "mem" (proj t) mem;
      def "addr" (proj t) addr;
      def "endian" C.(!!t @-> returning Endian.partial) endian;
      def "size" C.(!!t @-> returning Size.partial) size;
      def "exp" (proj t) exp;
      def "var" (proj Var.t) var;
      def "lhs" (proj t) lhs;
      def "rhs" (proj t) rhs;
      def "unop" C.(!!t @-> returning unop_p) unop;
      def "binop" C.(!!t @-> returning binop_p) binop;
      def "cast" C.(!!t @-> returning cast_p) cast;
      def "value"  (proj Word.t) value;
      def "cast_size" C.(!!t @-> returning Unsigned.partial) cast_size;
      def "extract_hibit"
        C.(!!t @-> returning Unsigned.partial) extract_hibit;
      def "extract_lobit"
        C.(!!t @-> returning Unsigned.partial) extract_lobit;
      def "create_load"
        C.(!!t @-> !!t @-> Endian.t @-> Size.t @-> returning !!t)
        create_load;
      def "unknown_msg" C.(!!t @-> returning OString.t) unknown_msg;
      def "unknown_typ" C.(!!t @-> returning !?Type.t) unknown_type;
      def "create_store"
        C.(!!t @-> !!t @-> !!t @-> Endian.t @-> Size.t @-> returning !!t)
        create_store;
      def "create_binop"
        C.(binop_t @-> !!t @-> !!t @-> returning !!t) Bil.binop;
      def "create_unop"
        C.(unop_t @-> !!t @-> returning !!t) Bil.unop;
      def "create_var" C.(!!Var.t @-> returning !!t) Bil.var;
      def "create_int" C.(!!Word.t @-> returning !!t) Bil.int;
      def "create_cast" C.(cast_t @-> int @-> !!t @-> returning !!t) Bil.cast;
      def "create_let" C.(!!Var.t @-> !!t @-> !!t @-> returning !!t) Bil.let_;
      def "create_unknown" C.(string @-> !!Type.t @-> returning !!t) Bil.unknown;
      def "create_ite" C.(!!t @-> !!t @-> !!t @-> returning !!t)
        (fun if_ then_ else_ -> Bil.ite ~if_ ~then_ ~else_);
      def "create_extract" C.(int @-> int @-> !!t @-> returning !!t)
        (fun lo hi x -> Bil.extract ~lo ~hi x);
      def "create_concat" C.(!!t @-> !!t @-> returning !!t) Bil.concat ;
      def "fold_consts" C.(!!t @-> returning !!t) Exp.(fixpoint fold_consts);
      def "free_vars" C.(!!t @-> returning !!Var.pset) Exp.free_vars;
  end

  module Stmt = struct
    include Regular.Make(struct
        type t = stmt
        let name = "stmt"
        module T = Stmt
      end)

    module Tag = struct
      type t =
        [`Move | `Jmp | `Special | `While | `If | `CpuExn]
      [@@deriving compare, sexp, enumerate]
    end

    let tag = Enum.define (module Tag) "stmt_tag"
    let tag_t = Enum.total tag

    let to_tag = function
      | Bil.Move _ -> `Move
      | Bil.Jmp _ -> `Jmp
      | Bil.Special _ -> `Special
      | Bil.While (_,_) -> `While
      | Bil.If (_,_,_) -> `If
      | Bil.CpuExn _ -> `CpuExn

    let exp = function
      | Bil.Move (_,x)
      | Bil.Jmp x
      | Bil.While (x,_)
      | Bil.If (x,_,_) -> Some x
      | _ -> None

    let var = function
      | Bil.Move (x,_) -> Some x
      | _ -> None

    let stmts = function
      | Bil.While (_,x) -> Some (Seq.Bap.of_list x)
      | _ -> None

    let true_stmts = function
      | Bil.If (_,x,_) -> Some (Seq.Bap.of_list x)
      | _ -> None

    let false_stmts = function
      | Bil.If (_,_,x) -> Some (Seq.Bap.of_list x)
      | _ -> None

    let cpuexn = function
      | Bil.CpuExn n -> Some n
      | _ -> None

    let jmp = function
      | Bil.Jmp x -> Some x
      | _ -> None

    let () =
      def "tag" C.(!!t @-> returning tag_t) to_tag;
      def "exp" C.(!!t @-> returning !?Exp.t) exp;
      def "var" C.(!!t @-> returning !?Var.t) var;
      def "stmts" C.(!!t @-> returning !?seq) stmts;
      def "true_stmts" C.(!!t @-> returning !?seq) true_stmts;
      def "false_stmts" C.(!!t @-> returning !?seq) false_stmts;
      def "cpuexn" C.(!!t @-> returning Unsigned.partial) cpuexn;
      def "jmp" C.(!!t @-> returning !?Exp.t) jmp;
      def "create_move" C.(!!Var.t @-> !!Exp.t @-> returning !!t) Bil.move;
      def "create_jmp" C.(!!Exp.t @-> returning !!t) Bil.jmp;
      def "create_special" C.(string @-> returning !!t) Bil.special;
      def "create_if" C.(!!Exp.t @-> !!seq @-> !!seq @-> returning !!t)
        (fun exp xs ys -> Bil.if_ exp (Seq.Bap.to_list xs) (Seq.Bap.to_list ys));
      def "create_cpuexn" C.(int @-> returning !!t) Bil.cpuexn;
      def "is_referenced" C.(!!Var.t @-> !!t @-> returning bool)
        Stmt.is_referenced;
      def "fold_consts" C.(!!t @-> returning !?t)
        (fun s -> List.hd (Bil.fixpoint Bil.fold_consts [s]));
      def "free_vars" C.(!!t @-> returning !!Var.pset) Stmt.free_vars;

  end

  module Tid = struct
    include Regular.Make(struct
        let name = "tid"
        module T = Tid
      end)

    let () =
      def "create" C.(void @-> returning !!t) Tid.create;
      def "set_name" C.(!!t @-> string @-> returning void) Tid.set_name;
      def "name" C.(!!t @-> returning OString.t) Tid.name;
      def "from_string" C.(string @-> returning !?t)
        (Error.lift1 Tid.from_string)
  end

  module Bil = struct
    let t  = Stmt.list

    let def fn = def ("bil_" ^ fn);;

    Internal.typedef !!t "bap_bil_t";;

    Data.instance (module Bil) t;
    Printable.instance (module Bil) t;

    def "is_referenced"
      C.(!!Var.t @-> !!t @-> returning bool)
      Bil.is_referenced;

    def "is_assigned"
      C.(!!Var.t @-> !!t @-> returning bool)
      (fun var bil -> Bil.is_assigned var bil);

    def "is_strictly_assigned"
      C.(!!Var.t @-> !!t @-> returning bool)
      (fun var bil -> Bil.is_assigned ~strict:true var bil);

    def "substitute"
      C.(!!Exp.t @-> !!Exp.t @-> !!t @-> returning !!t)
      Bil.substitute;

    def "substitute_var"
      C.(!!Var.t @-> !!Exp.t @-> !!t @-> returning !!t)
      Bil.substitute_var;

    def "free_vars"
      C.(!!t @-> returning !!Var.pset) Bil.free_vars;

    def "fold_consts" C.(!!t @-> returning !!t)
      Bil.(fixpoint fold_consts)
  end

  module Reg = struct
    let code = Reg.code
    include Regular.Make(struct
        module T = Reg
        let name = "reg"
      end);;

    def "code" C.(!!t @-> returning int) Reg.code;
    def "name" C.(!!t @-> returning OString.t) Reg.name;
  end

  module Op = struct
    module Tag = struct
      type t = [`Reg | `Imm | `Fmm]
      [@@deriving enumerate, compare, sexp]
    end

    let enum = Enum.define (module Tag) "op_tag"
    let tag = Enum.total enum

    let to_tag = function
      | Op.Reg _ -> `Reg
      | Op.Imm _ -> `Imm
      | Op.Fmm _ -> `Fmm

    let to_float = function
      | Op.Fmm x -> Fmm.to_float x
      | _ -> 0.0

    let to_int64 = function
      | Op.Imm x -> Imm.to_int64 x
      | _ -> 0L

    let to_reg = function
      | Op.Reg x -> Some x
      | _ -> None

    let to_reg_code = function
      | Op.Reg x -> Reg.code x
      | _ -> -1


    include Regular.Make(struct
        module T = Op
        let name = "op"
      end);;

    def "tag" C.(!!t @-> returning tag) to_tag;
    def "to_float" C.(!!t @-> returning float) to_float;
    def "to_int64" C.(!!t @-> returning int64_t) to_int64;
    def "to_reg" C.(!!t @-> returning !?Reg.t) to_reg;
    def "to_reg_code" C.(!!t @-> returning int) to_reg_code;
  end

  module Memory = struct
    module Bap = Memory
    let t : mem opaque = Opaque.newtype "memory"

    let def fn = def ("memory_" ^ fn);;

    def "create"
      C.(Endian.t @-> !!Word.t @-> ptr char @-> int @-> returning !?t)
      (fun endian addr data len ->
         let ba = C.bigarray_of_ptr C.array1 len Bigarray.char data in
         Error.lift1 (Memory.create endian addr) ba);

    def "of_file"
      C.(Endian.t @-> !!Word.t @-> string @-> returning !?t)
      (fun endian -> Error.lift2 (Memory.of_file endian));

    def "view"
      C.(!!t @-> !!Word.t @-> int @-> returning !?t)
      (fun t from words -> Error.lift (Memory.view ~from ~words t));

    def "merge"
      C.(!!t @-> !!t @-> returning !?t)
      (Error.lift2 Memory.merge);

    def "endian" C.(!!t @-> returning Endian.t) Memory.endian;

    def "max_addr" C.(!!t @-> returning !!Word.t) Memory.max_addr;
    def "min_addr" C.(!!t @-> returning !!Word.t) Memory.min_addr;
    def "length" C.(!!t @-> returning int) Memory.length;

    def "data" C.(!!t @-> returning (ptr char)) begin fun mem ->
      let buff = Memory.to_buffer mem in
      let base = Bigsubstring.base buff in
      let base_ptr = C.bigarray_start C.array1 base in
      C.(base_ptr +@ Bigsubstring.pos buff)
    end
  end

  module Memmap = struct
    module Make(Value : sig
        type t
        val t : t opaque
      end) = struct
      module Binding = struct
        module T = struct type t = mem * Value.t end
        type t = T.t
        let name = "mem_" ^ Opaque.name Value.t ^ "_pair"
        let t : t opaque = Opaque.newtype name
        let seq : t seq opaque = Seq.instance (module T) t;;
        def (name ^ "_fst") C.(!!t @-> returning !!Memory.t) fst;
        def (name ^ "_snd") C.(!!t @-> returning !!Value.t) snd;
      end

      module Memmap0 = struct
        type elt = Value.t
        type t = elt memmap
        include (Memmap : Container.S1 with type 'a t := 'a memmap)
      end

      let name = "mem_" ^ Opaque.name Value.t ^ "_map"

      let t : Value.t memmap opaque = Opaque.newtype name;;

      let def fn = def (name ^ "_" ^ fn);;

      Container.instance (module Memmap0) t Value.t;;

      def "empty" C.(void @-> returning !!t) (fun () -> Memmap.empty);
      def "add" C.(!!t @-> !!Memory.t @-> !!Value.t @-> returning !!t)
        Memmap.add;
      def "dominators" C.(!!t @-> !!Memory.t @-> returning !!Binding.seq)
        Memmap.dominators;
      def "intersections" C.(!!t @-> !!Memory.t @-> returning !!Binding.seq)
        Memmap.intersections;
      def "intersects" C.(!!t @-> !!Memory.t @-> returning bool)
        Memmap.intersects;
      def "dominates" C.(!!t @-> !!Memory.t @-> returning bool)
        Memmap.dominates;
      def "contains" C.(!!t @-> !!Word.t @-> returning bool)
        Memmap.contains;
      def "lookup" C.(!!t @-> !!Word.t @-> returning !!Binding.seq)
        Memmap.lookup;
      def "enum" C.(!!t @-> returning !!Binding.seq) Memmap.to_sequence;
    end
    module Value = Make(Value)
    module Unit = Make(struct
        type t = unit
        let t : t opaque = Opaque.newtype "unit"
      end)
  end
  module Image = struct
    module Bap = Image
    module Segment = struct
      module Seg = Image.Segment
      include Regular.Make(struct
          module T = Seg
          let name = "segment"
        end);;

      def "name" C.(!!t @-> returning OString.t) Seg.name;
      def "is_writable" C.(!!t @-> returning bool) Seg.is_writable;
      def "is_readable" C.(!!t @-> returning bool) Seg.is_readable;
      def "is_exectutable" C.(!!t @-> returning bool) Seg.is_executable;
    end

    module Symbol = struct
      module Sym = Image.Symbol
      include Regular.Make(struct
          module T = Sym
          let name = "symbol"
        end);;

      def "name" C.(!!t @-> returning OString.t) Sym.name;
      def "is_function" C.(!!t @-> returning bool) Sym.is_function;
      def "is_debug" C.(!!t @-> returning bool) Sym.is_debug;
    end

    let t : image opaque = Opaque.newtype "image"

    let warnings = ref []

    let lift_result res = match Error.lift res with
      | None -> None
      | Some (t,warns) -> warnings := warns; Some t


    let def fn = def ("image_" ^ fn)
    ;;

    def "create" C.(string @-> string_opt @-> returning !?t)
      (fun path backend -> lift_result (Image.create ?backend path));

    def "of_data"
      C.(ptr char @-> int @-> string_opt @-> returning !?t )
      begin fun data len backend ->
        let ba = C.bigarray_of_ptr C.array1 len Bigarray.char data in
        lift_result (Image.of_bigstring ?backend ba)
      end;

    def "entry_point" C.(!!t @-> returning !!Word.t) Image.entry_point;
    def "filename" C.(!!t @-> returning OString.nullable) Image.filename;
    def "arch" C.(!!t @-> returning Arch.total) Image.arch;
    def "length" C.(!!t @-> returning int)
      (fun img -> Bigstring.length (Image.data img));;
    def "data" C.(!!t @-> returning (ptr char))
      (fun img -> C.bigarray_start C.array1 (Image.data img));
    def "memory" C.(!!t @-> returning !!Memmap.Value.t) Image.memory;
    def "memory_of_segment"
      C.(!!t @-> !!Segment.t @-> returning !!Memory.t)
      Image.memory_of_segment;
    def "is_symbol_contiguous"
      C.(!!t @-> !!Symbol.t @-> returning bool)
      (fun img sym -> Seq.Bap.is_empty (snd (Image.memory_of_symbol img sym)));
    def "memory_of_contiguous_symbol"
      C.(!!t @-> !!Symbol.t @-> returning !!Memory.t)
      (fun img sym -> fst (Image.memory_of_symbol img sym));
    def "symbols_of_segment"
      C.(!!t @-> !!Segment.t @-> returning !!Symbol.seq)
      Image.symbols_of_segment;
    def "segment_of_symbol"
      C.(!!t @-> !!Symbol.t @-> returning !!Segment.t)
      Image.segment_of_symbol;
    def "segments" C.(!!t @-> returning !!Segment.seq)
      (fun img -> Image.segments img |> Table.elements);
    def "symbols" C.(!!t @-> returning !!Symbol.seq)
      (fun img -> Image.symbols img |> Table.elements);
  end

  module Insn = struct
    module Bap = Insn
    include Regular.Make(struct
        module T = Insn
        let name = "insn"
      end);;

    def "name" C.(!!t @-> returning OString.t) Insn.name;
    def "asm" C.(!!t @-> returning OString.t) Insn.asm;
    def "bil" C.(!!t @-> returning !!Bil.t) Insn.bil;
    def "ops" C.(!!t @-> returning !!Op.seq)
      (fun insn -> Seq.Bap.of_array (Insn.ops insn));
    def "ops_length" C.(!!t @-> returning int)
      (fun insn -> Array.length (Insn.ops insn));
    def "ops_nth" C.(!!t @-> int @-> returning !?Op.t)
      (fun insn i ->
         let ops = Insn.ops insn in
         if i < Array.length ops then Some ops.(i) else None);
    def "is_jump" C.(!!t @-> returning bool) Insn.(is jump);
    def "is_conditional" C.(!!t @-> returning bool) Insn.(is conditional);
    def "is_indirect" C.(!!t @-> returning bool) Insn.(is indirect);
    def "is_call" C.(!!t @-> returning bool) Insn.(is call);
    def "is_return" C.(!!t @-> returning bool) Insn.(is return);
    def "may_affect_control_flow" C.(!!t @-> returning bool)
      Insn.(may affect_control_flow );
    def "may_load" C.(!!t @-> returning bool) Insn.(may load);
    def "may_store" C.(!!t @-> returning bool) Insn.(may store);
  end

  module Edge_kind = struct
    module Tag = struct
      type t = [`Tree | `Back | `Cross | `Forward]
      [@@deriving enumerate, compare, sexp]
    end
    let tag : edge_kind Enum.t =
      Enum.define (module Tag) "graph_edge_kind"
    let t = Enum.total tag
  end

  module Graph(Spec : sig
      module G : Graph
      val namespace : string
      val node : G.node opaque
      val edge : G.edge opaque
      val nodes : G.node seq opaque
      val edges : G.edge seq opaque
      val node_label : G.Node.label C.typ
      val edge_label : G.Edge.label C.typ
    end) = struct
    open Spec

    let t : G.t opaque = Opaque.newtype namespace
    let g = !!t
    let n = !!node
    let e = !!edge
    let nl = node_label
    let el = edge_label

    let def fn = def (namespace ^ "_" ^ fn)

    module Dfs = struct
      type params = Params

      let s = C.(ptr void)
      let params : params C.structure ctype =
        C.structure (namespace ^ "_visitor")

      let (%:) name typ =
        C.field params name (fn_opt typ)

      let start_tree =
        "start_tree" %: C.(n @-> s @-> returning void)
      let enter_node =
        "enter_node" %: C.(int @-> n @-> s @-> returning void)
      let leave_node =
        "leave_node" %: C.(int @-> n @-> s @-> returning void)
      let enter_edge =
        "enter_edge" %: C.(Edge_kind.t @-> e @-> s @-> returning void)
      let leave_edge =
        "leave_edge" %: C.(Edge_kind.t @-> e @-> s @-> returning void)

      let () =
        C.seal params;
        Internal.structure params;
    end

    module Node = struct
      let def fn = def ("node_" ^ fn);;
      def "create" C.(nl @-> returning n) G.Node.create;
      def "label" C.(n @-> returning nl) G.Node.label;
      def "mem" C.(n @-> g @-> returning bool) G.Node.mem;
      def "succs" C.(n @-> g @-> returning !!nodes) G.Node.succs;
      def "preds" C.(n @-> g @-> returning !!nodes) G.Node.preds;
      def "inputs" C.(n @-> g @-> returning !!edges) G.Node.inputs;
      def "outputs" C.(n @-> g @-> returning !!edges) G.Node.outputs;
      def "in_degree" C.(n @-> g @-> returning int) (G.Node.degree ~dir:`In);
      def "out_degree" C.(n @-> g @-> returning int) (G.Node.degree ~dir:`Out);
      def "insert" C.(n @-> g @-> returning g) G.Node.insert;
      def "update" C.(n @-> nl @-> g @-> returning g) G.Node.update;
      def "remove" C.(n @-> g @-> returning g) G.Node.remove;
      def "has_edge" C.(n @-> n @-> g @-> returning bool) G.Node.has_edge;
      def "edge" C.(n @-> n @-> g @-> returning !?edge) G.Node.edge;
    end;;

    module Edge = struct
      let def fn = def ("edge_" ^ fn);;
      def "create" C.(n @-> n @-> el @-> returning e) G.Edge.create;
      def "label" C.(e @-> returning el) G.Edge.label;
      def "src" C.(e @-> returning n) G.Edge.src;
      def "dst" C.(e @-> returning n) G.Edge.dst;
      def "mem" C.(e @-> g @-> returning bool) G.Edge.mem;
      def "insert" C.(e @-> g @-> returning g) G.Edge.insert;
      def "update" C.(e @-> el @-> g @-> returning g) G.Edge.update;
      def "remove" C.(e @-> g @-> returning g) G.Edge.remove;
    end;;

    def "empty" C.(void @-> returning g) (fun () -> G.empty);
    def "nodes" C.(g @-> returning !!nodes) G.nodes;
    def "edges" C.(g @-> returning !!edges) G.edges;

    def "union" C.(g @-> g @-> returning g)
      (Graphlib.union (module G));

    def "inter" C.(g @-> g @-> returning g)
      (Graphlib.inter (module G));

    def "dfs"
      C.(g @-> ptr Dfs.params @-> ptr void @->
         bool @-> !?node @-> returning void)
      begin fun g vis_ptr data rev start ->
        let open Dfs in
        let vis = C.(!@vis_ptr) in
        let (<~) fld f = Option.iter (C.getf vis fld) ~f in
        let start_tree node () = start_tree <~ fun f -> f node data in
        let enter_node n node () = enter_node <~ fun f -> f n node data in
        let leave_node n node () = leave_node <~ fun f -> f n node data in
        let enter_edge n edge () = enter_edge <~ fun f -> f n edge data in
        let leave_edge n edge () = leave_edge <~ fun f -> f n edge data in
        Graphlib.depth_first_search (module G)
          ~rev ?start
          ~start_tree ~enter_node ~leave_node
          ~enter_edge ~leave_edge g ~init:()
      end;

    def "get_nodes_in_postorder"
      C.(g @-> bool @-> !?node @-> returning !!nodes)
      (fun g rev start ->
         Graphlib.postorder_traverse (module G) ?start ~rev g);

    def "get_nodes_in_reverse_postorder"
      C.(g @-> bool @-> !?node @-> returning !!nodes)
      (fun g rev start ->
         Graphlib.reverse_postorder_traverse (module G) ?start ~rev g);

    def "is_reachable"
      C.(g @-> bool @-> n @-> n @-> returning bool)
      (fun g rev n1 n2 -> Graphlib.is_reachable (module G) g ~rev n1 n2);

    def "visit_reachable"
      C.(g @-> bool @-> n @->
         fn (n @-> ptr void @-> returning void) @->
         ptr void @->
         returning void)
      begin fun g rev n f data ->
        Graphlib.fold_reachable ~rev (module G) ~init:() ~f:(fun () n ->
            f n data) g n
      end;
  end

  module Edge = struct
    module Tag = struct
      type t = [`Jump | `Cond | `Fall]
      [@@deriving enumerate, compare, sexp]
    end
    let tag : edge Enum.t = Enum.define (module Tag) "edge"
    let t = Enum.total tag
  end

  module Code = struct
    module T = struct
      type t = mem * insn

      include Printable.Bap.Make(struct
          type nonrec t = t
          let module_name = None
          let pp ppf (mem,insn) =
            Format.fprintf ppf "%a\t\t%a" Memory.Bap.pp mem Insn.Bap.pp insn
        end)
    end
    type t = T.t
    let t : t opaque = Opaque.newtype "code"
    let seq : t seq opaque = Seq.instance (module T) t;;
    let list = Opaque.view seq
          ~write:Seq.Bap.of_list
          ~read:Seq.Bap.to_list;;

    Printable.instance (module T) t;
    def "code_mem" C.(!!t @-> returning !!Memory.t) fst;
    def "code_insn" C.(!!t @-> returning !!Insn.t) snd;
    def "code_length" C.(!!t @-> returning int)
      (fun (mem,_) -> Memory.Bap.length mem);
    def "code_addr" C.(!!t @-> returning int64_t)
      (fun (mem,_) -> Memory.Bap.min_addr mem |> Addr.to_int64 |> function
         | Error _ -> 0L
         | Ok n -> n)

  end


  module Block = struct
    let t = Opaque.newtype "block"
    let seq = Seq.instance (module Block) t
    let def fn = def ("block_" ^ fn);;

    def "create" C.(!!Memory.t @-> !!Code.list @-> returning !!t)
      Block.create;
    def "addr" C.(!!t @-> returning !!Word.t) Block.addr;
    def "memory" C.(!!t @-> returning !!Memory.t) Block.memory;
    def "leader" C.(!!t @-> returning !!Insn.t) Block.leader;
    def "terminator" C.(!!t @-> returning !!Insn.t) Block.terminator;
    def "insns" C.(!!t @-> returning !!Code.list) Block.insns;
  end

  module Cfg = struct
    include Graph(struct
      module G = Graphs.Cfg
      let namespace = "cfg"
      let node = Block.t
      let nodes = Block.seq
      let edge : G.edge opaque = Opaque.newtype "cfg_edge"
      let edges = Seq.instance (module G.Edge) edge
      let edge_label = Edge.t
      let node_label_opaque : G.Node.label opaque =
        Opaque.newtype "cfg_node_label"
      let node_label = !!node_label_opaque
    end)
  end

  module Source = struct
    let rooter
      : rooter source opaque = Opaque.newtype "rooter_source"
    let brancher
      : brancher source opaque = Opaque.newtype "brancher_source"
    let symbolizer
      : symbolizer source opaque = Opaque.newtype "symbolizer_source"
    let reconstructor
      : reconstructor source opaque =
      Opaque.newtype "reconstructor_source"

    let factory
        (type t) (module Factory : Source.Factory.S with type t = t)
        typ name =
      let def fn x = def (name ^ "_factory_" ^ fn) x in
      def "find" C.(string @-> returning !?typ) Factory.find
  end

  module Rooter = struct
    let t : rooter opaque = Opaque.newtype "rooter"
    let def fn = def ("rooter_" ^ fn);;
    Source.factory (module Rooter.Factory) Source.rooter "rooter";
    def "create" C.(!!Word.seq @-> returning !!t) Rooter.create;
    def "roots" C.(!!t @-> returning !!Word.seq) Rooter.roots;
    def "union" C.(!!t @-> !!t @-> returning !!t) Rooter.union;
  end

  module Brancher = struct
    let t : brancher opaque = Opaque.newtype "brancher";;
    Source.factory (module Brancher.Factory) Source.brancher "brancher"
  end

  module Symbolizer = struct
    let t : symbolizer opaque = Opaque.newtype "symbolizer";;
    Source.factory (module Symbolizer.Factory) Source.symbolizer "symbolizer"
  end

  module Reconstructor = struct
    let t : reconstructor opaque = Opaque.newtype "reconstructor";;
    Source.factory (module Reconstructor.Factory) Source.reconstructor "reconstructor"
  end

  module Disasm = struct
    let t : disasm opaque = Opaque.newtype "disasm"
    let insn = Disasm.insn

    type params = Params

    let params : params C.structure ctype =
      C.structure "bap_disasm_parameters_t"

    let params_field name typ =
      C.field params ("bap_disasm_" ^ name) typ

    let rooter_p = params_field "rooter" !?Rooter.t
    let brancher_p = params_field "brancher" !?Brancher.t
    let backend_p = params_field "backend" C.string_opt

    let () = C.seal params
    let () = Internal.structure params

    let params_get fld t =
      if C.is_null t then None
      else C.getf C.(!@t) fld

    let def fn = def ("disasm_" ^ fn);;


    def "of_cfg" C.(!!Cfg.t @-> returning !!t) Disasm.create;

    def "of_mem"
      C.(Arch.total @-> !!Memory.t @-> ptr params @-> returning !?t)
      begin fun arch mem params ->
        Disasm.of_mem
          ?rooter:(params_get rooter_p params)
          ?brancher:(params_get brancher_p params)
          ?backend:(params_get backend_p params) arch mem |>
        Error.lift
      end;

    def "of_image"
      C.(!!Image.t @-> ptr params @-> returning !?t)
      begin fun image params ->
        Disasm.of_image
          ?rooter:(params_get rooter_p params)
          ?brancher:(params_get brancher_p params)
          ?backend:(params_get backend_p params) image |>
        Error.lift
      end;

    def "of_file"
      C.(string @-> ptr params @-> string_opt @-> returning !?t)
      begin fun name params loader ->
        Disasm.of_file
          ?loader
          ?rooter:(params_get rooter_p params)
          ?brancher:(params_get brancher_p params)
          ?backend:(params_get backend_p params) name |>
        Error.lift
      end;

    def "cfg" C.(!!t @-> returning !!Cfg.t) Disasm.cfg;
    def "merge" C.(!!t @-> !!t @-> returning !!t) Disasm.merge;
    def "code" C.(!!t @-> returning !!Code.seq) Disasm.insns;
    def "insns" C.(!!t @-> returning !!Insn.seq)
      Seq.Bap.(fun d -> Disasm.insns d >>| snd);


    module Basic = struct
      module Dis = struct
        open Bap.Std
        module Dis = Disasm_expert.Basic
        include struct
          open Dis
          type t = {
            lifter  : lifter;
            disasm  : (empty, empty) Dis.t;
            endian  : endian;
            awidth  : int;
          }
        end

        open Or_error.Monad_infix

        let lifter_of_arch arch =
          let module Target = (val target_of_arch arch) in
          Target.lift

        let unknown_semantics = Error.failf "unknown semantics"
        let unknown _ _ = unknown_semantics

        let default_backend () = match Dis.available_backends () with
          | [] -> Error.failf "no disassemblers are available"
          | x :: _ -> Ok x

        let create arch =
          default_backend () >>= fun backend ->
          Dis.create ~backend (Arch.to_string arch) >>| fun disasm -> {
            disasm; lifter = lifter_of_arch arch;
            endian = Arch.endian arch;
            awidth = Size.in_bits (Arch.addr_size arch);
          }

        let create_expert target backend cpu awidth endian debug_level =
          let backend = match backend with
            | Some backend -> Ok backend
            | None -> default_backend () in
          backend >>= fun backend ->
          Dis.create ~debug_level ?cpu ~backend target >>| fun disasm ->
          match Arch.of_string target with
          | None -> {
              disasm;
              lifter = unknown;
              awidth;
              endian = match endian with
                | None -> LittleEndian
                | Some endian -> endian
            }
          | Some arch -> {
              disasm;
              lifter = lifter_of_arch arch;
              endian = Arch.endian arch;
              awidth = Size.in_bits (Arch.addr_size arch);
            }

        let close {disasm} = Dis.close disasm

        let next {disasm; awidth; lifter; endian} ptr len addr =
          let data = C.bigarray_of_ptr C.array1 len Bigarray.char ptr in
          let addr = Addr.of_int64 ~width:awidth addr in
          Memory.create endian addr data >>= Dis.insn_of_mem disasm >>= function
          | _,None,_ -> Error.failf "no instruction"
          | mem,Some insn,_ -> Or_error.return @@ match lifter mem insn with
            | Ok bil  -> mem, Insn.of_basic ~bil insn
            | Error _ -> mem, Insn.of_basic insn
      end

      let t = Opaque.newtype "disasm_basic"
      let def fn = def ("basic_" ^ fn);;

      def "create" C.(Arch.total @-> returning !?t)
        (Error.lift1 Dis.create);

      def "create_expert" C.(string @->
                             string_opt @->
                             string_opt @->
                             int @->
                             Endian.partial @->
                             int @-> returning !?t)
      (fun t b c a e d -> Error.lift (Dis.create_expert t b c a e d));

      def "next" C.(!!t @-> ptr char @-> int @-> int64_t @-> returning !?Code.t)
        (fun d ptr len addr -> Dis.next d ptr len addr |> Error.lift);

      def "close" C.(!!t @-> returning void) Dis.close;
    end

  end

  module Term = struct
    module Bap = Term
    type enum =
        Top | Sub | Arg | Blk | Def | Phi | Jmp
    [@@deriving sexp, enumerate]

    let tag = Term.switch
        ~program:(fun _ -> Top)
        ~sub:(fun _ -> Sub)
        ~arg:(fun _ -> Arg)
        ~blk:(fun _ -> Blk)
        ~phi:(fun _ -> Phi)
        ~def:(fun _ -> Def)
        ~jmp:(fun _ -> Jmp)

    type a = A

    let t : a term opaque = Opaque.newtype "term"
    let term = t

    module Attr = struct
      type t = a term
      let t = term
      let get t = Fn.flip Term.get_attr t
      let set t = Fn.flip Term.set_attr t
      let has t = Fn.flip Term.has_attr t
      let rem t = Fn.flip Term.del_attr t
    end

    let () =
      let def fn = def ("term_" ^ fn) in
      def "name" C.(!!t @-> returning OString.t) Term.name;
      def "clone" C.(!!t @-> returning !!t) Term.clone;
      def "tid" C.(!!t @-> returning !!Tid.t) Term.tid;
      def "same" C.(!!t @-> !!t @-> returning bool) Term.same;
      def "attrs" C.(!!t @-> returning !!Value.Dict.t) Term.attrs;
      ()

    module type Expressible = sig
      type t
      val free_vars : t -> Var.Bap.Set.t
      val map_exp : t -> f:(exp -> exp) -> t
      val substitute : t -> exp -> exp -> t
    end

    let expressible (type t) (module Term : Expressible with type t = t) t =
      let def fn =  def (Opaque.name t ^ "_" ^ fn) in
      def "map_exp"
        C.(!!t @-> fn (!!Exp.t @-> ptr void @-> returning !!Exp.t)
           @-> ptr void @-> returning !!t)
        (fun jmp f data -> Term.map_exp jmp ~f:(fun exp -> f exp data));
      def "free_vars" C.(!!t @-> returning !!Var.pset) Term.free_vars;
      def "substitute"
        C.(!!t @-> !!Exp.t @-> !!Exp.t @-> returning !!t)
        Term.substitute


    let parentof ~child cls t =
      let def fn = def (String.concat ~sep:"_" [
          Opaque.name t; Opaque.name child; fn
        ]) in
      def "length" C.(!!t @-> returning int) (Term.length cls);
      def "find" C.(!!t @-> !!Tid.t @-> returning !?child) (Term.find cls);
      def "update" C.(!!t @-> !!child @-> returning !!t) (Term.update cls);
      def "remove" C.(!!t @-> !!Tid.t @-> returning !!t) (Term.remove cls);
      def "first"  C.(!!t @-> returning !?child) (Term.first cls);
      def "last"   C.(!!t @-> returning !?child) (Term.last cls);
      def "next"   C.(!!t @-> !!Tid.t @-> returning !?child) (Term.next cls);
      def "prev"   C.(!!t @-> !!Tid.t @-> returning !?child) (Term.prev cls);
      def "nth"    C.(!!t @-> int @-> returning !?child) (Term.nth cls);
      def "change"
        C.(!!t @-> !!Tid.t @-> fn (!?child @-> returning !?child) @-> returning !!t)
        (Term.change cls);
      def "append" C.(!!t @-> !!child @-> returning !!t) (Term.append cls);
      def "append_after" C.(!!t @-> !!child @-> !!Tid.t @-> returning !!t)
        (fun p c after -> Term.append cls ~after p c);
      def "prepend" C.(!!t @-> !!child @-> returning !!t) (Term.prepend cls);
      def "prepend_before" C.(!!t @-> !!child @-> !!Tid.t @-> returning !!t)
        (fun p c before -> Term.prepend cls ~before p c);
      ()

    let enumerator ~elt ~seq cls t =
      let def fn = def (String.concat ~sep:"_" ([
          Opaque.name t;
          Opaque.name elt ^ "s";
        ] @ (if String.is_empty fn then [] else [fn]))) in
      def "" C.(!!t @-> returning !!seq) (Term.enum cls);
      def "after" C.(!!t @-> !!Tid.t @-> returning !!seq) (Term.after cls);
      def "after_rev" C.(!!t @-> !!Tid.t @-> returning !!seq)
        (Term.after cls ~rev:true);
      def "before" C.(!!t @-> !!Tid.t @-> returning !!seq) (Term.before cls);
      def "before_rev" C.(!!t @-> !!Tid.t @-> returning !!seq)
        (Term.before cls ~rev:true);
      ()

    module Make(Spec : sig
        type t
        val name : string
        module T : Regular.Bap.S with type t = t term
      end) = struct
      include Regular.Make(Spec);;
      Opaque.instanceof ~base:term t;

    end
  end

  module Label = struct
    module Tag = struct
      type t = [`Direct | `Indirect]
      [@@deriving enumerate, sexp, compare]
    end

    let tag = Enum.define (module Tag) "label_tag"
    let tag_t = Enum.total tag
    let t : label opaque = Opaque.newtype "label"

    let to_tag = function
      | Indirect _ -> `Indirect
      | Direct _ -> `Direct

    let tid = function
      | Direct tid -> Some tid
      | _ -> None

    let target = function
      | Indirect exp -> Some exp
      | _ -> None

    let () =
      def "tag" C.(!!t @-> returning tag_t) to_tag;
      def "tid" C.(!!t @-> returning !?Tid.t) tid;
      def "target" C.(!!t @-> returning !?Exp.t) target;
      def "create_indirect" C.(!!Exp.t @-> returning !!t)
        (fun exp -> Indirect exp);
      def "create_direct" C.(!!Tid.t @-> returning !!t)
        (fun exp -> Direct exp)
  end


  module Call = struct
    module Bap = Call
    include Regular.Make(struct
        let name = "call"
        module T = Call
      end)

    let t : call opaque = Opaque.newtype "call"

    let () =
      def "create" C.(!!Label.t @-> !!Label.t @-> returning !!t)
        (fun target return -> Call.create ~return ~target ());
      def "create_no_return" C.(!!Label.t @-> returning !!t)
        (fun target -> Call.create ~target ());
      def "target" C.(!!t @-> returning !!Label.t) Call.target;
      def "return" C.(!!t @-> returning !?Label.t) Call.return;
      def "with_target" C.(!!t @-> !!Label.t @-> returning !!t)
        Call.with_target;
      def "with_return" C.(!!t @-> !!Label.t @-> returning !!t)
        Call.with_return;
      def "with_noreturn" C.(!!t @-> returning !!t)
        Call.with_noreturn;
  end

  module Interrupt = struct
    let t : (int * tid) opaque = Opaque.newtype "interrupt"

    let def fn = def ("interrupt_" ^ fn)
    let () =
      def "number" C.(!!t @-> returning int) fst;
      def "return" C.(!!t @-> returning !!Tid.t) snd;
  end

  module Jmp = struct
    include Term.Make(struct
        type t = jmp and p = blk
        let name = "jmp" and cls = jmp_t
        module T = Jmp
      end)

    module Kind = struct
      module Tag = struct
        type t = [`Call | `Goto | `Ret | `Int]
        [@@deriving enumerate, compare, sexp]
      end
      let enum = Enum.define (module Tag) "jmp_kind"
      let t = Enum.total enum
      let t_opt = Enum.partial enum
    end

    let kind jmp = match Jmp.kind jmp with
      | Call _ -> `Call
      | Goto _ -> `Goto
      | Ret _ -> `Ret
      | Int _ -> `Int

    let target jmp = match Jmp.kind jmp with
      | Call call -> Some (Call.Bap.target call)
      | Goto dst | Ret dst -> Some dst
      | _ -> None

    let to_call jmp = match Jmp.kind jmp with
      | Call call -> Some call
      | _ -> None

    let to_interrupt jmp = match Jmp.kind jmp with
      | Int (n,ret) -> Some (n,ret)
      | _ -> None
    ;;

    Term.expressible (module Jmp) t;;
    def "kind" C.(!!t @-> returning Kind.t) kind;;
    def "target" C.(!!t @-> returning !?Label.t) target;;
    def "to_call" C.(!!t @-> returning !?Call.t) to_call;;
    def "to_interrupt" C.(!!t @-> returning !?Interrupt.t)
      to_interrupt;;
    def "create_call"
      C.(!!Call.t @-> !?Tid.t @-> !?Exp.t @-> returning !!t)
      (fun call tid cond -> Jmp.create_call ?tid ?cond call);;
    def "create_goto"
      C.(!!Label.t @-> !?Tid.t @-> !?Exp.t @-> returning !!t)
      (fun dst tid cond -> Jmp.create_goto ?tid ?cond dst);;
    def "create_ret"
      C.(!!Label.t @-> !?Tid.t @-> !?Exp.t @-> returning !!t)
      (fun dst tid cond -> Jmp.create_ret ?tid ?cond dst);;
    def "create_int"
      C.(int @-> !!Tid.t @-> !?Tid.t @-> !?Exp.t @-> returning !!t)
      (fun n ret tid cond -> Jmp.create_int ?tid ?cond n ret);;
    def "exps" C.(!!t @-> returning !!Exp.seq) Jmp.exps;;
    def "cond" C.(!!t @-> returning !!Exp.t) Jmp.cond;;
    def "with_cond" C.(!!t @-> !!Exp.t @-> returning !!t) Jmp.with_cond;;
  end

  module Def = struct
    include Term.Make(struct
        type t = def and p = blk
        let name = "def" and cls = def_t
        module T = Def
      end);;

    Term.expressible (module Def) t;;
    def "create" C.(!!Var.t @-> !!Exp.t @-> !?Tid.t @-> returning !!t)
      (fun var exp tid -> Def.create ?tid var exp);;
    def "lhs" C.(!!t @-> returning !!Var.t) Def.lhs;;
    def "rhs" C.(!!t @-> returning !!Exp.t) Def.rhs;;
  end

  module Phi = struct
    include Term.Make(struct
        type t = phi and p = blk
        let name = "phi" and cls = phi_t
        module T = Phi
      end);;

    Term.expressible (module Phi) t;;

    def "create" C.(!!Var.t @-> !!Tid.t @-> !!Exp.t @-> !?Tid.t @->
                   returning !!t)
      (fun var inc exp tid -> Phi.create ?tid var inc exp);;
    def "lhs" C.(!!t @-> returning !!Var.t) Phi.lhs;;
    def "with_lhs" C.(!!t @-> !!Var.t @-> returning !!t) Phi.with_lhs;
    def "update" C.(!!t @-> !!Tid.t @-> !!Exp.t @-> returning !!t) Phi.update;
    def "select" C.(!!t @-> !!Tid.t @-> returning !?Exp.t) Phi.select;
    def "select_or_unknown" C.(!!t @-> !!Tid.t @-> returning !!Exp.t) Phi.select_or_unknown;
    def "remove" C.(!!t @-> !!Tid.t @-> returning !!t) Phi.remove;
    def "incomming" C.(!!t @-> returning !!Tid.seq)
      Seq.Bap.(fun t -> Phi.values t >>| fst);
    def "exps" C.(!!t @-> returning !!Exp.seq)
      Seq.Bap.(fun t -> Phi.values t >>| snd)
  end

  module Blk = struct
    include Term.Make(struct
        type t = blk and p = sub
        let name = "blk" and cls = blk_t
        module T = Blk
      end);;

    let blk = t

    module Pair = struct
      let t : (blk term * blk term) opaque = Opaque.newtype "blk_pair";;
      def "blk_pair_fst" C.(!!t @-> returning !!blk) fst;
      def "blk_pair_snd" C.(!!t @-> returning !!blk) snd;
    end;;

    module Expressible = struct
      include Blk
      let map_exp t ~f = map_exp t ~f
      let substitute t e g = substitute t e g
    end;;

    Term.expressible (module Expressible) t;;
    Term.parentof ~child:Phi.t phi_t t;;
    Term.parentof ~child:Def.t def_t t;;
    Term.parentof ~child:Jmp.t jmp_t t;;
    Term.enumerator ~elt:Phi.t ~seq:Phi.seq phi_t t;;
    Term.enumerator ~elt:Def.t ~seq:Def.seq def_t t;;
    Term.enumerator ~elt:Jmp.t ~seq:Jmp.seq jmp_t t;;

    def "create" C.(!?Tid.t @-> returning !!t) (fun tid -> Blk.create ?tid ());;
    def "lift" C.(!!Cfg.t @-> !!Block.t @-> returning !!list) Blk.lift;
    def "from_insn" C.(!!Insn.t @-> returning !!list) Blk.from_insn;
    def "split_before" C.(!!t @-> !!Def.t @-> returning !!Pair.t) Blk.split_before;
    def "split_after" C.(!!t @-> !!Def.t @-> returning !!Pair.t) Blk.split_after;
    def "split_top" C.(!!t @-> returning !!Pair.t) Blk.split_top;
    def "split_bot" C.(!!t @-> returning !!Pair.t) Blk.split_bot;
    def "split_while"
      C.(!!t @->
         fn (!!Def.t @-> ptr void @-> returning bool) @->
         ptr void @-> returning !!Pair.t)
      (fun blk f data -> Blk.split_while blk ~f:(fun def -> f def data));
    def "defines_var" C.(!!t @-> !!Var.t @-> returning bool) Blk.defines_var;
    def "uses_var" C.(!!t @-> !!Var.t @-> returning bool) Blk.uses_var ;

    module Builder = struct
      let t : Blk.Builder.t opaque = Opaque.newtype "blk_builder"
      let def fn = def ("builder_" ^ fn);;
      def "create" C.(!?Tid.t @-> returning !!t) (fun tid ->
          Blk.Builder.create ?tid ());
      def "add_phi" C.(!!t @-> !!Phi.t @-> returning void) Blk.Builder.add_phi;
      def "add_def" C.(!!t @-> !!Def.t @-> returning void) Blk.Builder.add_def;
      def "add_jmp" C.(!!t @-> !!Jmp.t @-> returning void) Blk.Builder.add_jmp;
      def "result" C.(!!t @-> returning !!blk) Blk.Builder.result;
    end

  end

  module Arg = struct
    module Bap = Arg
    include Term.Make(struct
        type t = arg and p = sub
        let name = "arg" and cls = arg_t
        module T = Arg
      end)

    module Intent = struct
      module Tag = struct
        type t = intent = In | Out | Both
        [@@deriving enumerate, compare, sexp]
      end
      let enum = Enum.define (module Tag) ~invalid:"unknown" "intent"
      let t = Enum.partial enum
    end;;

    def "create"
      C.(!!Var.t @-> !!Exp.t @-> Intent.t @-> !?Tid.t @-> returning !!t)
      (fun var exp intent tid -> Arg.create ?tid ?intent var exp);
    def "lhs" C.(!!t @-> returning !!Var.t) Arg.lhs;
    def "rhs" C.(!!t @-> returning !!Exp.t) Arg.rhs;
    def "intent" C.(!!t @-> returning Intent.t) Arg.intent;
    def "with_intent" C.(!!t @-> Intent.t @-> returning !!t)
      (fun t intent -> match intent with
         | None -> Arg.with_unknown_intent t
         | Some intent -> Arg.with_intent t intent)
  end

  module Tidgraph = Graph(struct
      module G = Graphs.Tid
      let namespace = "tidgraph"
      let node = Tid.t
      let nodes = Tid.seq
      let node_label = !!Tid.t
      let edge : G.edge opaque = Opaque.newtype "tidgraph_edge"
      let edges = Seq.instance (module G.Edge) edge
      let edge_label = !!Tid.t
    end)

  module Irgraph = struct
    include Graph(struct
        module G = Graphs.Ir
        let namespace = "irgraph"
        let node : G.node opaque = Opaque.newtype "irgraph_node"
        let nodes = Seq.instance (module G.Node) node
        let edge : G.edge opaque = Opaque.newtype "irgraph_edge"
        let edges = Seq.instance (module G.Edge) edge
        let node_label = !!Blk.t
        let opaque_edge_label : G.Edge.label opaque =
          Opaque.newtype "irgraph_edge_label"
        let edge_label = !!opaque_edge_label
      end)

    module G = Graphs.Ir

    module Edge_extra = struct
      open Edge;;
      def "jmps_before" C.(e @-> g @-> returning !!Jmp.seq)
        (G.Edge.jmps `before);
      def "jmps_after" C.(e @-> g @-> returning !!Jmp.seq)
        (G.Edge.jmps `after);
      def "jmp" C.(e @-> returning !!Jmp.t) G.Edge.jmp;
      def "tid" C.(e @-> returning !!Tid.t) G.Edge.tid;
      def "cond" C.(e @-> g @-> returning !!Exp.t) G.Edge.cond;
    end
  end

  module Sub = struct
    module Bap = Sub
    include Term.Make(struct
        type t = sub and p = program
        let name = "sub" and cls = sub_t
        module T = Sub
      end);;
    Term.parentof ~child:Arg.t arg_t t;
    Term.parentof ~child:Blk.t blk_t t;
    Term.enumerator ~elt:Arg.t ~seq:Arg.seq arg_t t;;
    Term.enumerator ~elt:Blk.t ~seq:Blk.seq blk_t t;;
    def "lift"
      C.(!!Block.t @-> !!Cfg.t @-> returning !!t) Sub.lift;
    def "name" C.(!!t @-> returning OString.t) Sub.name;
    def "with_name" C.(!!t @-> string @-> returning !!t)
      Sub.with_name;
    def "ssa" C.(!!t @-> returning !!t) Sub.ssa;
    def "is_ssa" C.(!!t @-> returning bool) Sub.is_ssa;
    def "free_vars" C.(!!t @-> returning !!Var.pset) Sub.free_vars;
    def "to_graph" C.(!!t @-> returning !!Tidgraph.t) Sub.to_graph;
    def "to_cfg" C.(!!t @-> returning !!Irgraph.t) Sub.to_cfg;
    def "of_cfg" C.(!!Irgraph.t @-> returning !!t) Sub.of_cfg;

    module Builder = struct
      let sub = t
      let t : Sub.Builder.t opaque = Opaque.newtype "sub_builder"
      let def fn = def ("builder_" ^ fn);;
      def "create" C.(!?Tid.t @-> string_opt @-> returning !!t)
        (fun tid name -> Sub.Builder.create ?tid ?name ());
      def "add_arg" C.(!!t @-> !!Arg.t @-> returning void)
        Sub.Builder.add_arg;
      def "add_blk" C.(!!t @-> !!Blk.t @-> returning void)
        Sub.Builder.add_blk;
    end
  end

  module Callgraph = struct
    include Graph(struct
        module G = Graphs.Callgraph
        let namespace = "callgraph"
        let node = Tid.t
        let nodes = Tid.seq
        let edge : G.edge opaque = Opaque.newtype "callgraph_edge"
        let edges = Seq.instance (module G.Edge) edge
        let node_label = !!Tid.t
        let edge_label = !!Jmp.list
      end)
  end

  module Symtab = struct
    module Fn = struct
      module T = struct
        type t = string * block * cfg
      end
      let t : T.t opaque = Opaque.newtype "symbtab_fn";;

      let seq = Seq.instance (module T) t;;
      let list = Opaque.view seq
          ~write:Seq.Bap.of_list
          ~read:Seq.Bap.to_list;;

      def "symtab_fn_name" C.(!!t @-> returning OString.t) fst3;
      def "symtab_fn_entry" C.(!!t @-> returning !!Block.t) snd3;
      def "symtab_fn_graph" C.(!!t @-> returning !!Cfg.t) trd3;
      def "symtab_fn_span" C.(!!t @-> returning !!Memmap.Unit.t) Symtab.span;
    end

    let fn = Fn.t
    let t : symtab opaque = Opaque.newtype "symbtab"

    let def fn = def ("symbtab_" ^ fn);;
    def "empty" C.(void @-> returning !!t) (fun () -> Symtab.empty);
    def "add_symbol" C.(!!t @-> !!fn @-> returning !!t) Symtab.add_symbol;
    def "remove" C.(!!t @-> !!fn @-> returning !!t) Symtab.remove;
    def "find_by_name" C.(!!t @-> string @-> returning !?fn) Symtab.find_by_name;
    def "find_by_start" C.(!!t @-> !!Word.t @-> returning !?fn) Symtab.find_by_start;
    def "owners" C.(!!t @-> !!Word.t @-> returning !!Fn.list) Symtab.owners;
    def "dominators" C.(!!t @-> !!Memory.t @-> returning !!Fn.list) Symtab.dominators;
    def "intersecting" C.(!!t @-> !!Memory.t @-> returning !!Fn.list) Symtab.intersecting;
    def "enum" C.(!!t @-> returning !!Fn.seq) Symtab.to_sequence;
  end

  module Program = struct
    include Term.Make(struct
        type t = program
        let name = "program"
        module T = Program
      end);;

    Term.parentof ~child:Sub.t sub_t t;
    Term.enumerator ~elt:Sub.t ~seq:Sub.seq sub_t t;

    def "create" C.(!?Tid.t @-> returning !!t)
      (fun tid -> Program.create ?tid ());
    def "lift" C.(!!Symtab.t @-> returning !!t) Program.lift;
    def "to_graph" C.(!!t @-> returning !!Callgraph.t)
      Program.to_graph;
    def "lookup_sub" C.(!!t @-> !!Tid.t @-> returning !?Sub.t)
      (Program.lookup sub_t);
    def "lookup_blk" C.(!!t @-> !!Tid.t @-> returning !?Blk.t)
      (Program.lookup blk_t);
    def "lookup_arg" C.(!!t @-> !!Tid.t @-> returning !?Arg.t)
      (Program.lookup arg_t);
    def "lookup_phi" C.(!!t @-> !!Tid.t @-> returning !?Phi.t)
      (Program.lookup phi_t);
    def "lookup_def" C.(!!t @-> !!Tid.t @-> returning !?Def.t)
      (Program.lookup def_t);
    def "lookup_jmp" C.(!!t @-> !!Tid.t @-> returning !?Jmp.t)
      (Program.lookup jmp_t);
    def "parentof_blk" C.(!!t @-> !!Tid.t @-> returning !?Sub.t)
      (Program.parent blk_t);
    def "parentof_arg" C.(!!t @-> !!Tid.t @-> returning !?Sub.t)
      (Program.parent arg_t);
    def "parentof_phi" C.(!!t @-> !!Tid.t @-> returning !?Blk.t)
      (Program.parent phi_t);
    def "parentof_def" C.(!!t @-> !!Tid.t @-> returning !?Blk.t)
      (Program.parent def_t);
    def "parentof_jmp" C.(!!t @-> !!Tid.t @-> returning !?Blk.t)
      (Program.parent jmp_t);


    module Builder = struct
      let program = t
      let t : Program.Builder.t opaque =
        Opaque.newtype "program_builder"
      let def fn = def ("builder_" ^ fn);;
      def "create" C.(!?Tid.t @-> returning !!t)
        (fun tid -> Program.Builder.create ?tid ());
      def "add_sub" C.(!!t @-> !!Sub.t @-> returning void)
        Program.Builder.add_sub;
      def "result" C.(!!t @-> returning !!program) Program.Builder.result;
    end


  end

  module Project = struct
    module Bap = Project
    let t : project opaque = Opaque.newtype "project"
    let proj_t = !!t
    let proj_opt = !?t

    let def name typ impl =
      def ("project_" ^ name) typ impl

    type params = Params

    let params : params C.structure ctype =
      C.structure "bap_project_parameters_t"

    let params_field name typ =
      C.field params name typ

    let rooter_p = params_field "rooter" !?Source.rooter
    let brancher_p = params_field "brancher" !?Source.brancher
    let symbolizer_p = params_field "symbolizer" !?Source.symbolizer
    let reconstructor_p = params_field "reconstructor" !?Source.reconstructor
    let disassembler_p = params_field "disassember" C.string_opt

    let () = C.seal params
    let () = Internal.structure params

    module Attr = struct
      type t = project
      let t = t
      let get t = Fn.flip Project.get t
      let set t = Fn.flip Project.set t
      let has t = Fn.flip Project.has t
      let rem tag t = failwith "not implemented"
    end

    module Input = struct
      let t : Project.input opaque = Opaque.newtype "project_input"

      let def name typ impl = def ("input_" ^ name) typ impl;;

      def "file" C.(string @-> string_opt @-> returning !!t)
        (fun filename loader -> Project.Input.file ?loader ~filename);
      def "binary"
        C.(string @-> Arch.total @-> !?Word.t @-> returning !!t)
        (fun filename arch base ->
           Project.Input.binary ?base ~filename arch);
      def "register_loader"
        C.(string @-> fn (string @-> returning !!t) @-> returning void)
        Project.Input.register_loader;
    end

    let create input params =
      let (~@) fld =
        if C.is_null params then None
        else C.getf C.(!@params) fld in
      Project.create
        ?disassembler:~@disassembler_p
        ?brancher:~@brancher_p
        ?symbolizer:~@symbolizer_p
        ?rooter:~@rooter_p
        ?reconstructor:~@reconstructor_p
        input
    ;;

    Data.instance (module Project) t;

    def "create" C.(!!Input.t @-> ptr params @-> returning !?t)
      (Error.lift2 create);

    def "arch" C.(proj_t @-> returning Arch.total)
      Project.arch;

    def "program" C.(proj_t @-> returning !!Program.t)
      Project.program;

    def "with_program" C.(proj_t @-> !!Program.t @-> returning proj_t)
      Project.with_program;

    def "symbols" C.(proj_t @-> returning !!Symtab.t)
      Project.symbols;

    def "with_symbols" C.(proj_t @-> !!Symtab.t @-> returning proj_t)
      Project.with_symbols;

    def "memory" C.(proj_t @-> returning !!Memmap.Value.t)
      Project.memory;

    def "storage" C.(proj_t @-> returning !!Value.Dict.t) Project.storage;
    def "with_storage"
      C.(proj_t @-> !!Value.Dict.t @-> returning proj_t)
      Project.with_storage;

    module Pass = struct
      type spec = Spec
      let pass : spec C.structure ctype =
        C.structure "bap_pass_t"

      let autorun = C.field pass "autorun" C.bool
      let runonce = C.field pass "runonce" C.bool
      let deps = C.field pass "deps" C.(ptr string_opt)
      let name = C.field pass "name" C.(string_opt)
      let run = C.field pass "run" C.(fn (proj_t @-> ptr void @-> returning proj_opt));;
      C.seal pass;;
      Internal.structure pass;;

      let run_pass proj name =
        match Project.find_pass name with
        | None -> Error.failf "Failed to find pass: %s" name
        | Some pass -> match Project.Pass.run pass proj with
          | Ok proj -> Ok proj
          | Error (Project.Pass.Unsat_dep (p,n)) ->
            Error.failf "Dependency `%s' of pass `%s' is not loaded"
              n (Project.Pass.name p)
          | Error (Project.Pass.Runtime_error (p, Exn.Reraised (bt,exn))) ->
            Error.failf "Pass `%s' failed at runtime with: %a\nBacktrace:\n%s"
              (Project.Pass.name p) Exn.pp exn bt
          | Error (Project.Pass.Runtime_error (p,exn)) ->
              Error.failf "Pass `%s' failed at runtime with: %a\n"
              (Project.Pass.name p) Exn.pp exn


      let def fn = def ("pass_" ^ fn);;

      def "run" C.(proj_t @-> string @-> returning proj_opt)
        (Error.lift2 run_pass);

      def "register" C.(ptr pass @-> ptr void @-> returning void)
        begin fun p data ->
          let get f = C.getf C.(!@p) f in
          let deps = list_of_nullterminated_array (get deps) in
          let f = get run in
          Project.register_pass
            ~autorun:(get autorun)
            ~runonce:(get runonce)
            ?name:(get name)
            ~deps (fun p -> match f p data with
                | None -> p
                | Some p -> p)
        end
    end
  end


  module Attributes = struct
    module type Dict = sig
      type t
      val t   : t opaque
      val get : 'a Value.Bap.tag -> t -> 'a option
      val set : 'a Value.Bap.tag -> t -> 'a -> t
      val has : 'a Value.Bap.tag -> t -> bool
      val rem : 'a Value.Bap.tag -> t -> t
    end

    let register_dict_ops (type t)
        (module Dict : Dict) ns a a_opt tag =
      let def fn = def (ns fn) in
      def "get" C.(!!Dict.t @-> returning a_opt) (Dict.get tag);
      def "set" C.(!!Dict.t @-> a @-> returning !!Dict.t) (Dict.set tag);
      def "has" C.(!!Dict.t @-> returning bool) (Dict.has tag);
      def "remove" C.(!!Dict.t @-> returning !!Dict.t) (Dict.rem tag)

    let register_void_dict_ops (module Dict : Dict) ns tag =
      let def fn = def (ns fn) in
      def "set" C.(!!Dict.t @-> bool @-> returning !!Dict.t)
        (fun dict set -> if set
          then Dict.set tag dict ()
          else Dict.rem tag dict);
      def "has" C.(!!Dict.t @-> returning bool) (Dict.has tag)

    module NS = struct
      let term name fn = "term_" ^ fn ^ "_" ^ name
      let dict name fn = "value_dict_" ^ fn ^ "_" ^ name
      let proj name fn = "project_" ^ fn ^ "_" ^ name
    end

    let tagname tag =
      Value.Bap.Tag.name tag |>
      Stringext.replace_all ~pattern:"-" ~with_:"_"

    let register (type a)
        ~total ~nullable (tag : a Value.Bap.tag) a =
      let name = tagname tag in
      let t = Value.t in
      let a = total a and a_opt = nullable a in
      let module Value_interface = struct
        let def fn = def ("value_" ^ fn ^ "_" ^ name);;
        def "create" C.(a @-> returning !!t) (Value.Bap.create tag);
        def "get" C.(!!t @-> returning a_opt) (Value.Bap.get tag);
        def "is" C.(!!t @-> returning bool) (Value.Bap.is tag);
      end in
      let module Memory_interface = struct
        def ("project_memory_tag_with_" ^ name)
          C.(!!Project.t @-> !!Memory.t @-> a @-> returning !!Project.t)
          (fun proj mem v -> Project.Bap.tag_memory proj mem tag v)
      end in
      let dict m ns = register_dict_ops m (ns name) a a_opt tag in
      dict (module Value.Dict) NS.dict;
      dict (module Term.Attr) NS.term;
      dict (module Project.Attr) NS.proj;
      ()

    let register_void_tag (tag : unit Value.Bap.tag) =
      let name = tagname tag in
      let module Value_interface = struct
        let def fn = def ("value_" ^ fn ^ "_" ^ name);;
        def "create" C.(void @-> returning !!Value.t)
          (Value.Bap.create tag);
        def "is" C.(!!Value.t @-> returning bool)
          (Value.Bap.is tag);
      end in
      let module Memory_interface = struct
        def ("project_memory_tag_with_" ^ name)
          C.(!!Project.t @-> !!Memory.t @-> returning !!Project.t)
          (fun proj mem -> Project.Bap.tag_memory proj mem tag ())
      end in
      let dict m ns =
        register_void_dict_ops m (ns name) tag in
      dict (module Value.Dict) NS.dict;
      dict (module Term.Attr) NS.term;
      dict (module Project.Attr) NS.proj

    let register_opaque_tag tag =
      register tag ~nullable:Opaque.nullable ~total:Opaque.total

    let register_enum_tag tag =
      register tag ~nullable:Enum.partial ~total:Enum.total

    let register_string_tag tag =
      register tag ~nullable:snd ~total:fst OString.(t,nullable);
      def ("project_substitute_" ^ tagname tag)
        C.(!!Project.t @-> !!Memory.t @-> string @->
           returning !!Project.t)
        (fun proj mem x -> Project.Bap.substitute proj mem tag x)

    (* treat the weight tag specialy, do not generalize
       this function to all floats *)
    let register_weight_tag () =
      let nullable = C.view C.float
          ~read:(fun x -> if x < 0. then None else Some x)
          ~write:(function
              | None -> ~-.1.
              | Some x -> x) in
      register weight ~nullable:snd ~total:fst (C.float, nullable)

    let () =
      register_string_tag comment;
      register_string_tag python;
      register_string_tag shell;
      register_void_tag mark;
      register_weight_tag ();
      register_string_tag filename;
      register_enum_tag color Color.t;
      register_enum_tag foreground Color.t;
      register_enum_tag background Color.t;
      register_opaque_tag address Word.t;
      register_opaque_tag Disasm.insn Insn.t;
      register_opaque_tag Term.Bap.origin Tid.t;
      register_void_tag Term.Bap.synthetic;
      register_void_tag Term.Bap.live;
      register_void_tag Term.Bap.dead;
      register_void_tag Term.Bap.visited;
      register_opaque_tag Term.Bap.precondition Exp.t;
      register_opaque_tag Term.Bap.invariant Exp.t;
      (* register_string_tag Sub.Bap.aliases; *)
      register_void_tag Sub.Bap.const;
      register_void_tag Sub.Bap.stub;
      register_void_tag Sub.Bap.extern;
      register_void_tag Sub.Bap.leaf;
      register_void_tag Sub.Bap.malloc;
      register_void_tag Sub.Bap.noreturn;
      register_void_tag Sub.Bap.returns_twice;
      register_void_tag Sub.Bap.nothrow;
      register_void_tag Arg.Bap.warn_unused;
      register_void_tag Arg.Bap.alloc_size;
      register_void_tag Arg.Bap.nonnull;
      register_string_tag Arg.Bap.format;
      register_opaque_tag Image.Bap.segment Image.Segment.t;
      register_string_tag Image.Bap.section;
      register_string_tag Image.Bap.symbol;
  end
end
