open Core_kernel.Std
open Bap.Std
open Regular.Std
open Bap_plugins.Std
open Format
module C = Ctypes

include Self()

let obj_addr x =
  Nativeint.shift_left (Nativeint.of_int (Obj.magic x)) 1

let name = None

module Make( Internal : Cstubs_inverted.INTERNAL) =
struct

  module Enum : sig
    module type T = sig
      type t [@@deriving enumerate, compare, sexp]
    end
    type 'a t
    val define : ?first:int -> (module T with type t = 'a) -> string -> 'a t
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

    let define ?(first=0) (type t) (module E: T with type t = t) name =
      assert (first >= 0);
      let enum = Array.of_list E.all in
      Array.sort enum ~cmp:E.compare;
      let read i = Array.get enum (i - first) in
      let write t =
        match
          Array.binary_search ~compare:E.compare enum `First_equal_to t
        with None -> assert false
           | Some x -> x + first in
      let read_opt i = if i = -1  then None else Some (read i) in
      let write_opt = function
        | None -> -1
        | Some x -> write x in
      let pref = String.uppercase ("bap_" ^ name) in
      let view read write =
        C.view ~format_typ:(fun k ppf -> fprintf ppf "enum %s_tag%t" name k)
          ~read ~write C.int in
      let cases = List.mapi E.all ~f:(fun i x ->
          let name = String.uppercase (Sexp.to_string (E.sexp_of_t x)) in
          pref ^ "_" ^ name, Int64.of_int (first + i)) in
      let cases = (pref ^ "_INVALID", Int64.of_int (~-1)) :: cases in
      let t = view read write in
      let partial = view read_opt write_opt in
      Internal.enum cases t;
      Internal.typedef t (name ^ "_tag");
      {total = t; partial}

    let total t = t.total
    let partial t = t.partial
  end


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
      def "error_clean" C.(void @-> returning void) clear;
  end

  let () =
    def "version" C.(void @-> returning OString.t) version;
    def "_standalone_init" C.(int @-> ptr string @-> returning int)
      standalone_init


  let fn = Foreign.funptr


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
      ()


  end

  module Seq = struct
    module ML = Seq
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

  module Regular = struct
    module ML = Regular
    let instance (type t) (module T : Regular.S with type t = t) t =
      let name = Opaque.name t in
      let def fn = def (name ^ "_" ^ fn) in
      let t = Opaque.total t in
      def "to_string" C.(t @-> returning OString.t) T.to_string;
      def "compare" C.(t @-> t @-> returning int) T.compare;
      def "equal"   C.(t @-> t @-> returning bool) T.equal;
      def "hash"    C.(t @-> returning int) T.hash



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
          ~write:Seq.ML.of_list
          ~read:Seq.ML.to_list

      let pset = Opaque.view set
          ~write:set_of_pset
          ~read:pset_of_set

      let def fn typ impl =
        def (name ^ "_" ^ fn) typ impl

      let () =
        instance (module T) t;
    end
  end

  module Value = struct
    module ML = Value
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
      [@@deriving compare, enumerate, sexp]
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
    let t = Enum.define (module Arch) "bap_arch"
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

    let enum = Enum.define (module Tag) "type"
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
    module ML = Var
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

    let tag = Enum.define (module Tag) "exp"
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

    let tag = Enum.define (module Tag) "stmt"
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
      | Bil.While (_,x) -> Some (Seq.ML.of_list x)
      | _ -> None

    let true_stmts = function
      | Bil.If (_,x,_) -> Some (Seq.ML.of_list x)
      | _ -> None

    let false_stmts = function
      | Bil.If (_,_,x) -> Some (Seq.ML.of_list x)
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
        (fun exp xs ys -> Bil.if_ exp (Seq.ML.to_list xs) (Seq.ML.to_list ys));
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
    let def x = def ("tid_" ^ x)

    let () =
      def "create" C.(void @-> returning !!t) Tid.create;
      def "set_name" C.(!!t @-> string @-> returning void) Tid.set_name;
      def "name" C.(!!t @-> returning OString.t) Tid.name;
      def "from_string" C.(string @-> returning !?t)
        (Error.lift1 Tid.from_string)
  end

  module Bil = struct
    let t  = Stmt.list
    let def fn = def ("bil_" ^ fn)

    let () =
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

    let enum = Enum.define (module Tag) "op"
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

  module Insn = struct
    module Property = struct

    end
    include Regular.Make(struct
        module T = Insn
        let name = "insn"
      end);;

    def "name" C.(!!t @-> returning OString.t) Insn.name;
    def "asm" C.(!!t @-> returning OString.t) Insn.asm;
    def "bil" C.(!!t @-> returning !!Bil.t) Insn.bil;
    def "ops" C.(!!t @-> returning !!Op.seq)
      (fun insn -> Seq.ML.of_array (Insn.ops insn));
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

  module Block = struct
    let t = Opaque.newtype "block"
  end

  module Cfg = struct
    let t = Opaque.newtype "cfg"
  end

  module Term = struct
    module ML = Term
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

    module type Leaf = sig
      type t
      val free_vars : t -> Var.ML.Set.t
      val map_exp : t -> f:(exp -> exp) -> t
      val substitute : t -> exp -> exp -> t
    end

    let leaf (type t) (module Term : Leaf with type t = t) t =
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
        type p
        val name : string
        val cls : (p,t) cls
        module T : Regular.ML.S with type t = t term
      end) = struct
      open Spec

      let t = Opaque.newtype name
      let seq = Seq.instance (module T) t

      let def fn typ impl =
        def (name ^ "_" ^ fn) typ impl

      let () =
        Opaque.instanceof ~base:term t;
        Regular.instance (module T) t;
    end
  end

  module Label = struct
    module Tag = struct
      type t = [`Direct | `Indirect]
      [@@deriving enumerate, sexp, compare]
    end

    let tag = Enum.define (module Tag) "label"
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
    module ML = Call
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
      | Call call -> Some (Call.ML.target call)
      | Goto dst | Ret dst -> Some dst
      | _ -> None

    let to_call jmp = match Jmp.kind jmp with
      | Call call -> Some call
      | _ -> None

    let to_interrupt jmp = match Jmp.kind jmp with
      | Int (n,ret) -> Some (n,ret)
      | _ -> None
    ;;

    Term.leaf (module Jmp) t;;
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

    Term.leaf (module Def) t;;
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

    Term.leaf (module Phi) t;;

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
      Seq.ML.(fun t -> Phi.values t >>| fst);
    def "exps" C.(!!t @-> returning !!Exp.seq)
      Seq.ML.(fun t -> Phi.values t >>| snd)
  end

  module Blk = struct
    include Term.Make(struct
        type t = blk and p = sub
        let name = "blk" and cls = blk_t
        module T = Blk
      end);;

    Term.parentof ~child:Phi.t phi_t t;;
    Term.parentof ~child:Def.t def_t t;;
    Term.parentof ~child:Jmp.t jmp_t t;;

    def "create" C.(!?Tid.t @-> returning !!t)
      (fun tid -> Blk.create ?tid ());;

  end

  module Arg = struct
    include Term.Make(struct
        type t = arg and p = sub
        let name = "arg" and cls = arg_t
        module T = Arg
      end)


  end

  module Sub = struct
    module ML = Sub
    include Term.Make(struct
        type t = sub and p = program
        let name = "sub" and cls = sub_t
        module T = Sub
      end)

    let () =
      Term.parentof ~child:Arg.t arg_t t;
      Term.parentof ~child:Blk.t blk_t t;
      def "lift"
        C.(!!Block.t @-> !!Cfg.t @-> returning !!t) Sub.lift;
      def "name" C.(!!t @-> returning OString.t) Sub.name;
      def "with_name" C.(!!t @-> string @-> returning !!t)
        Sub.with_name;
      def "ssa" C.(!!t @-> returning !!t) Sub.ssa;
      def "is_ssa" C.(!!t @-> returning bool) Sub.is_ssa;
  end

  module Program = struct
    let t : program term opaque = Opaque.newtype "program"

    let def name typ = def ("program_" ^ name) typ

    let () =
      Regular.instance (module Program) t;
      Term.enumerator ~elt:Sub.t ~seq:Sub.seq sub_t t

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
    let () =
      Source.factory (module Rooter.Factory) Source.rooter "rooter"
  end

  module Brancher = struct
    let t : brancher opaque = Opaque.newtype "brancher"
  end

  module Symbolizer = struct
    let t : symbolizer opaque = Opaque.newtype "symbolizer"
  end

  module Reconstructor = struct
    let t : reconstructor opaque = Opaque.newtype "reconstructor"
  end

  module Project = struct
    let t : project opaque = Opaque.newtype "project"
    let proj_t = !!t

    let def name typ impl =
      def ("project_" ^ name) typ impl

    type params = Params

    let params : params C.structure ctype =
      C.structure "bap_project_parameters_t"

    let params_field name typ =
      C.field params ("bap_project_" ^ name) typ

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

      let def name typ impl = def ("input_" ^ name) typ impl

      let () =
        def "file" C.(string @-> string_opt @-> returning !!t)
          (fun filename loader -> Project.Input.file ?loader ~filename);
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

    let () =
      def "create" C.(!!Input.t @-> ptr params @-> returning !?t)
        (Error.lift2 create);

      def "arch" C.(proj_t @-> returning Arch.total)
        Project.arch;

      def "program" C.(proj_t @-> returning !!Program.t)
        Project.program;
  end


  module Attributes = struct
    module type Dict = sig
      type t
      val t   : t opaque
      val get : 'a Value.ML.tag -> t -> 'a option
      val set : 'a Value.ML.tag -> t -> 'a -> t
      val has : 'a Value.ML.tag -> t -> bool
      val rem : 'a Value.ML.tag -> t -> t
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
      Value.ML.Tag.name tag |>
      Stringext.replace_all ~pattern:"-" ~with_:"_"

    let register (type a)
        ~total ~nullable (tag : a Value.ML.tag) a =
      let name = tagname tag in
      let def fn = def ("value_" ^ fn ^ "_" ^ name) in
      let t = Value.t in
      let a = total a and a_opt = nullable a in
      def "create" C.(a @-> returning !!t) (Value.ML.create tag);
      def "get" C.(!!t @-> returning a_opt) (Value.ML.get tag);
      def "is" C.(!!t @-> returning bool) (Value.ML.is tag);
      let dict m ns = register_dict_ops m (ns name) a a_opt tag in
      dict (module Value.Dict) NS.dict;
      dict (module Term.Attr) NS.term;
      dict (module Project.Attr) NS.proj;
      ()

    let register_void_tag (tag : unit Value.ML.tag) =
      let name = tagname tag in
      let def fn = def (fn ^ "_" ^ name) in
      def "create" C.(void @-> returning !!Value.t) (Value.ML.create tag);
      def "is" C.(!!Value.t @-> returning bool) (Value.ML.is tag);
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
      register tag ~nullable:snd ~total:fst OString.(t,nullable)

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
      register_opaque_tag Term.ML.origin Tid.t;
      register_void_tag Term.ML.synthetic;
      register_void_tag Term.ML.live;
      register_void_tag Term.ML.dead;
      register_void_tag Term.ML.visited;
      register_opaque_tag Term.ML.precondition Exp.t;
      register_opaque_tag Term.ML.invariant Exp.t;
      (* register_string_tag Sub.ML.aliases; *)
      register_void_tag Sub.ML.const;
      register_void_tag Sub.ML.stub;
      register_void_tag Sub.ML.extern;
      register_void_tag Sub.ML.leaf;
      register_void_tag Sub.ML.malloc;
      register_void_tag Sub.ML.noreturn;
      register_void_tag Sub.ML.returns_twice;
      register_void_tag Sub.ML.nothrow;
  end
end
