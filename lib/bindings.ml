open Core_kernel.Std
open Bap.Std
open Regular.Std
open Bap_plugins.Std
open Format

include Self()

let obj_addr x =
  Nativeint.shift_left (Nativeint.of_int (Obj.magic x)) 1

module Make( Internal : Cstubs_inverted.INTERNAL) =
struct
  module C = Ctypes

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
      let pref = String.uppercase name in
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

  module Opaque : sig
    type 'a t

    val newtype : ?prefix:string -> ?suffix:string -> string -> 'a t
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
  end

  type 'a opaque = 'a Opaque.t

  let (!!) = Opaque.total
  let (!?) = Opaque.nullable

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


  let fn = Foreign.funptr

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
      let def fn = def (fn ^ "_seq") in
      def "is_empty" C.(!!t @-> returning bool) Seq.is_empty;
      def "length" C.(!!t @-> returning int) Seq.length;
      def "append" C.(!!t @-> !!t @-> returning !!t) Seq.append;
      def "sub" C.(!!t @-> int @-> int @-> returning !!t)
        (fun t pos len -> Seq.sub t ~pos ~len);
      def "take" C.(!!t @-> int @-> returning !!t) Seq.take;
      def "drop" C.(!!t @-> int @-> returning !!t) Seq.drop;
      ()

    let apply hf seq f = hf seq ~f

    let iterator name seq iter elt =
      let def fn = def (name ^ "_seq_iterator_" ^ fn) in
      def "create" C.(!!seq @-> returning !!iter) Iter.create;
      def "next" C.(!!iter @-> returning !?elt) Iter.next;
      def "value" C.(!!iter @-> returning !?elt) Iter.value;
      def "has_next" C.(!!iter @-> returning bool) Iter.has_next;
      def "reset" C.(!!iter @-> returning void) Iter.reset

    let instance (type t) (module T : T with type t = t) elt =
      let name = Opaque.name elt in
      let seq : t seq opaque = Opaque.newtype (name ^ "_seq") in
      let iter :  t Iter.t opaque = Opaque.newtype (name ^ "_seq_iterator") in
      let def fn = def (name ^ "_seq_" ^ fn) in
      Opaque.instanceof ~base:t seq;
      def "iter"
        C.(!!seq @-> fn (!!elt @-> returning void) @-> returning void)
        (apply Seq.iter);
      def "map"
        C.(!!seq @-> fn (!!elt @-> returning !!elt) @-> returning !!seq)
        (apply Seq.map);
      def "find"
        C.(!!seq @-> fn (!!elt @-> returning bool) @-> returning !?elt)
        (apply Seq.find);
      def "exists"
        C.(!!seq @-> fn (!!elt @-> returning bool) @-> returning bool)
        (apply Seq.exists);
      def "forall"
        C.(!!seq @-> fn (!!elt @-> returning bool) @-> returning bool)
        (apply Seq.for_all);
      def "count"
        C.(!!seq @-> fn (!!elt @-> returning bool) @-> returning int)
        (apply Seq.count);
      def "mem" C.(!!seq @-> !!elt @-> returning bool) Seq.mem;

      iterator name seq iter elt;

      seq
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
        type t
        val name : string
        module T : Regular.S with type t = t
      end) = struct
      open Spec

      let t = Opaque.newtype name
      let seq = Seq.instance (module T) t

      let def fn typ impl =
        def (name ^ "_" ^ fn) typ impl

      let () =
        instance (module T) t;
    end

  end


  module Endian = struct
    module T = struct
      type t = Word.endian
    end

    let t : endian opaque = Opaque.newtype "endian"
  end

  (* small positive int *)
  module Unsigned = struct
    let t = C.int
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
  end


  module Arch = struct
    let t = Enum.define (module Arch) "bap_arch"
    let total = Enum.total t
    let partial = Enum.partial t

    let size arch = (Arch.addr_size arch :> Size.T.all)

    let () =
      let def fn = def ("arch_" ^ fn) in
      def "endian" C.(total @-> returning !!Endian.t) Arch.endian;
      def "addr_size" C.(total @-> returning Size.t) size ;
      def "to_string" C.(total @-> returning OString.t) Arch.to_string ;
      def "of_string" C.(string @-> returning partial) Arch.of_string;
  end

  module Var = struct
    include Regular.Make(struct
        type t = var
        let name = "var"
        module T = Var
      end)
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

    let () =
      let proj p = C.(!!t @-> returning !?p) in
      def "tag" C.(!!t @-> returning tag_t) to_tag;
      def "mem" (proj t) mem;
      def "addr" (proj t) addr;
      def "endian" (proj Endian.t) endian;
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

      ()


  end

  module Tid = struct
    let t : tid opaque = Opaque.newtype "tid"

    let def x = def ("tid_" ^ x)

    let () =
      Regular.instance (module Tid) t;
      def "create" C.(void @-> returning !!t) Tid.create;
      def "set_name" C.(!!t @-> string @-> returning void) Tid.set_name;
      def "name" C.(!!t @-> returning OString.t) Tid.name;
      def "from_string" C.(string @-> returning !?t)
        (Error.lift1 Tid.from_string)
  end

  module Block = struct
    let t = Opaque.newtype "block"
  end

  module Cfg = struct
    let t = Opaque.newtype "cfg"
  end

  module Term = struct
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


    type void = Void

    let t : void term opaque = Opaque.newtype "term"
    let term = t

    let () =
      let def fn = def ("term_" ^ fn) in
      def "name" C.(!!t @-> returning OString.t) Term.name;
      def "clone" C.(!!t @-> returning !!t) Term.clone;
      def "tid" C.(!!t @-> returning !!Tid.t) Term.tid;
      def "same" C.(!!t @-> !!t @-> returning bool) Term.same;
      ()


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




  module Jmp = struct
    include Term.Make(struct
        type t = jmp and p = blk
        let name = "jmp" and cls = jmp_t
        module T = Jmp
      end)
  end

  module Def = struct
    include Term.Make(struct
        type t = def and p = blk
        let name = "def" and cls = def_t
        module T = Def
      end)
  end

  module Phi = struct
    include Term.Make(struct
        type t = phi and p = blk
        let name = "phi" and cls = phi_t
        module T = Phi
      end)
  end

  module Blk = struct
    include Term.Make(struct
        type t = blk and p = sub
        let name = "blk" and cls = blk_t
        module T = Blk
      end)

    let () =
      Term.parentof ~child:Phi.t phi_t t;
      Term.parentof ~child:Def.t def_t t;
      Term.parentof ~child:Jmp.t jmp_t t;
  end

  module Arg = struct
    include Term.Make(struct
        type t = arg and p = sub
        let name = "arg" and cls = arg_t
        module T = Arg
      end)

  end

  module Sub = struct
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
end
