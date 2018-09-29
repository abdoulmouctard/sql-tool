module MMap:
sig

  (**************************************************************************)
  (*                                                                        *)
  (*                                 OCaml                                  *)
  (*                                                                        *)
  (*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
  (*                                                                        *)
  (*   Copyright 1996 Institut National de Recherche en Informatique et     *)
  (*     en Automatique.                                                    *)
  (*                                                                        *)
  (*   All rights reserved.  This file is distributed under the terms of    *)
  (*   the GNU Lesser General Public License version 2.1, with the          *)
  (*   special exception on linking described in the file LICENSE.          *)
  (*                                                                        *)
  (**************************************************************************)

  (** Association tables over ordered types.

     This module implements applicative association tables, also known as
     finite maps or dictionaries, given a total ordering function
     over the keys.
     All operations over maps are purely applicative (no side-effects).
     The implementation uses balanced binary trees, and therefore searching
     and insertion take time logarithmic in the size of the map.

     For instance:
     {[
       module IntPairs =
         struct
           type t = int * int
           let compare (x0,y0) (x1,y1) =
             match Pervasives.compare x0 x1 with
                 0 -> Pervasives.compare y0 y1
               | c -> c
         end

       module PairsMap = Map.Make(IntPairs)

       let m = PairsMap.(empty |> add (0,1) "hello" |> add (1,0) "world")
     ]}

     This creates a new module [PairsMap], with a new type ['a PairsMap.t]
     of maps from [int * int] to ['a]. In this example, [m] contains [string]
     values so its type is [string PairsMap.t].
  *)

  module type OrderedType =
    sig
      type t
        (** The type of the map keys. *)

      val compare : t -> t -> int
        (** A total ordering function over the keys.
            This is a two-argument function [f] such that
            [f e1 e2] is zero if the keys [e1] and [e2] are equal,
            [f e1 e2] is strictly negative if [e1] is smaller than [e2],
            and [f e1 e2] is strictly positive if [e1] is greater than [e2].
            Example: a suitable ordering function is the generic structural
            comparison function {!Pervasives.compare}. *)
    end
  (** Input signature of the functor {!Map.Make}. *)

  module type S =
    sig
      type key
      (** The type of the map keys. *)

      type (+'a) t
      (** The type of maps from type [key] to type ['a]. *)

      val empty: 'a t
      (** The empty map. *)

      val is_empty: 'a t -> bool
      (** Test whether a map is empty or not. *)

      val mem: key -> 'a t -> bool
      (** [mem x m] returns [true] if [m] contains a binding for [x],
         and [false] otherwise. *)

      val add: key -> 'a -> 'a t -> 'a t
      (** [add x y m] returns a map containing the same bindings as
         [m], plus a binding of [x] to [y]. If [x] was already bound
         in [m] to a value that is physically equal to [y],
         [m] is returned unchanged (the result of the function is
         then physically equal to [m]). Otherwise, the previous binding
         of [x] in [m] disappears.
         @before 4.03 Physical equality was not ensured. *)

      val singleton: key -> 'a -> 'a t
      (** [singleton x y] returns the one-element map that contains a binding [y]
          for [x].
          @since 3.12.0
       *)

      val remove: key -> 'a t -> 'a t
      (** [remove x m] returns a map containing the same bindings as
         [m], except for [x] which is unbound in the returned map.
         If [x] was not in [m], [m] is returned unchanged
         (the result of the function is then physically equal to [m]).
         @before 4.03 Physical equality was not ensured. *)

      val merge:
           (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
      (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
          and of [m2]. The presence of each such binding, and the corresponding
          value, is determined with the function [f].
          @since 3.12.0
       *)

      val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
      (** [union f m1 m2] computes a map whose keys is the union of keys
          of [m1] and of [m2].  When the same binding is defined in both
          arguments, the function [f] is used to combine them.
          @since 4.03.0
      *)

      val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
      (** Total ordering between maps.  The first argument is a total ordering
          used to compare data associated with equal keys in the two maps. *)

      val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
         equal, that is, contain equal keys and associate them with
         equal data.  [cmp] is the equality predicate used to compare
         the data associated with the keys. *)

      val iter: (key -> 'a -> unit) -> 'a t -> unit
      (** [iter f m] applies [f] to all bindings in map [m].
         [f] receives the key as first argument, and the associated value
         as second argument.  The bindings are passed to [f] in increasing
         order with respect to the ordering over the type of the keys. *)

      val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
         where [k1 ... kN] are the keys of all bindings in [m]
         (in increasing order), and [d1 ... dN] are the associated data. *)

      val for_all: (key -> 'a -> bool) -> 'a t -> bool
      (** [for_all p m] checks if all the bindings of the map
          satisfy the predicate [p].
          @since 3.12.0
       *)

      val exists: (key -> 'a -> bool) -> 'a t -> bool
      (** [exists p m] checks if at least one binding of the map
          satisfy the predicate [p].
          @since 3.12.0
       *)

      val filter: (key -> 'a -> bool) -> 'a t -> 'a t
      (** [filter p m] returns the map with all the bindings in [m]
          that satisfy predicate [p]. If [p] satisfies every binding in [m],
          [m] is returned unchanged (the result of the function is then
          physically equal to [m])
          @since 3.12.0
         @before 4.03 Physical equality was not ensured.
       *)

      val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
      (** [partition p m] returns a pair of maps [(m1, m2)], where
          [m1] contains all the bindings of [s] that satisfy the
          predicate [p], and [m2] is the map with all the bindings of
          [s] that do not satisfy [p].
          @since 3.12.0
       *)

      val cardinal: 'a t -> int
      (** Return the number of bindings of a map.
          @since 3.12.0
       *)

      val bindings: 'a t -> (key * 'a) list
      (** Return the list of all bindings of the given map.
         The returned list is sorted in increasing order with respect
         to the ordering [Ord.compare], where [Ord] is the argument
         given to {!Map.Make}.
          @since 3.12.0
       *)

      val min_binding: 'a t -> (key * 'a)
      (** Return the smallest binding of the given map
         (with respect to the [Ord.compare] ordering), or raise
         [Not_found] if the map is empty.
          @since 3.12.0
       *)

      val max_binding: 'a t -> (key * 'a)
      (** Same as {!Map.S.min_binding}, but returns the largest binding
          of the given map.
          @since 3.12.0
       *)

      val choose: 'a t -> (key * 'a)
      (** Return one binding of the given map, or raise [Not_found] if
         the map is empty. Which binding is chosen is unspecified,
         but equal bindings will be chosen for equal maps.
          @since 3.12.0
       *)

      val split: key -> 'a t -> 'a t * 'a option * 'a t
      (** [split x m] returns a triple [(l, data, r)], where
            [l] is the map with all the bindings of [m] whose key
          is strictly less than [x];
            [r] is the map with all the bindings of [m] whose key
          is strictly greater than [x];
            [data] is [None] if [m] contains no binding for [x],
            or [Some v] if [m] binds [v] to [x].
          @since 3.12.0
       *)

      val find: key -> 'a t -> 'a
      (** [find x m] returns the current binding of [x] in [m],
         or raises [Not_found] if no such binding exists. *)

      val map: ('a -> 'b) -> 'a t -> 'b t
      (** [map f m] returns a map with same domain as [m], where the
         associated value [a] of all bindings of [m] has been
         replaced by the result of the application of [f] to [a].
         The bindings are passed to [f] in increasing order
         with respect to the ordering over the type of the keys. *)

      val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
      (** Same as {!Map.S.map}, but the function receives as arguments both the
         key and the associated value for each binding of the map. *)


    end
  (** Output signature of the functor {!Map.Make}. *)

  module Make (Ord : OrderedType) : S with type key = Ord.t
  (** Functor building an implementation of the map structure
     given a totally ordered type. *)

end
=
struct

  (**************************************************************************)
  (*                                                                        *)
  (*                                 OCaml                                  *)
  (*                                                                        *)
  (*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
  (*                                                                        *)
  (*   Copyright 1996 Institut National de Recherche en Informatique et     *)
  (*     en Automatique.                                                    *)
  (*                                                                        *)
  (*   All rights reserved.  This file is distributed under the terms of    *)
  (*   the GNU Lesser General Public License version 2.1, with the          *)
  (*   special exception on linking described in the file LICENSE.          *)
  (*                                                                        *)
  (**************************************************************************)

  module type OrderedType =
    sig
      type t
      val compare: t -> t -> int
    end

  module type S =
    sig
      type key
      type +'a t
      val empty: 'a t
      val is_empty: 'a t -> bool
      val mem:  key -> 'a t -> bool
      val add: key -> 'a -> 'a t -> 'a t
      val singleton: key -> 'a -> 'a t
      val remove: key -> 'a t -> 'a t
      val merge:
            (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
      val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
      val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      val iter: (key -> 'a -> unit) -> 'a t -> unit
      val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val for_all: (key -> 'a -> bool) -> 'a t -> bool
      val exists: (key -> 'a -> bool) -> 'a t -> bool
      val filter: (key -> 'a -> bool) -> 'a t -> 'a t
      val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
      val cardinal: 'a t -> int
      val bindings: 'a t -> (key * 'a) list
      val min_binding: 'a t -> (key * 'a)
      val max_binding: 'a t -> (key * 'a)
      val choose: 'a t -> (key * 'a)
      val split: key -> 'a t -> 'a t * 'a option * 'a t
      val find: key -> 'a t -> 'a
      val map: ('a -> 'b) -> 'a t -> 'b t
      val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    end

  module Make(Ord: OrderedType) = struct

      type key = Ord.t

      type 'a t =
          Empty
        | Node of 'a t * key * 'a * 'a t * int

      let height = function
          Empty -> 0
        | Node(_,_,_,_,h) -> h

      let create l x d r =
        let hl = height l and hr = height r in
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

      let singleton x d = Node(Empty, x, d, Empty, 1)

      let bal l x d r =
        let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
        let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
        if hl > hr + 2 then begin
          match l with
            Empty -> invalid_arg "Map.bal"
          | Node(ll, lv, ld, lr, _) ->
              if height ll >= height lr then
                create ll lv ld (create lr x d r)
              else begin
                match lr with
                  Empty -> invalid_arg "Map.bal"
                | Node(lrl, lrv, lrd, lrr, _)->
                    create (create ll lv ld lrl) lrv lrd (create lrr x d r)
              end
        end else if hr > hl + 2 then begin
          match r with
            Empty -> invalid_arg "Map.bal"
          | Node(rl, rv, rd, rr, _) ->
              if height rr >= height rl then
                create (create l x d rl) rv rd rr
              else begin
                match rl with
                  Empty -> invalid_arg "Map.bal"
                | Node(rll, rlv, rld, rlr, _) ->
                    create (create l x d rll) rlv rld (create rlr rv rd rr)
              end
        end else
          Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

      let empty = Empty

      let is_empty = function Empty -> true | _ -> false

      let rec add x data = function
          Empty ->
            Node(Empty, x, data, Empty, 1)
        | Node(l, v, d, r, h) as m ->
            let c = Ord.compare x v in
            if c = 0 then
              if d == data then m else Node(l, x, data, r, h)
            else if c < 0 then
              let ll = add x data l in
              if l == ll then m else bal ll v d r
            else
              let rr = add x data r in
              if r == rr then m else bal l v d rr

      let rec find x = function
          Empty ->
            raise Not_found
        | Node(l, v, d, r, _) ->
            let c = Ord.compare x v in
            if c = 0 then d
            else find x (if c < 0 then l else r)

      let rec mem x = function
          Empty ->
            false
        | Node(l, v, _, r, _) ->
            let c = Ord.compare x v in
            c = 0 || mem x (if c < 0 then l else r)

      let rec min_binding = function
          Empty -> raise Not_found
        | Node(Empty, x, d, _, _) -> (x, d)
        | Node(l, _, _, _, _) -> min_binding l

      let rec max_binding = function
          Empty -> raise Not_found
        | Node(_, x, d, Empty, _) -> (x, d)
        | Node(_, _, _, r, _) -> max_binding r

      let rec remove_min_binding = function
          Empty -> invalid_arg "Map.remove_min_elt"
        | Node(Empty, _, _, r, _) -> r
        | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

      let merge t1 t2 =
        match (t1, t2) with
          (Empty, t) -> t
        | (t, Empty) -> t
        | (_, _) ->
            let (x, d) = min_binding t2 in
            bal t1 x d (remove_min_binding t2)

      let rec remove x = function
          Empty ->
            Empty
        | (Node(l, v, d, r, _) as t) ->
            let c = Ord.compare x v in
            if c = 0 then merge l r
            else if c < 0 then
              let ll = remove x l in if l == ll then t else bal ll v d r
            else
              let rr = remove x r in if r == rr then t else bal l v d rr

      let rec iter f = function
          Empty -> ()
        | Node(l, v, d, r, _) ->
            iter f l; f v d; iter f r

      let rec map f = function
          Empty ->
            Empty
        | Node(l, v, d, r, h) ->
            let l' = map f l in
            let d' = f d in
            let r' = map f r in
            Node(l', v, d', r', h)

      let rec mapi f = function
          Empty ->
            Empty
        | Node(l, v, d, r, h) ->
            let l' = mapi f l in
            let d' = f v d in
            let r' = mapi f r in
            Node(l', v, d', r', h)

      let rec fold f m accu =
        match m with
          Empty -> accu
        | Node(l, v, d, r, _) ->
            fold f r (f v d (fold f l accu))

      let rec for_all p = function
          Empty -> true
        | Node(l, v, d, r, _) -> p v d && for_all p l && for_all p r

      let rec exists p = function
          Empty -> false
        | Node(l, v, d, r, _) -> p v d || exists p l || exists p r

      (* Beware: those two functions assume that the added k is *strictly*
         smaller (or bigger) than all the present keys in the tree; it
         does not test for equality with the current min (or max) key.

         Indeed, they are only used during the "join" operation which
         respects this precondition.
      *)

      let rec add_min_binding k v = function
        | Empty -> singleton k v
        | Node (l, x, d, r, _) ->
          bal (add_min_binding k v l) x d r

      let rec add_max_binding k v = function
        | Empty -> singleton k v
        | Node (l, x, d, r, _) ->
          bal l x d (add_max_binding k v r)

      (* Same as create and bal, but no assumptions are made on the
         relative heights of l and r. *)

      let rec join l v d r =
        match (l, r) with
          (Empty, _) -> add_min_binding v d r
        | (_, Empty) -> add_max_binding v d l
        | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
            if lh > rh + 2 then bal ll lv ld (join lr v d r) else
            if rh > lh + 2 then bal (join l v d rl) rv rd rr else
            create l v d r

      (* Merge two trees l and r into one.
         All elements of l must precede the elements of r.
         No assumption on the heights of l and r. *)

      let concat t1 t2 =
        match (t1, t2) with
          (Empty, t) -> t
        | (t, Empty) -> t
        | (_, _) ->
            let (x, d) = min_binding t2 in
            join t1 x d (remove_min_binding t2)

      let concat_or_join t1 v d t2 =
        match d with
        | Some d -> join t1 v d t2
        | None -> concat t1 t2

      let rec split x = function
          Empty ->
            (Empty, None, Empty)
        | Node(l, v, d, r, _) ->
            let c = Ord.compare x v in
            if c = 0 then (l, Some d, r)
            else if c < 0 then
              let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
            else
              let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)

      let rec merge f s1 s2 =
        match (s1, s2) with
          (Empty, Empty) -> Empty
        | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
            let (l2, d2, r2) = split v1 s2 in
            concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
        | (_, Node (l2, v2, d2, r2, _)) ->
            let (l1, d1, r1) = split v2 s1 in
            concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
        | _ ->
            assert false

      let rec union f s1 s2 =
        match (s1, s2) with
        | (Empty, s) | (s, Empty) -> s
        | (Node (l1, v1, d1, r1, h1), Node (l2, v2, d2, r2, h2)) ->
            if h1 >= h2 then
              let (l2, d2, r2) = split v1 s2 in
              let l = union f l1 l2 and r = union f r1 r2 in
              match d2 with
              | None -> join l v1 d1 r
              | Some d2 -> concat_or_join l v1 (f v1 d1 d2) r
            else
              let (l1, d1, r1) = split v2 s1 in
              let l = union f l1 l2 and r = union f r1 r2 in
              match d1 with
              | None -> join l v2 d2 r
              | Some d1 -> concat_or_join l v2 (f v2 d1 d2) r

      let rec filter p = function
          Empty -> Empty
        | Node(l, v, d, r, _) as t ->
            (* call [p] in the expected left-to-right order *)
            let l' = filter p l in
            let pvd = p v d in
            let r' = filter p r in
            if pvd then if l==l' && r==r' then t else join l' v d r'
            else concat l' r'

      let rec partition p = function
          Empty -> (Empty, Empty)
        | Node(l, v, d, r, _) ->
            (* call [p] in the expected left-to-right order *)
            let (lt, lf) = partition p l in
            let pvd = p v d in
            let (rt, rf) = partition p r in
            if pvd
            then (join lt v d rt, concat lf rf)
            else (concat lt rt, join lf v d rf)

      type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

      let rec cons_enum m e =
        match m with
          Empty -> e
        | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

      let compare cmp m1 m2 =
        let rec compare_aux e1 e2 =
            match (e1, e2) with
            (End, End) -> 0
          | (End, _)  -> -1
          | (_, End) -> 1
          | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
              let c = Ord.compare v1 v2 in
              if c <> 0 then c else
              let c = cmp d1 d2 in
              if c <> 0 then c else
              compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
        in compare_aux (cons_enum m1 End) (cons_enum m2 End)

      let equal cmp m1 m2 =
        let rec equal_aux e1 e2 =
            match (e1, e2) with
            (End, End) -> true
          | (End, _)  -> false
          | (_, End) -> false
          | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
              Ord.compare v1 v2 = 0 && cmp d1 d2 &&
              equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
        in equal_aux (cons_enum m1 End) (cons_enum m2 End)

      let rec cardinal = function
          Empty -> 0
        | Node(l, _, _, r, _) -> cardinal l + 1 + cardinal r

      let rec bindings_aux accu = function
          Empty -> accu
        | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

      let bindings s =
        bindings_aux [] s

      let choose = min_binding

  end

end






module type DATA =
sig

  type domain

  type value

  val string_of_value: value -> string

  val string_of_domain: domain -> string

  val value_of_string: domain -> string -> value

  val domain_of_string: string -> domain

end


module type S =
sig

  type domain

  type value

  type relation

  type tuple

  type attribute

  val attribute: attribute -> tuple -> value option

  val domain: relation -> attribute -> domain

  val width: relation -> int

  val cardinal: relation -> int

  val distinct: relation -> relation

  val selection: (tuple -> bool) -> relation -> relation

  val projection: (domain * (tuple -> value option)) list -> relation -> relation

  val union: relation -> relation -> relation

  val inter: relation -> relation -> relation

  val diff: relation -> relation -> relation

  val crossjoin: relation -> relation -> relation

  val innerjoin: (tuple -> tuple -> bool) -> relation -> relation -> relation

  val leftouterjoin: (tuple -> tuple -> bool) -> relation -> relation -> relation

  val fullouterjoin: (tuple -> tuple -> bool) -> relation -> relation -> relation

  val fold: bool -> ('a -> value -> 'a) -> 'a -> attribute -> relation -> 'a

  val aggregate: attribute list -> (domain * (relation -> value option)) list -> relation -> relation

  val from_file: string -> char -> (string * attribute) list * relation

  val to_file: string -> char -> (attribute * string) list -> relation -> unit

  val print: char -> (attribute * string) list -> relation -> unit

end



module Make(V: DATA)
=
struct
  type value = V.value

  type domain = V.domain

  type tuple = (V.value option) array

  module TSet = MMap.Make(struct

    type t = tuple

    let compare t1 t2 = Pervasives.compare t1 t2
(*
      let l1 = Array.length t1
      and l2 = Array.length t2 in
      let d = Pervasives.compare l1 l2 in
      if d <> 0 then d
      else
        let d = ref 0 in
        try
          for i = 0 to l1-1 do
            d := (match t1.(i), t2.(i) with
              | None, None -> 0
              | None, _ -> -1
              | _, None -> 1
              | Some v1, Some v2 -> Pervasives.compare v1 v2);
            if !d <> 0 then raise Not_found
          done;
          0
        with Not_found -> !d
*)
  end)

  type relation = {
    card: int;
    ext: int TSet.t;
    sch: V.domain array;
  }

  type attribute = int

  let attribute a t = t.(a)

  let domain r a = r.sch.(a)

  let width r = Array.length r.sch

  let cardinal r = r.card

  let selection pred r =
    let card' = ref r.card in
    let ext' = TSet.filter (fun t n -> if pred t then true else (card' := !card' - n; false)) r.ext in
    { r with
      ext = ext';
      card = !card';
    }

  let projection lcol =
    let acol = Array.of_list lcol in
    let af = Array.map snd acol
    and sch = Array.map fst acol in
    fun r ->
      let ext = TSet.fold (fun t n acc ->
        let t' = Array.map (fun f -> f t) af in
        let n' = try n + TSet.find t' acc with Not_found -> n in
        TSet.add t' n' acc
      ) r.ext TSet.empty in
      { r with ext = ext; sch = sch } 

  let crossjoin r1 r2 =
    let sch = Array.append r1.sch r2.sch in
    let ext = TSet.fold (fun t1 n1 acc -> TSet.fold (fun t2 n2 acc ->
      TSet.add (Array.append t1 t2) (n1 * n2) acc
    ) r2.ext acc) r1.ext TSet.empty in
    { card = r1.card * r2.card; ext = ext; sch = sch } 

  let innerjoin pred r1 r2 =
    let card = ref 0 in
    let sch = Array.append r1.sch r2.sch in
    let ext = TSet.fold (fun t1 n1 acc -> TSet.fold (fun t2 n2 acc ->
      if pred t1 t2
      then (
        card := !card + n1 * n2;
        TSet.add (Array.append t1 t2) (n1 * n2) acc
      ) else
        acc
    ) r2.ext acc) r1.ext TSet.empty in
    { card = !card; ext = ext; sch = sch } 

  let leftouterjoin pred r1 r2 =
    let card = ref 0 in
    let sch = Array.append r1.sch r2.sch in
    let dummyt2 = Array.make (width r2) None in
    let ext = TSet.fold (fun t1 n1 acc ->
      let somet2 = ref false in
      let acc = TSet.fold (fun t2 n2 acc ->
        if pred t1 t2
        then (
          card := !card + n1 * n2;
          somet2 := true;
          TSet.add (Array.append t1 t2) (n1 * n2) acc
        ) else
          acc
      ) r2.ext acc in
      if !somet2 then acc else (
        card := !card + n1;
        TSet.add (Array.append t1 dummyt2) n1 acc
      )
    ) r1.ext TSet.empty in
    { card = !card; ext = ext; sch = sch } 


  let fullouterjoin pred r1 r2 =
    let card = ref 0 in
    let sch = Array.append r1.sch r2.sch in
    let dummyt2 = Array.make (width r2) None in
    let dummyt1 = Array.make (width r1) None in
    let t2orphans = ref r2.ext in
    let ext = TSet.fold (fun t1 n1 acc ->
      let somet2 = ref false in
      let acc = TSet.fold (fun t2 n2 acc ->
        if pred t1 t2
        then (
          card := !card + n1 * n2;
          somet2 := true;
          t2orphans := TSet.remove t2 !t2orphans;
          TSet.add (Array.append t1 t2) (n1 * n2) acc
        ) else
          acc
      ) r2.ext acc in
      if !somet2 then acc else (
        card := !card + n1;
        TSet.add (Array.append t1 dummyt2) n1 acc
      )
    ) r1.ext TSet.empty in
    let ext = TSet.fold (fun t2 n2 acc ->
      card := !card + n2;
      TSet.add (Array.append dummyt1 t2) n2 acc
    ) !t2orphans ext in
    { card = !card; ext = ext; sch = sch } 

  module VSet = Set.Make(struct type t = value let compare = Pervasives.compare end)

  let fold_all f z a r =
    TSet.fold (fun t n acc ->
      match t.(a) with
      | None -> acc
      | Some v -> (
        let acc' = ref acc in
        for i = 1 to n do
          acc' := f !acc' v
        done;
        !acc'
      )
    ) r.ext z

  let fold_distinct f z a r =
    let seen = ref VSet.empty in
    TSet.fold (fun t n acc ->
      match t.(a) with
      | Some v when not (VSet.mem v !seen) -> (
        seen := VSet.add v !seen;
        f acc v
      )
      | _ -> acc
    ) r.ext z

  let fold dist = if dist then (fun f z a r -> fold_distinct f z a r) else (fun f z a r -> fold_all f z a r)

  let distinct r = { r with ext = TSet.map (fun _ -> 1) r.ext }

  let aggregate la lcol r =
    let card = ref 0 in
    let presch, restrict =
      let aa = Array.of_list la in
      (Array.map (fun a -> r.sch.(a)) aa), (fun t -> Array.map (fun a -> t.(a)) aa)
    in
    let postsch, af =
      let acol = Array.of_list lcol in
      Array.map fst acol, Array.map snd acol
    in
    let partition = TSet.fold (fun t n acc ->
      let t' = restrict t in
      let grp = try TSet.find t' acc with Not_found -> (incr card; { card = 0; ext = TSet.empty; sch = r.sch }) in
      let grp' = { grp with card = grp.card + n; ext = TSet.add t n grp.ext } in
      TSet.add t' grp' acc
    ) r.ext TSet.empty in
    let ext = TSet.fold (fun t' grp acc ->
      TSet.add (Array.append t' (Array.map (fun f -> f grp) af)) 1 acc
    ) partition TSet.empty in
    { card = !card; ext = ext; sch = Array.append presch postsch }

  let union r1 r2 =
    assert (Pervasives.compare r1.sch r2.sch = 0);
    { card = r1.card + r2.card;
      ext = TSet.merge (fun _ n1 n2 -> match (n1, n2) with None, None -> None | Some _, None -> n1 | None, Some _ -> n2 | Some i1, Some i2 -> Some (i1 + i2)) r1.ext r2.ext;
      sch = r1.sch
    }

  let inter r1 r2 =
    assert (Pervasives.compare r1.sch r2.sch = 0);
    let card = ref 0 in
    let ext = TSet.merge (fun _ n1 n2 -> match (n1, n2) with
      | None, None
      | Some _, None
      | None, Some _ -> None
      | Some i1, Some i2 -> (let i = min i1 i2 in card := !card + i; Some i)
    ) r1.ext r2.ext in
    { card = !card;
      ext = ext;
      sch = r1.sch
    }

  let diff r1 r2 =
    assert (Pervasives.compare r1.sch r2.sch = 0);
    let card = ref 0 in
    let ext = TSet.merge (fun _ n1 n2 -> match (n1, n2) with
      | None, None -> None
      | Some i, None -> (card := !card + i; n1)
      | None, Some _ -> None
      | Some i1, Some i2 -> (let i = i1 - i2 in if i <= 0 then None else (card := !card + i; Some i))
    ) r1.ext r2.ext in
    { card = !card;
      ext = ext;
      sch = r1.sch
    }

  let from_file fname sep =
    let split s =
      let r = ref [] in
      let j = ref (String.length s) in
      for i = String.length s - 1 downto 0 do
        if String.unsafe_get s i = sep then begin
          r := String.sub s (i + 1) (!j - i - 1) :: !r;
          j := i
        end
      done;
      Array.of_list (String.sub s 0 !j :: !r)
    in
    let fd = Pervasives.open_in fname in
    let l, sch =
      let l = ref [] in
      let a =
        Array.mapi (fun k s ->
          try
            let i = String.index s '(' in
            let j = String.rindex s ')' in
            if i != 0 then (l := (String.sub s 0 i, k) :: !l);
            V.domain_of_string (String.sub s (i+1) (j-i-1));
          with _ -> V.domain_of_string s
        ) (split (Pervasives.input_line fd)) in
      List.sort Pervasives.compare !l, a
    in
    let ext = ref TSet.empty in
    let card = ref 0 in
    let rec tuple () =
      try
        let t = Array.mapi (fun i s -> try Some (V.value_of_string sch.(i) s) with _ -> None) (split (Pervasives.input_line fd)) in
        ext := TSet.add t (1 + try TSet.find t !ext with Not_found -> 0) !ext;
        incr card;
        tuple ()
      with End_of_file -> ()
    in
    tuple ();
    l, { card = !card; ext = !ext; sch = sch }

  let output fd sep l r =
    match width r with
    | 0 -> ()
    | w -> (
      let o_arr tostr a =
        Pervasives.output_string fd (tostr 0 a.(0));
        for i = 1 to w-1 do
          Pervasives.output_char fd sep;
          Pervasives.output_string fd (tostr i a.(i));
        done;
        Pervasives.output_char fd '\n'
      in
      o_arr (fun i s -> try let attname = List.assoc i l in Printf.sprintf "%s(%s)" attname (V.string_of_domain s) with Not_found -> V.string_of_domain s) r.sch;
      TSet.iter (fun t n ->
        for i = 1 to n do o_arr (fun _ -> (function None -> "" | Some v -> V.string_of_value v)) t done
      ) r.ext
    )

  let to_file fname sep l r = output (Pervasives.open_out fname) sep l r

  let print sep r = output stdout sep r 

end



