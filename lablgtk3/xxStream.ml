(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt       *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t = 'a cell option
and 'a cell = { mutable count : int; mutable data : 'a data }
and 'a data =
    Sempty
  | Scons of 'a * 'a data
  | Sapp of 'a data * 'a data
  | Slazy of 'a data Lazy.t
  | Sgen of 'a gen
  | Sbuffio : buffio -> char data
and 'a gen = { mutable curr : 'a option option; func : int -> 'a option }
and buffio =
  { ic : in_channel; buff : bytes; mutable len : int; mutable ind : int }

exception Failure
exception Error of string

let count = function
  | None -> 0
  | Some { count } -> count

let data = function
  | None -> Sempty
  | Some { data } -> data

let fill_buff b =
  b.len <- input b.ic b.buff 0 (Bytes.length b.buff); b.ind <- 0

let rec get_data : type v. int -> v data -> v data = fun count d -> match d with
 (* Returns either Sempty or Scons(a, _) even when d is a generator
    or a buffer. In those cases, the item a is seen as extracted from
 the generator/buffer.
 The count parameter is used for calling `Sgen-functions'.  *)
   Sempty | Scons (_, _) -> d
 | Sapp (d1, d2) ->
     begin match get_data count d1 with
       Scons (a, d11) -> Scons (a, Sapp (d11, d2))
     | Sempty -> get_data count d2
     | _ -> assert false
     end
 | Sgen {curr = Some None} -> Sempty
 | Sgen ({curr = Some(Some a)} as g) ->
     g.curr <- None; Scons(a, d)
 | Sgen g ->
     begin match g.func count with
       None -> g.curr <- Some(None); Sempty
     | Some a -> Scons(a, d)
         (* Warning: anyone using g thinks that an item has been read *)
     end
 | Sbuffio b ->
     if b.ind >= b.len then fill_buff b;
     if b.len == 0 then Sempty else
       let r = Bytes.unsafe_get b.buff b.ind in
       (* Warning: anyone using g thinks that an item has been read *)
       b.ind <- succ b.ind; Scons(r, d)
 | Slazy f -> get_data count (Lazy.force f)

let rec peek_data : type v. v cell -> v option = fun s ->
 (* consult the first item of s *)
 match s.data with
   Sempty -> None
 | Scons (a, _) -> Some a
 | Sapp (_, _) ->
     begin match get_data s.count s.data with
       Scons(a, _) as d -> s.data <- d; Some a
     | Sempty -> None
     | _ -> assert false
     end
 | Slazy f -> s.data <- (Lazy.force f); peek_data s
 | Sgen {curr = Some a} -> a
 | Sgen g -> let x = g.func s.count in g.curr <- Some x; x
 | Sbuffio b ->
     if b.ind >= b.len then fill_buff b;
     if b.len == 0 then begin s.data <- Sempty; None end
     else Some (Bytes.unsafe_get b.buff b.ind)

let peek = function
  | None -> None
  | Some s -> peek_data s

let rec junk_data : type v. v cell -> unit = fun s ->
  match s.data with
    Scons (_, d) -> s.count <- (succ s.count); s.data <- d
  | Sgen ({curr = Some _} as g) -> s.count <- (succ s.count); g.curr <- None
  | Sbuffio b ->
      if b.ind >= b.len then fill_buff b;
      if b.len == 0 then s.data <- Sempty
      else (s.count <- (succ s.count); b.ind <- succ b.ind)
  | _ ->
      match peek_data s with
        None -> ()
      | Some _ -> junk_data s


let junk = function
  | None -> ()
  | Some data -> junk_data data


(* Stream building functions *)

let from f = Some {count = 0; data = Sgen {curr = None; func = f}}

let of_channel ic =
  Some {count = 0;
        data = Sbuffio {ic = ic; buff = Bytes.create 4096; len = 0; ind = 0}}
