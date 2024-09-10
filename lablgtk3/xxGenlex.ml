(* A local copy of genlex.ml from the OCaml distribution *)

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type token =
    Kwd of string
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Char of char

(* The string buffering machinery *)

let initial_buffer = Bytes.create 32

let buffer = ref initial_buffer
let bufpos = ref 0

let reset_buffer () = buffer := initial_buffer; bufpos := 0

let store c =
  if !bufpos >= Bytes.length !buffer then begin
    let newbuffer = Bytes.create (2 * !bufpos) in
    Bytes.blit !buffer 0 newbuffer 0 !bufpos;
    buffer := newbuffer
  end;
  Bytes.set !buffer !bufpos c;
  incr bufpos

let get_string () =
  let s = Bytes.sub_string !buffer 0 !bufpos in buffer := initial_buffer; s

(* The lexer *)

let make_lexer keywords =
  let kwd_table = Hashtbl.create 17 in
  List.iter (fun s -> Hashtbl.add kwd_table s (Kwd s)) keywords;
  let ident_or_keyword id =
    try Hashtbl.find kwd_table id with
      Not_found -> Ident id
  and keyword_or_error c =
    let s = String.make 1 c in
    try Hashtbl.find kwd_table s with
      Not_found -> raise (XxStream.Error ("Illegal character " ^ s))
  in
  let rec next_token (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some (' ' | '\010' | '\013' | '\009' | '\026' | '\012') ->
        XxStream.junk strm__; next_token strm__
    | Some ('A'..'Z' | 'a'..'z' | '_' | '\192'..'\255' as c) ->
        XxStream.junk strm__;
        let s = strm__ in reset_buffer (); store c; ident s
    | Some
        ('!' | '%' | '&' | '$' | '#' | '+' | '/' | ':' | '<' | '=' | '>' |
         '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c) ->
        XxStream.junk strm__;
        let s = strm__ in reset_buffer (); store c; ident2 s
    | Some ('0'..'9' as c) ->
        XxStream.junk strm__;
        let s = strm__ in reset_buffer (); store c; number s
    | Some '\'' ->
        XxStream.junk strm__;
        let c =
          try char strm__ with
            XxStream.Failure -> raise (XxStream.Error "")
        in
        begin match XxStream.peek strm__ with
          Some '\'' -> XxStream.junk strm__; Some (Char c)
        | _ -> raise (XxStream.Error "")
        end
    | Some '\"' ->
        XxStream.junk strm__;
        let s = strm__ in reset_buffer (); Some (String (string s))
    | Some '-' -> XxStream.junk strm__; neg_number strm__
    | Some '(' -> XxStream.junk strm__; maybe_comment strm__
    | Some c -> XxStream.junk strm__; Some (keyword_or_error c)
    | _ -> None
  and ident (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some
        ('A'..'Z' | 'a'..'z' | '\192'..'\255' | '0'..'9' | '_' | '\'' as c) ->
        XxStream.junk strm__; let s = strm__ in store c; ident s
    | _ -> Some (ident_or_keyword (get_string ()))
  and ident2 (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some
        ('!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':' | '<' | '=' |
         '>' | '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c) ->
        XxStream.junk strm__; let s = strm__ in store c; ident2 s
    | _ -> Some (ident_or_keyword (get_string ()))
  and neg_number (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some ('0'..'9' as c) ->
        XxStream.junk strm__;
        let s = strm__ in reset_buffer (); store '-'; store c; number s
    | _ -> let s = strm__ in reset_buffer (); store '-'; ident2 s
  and number (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some ('0'..'9' as c) ->
        XxStream.junk strm__; let s = strm__ in store c; number s
    | Some '.' ->
        XxStream.junk strm__; let s = strm__ in store '.'; decimal_part s
    | Some ('e' | 'E') ->
        XxStream.junk strm__; let s = strm__ in store 'E'; exponent_part s
    | _ -> Some (Int (int_of_string (get_string ())))
  and decimal_part (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some ('0'..'9' as c) ->
        XxStream.junk strm__; let s = strm__ in store c; decimal_part s
    | Some ('e' | 'E') ->
        XxStream.junk strm__; let s = strm__ in store 'E'; exponent_part s
    | _ -> Some (Float (float_of_string (get_string ())))
  and exponent_part (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some ('+' | '-' as c) ->
        XxStream.junk strm__; let s = strm__ in store c; end_exponent_part s
    | _ -> end_exponent_part strm__
  and end_exponent_part (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some ('0'..'9' as c) ->
        XxStream.junk strm__; let s = strm__ in store c; end_exponent_part s
    | _ -> Some (Float (float_of_string (get_string ())))
  and string (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some '\"' -> XxStream.junk strm__; get_string ()
    | Some '\\' ->
        XxStream.junk strm__;
        let c =
          try escape strm__ with
            XxStream.Failure -> raise (XxStream.Error "")
        in
        let s = strm__ in store c; string s
    | Some c -> XxStream.junk strm__; let s = strm__ in store c; string s
    | _ -> raise XxStream.Failure
  and char (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some '\\' ->
        XxStream.junk strm__;
        begin try escape strm__ with
          XxStream.Failure -> raise (XxStream.Error "")
        end
    | Some c -> XxStream.junk strm__; c
    | _ -> raise XxStream.Failure
  and escape (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some 'n' -> XxStream.junk strm__; '\n'
    | Some 'r' -> XxStream.junk strm__; '\r'
    | Some 't' -> XxStream.junk strm__; '\t'
    | Some ('0'..'9' as c1) ->
        XxStream.junk strm__;
        begin match XxStream.peek strm__ with
          Some ('0'..'9' as c2) ->
            XxStream.junk strm__;
            begin match XxStream.peek strm__ with
              Some ('0'..'9' as c3) ->
                XxStream.junk strm__;
                Char.chr
                  ((Char.code c1 - 48) * 100 + (Char.code c2 - 48) * 10 +
                     (Char.code c3 - 48))
            | _ -> raise (XxStream.Error "")
            end
        | _ -> raise (XxStream.Error "")
        end
    | Some c -> XxStream.junk strm__; c
    | _ -> raise XxStream.Failure
  and maybe_comment (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some '*' ->
        XxStream.junk strm__; let s = strm__ in comment s; next_token s
    | _ -> Some (keyword_or_error '(')
  and comment (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some '(' -> XxStream.junk strm__; maybe_nested_comment strm__
    | Some '*' -> XxStream.junk strm__; maybe_end_comment strm__
    | Some _ -> XxStream.junk strm__; comment strm__
    | _ -> raise XxStream.Failure
  and maybe_nested_comment (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some '*' -> XxStream.junk strm__; let s = strm__ in comment s; comment s
    | Some _ -> XxStream.junk strm__; comment strm__
    | _ -> raise XxStream.Failure
  and maybe_end_comment (strm__ : _ XxStream.t) =
    match XxStream.peek strm__ with
      Some ')' -> XxStream.junk strm__; ()
    | Some '*' -> XxStream.junk strm__; maybe_end_comment strm__
    | Some _ -> XxStream.junk strm__; comment strm__
    | _ -> raise XxStream.Failure
  in
  fun input -> XxStream.from (fun _count -> next_token input)
