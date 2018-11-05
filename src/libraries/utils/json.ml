# 27 "src/libraries/utils/json.mll"
 

type t =
  | Null
  | True | False
  | String of string
  | Number of string
  | Int of int
  | Float of float
  | Array of t list
  | Assoc of (string * t) list

let equal = (=)
let compare = Pervasives.compare

type token = EOF | TRUE | FALSE | NULL | KEY of char
           | STR of string | INT of string | DEC of string

# 21 "src/libraries/utils/json.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\245\255\246\255\000\000\000\000\000\000\250\255\011\000\
    \033\000\078\000\253\255\254\255\255\255\093\000\021\000\103\000\
    \001\000\000\000\249\255\000\000\000\000\002\000\248\255\001\000\
    \003\000\247\255\002\000\248\255\249\255\070\000\255\255\250\255\
    \251\255\252\255\253\255\254\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\010\000\010\000\010\000\255\255\004\000\
    \003\000\010\000\255\255\255\255\255\255\255\255\004\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\007\000\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\000\000\027\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\011\000\012\000\000\000\028\000\011\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \011\000\000\000\010\000\000\000\030\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\006\000\009\000\007\000\000\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\006\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\007\000\
    \013\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\006\000\000\000\006\000\029\000\000\000\
    \000\000\019\000\000\000\000\000\000\000\018\000\004\000\022\000\
    \031\000\000\000\000\000\000\000\020\000\024\000\003\000\025\000\
    \013\000\000\000\016\000\021\000\005\000\023\000\017\000\000\000\
    \000\000\000\000\000\000\006\000\007\000\006\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \015\000\000\000\015\000\000\000\000\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\000\000\035\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\034\000\000\000\000\000\000\000\
    \032\000\000\000\033\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\028\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\026\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\026\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\008\000\
    \007\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\000\000\255\255\000\000\026\000\255\255\
    \255\255\004\000\255\255\255\255\255\255\017\000\000\000\021\000\
    \029\000\255\255\255\255\255\255\019\000\023\000\000\000\024\000\
    \007\000\255\255\005\000\020\000\000\000\003\000\016\000\255\255\
    \255\255\255\255\255\255\000\000\009\000\000\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \013\000\255\255\013\000\255\255\255\255\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\255\255\029\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\029\000\255\255\255\255\255\255\
    \029\000\255\255\029\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\026\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 47 "src/libraries/utils/json.mll"
      ( Lexing.new_line lexbuf ; token lexbuf )
# 156 "src/libraries/utils/json.ml"

  | 1 ->
# 48 "src/libraries/utils/json.mll"
                    ( token lexbuf )
# 161 "src/libraries/utils/json.ml"

  | 2 ->
# 49 "src/libraries/utils/json.mll"
      ( let buffer = Buffer.create 80 in
        string buffer lexbuf ;
        STR(Buffer.contents buffer) )
# 168 "src/libraries/utils/json.ml"

  | 3 ->
# 52 "src/libraries/utils/json.mll"
                    ( INT(Lexing.lexeme lexbuf) )
# 173 "src/libraries/utils/json.ml"

  | 4 ->
# 54 "src/libraries/utils/json.mll"
    ( DEC(Lexing.lexeme lexbuf) )
# 178 "src/libraries/utils/json.ml"

  | 5 ->
let
# 55 "src/libraries/utils/json.mll"
                                 c
# 184 "src/libraries/utils/json.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 55 "src/libraries/utils/json.mll"
                                   ( KEY c )
# 188 "src/libraries/utils/json.ml"

  | 6 ->
# 56 "src/libraries/utils/json.mll"
         ( TRUE )
# 193 "src/libraries/utils/json.ml"

  | 7 ->
# 57 "src/libraries/utils/json.mll"
          ( FALSE )
# 198 "src/libraries/utils/json.ml"

  | 8 ->
# 58 "src/libraries/utils/json.mll"
         ( NULL )
# 203 "src/libraries/utils/json.ml"

  | 9 ->
# 59 "src/libraries/utils/json.mll"
      ( EOF )
# 208 "src/libraries/utils/json.ml"

  | 10 ->
# 60 "src/libraries/utils/json.mll"
    ( failwith "un-recognised token" )
# 213 "src/libraries/utils/json.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and string buffer lexbuf =
   __ocaml_lex_string_rec buffer lexbuf 26
and __ocaml_lex_string_rec buffer lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 63 "src/libraries/utils/json.mll"
        ( () )
# 225 "src/libraries/utils/json.ml"

  | 1 ->
# 64 "src/libraries/utils/json.mll"
           ( Buffer.add_char buffer '\\' ; string buffer lexbuf )
# 230 "src/libraries/utils/json.ml"

  | 2 ->
# 65 "src/libraries/utils/json.mll"
          ( Buffer.add_char buffer '\n' ; string buffer lexbuf )
# 235 "src/libraries/utils/json.ml"

  | 3 ->
# 66 "src/libraries/utils/json.mll"
          ( Buffer.add_char buffer '\t' ; string buffer lexbuf )
# 240 "src/libraries/utils/json.ml"

  | 4 ->
# 67 "src/libraries/utils/json.mll"
          ( Buffer.add_char buffer '\r' ; string buffer lexbuf )
# 245 "src/libraries/utils/json.ml"

  | 5 ->
# 68 "src/libraries/utils/json.mll"
           ( Buffer.add_char buffer '"' ; string buffer lexbuf )
# 250 "src/libraries/utils/json.ml"

  | 6 ->
# 69 "src/libraries/utils/json.mll"
               ( failwith "non-terminated string" )
# 255 "src/libraries/utils/json.ml"

  | 7 ->
let
# 70 "src/libraries/utils/json.mll"
         c
# 261 "src/libraries/utils/json.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 70 "src/libraries/utils/json.mll"
           ( Buffer.add_char buffer c ; string buffer lexbuf )
# 265 "src/libraries/utils/json.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_rec buffer lexbuf __ocaml_lex_state

;;

# 73 "src/libraries/utils/json.mll"
 

type input = {
  lexbuf : Lexing.lexbuf ;
  mutable token : token ;
}

let skip input =
  if input.token <> EOF then input.token <- token input.lexbuf

(* Termination hints:
   - unless EOF, parse_value always eat a token
   - parse_array always eat a token or call parse_value with non-EOF input
   - parse_object always eat a token
   - parse_entry always eat a token or call parse_value with non-EOF input
*)
let rec parse_value input =
  match input.token with
  | EOF -> Null
  | TRUE -> skip input ; True
  | FALSE -> skip input ; False
  | NULL -> skip input ; Null
  | STR a -> skip input ; String a
  | INT a -> skip input ; (try Int(int_of_string a) with _ -> Number a)
  | DEC a -> skip input ; (try Float(float_of_string a) with _ -> Number a)
  | KEY '[' -> skip input ; Array (parse_array [] input)
  | KEY '{' -> skip input ; Assoc (parse_object [] input)
  | _ -> failwith "unexpected token"

and parse_array es input =
  match input.token with
  | EOF -> failwith "non-terminated array"
  | KEY ']' -> skip input ; List.rev es
  | KEY ',' -> skip input ; parse_array es input
  | _ -> let e = parse_value input in parse_array (e::es) input

and parse_object es input =
  match input.token with
  | EOF -> failwith "non-terminated record"
  | KEY '}' -> skip input ; List.rev es
  | KEY ',' -> skip input ; parse_object es input
  | STR a -> skip input ; let e = parse_entry a input in parse_object (e::es) input
  | _ -> failwith "missing name"

and parse_entry a input =
  match input.token with
  | EOF -> failwith "non-terminated record"
  | KEY ':' -> skip input ; parse_entry a input
  | _ -> a , parse_value input

let parse_file input =
  let content = parse_value input in
  if input.token <> EOF then failwith "unexpected end-of-file" ;
  content

exception Error of string * int * string

let error lexbuf msg =
  let open Lexing in
  let position = Lexing.lexeme_start_p lexbuf in
  let token = Lexing.lexeme lexbuf in
  Error(position.pos_fname,position.pos_lnum,
        Printf.sprintf "%s (at %S)" msg token)

let load_lexbuf lexbuf =
  try
    let token = token lexbuf in
    parse_file { lexbuf ; token }
  with Failure msg -> raise (error lexbuf msg)

let load_string text = load_lexbuf (Lexing.from_string text)
let load_channel ?file inc =
  let lexbuf = Lexing.from_channel inc in
  begin
    match file with
    | None -> ()
    | Some pos_fname ->
      let open Lexing in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname }
  end ;
  load_lexbuf lexbuf

let load_file file =
  let inc = open_in file in
  try
    let content = load_channel ~file inc in
    close_in inc ; content
  with e ->
    close_in inc ; raise e

let rec pp fmt v = let open Format in
  match v with
  | Null -> pp_print_string fmt "null"
  | True -> pp_print_string fmt "true"
  | False -> pp_print_string fmt "false"
  | String s -> fprintf fmt "%S" s
  | Number s -> pp_print_string fmt s
  | Int a -> pp_print_int fmt a
  | Float f -> pp_print_float fmt f
  | Array [] -> pp_print_string fmt "[]"
  | Array (e::es) ->
      Format.fprintf fmt "@[<hov 2>[ %a" pp e ;
      List.iter (fun e -> Format.fprintf fmt ",@ %a" pp e) es ;
      Format.fprintf fmt " ]@]"
  | Assoc [] -> pp_print_string fmt "{}"
  | Assoc (e::es) ->
      Format.fprintf fmt "@[<hov 2>{ %a" pp_entry e ;
      List.iter (fun e -> Format.fprintf fmt ",@ %a" pp_entry e) es ;
      Format.fprintf fmt " }@]"

and pp_entry fmt (a,v) = Format.fprintf fmt "@[<hov 2>%S: %a@]" a pp v

let dump_string f s =
  let quote = "\"" in
  f quote ; f (String.escaped s) ; f quote

let rec dump f = function
  | Null -> f "null"
  | True -> f "true"
  | False -> f "false"
  | String s -> dump_string f s
  | Number s -> f s
  | Int a -> f (string_of_int a)
  | Float x -> f (string_of_float x)
  | Array [] -> f "[]"
  | Array (e::es) ->
      f "[" ; dump f e ;
      List.iter (fun e -> f "," ; dump f e) es ;
      f "]"
  | Assoc [] -> f "{}"
  | Assoc (e::es) ->
      f "{" ;
      dump_entry f e ;
      List.iter (fun e -> f "," ; dump_entry f e) es ;
      f "}"

and dump_entry f (a,v) =
  dump_string f a ; f ":" ; dump f v

let pp_dump fmt v =
  dump (Format.pp_print_string fmt) v

let save_buffer ?(pretty=true) buffer v =
  if pretty then
    Format.fprintf (Format.formatter_of_buffer buffer) "@[%a@]@." pp v
  else
    (dump (Buffer.add_string buffer) v ; Buffer.add_char buffer '\n' )

let save_string ?(pretty=true) v =
  let buffer = Buffer.create 80 in
  save_buffer ~pretty buffer v ;
  Buffer.contents buffer

let save_channel ?(pretty=true) out v =
  if pretty then
    Format.fprintf (Format.formatter_of_out_channel out) "@[%a@]@." pp v
  else
    (dump (output_string out) v ; output_char out '\n' ; flush out)

let save_formatter ?(pretty=true) fmt v =
  if pretty then pp fmt v else pp_dump fmt v

let save_file ?(pretty=true) file v =
  let out = open_out file in
  try
    save_channel ~pretty out v ;
    close_out out
  with e ->
    close_out out ; raise e

let invalid name = raise (Invalid_argument ("Json." ^ name))

let bool = function
  | True -> true
  | False -> false
  | _ -> invalid "bool"

let int = function
  | Null -> 0
  | Int n -> n
  | Float f -> (try int_of_float f with _ -> invalid "int")
  | Number s | String s -> (try int_of_string s with _ -> invalid "int")
  | _ -> invalid "int"

let float = function
  | Null -> 0.0
  | Float f -> f
  | Int n -> (try float_of_int n with _ -> invalid "float")
  | Number s | String s -> (try float_of_string s with _ -> invalid "float")
  | _ -> invalid "float"

let string = function
  | Null -> ""
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Number s | String s -> s
  | _ -> invalid "string"

let list = function
  | Null -> []
  | Array es -> es
  | _ -> invalid "list"

let array = function
  | Null -> [| |]
  | Array es -> Array.of_list es
  | _ -> invalid "array"

let field f = function
  | Null -> raise Not_found
  | Assoc fs -> List.assoc f fs
  | _ -> invalid "field"

let fold f v w = match v with
  | Null -> w
  | Assoc fs -> List.fold_left (fun w (e,v) -> f e v w) w fs
  | _ -> invalid "fold"

let of_bool b = if b then True else False
let of_int k = Int k
let of_string s = String s
let of_float f = Float f
let of_list xs = Array xs
let of_array xs = Array (Array.to_list xs)
let of_fields m = Assoc m


# 500 "src/libraries/utils/json.ml"
