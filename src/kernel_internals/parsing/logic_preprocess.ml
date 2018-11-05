# 25 "src/kernel_internals/parsing/logic_preprocess.mll"
 
  open Lexing
  type end_of_buffer = NEWLINE | SPACE | CHAR
  let preprocess_buffer = Buffer.create 1024

  let output_buffer = Buffer.create 1024
  (* Standard prohibits the predefined macros to be subject of a #define
     (or #undef) directive. We thus have to filter the definition of these
     macros from gcc's output (gcc emits a warning otherwise).
     The list of predefined macros is taken from C11 standard, in the order
     in which they are defined in Section 6.10.8
   *)
  let blacklisted_macros = [
    (* 6.10.8.1 mandatory macros. *)
    "__DATE__"; "__FILE"; "__LINE__"; "__STDC__"; "__STDC_HOSTED__";
    "__STDC_VERSION__"; "__TIME__";
    (* 6.10.8.2 environment macros *)
    "__STDC_ISO_10646__"; "__STDC_MB_MIGHT_NEQ_WC__";
    "__STDC_UTF_16__"; "__STDC_UTF_32__";
    (* 6.10.8.3 conditional feature macros *)
    "__STDC_ANALYZABLE__"; "__STDC_IEC_559__"; "__STDC_IEC_559_COMPLEX__";
    "__STDC_LIB_EXT1__"; "__STD_NO_ATOMICS__"; "__STD_NO_COMPLEX__";
    "__STDC_NO_THREADS__"; "__STDC_NO_VLA__";
    (* expanding assert, an ACSL keyword, is not a good idea. *)
    "assert"]
  let is_newline = ref CHAR
  let curr_file = ref ""
  let curr_line = ref 1
  let has_annot = ref false

  let reset () =
    Buffer.clear preprocess_buffer;
    Buffer.clear output_buffer;
    is_newline := CHAR;
    curr_file := "";
    curr_line := 1;
    has_annot := false

  let backslash = "__ANNOT_BACKSLASH__"
  let annot_content = "__ANNOT_CONTENT__"

  let re_backslash = Str.regexp_string backslash
  let re_annot_content = Str.regexp_string annot_content

  (* Delimiters for the various annotations in the preprocessing buffer.
     We have one delimiter for the beginning of an annotation (to discard
     #defines along the way), and three delimiters for the various ways
     an annotation can end:
      - on a normal line
      - with a newline
      - with a newline inside a comment (only for one-line annotations)
     When preprocessed annotations are inserted back in the main file, this will
     result in distinct translation to preserve line numbers while avoiding
     ill-formed annotations.
  *)
  let annot_beg =         "////////////////__ANNOT_BEG__"
  let annot_end =         "////////////////__ANNOT_END__"
  let annot_end_nl  =     "////////////////__ANNOT_END_NL__"
  let annot_end_comment = "////////////////__ANNOT_END_COMMENT__"

  let abort_preprocess reason =
    let source = {Lexing.dummy_pos with Lexing.pos_fname = !curr_file;
                  pos_lnum = !curr_line;}
    in
    Kernel.error ~source
      "Can't preprocess annotation: %s\nSome annotations will be kept as is"
      reason

  let next_preprocessed file =
    let content = Buffer.create 80 in
    let rec ignore_content () =
      let s = input_line file in
      if s <> annot_beg then ignore_content ()
    in
    let rec get_annot first =
      let s = input_line file in
      if s = annot_end then false, Buffer.contents content
      else if s = annot_end_nl then true, Buffer.contents content
      else if s = annot_end_comment then begin
        Buffer.add_char content '\n';
        false, Buffer.contents content
      end else begin
        if not first then Buffer.add_char content '\n';
        Buffer.add_string content s;
        get_annot false
      end
    in
    let replace_backslash s = Str.global_replace re_backslash "\\\\" s in
    try
      ignore_content ();
      ignore (input_line file); (* ignore the #line directive *)
      let with_nl, content = get_annot true in
      with_nl, replace_backslash content
    with End_of_file ->
      Kernel.fatal
        "too few annotations in result file while pre-processing annotations"

  let output_result outfile preprocessed content =
    let rec aux = function
      | [] -> ()
      | [s] -> output_string outfile s
      | content :: rem ->
          output_string outfile content;
          output_string outfile "/*@";
          let with_nl, pp_content = next_preprocessed preprocessed in
          output_string outfile pp_content;
          output_string outfile "*/";
          if with_nl then output_char outfile '\n';
          aux rem
    in aux content

  let preprocess_annots suffix cpp outfile =
    if !has_annot then begin
      let debug =
        Kernel.debug_atleast 3 ||
          Kernel.is_debug_key_enabled Kernel.dkey_parser
      in
      let ppname =
        try Extlib.temp_file_cleanup_at_exit ~debug "ppannot" suffix
        with Extlib.Temp_file_error s ->
          Kernel.abort
            "Could not open temporary file for logic pre-processing: %s" s
      in
      let ppfile = open_out ppname in
      Buffer.output_buffer ppfile preprocess_buffer;
      close_out ppfile;
      let cppname = Extlib.temp_file_cleanup_at_exit ~debug "cppannot" suffix in
      let res = Sys.command (cpp ppname cppname) in
      let result_file =
        if res <> 0 then begin
          abort_preprocess "Preprocessor call exited with an error";
          if not debug then Extlib.safe_remove cppname;
          ppname
        end else cppname
      in
      let result = open_in result_file in
      let content =
        Str.split_delim re_annot_content (Buffer.contents output_buffer)
      in
      output_result outfile result content;
      close_in result
    end else begin
      Buffer.output_buffer outfile output_buffer
    end;
    flush outfile

  let add_preprocess_line_info () =
    Printf.bprintf
      preprocess_buffer "# %d %s \n" !curr_line !curr_file

  let make_newline () = incr curr_line

  let process_annot_start () =
    is_newline := CHAR;
    has_annot := true;
    Buffer.add_string output_buffer annot_content;
    Buffer.add_string preprocess_buffer annot_beg;
    Buffer.add_char preprocess_buffer '\n';
    add_preprocess_line_info()

# 163 "src/kernel_internals/parsing/logic_preprocess.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\245\255\246\255\247\255\248\255\249\255\001\000\004\000\
    \017\000\000\000\031\000\000\000\000\000\000\000\001\000\001\000\
    \002\000\081\000\160\000\000\000\006\000\006\000\004\000\008\000\
    \018\001\028\000\010\000\030\000\031\000\254\255\029\000\032\000\
    \252\255\000\000\253\255\250\255\001\000\251\255\081\000\250\255\
    \251\255\252\255\253\255\003\000\001\000\255\255\254\255\036\000\
    \253\255\003\000\255\255\254\255\082\000\249\255\250\255\251\255\
    \252\255\085\000\253\255\254\255\255\255\212\000\249\255\250\255\
    \251\255\252\255\213\000\253\255\254\255\255\255\023\001\250\255\
    \252\255\253\255\083\000\255\255\251\255\254\255\024\001\250\255\
    \252\255\253\255\050\000\255\255\251\255\254\255\044\001\247\255\
    \248\255\249\255\250\255\251\255\252\255\028\000\254\255\029\000\
    \255\255\253\255\130\000\252\255\253\255\030\000\255\255\254\255\
    \025\001\250\255\251\255\215\000\254\255\255\255\252\255\253\255\
    \027\001\251\255\252\255\003\000\254\255\255\255\253\255\030\001\
    \252\255\253\255\254\255\255\255\045\001\250\255\047\000\252\255\
    \253\255\254\255\255\255\251\255\087\000\254\255\255\255\088\000\
    \254\255\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\255\255\010\000\010\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\255\255\255\255\005\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\005\000\005\000\255\255\255\255\255\255\
    \255\255\002\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\006\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\006\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\005\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\005\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\008\000\255\255\008\000\
    \255\255\255\255\255\255\255\255\255\255\003\000\255\255\255\255\
    \255\255\255\255\255\255\005\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\004\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\005\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\027\000\255\255\027\000\028\000\000\000\035\000\032\000\
    \000\000\255\255\000\000\000\000\255\255\000\000\039\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\000\000\000\000\048\000\
    \000\000\255\255\000\000\000\000\053\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\062\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\071\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\079\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\087\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\255\255\
    \000\000\000\000\099\000\000\000\000\000\255\255\000\000\000\000\
    \105\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \113\000\000\000\000\000\255\255\000\000\000\000\000\000\120\000\
    \000\000\000\000\000\000\000\000\125\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\133\000\000\000\000\000\136\000\
    \000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\005\000\045\000\000\000\010\000\000\000\000\000\
    \000\000\000\000\000\000\026\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\026\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\007\000\010\000\118\000\000\000\002\000\
    \010\000\029\000\026\000\031\000\025\000\046\000\050\000\006\000\
    \030\000\026\000\051\000\025\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\255\255\010\000\
    \028\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\097\000\096\000\103\000\049\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\085\000\017\000\040\000\056\000\036\000\131\000\059\000\
    \033\000\134\000\137\000\000\000\020\000\013\000\014\000\017\000\
    \012\000\022\000\015\000\021\000\017\000\024\000\019\000\016\000\
    \009\000\017\000\023\000\042\000\054\000\077\000\000\000\060\000\
    \041\000\011\000\000\000\034\000\037\000\034\000\037\000\000\000\
    \043\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\009\000\102\000\000\000\084\000\000\000\
    \000\000\000\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\101\000\044\000\057\000\076\000\
    \018\000\058\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\000\000\000\000\000\000\000\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\000\000\000\000\000\000\000\000\065\000\068\000\
    \000\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\063\000\069\000\000\000\111\000\018\000\
    \004\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\024\000\255\255\255\255\255\255\255\255\
    \255\255\072\000\080\000\109\000\255\255\117\000\000\000\000\000\
    \122\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \066\000\067\000\024\000\110\000\000\000\000\000\094\000\130\000\
    \000\000\073\000\000\000\000\000\000\000\116\000\000\000\081\000\
    \108\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\091\000\123\000\088\000\127\000\
    \000\000\255\255\055\000\089\000\128\000\000\000\095\000\134\000\
    \137\000\000\000\000\000\093\000\126\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\092\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\074\000\082\000\107\000\000\000\115\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\100\000\000\000\000\000\000\000\000\000\000\000\
    \090\000\129\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\064\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\075\000\
    \083\000\106\000\000\000\114\000\000\000\000\000\121\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\130\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\044\000\255\255\007\000\255\255\255\255\
    \255\255\255\255\255\255\026\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\008\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\007\000\115\000\255\255\000\000\
    \010\000\028\000\026\000\006\000\026\000\043\000\047\000\000\000\
    \006\000\008\000\049\000\008\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\025\000\010\000\
    \027\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\093\000\095\000\101\000\047\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\082\000\017\000\038\000\052\000\030\000\126\000\057\000\
    \031\000\132\000\135\000\255\255\019\000\012\000\013\000\016\000\
    \007\000\009\000\014\000\020\000\021\000\023\000\011\000\015\000\
    \007\000\017\000\022\000\038\000\052\000\074\000\255\255\057\000\
    \038\000\007\000\255\255\033\000\036\000\033\000\036\000\255\255\
    \038\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\010\000\098\000\255\255\082\000\255\255\
    \255\255\255\255\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\098\000\038\000\052\000\074\000\
    \017\000\057\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\255\255\255\255\255\255\255\255\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\255\255\255\255\255\255\255\255\061\000\066\000\
    \255\255\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\061\000\066\000\255\255\107\000\018\000\
    \000\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\024\000\025\000\030\000\027\000\028\000\
    \031\000\070\000\078\000\104\000\047\000\112\000\255\255\255\255\
    \119\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \061\000\066\000\024\000\107\000\255\255\255\255\086\000\124\000\
    \255\255\070\000\255\255\255\255\255\255\112\000\255\255\078\000\
    \104\000\255\255\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\086\000\119\000\086\000\124\000\
    \255\255\038\000\052\000\086\000\124\000\255\255\086\000\132\000\
    \135\000\255\255\255\255\086\000\124\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\086\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\070\000\078\000\104\000\255\255\112\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\098\000\255\255\255\255\255\255\255\255\255\255\
    \086\000\124\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\061\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\070\000\
    \078\000\104\000\255\255\112\000\255\255\255\255\119\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\086\000\124\000";
  Lexing.lex_base_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \010\000\000\000\036\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\000\000\000\000\000\000\000\000\000\000\002\000\
    \062\000\000\000\002\000\002\000\000\000\026\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_backtrk_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\020\000\020\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_default_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_trans_code =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\017\000\009\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\017\000\009\000\001\000\023\000\000\000\000\000\000\000\
    \000\000\000\000\009\000\000\000\000\000\001\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\001\000\000\000\000\000\001\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\001\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\017\000\017\000\001\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check_code =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\007\000\017\000\026\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\008\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \007\000\017\000\026\000\000\000\027\000\255\255\255\255\255\255\
    \255\255\255\255\008\000\255\255\255\255\010\000\255\255\255\255\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\010\000\255\255\255\255\024\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\024\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\021\000\023\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_code =
   "\255\004\255\255\006\255\005\255\255\006\255\255\005\255\006\255\
    \255\007\255\255\000\007\255\008\255\255\000\004\001\005\002\006\
    \003\008\255";
}

let rec main lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 9 (-1); __ocaml_lex_main_rec lexbuf 0
and __ocaml_lex_main_rec lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 187 "src/kernel_internals/parsing/logic_preprocess.mll"
                                                                      m
# 517 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 188 "src/kernel_internals/parsing/logic_preprocess.mll"
      (
        let blacklisted = List.mem m blacklisted_macros in
        if not blacklisted then
          Buffer.add_string preprocess_buffer (lexeme lexbuf);
        macro blacklisted lexbuf
      )
# 526 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
let
# 194 "src/kernel_internals/parsing/logic_preprocess.mll"
                                                       line
# 532 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 195 "src/kernel_internals/parsing/logic_preprocess.mll"
                                     file
# 537 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(2) lexbuf.Lexing.lex_mem.(3) in
# 196 "src/kernel_internals/parsing/logic_preprocess.mll"
    ( (try
        curr_line := (int_of_string line) -1
       with Failure _ -> curr_line:= -1);
      if file <> "" then curr_file := file;
      Buffer.add_string output_buffer (lexeme lexbuf);
      make_newline();
      main lexbuf
    )
# 548 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
let
# 204 "src/kernel_internals/parsing/logic_preprocess.mll"
                        c
# 554 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3) in
# 204 "src/kernel_internals/parsing/logic_preprocess.mll"
                           ( (* Skip special doxygen comments. Use of '@'
                                instead of !Clexer.annot_char is intentional *)
        Buffer.add_string output_buffer (lexeme lexbuf);
        comment c lexbuf;)
# 561 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
let
# 208 "src/kernel_internals/parsing/logic_preprocess.mll"
                c
# 567 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
# 208 "src/kernel_internals/parsing/logic_preprocess.mll"
                   (
      if c = !Clexer.annot_char then begin
        process_annot_start ();
        annot lexbuf
      end else begin
        if c = '\n' then make_newline();
        Buffer.add_string output_buffer (lexeme lexbuf);
        comment c lexbuf;
      end)
# 579 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 4 ->
# 217 "src/kernel_internals/parsing/logic_preprocess.mll"
                      ( (* See comments for "/*@{" above *)
        Buffer.add_string output_buffer (lexeme lexbuf);
        oneline_comment lexbuf;
      )
# 587 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 5 ->
let
# 221 "src/kernel_internals/parsing/logic_preprocess.mll"
                c
# 593 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
# 221 "src/kernel_internals/parsing/logic_preprocess.mll"
                   (
      if c = !Clexer.annot_char then begin
        process_annot_start ();
        oneline_annot lexbuf
      end
      else if c = '\n' then begin
        make_newline ();
        Buffer.add_string output_buffer (lexeme lexbuf);
        main lexbuf
      end
      else begin
        Buffer.add_string output_buffer (lexeme lexbuf);
        oneline_comment lexbuf;
      end)
# 610 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 6 ->
# 235 "src/kernel_internals/parsing/logic_preprocess.mll"
         (
      make_newline (); Buffer.add_char output_buffer '\n'; main lexbuf )
# 616 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 7 ->
# 237 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( )
# 621 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 8 ->
# 238 "src/kernel_internals/parsing/logic_preprocess.mll"
        (
      Buffer.add_char output_buffer '"'; 
      c_string lexbuf )
# 628 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 9 ->
# 241 "src/kernel_internals/parsing/logic_preprocess.mll"
        (
      Buffer.add_char output_buffer '\'';
      c_char lexbuf )
# 635 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 10 ->
let
# 244 "src/kernel_internals/parsing/logic_preprocess.mll"
         c
# 641 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 244 "src/kernel_internals/parsing/logic_preprocess.mll"
           (
      Buffer.add_char output_buffer c;
      main lexbuf )
# 647 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_main_rec lexbuf __ocaml_lex_state

and macro blacklisted lexbuf =
   __ocaml_lex_macro_rec blacklisted lexbuf 38
and __ocaml_lex_macro_rec blacklisted lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 248 "src/kernel_internals/parsing/logic_preprocess.mll"
         (
      make_newline ();
      Buffer.add_char output_buffer '\n';
      macro blacklisted lexbuf
    )
# 663 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 255 "src/kernel_internals/parsing/logic_preprocess.mll"
       ( macro_comment blacklisted lexbuf )
# 668 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
# 256 "src/kernel_internals/parsing/logic_preprocess.mll"
      ( 
  if not blacklisted then
    Buffer.add_char preprocess_buffer '"';
  macro_string blacklisted lexbuf
)
# 677 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
# 261 "src/kernel_internals/parsing/logic_preprocess.mll"
      (
  if not blacklisted then
    Buffer.add_char preprocess_buffer '\'';
  macro_char blacklisted lexbuf
)
# 686 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 4 ->
# 266 "src/kernel_internals/parsing/logic_preprocess.mll"
       (
      if not blacklisted then
        Buffer.add_char preprocess_buffer '\n';
      make_newline ();
      Buffer.add_char output_buffer '\n';
      main lexbuf
    )
# 697 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 5 ->
let
# 273 "src/kernel_internals/parsing/logic_preprocess.mll"
       c
# 703 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 273 "src/kernel_internals/parsing/logic_preprocess.mll"
         (
           if not blacklisted then
             Buffer.add_char preprocess_buffer c;
           macro blacklisted lexbuf
         )
# 711 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_macro_rec blacklisted lexbuf __ocaml_lex_state

and macro_comment blacklisted lexbuf =
   __ocaml_lex_macro_comment_rec blacklisted lexbuf 47
and __ocaml_lex_macro_comment_rec blacklisted lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 279 "src/kernel_internals/parsing/logic_preprocess.mll"
       (
      make_newline ();

      macro_comment blacklisted lexbuf
    )
# 727 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 284 "src/kernel_internals/parsing/logic_preprocess.mll"
       ( macro blacklisted lexbuf )
# 732 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
# 285 "src/kernel_internals/parsing/logic_preprocess.mll"
     ( macro_comment blacklisted lexbuf )
# 737 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_macro_comment_rec blacklisted lexbuf __ocaml_lex_state

and macro_string blacklisted lexbuf =
   __ocaml_lex_macro_string_rec blacklisted lexbuf 52
and __ocaml_lex_macro_string_rec blacklisted lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 288 "src/kernel_internals/parsing/logic_preprocess.mll"
             s
# 750 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 288 "src/kernel_internals/parsing/logic_preprocess.mll"
               (
  if not blacklisted then Buffer.add_string preprocess_buffer s;
  macro_string blacklisted lexbuf
  )
# 757 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 292 "src/kernel_internals/parsing/logic_preprocess.mll"
         (
  make_newline();
  Buffer.add_char output_buffer '\n';
  macro_string blacklisted lexbuf
)
# 766 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
let
# 297 "src/kernel_internals/parsing/logic_preprocess.mll"
            s
# 772 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 297 "src/kernel_internals/parsing/logic_preprocess.mll"
              (
    if not blacklisted then Buffer.add_string preprocess_buffer s;
    macro_string blacklisted lexbuf
  )
# 779 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
# 301 "src/kernel_internals/parsing/logic_preprocess.mll"
       ( abort_preprocess "unterminated string in macro definition" )
# 784 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 4 ->
# 302 "src/kernel_internals/parsing/logic_preprocess.mll"
      ( abort_preprocess "unterminated string in macro definition" )
# 789 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 5 ->
# 303 "src/kernel_internals/parsing/logic_preprocess.mll"
      ( if not blacklisted then Buffer.add_char preprocess_buffer '"';
        macro blacklisted lexbuf )
# 795 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 6 ->
let
# 305 "src/kernel_internals/parsing/logic_preprocess.mll"
       c
# 801 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 305 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( if not blacklisted then Buffer.add_char preprocess_buffer c;
           macro_string blacklisted lexbuf )
# 806 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_macro_string_rec blacklisted lexbuf __ocaml_lex_state

and macro_char blacklisted lexbuf =
   __ocaml_lex_macro_char_rec blacklisted lexbuf 61
and __ocaml_lex_macro_char_rec blacklisted lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 308 "src/kernel_internals/parsing/logic_preprocess.mll"
            s
# 819 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 308 "src/kernel_internals/parsing/logic_preprocess.mll"
              (
  if not blacklisted then Buffer.add_string preprocess_buffer s;
  macro_char blacklisted lexbuf
  )
# 826 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 312 "src/kernel_internals/parsing/logic_preprocess.mll"
         (
  make_newline();
  Buffer.add_char output_buffer '\n';
  macro_char blacklisted lexbuf
)
# 835 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
let
# 317 "src/kernel_internals/parsing/logic_preprocess.mll"
            s
# 841 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 2) in
# 317 "src/kernel_internals/parsing/logic_preprocess.mll"
              (
    if not blacklisted then Buffer.add_string preprocess_buffer s;
    macro_char blacklisted lexbuf
  )
# 848 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
# 321 "src/kernel_internals/parsing/logic_preprocess.mll"
       ( abort_preprocess "unterminated char in macro definition" )
# 853 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 4 ->
# 322 "src/kernel_internals/parsing/logic_preprocess.mll"
      ( abort_preprocess "unterminated char in macro definition" )
# 858 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 5 ->
# 323 "src/kernel_internals/parsing/logic_preprocess.mll"
      ( if not blacklisted then Buffer.add_char preprocess_buffer '\'';
        macro blacklisted lexbuf )
# 864 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 6 ->
let
# 325 "src/kernel_internals/parsing/logic_preprocess.mll"
       c
# 870 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 325 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( if not blacklisted then Buffer.add_char preprocess_buffer c;
           macro_char blacklisted lexbuf )
# 875 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_macro_char_rec blacklisted lexbuf __ocaml_lex_state

and c_string lexbuf =
   __ocaml_lex_c_string_rec lexbuf 70
and __ocaml_lex_c_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 328 "src/kernel_internals/parsing/logic_preprocess.mll"
      ( abort_preprocess "unterminated string" )
# 887 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 329 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_string output_buffer (lexeme lexbuf); c_string lexbuf )
# 892 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
# 330 "src/kernel_internals/parsing/logic_preprocess.mll"
       ( Buffer.add_char output_buffer '"'; main lexbuf )
# 897 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
# 331 "src/kernel_internals/parsing/logic_preprocess.mll"
       ( make_newline ();
         Buffer.add_char output_buffer '\n';
         c_string lexbuf
       )
# 905 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 4 ->
# 335 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_string output_buffer (lexeme lexbuf); c_string lexbuf )
# 910 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 5 ->
let
# 336 "src/kernel_internals/parsing/logic_preprocess.mll"
       c
# 916 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 336 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_char output_buffer c; c_string lexbuf )
# 920 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_c_string_rec lexbuf __ocaml_lex_state

and c_char lexbuf =
   __ocaml_lex_c_char_rec lexbuf 78
and __ocaml_lex_c_char_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 339 "src/kernel_internals/parsing/logic_preprocess.mll"
      ( abort_preprocess "unterminated char" )
# 932 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 340 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_string output_buffer (lexeme lexbuf);
           c_char lexbuf )
# 938 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
# 342 "src/kernel_internals/parsing/logic_preprocess.mll"
      ( Buffer.add_char output_buffer '\''; main lexbuf )
# 943 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
# 343 "src/kernel_internals/parsing/logic_preprocess.mll"
       ( make_newline ();
         Buffer.add_char output_buffer '\n';
         c_char lexbuf
       )
# 951 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 4 ->
# 347 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_string output_buffer (lexeme lexbuf); c_char lexbuf )
# 956 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 5 ->
let
# 348 "src/kernel_internals/parsing/logic_preprocess.mll"
       c
# 962 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 348 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_char output_buffer c; c_char lexbuf )
# 966 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_c_char_rec lexbuf __ocaml_lex_state

and annot lexbuf =
   __ocaml_lex_annot_rec lexbuf 86
and __ocaml_lex_annot_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 351 "src/kernel_internals/parsing/logic_preprocess.mll"
          (
      if !is_newline = NEWLINE then
        Buffer.add_string preprocess_buffer annot_end_nl
      else begin
        Buffer.add_char preprocess_buffer '\n';
        Buffer.add_string preprocess_buffer annot_end;
      end;
      Buffer.add_char preprocess_buffer '\n';
      main lexbuf )
# 986 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 360 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( is_newline := NEWLINE;
           incr curr_line;
           Buffer.add_char preprocess_buffer '\n';
           annot lexbuf )
# 994 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
# 364 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_string preprocess_buffer "//";
           annot_comment lexbuf )
# 1000 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
# 366 "src/kernel_internals/parsing/logic_preprocess.mll"
        (
      if !is_newline = NEWLINE then is_newline:=SPACE;
      Buffer.add_char preprocess_buffer '@';
      annot lexbuf )
# 1008 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 4 ->
# 370 "src/kernel_internals/parsing/logic_preprocess.mll"
         (
      if !is_newline = NEWLINE then is_newline:=SPACE;
      Buffer.add_char preprocess_buffer ' ';
      annot lexbuf )
# 1016 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 5 ->
# 378 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( 
        is_newline := CHAR;
        Buffer.add_string preprocess_buffer backslash;
        annot lexbuf )
# 1024 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 6 ->
# 382 "src/kernel_internals/parsing/logic_preprocess.mll"
         (
        is_newline := CHAR;
        Buffer.add_char preprocess_buffer '\'';
        char annot lexbuf )
# 1032 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 7 ->
# 386 "src/kernel_internals/parsing/logic_preprocess.mll"
         (
        is_newline:=CHAR;
        Buffer.add_char preprocess_buffer '"';
        string annot lexbuf )
# 1040 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 8 ->
let
# 390 "src/kernel_internals/parsing/logic_preprocess.mll"
         c
# 1046 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 390 "src/kernel_internals/parsing/logic_preprocess.mll"
           ( is_newline := CHAR;
             Buffer.add_char preprocess_buffer c;
             annot lexbuf )
# 1052 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_annot_rec lexbuf __ocaml_lex_state

and annot_comment lexbuf =
   __ocaml_lex_annot_comment_rec lexbuf 98
and __ocaml_lex_annot_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 395 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( incr curr_line; is_newline:=NEWLINE;
           Buffer.add_char preprocess_buffer '\n';
           annot lexbuf
         )
# 1067 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 399 "src/kernel_internals/parsing/logic_preprocess.mll"
         (
        Buffer.add_char preprocess_buffer '\n';
        Buffer.add_string preprocess_buffer annot_end;
        Buffer.add_char preprocess_buffer '\n';
        main lexbuf )
# 1076 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
# 404 "src/kernel_internals/parsing/logic_preprocess.mll"
        ( abort_preprocess "eof in the middle of a comment" )
# 1081 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
let
# 405 "src/kernel_internals/parsing/logic_preprocess.mll"
         c
# 1087 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 405 "src/kernel_internals/parsing/logic_preprocess.mll"
           (
    Buffer.add_char preprocess_buffer c; annot_comment lexbuf )
# 1092 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_annot_comment_rec lexbuf __ocaml_lex_state

and char annot lexbuf =
   __ocaml_lex_char_rec annot lexbuf 104
and __ocaml_lex_char_rec annot lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 410 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( incr curr_line; is_newline:=NEWLINE;
           Buffer.add_char preprocess_buffer '\n';
           char annot lexbuf
         )
# 1107 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 414 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( is_newline:=CHAR;
           Buffer.add_char preprocess_buffer '\'';
           annot lexbuf )
# 1114 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
# 417 "src/kernel_internals/parsing/logic_preprocess.mll"
          ( is_newline:=CHAR;
            Buffer.add_string preprocess_buffer "\\'";
            char annot lexbuf )
# 1121 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
# 420 "src/kernel_internals/parsing/logic_preprocess.mll"
           ( is_newline:=CHAR;
            Buffer.add_string preprocess_buffer "\\\\";
            char annot lexbuf )
# 1128 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 4 ->
# 423 "src/kernel_internals/parsing/logic_preprocess.mll"
        ( abort_preprocess "eof while parsing a char literal" )
# 1133 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 5 ->
let
# 424 "src/kernel_internals/parsing/logic_preprocess.mll"
         c
# 1139 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 424 "src/kernel_internals/parsing/logic_preprocess.mll"
           ( is_newline:=CHAR;
             Buffer.add_char preprocess_buffer c;
             char annot lexbuf )
# 1145 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_char_rec annot lexbuf __ocaml_lex_state

and string annot lexbuf =
   __ocaml_lex_string_rec annot lexbuf 112
and __ocaml_lex_string_rec annot lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 429 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( incr curr_line; is_newline:=NEWLINE;
           Buffer.add_char preprocess_buffer '\n'; string annot lexbuf
         )
# 1159 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 432 "src/kernel_internals/parsing/logic_preprocess.mll"
        ( is_newline:=CHAR;
          Buffer.add_char preprocess_buffer '"'; annot lexbuf )
# 1165 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
# 434 "src/kernel_internals/parsing/logic_preprocess.mll"
           ( is_newline:=CHAR;
             Buffer.add_string preprocess_buffer "\\\"";
             string annot lexbuf )
# 1172 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
# 437 "src/kernel_internals/parsing/logic_preprocess.mll"
        ( abort_preprocess "eof while parsing a string literal" )
# 1177 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 4 ->
let
# 438 "src/kernel_internals/parsing/logic_preprocess.mll"
         c
# 1183 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 438 "src/kernel_internals/parsing/logic_preprocess.mll"
           ( is_newline:=CHAR;
             Buffer.add_char preprocess_buffer c;
             string annot lexbuf )
# 1189 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_rec annot lexbuf __ocaml_lex_state

and comment c lexbuf =
   __ocaml_lex_comment_rec c lexbuf 119
and __ocaml_lex_comment_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 444 "src/kernel_internals/parsing/logic_preprocess.mll"
        (
      Buffer.add_char output_buffer  '/';
      if c = '*' then
        main lexbuf
      else
        comment '/' lexbuf
      )
# 1207 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 451 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( make_newline (); Buffer.add_char output_buffer '\n';
           comment '\n' lexbuf )
# 1213 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
# 453 "src/kernel_internals/parsing/logic_preprocess.mll"
        ( abort_preprocess "eof while parsing C comment" )
# 1218 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
let
# 454 "src/kernel_internals/parsing/logic_preprocess.mll"
         c
# 1224 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 454 "src/kernel_internals/parsing/logic_preprocess.mll"
           ( Buffer.add_char output_buffer c; comment c lexbuf )
# 1228 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec c lexbuf __ocaml_lex_state

and oneline_annot lexbuf =
   __ocaml_lex_oneline_annot_rec lexbuf 124
and __ocaml_lex_oneline_annot_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 457 "src/kernel_internals/parsing/logic_preprocess.mll"
             (
      incr curr_line;
      Buffer.add_char preprocess_buffer '\n';
      Buffer.add_string preprocess_buffer annot_end_nl;
      Buffer.add_char preprocess_buffer '\n';
      main lexbuf )
# 1245 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
# 463 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_string preprocess_buffer backslash;
           oneline_annot lexbuf )
# 1251 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 2 ->
# 465 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_char preprocess_buffer '\'';
           char oneline_annot lexbuf )
# 1257 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 3 ->
# 467 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_char preprocess_buffer '"';
           string oneline_annot lexbuf )
# 1263 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 4 ->
# 469 "src/kernel_internals/parsing/logic_preprocess.mll"
         ( Buffer.add_string preprocess_buffer "//";
           oneline_annot_comment lexbuf )
# 1269 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 5 ->
let
# 471 "src/kernel_internals/parsing/logic_preprocess.mll"
         c
# 1275 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 471 "src/kernel_internals/parsing/logic_preprocess.mll"
           ( Buffer.add_char preprocess_buffer c;
             oneline_annot lexbuf )
# 1280 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_oneline_annot_rec lexbuf __ocaml_lex_state

and oneline_annot_comment lexbuf =
   __ocaml_lex_oneline_annot_comment_rec lexbuf 132
and __ocaml_lex_oneline_annot_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 475 "src/kernel_internals/parsing/logic_preprocess.mll"
             (
       incr curr_line;
       Buffer.add_char preprocess_buffer '\n';
       Buffer.add_string preprocess_buffer annot_end_comment;
       Buffer.add_char preprocess_buffer '\n';
       main lexbuf )
# 1297 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
let
# 481 "src/kernel_internals/parsing/logic_preprocess.mll"
         c
# 1303 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 481 "src/kernel_internals/parsing/logic_preprocess.mll"
           ( Buffer.add_char preprocess_buffer c;
             oneline_annot_comment lexbuf )
# 1308 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_oneline_annot_comment_rec lexbuf __ocaml_lex_state

and oneline_comment lexbuf =
   __ocaml_lex_oneline_comment_rec lexbuf 135
and __ocaml_lex_oneline_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 487 "src/kernel_internals/parsing/logic_preprocess.mll"
      ( make_newline();
        Buffer.add_string output_buffer (lexeme lexbuf);
        main lexbuf)
# 1322 "src/kernel_internals/parsing/logic_preprocess.ml"

  | 1 ->
let
# 490 "src/kernel_internals/parsing/logic_preprocess.mll"
         c
# 1328 "src/kernel_internals/parsing/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 490 "src/kernel_internals/parsing/logic_preprocess.mll"
           ( Buffer.add_char output_buffer c;
             oneline_comment lexbuf)
# 1333 "src/kernel_internals/parsing/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_oneline_comment_rec lexbuf __ocaml_lex_state

;;

# 493 "src/kernel_internals/parsing/logic_preprocess.mll"
 
  let file suffix cpp filename =
    reset ();
    let debug = Kernel.is_debug_key_enabled Kernel.dkey_parser in
    let inchan = open_in_bin filename in
    let lex = Lexing.from_channel inchan in
    let ppname =
      Extlib.temp_file_cleanup_at_exit ~debug
        (Filename.basename filename) ".pp"
    in
    let ppfile = open_out ppname in
    main lex;
    preprocess_annots suffix cpp ppfile;
    close_in inchan;
    close_out ppfile;
    ppname

# 1358 "src/kernel_internals/parsing/logic_preprocess.ml"
