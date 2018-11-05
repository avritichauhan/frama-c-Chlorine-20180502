module Filepath = struct let add_symbolic_dir _ _ = () end
module Config = struct
(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

# 24 "src/kernel_internals/runtime/config.ml.in"

let version = "Chlorine-20180502"

let is_gui = ref false

let ocamlc = "ocamlfind ocamlc"
let ocamlopt = "ocamlfind ocamlopt"
let ocaml_wflags = "-w -a"

let datadir = try Sys.getenv "FRAMAC_SHARE" with Not_found -> "/root/.opam/default/share/frama-c"
let () = Filepath.add_symbolic_dir "FRAMAC_SHARE" datadir
let libdir = try Sys.getenv "FRAMAC_LIB" with Not_found -> "/root/.opam/default/lib/frama-c"
let () = Filepath.add_symbolic_dir "FRAMAC_LIB" libdir
let plugin_dir =
  try
    let path = Sys.getenv "FRAMAC_PLUGIN" in
    Str.split (Str.regexp ":") path
  with Not_found ->
    try [ Sys.getenv "FRAMAC_LIB" ^ "/plugins" ]
    with Not_found -> [ "/root/.opam/default/lib/frama-c/plugins" ]

let plugin_path = String.concat ":" plugin_dir

let () = match plugin_dir with
  | [d] -> Filepath.add_symbolic_dir "FRAMAC_PLUGIN" d
  | ds ->
      Array.iteri
        (fun i d ->
           let path = Printf.sprintf "FRAMAC_PLUGIN#%d" (succ i) in
           Filepath.add_symbolic_dir path d)
        (Array.of_list ds)

let default_cpp = "gcc -E -C -I."

let default_cpp_args = " -C -I."

let env_or_default f vdefault =
  try
    let env = Sys.getenv "CPP" ^ default_cpp_args in
    if env=default_cpp then vdefault else f env
  with Not_found -> vdefault

let preprocessor = env_or_default (fun x -> x) default_cpp

let using_default_cpp = env_or_default (fun _ -> false) true

let preprocessor_is_gnu_like =
  env_or_default (fun _ -> false) true

let preprocessor_supported_arch_options = ["-m32"; "-m64"; "-m16"; ]

let preprocessor_keep_comments =
  env_or_default (fun _ -> true) true

let compilation_unit_names = ["frama_c_init";  "transitioning";  "FCSet";  "FCMap";  "FCBuffer";  "FCHashtbl";  "extlib";  "unmarshal";  "unmarshal_z";  "structural_descr";  "type";  "descr";  "pretty_utils";  "hook";  "bag";  "wto";  "vector";  "indexer";  "rgmap";  "bitvector";  "qstack";  "leftistheap";  "integer";  "filepath";  "json";  "rich_text";  "config";  "gui_init";  "log";  "cmdline";  "project_skeleton";  "datatype";  "journal";  "state";  "state_dependency_graph";  "state_topological";  "state_selection";  "project";  "state_builder";  "utf8_logic";  "binary_cache";  "hptmap";  "hptset";  "escape";  "cil_datatype";  "typed_parameter";  "dynamic";  "parameter_category";  "parameter_customize";  "parameter_state";  "parameter_builder";  "plugin";  "kernel";  "unicode";  "emitter";  "floating_point";  "rangemap";  "cil_types_debug";  "printer_builder";  "cilconfig";  "alpha";  "cil_state_builder";  "machdeps";  "cil_const";  "logic_env";  "logic_const";  "cil";  "errorloc";  "cil_printer";  "cil_descriptive_printer";  "cabs";  "cabshelper";  "logic_print";  "logic_utils";  "logic_parser";  "logic_lexer";  "logic_typing";  "ast_info";  "ast";  "cprint";  "cabsvisit";  "cabs2cil";  "globals";  "cfg";  "kernel_function";  "property";  "property_status";  "annotations";  "printer";  "logic_builtin";  "cabs_debug";  "lexerhack";  "clexer";  "cparser";  "logic_preprocess";  "mergecil";  "rmtmps";  "oneret";  "frontc";  "dataflow";  "ordered_stmt";  "wto_statement";  "dataflows";  "dataflow2";  "stmts_graph";  "dominators";  "service_graph";  "undefined_sequence";  "interpreted_automata";  "description";  "alarms";  "lattice_messages";  "abstract_interp";  "bottom";  "int_Base";  "bit_utils";  "fval";  "ival";  "base";  "origin";  "map_lattice";  "tr_offset";  "offsetmap";  "int_Intervals";  "locations";  "lmap";  "lmap_bitwise";  "visitor";  "statuses_by_call";  "db";  "command";  "task";  "filecheck";  "json_compilation_database";  "file";  "translate_lightweight";  "allocates";  "unroll_loops";  "asm_contracts";  "loop";  "exn_flow";  "destructors";  "logic_interp";  "infer_annotations";  "clone";  "filter";  "inline";  "special_hooks";  "messages";  "boot"; ]
let library_names = ["findlib.internal"; "findlib"; "ocamlgraph"; "unix"; "str"; "dynlink"; "bytes"; "zarith";]

let has_yojson = false

let dot = Some "dot"
end
(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

# 24 "src/kernel_internals/runtime/frama_c_config.ml.in"

(** This file is *not* linked in Frama-C. Instead, is it is concatenated
    to Config, to create a standalone executable *)

let version _ =
  Format.printf
    "Frama-C %s@\n\
     Environment:@\n  \
       FRAMAC_SHARE  = %S@\n  \
       FRAMAC_LIB    = %S@\n  \
       FRAMAC_PLUGIN = %S@."
    Config.version
    Config.datadir Config.libdir Config.plugin_path
  ;
  exit 0

let options = Arg.([
  "-print-share-path",
  Unit (fun _ -> Format.printf "%s%!" Config.datadir; exit 0),
  " Print the path of Frama-C share directory";

  "-print-libpath",
  Unit (fun _ -> Format.printf "%s%!" Config.libdir; exit 0),
  " Print the path of Frama-C kernel library";

  "-print-plugin-path",
  Unit (fun _ -> Format.printf "%s%!" Config.plugin_path; exit 0),
  " Print the path where Frama-C dynamic plug-ins are searched for";

  "-version",
  Unit version,
  " Display Frama-C version";
])

let usage = "\
Usage:  frama-c-config <option>"

let () =
  Arg.parse options (fun _ -> ()) usage;
  version () (* We only get here if no option has been specified *)
