// File: src/lisp/core/lisp_interpreter.py

LispInterpreter = {
  "source_dirs": ["input/lisp_programs/"],
  "output_dir": "output/lisp_results/",
  "workflows": [
    "parse_s_expr",
    "check_macros:config/ALN-Security-Model.aln",
    "dynamic_guard"
  ],
  "audit_logger": "logs/lisp_interpreter_audit.log"
}
