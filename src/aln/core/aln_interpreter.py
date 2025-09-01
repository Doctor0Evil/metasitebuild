// File: src/aln/core/aln_interpreter.py

ALNInterpreter = {
  "input_path": "input/aln_tasks/",
  "output_path": "output/aln_results/",
  "workflow": [
    "parse_syntax",
    "static_analysis",
    "compliance_check:config/ALN-Security-Model.aln"
  ],
  "logging": "logs/aln_interpreter.log"
}
