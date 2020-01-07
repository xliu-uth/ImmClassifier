#!/usr/bin/env cwl-runner
class: CommandLineTool
id: run-immclassifier
label: run-immclassifier
cwlVersion: v1.0

requirements:
  - class: DockerRequirement
    dockerPull: sgosline/imm-classifier
  - class: InlineJavascriptRequirement

baseCommand: ["Rscript", "/usr/local/bin/runIC.R"]

inputs:
  input_path:
    type: File
    inputBinding:
      position: 1
      prefix: --input
  prob_unknown:
    type: double
    inputBinding:
      position: 2
      prefix: --prob
  out-name:
    type: string
    inputBinding:
      position: 3
      prefix: --output

outputs:
  predictions:
    type: File
    outputBinding:
      glob: "*.txt"
