#!/usr/bin/env cwl-runner
class: CommandLineTool
id: run-immclassifier
label: run-immclassifier
cwlVersion: v1.0

requirements:
  - class: DockerRequirement
    dockerPull: sgosline/imm-classifier
  - class: InlineJavascriptRequirement
  - class: InitialWorkDirRequirement
    listing: $(inputs.synapse_config)

baseCommand: [Rscript, immClassifier.R, --testmode]

inputs:
  synapse_config:
    type: File
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
  output:
    type: string
    inputBinding:
      position: 3
      prefix: --output

outputs:
  type: File []
  outputBinding:
    glob: "*.txt"