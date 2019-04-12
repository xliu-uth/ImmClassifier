#!/usr/bin/env cwl-runner
class: Workflow
id: imm-class-workflow
label: imm-class-workflo
cwlVersion: v1.0

inputs:
  input-file:
    type: string
  output-name:
    type: string
  synapse_config:
    type: File
  output-id:
    type: string

 outputs:
   []

steps:
  get-input-file:
    run: https://raw.githubusercontent.com/Sage-Bionetworks/synapse-client-cwl-tools/master/synapse-get-tool.cwl
    in:
      synapseid: intput-file
      synapse_config: synapse_config
    out: [filepath]
  run-immclass:
    run: run-immclassifier.cwl
    in:
      input: get-input-file/filepath
    out:
      [predictions]
