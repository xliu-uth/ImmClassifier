class: Workflow
label: imm-class-workflow
id: imm-class-workflow
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
  prob_unknown:
    type: double

outputs:
  preds:
    type: 
      File
    outputSource: 
      run-immclass/predictions 

requirements:
  - class: SubworkflowFeatureRequirement

steps:
  get-input-file:
    run: https://raw.githubusercontent.com/Sage-Bionetworks/synapse-client-cwl-tools/master/synapse-get-tool.cwl
    in:
      synapseid: input-file
      synapse_config: synapse_config
    out: 
      [filepath]
  run-immclass:
    run: run-immclassifier.cwl
    in:
      synapse_config: synapse_config
      input_path: get-input-file/filepath
      prob_unknown: prob_unknown
      out-name: output-name
    out:
      [predictions]
