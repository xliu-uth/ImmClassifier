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
  known-preds: 
    type: File
    outputSource: run-immclass/known
  unknown-preds:
    type: File
    outputSource: run-immclass/unknown

requirements:
  - class: SubworkflowFeatureRequirement
  - class: ScatterFeatureRequirement

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
      output-name: output-name
    out:
      [known,unknown]
  store-output-files:
    run: https://raw.githubusercontent.com/Sage-Bionetworks/synapse-client-cwl-tools/master/synapse-store-tool.cwl
    in: 
      synapse_config: synapse_config
      file_to_store: run-immclass/known
      parentid: output-id
    out: 
      []

