let Prelude = ./Prelude.dhall

let JSON = Prelude.JSON

let Pipeline = ./pipeline_common.dhall

let BaseStep = ./step.dhall

let Step = { Type = BaseStep.Type, default = BaseStep.default ∧ {=} }

let Step/toJSON
    : Step.Type → JSON.Type
    = λ(step : Step.Type) →
        JSON.object (BaseStep.toJSONObjectFields step.(BaseStep.Type))

let ExecPipeline = Pipeline.Type ⩓ { steps : List Step.Type }

let default = Pipeline.default ∧ {=}

let toJSONObjectFields
    : ExecPipeline → List { mapKey : Text, mapValue : JSON.Type }
    = λ(pipeline : ExecPipeline) →
        let steps =
              Prelude.List.map Step.Type JSON.Type Step/toJSON pipeline.steps

        in    Pipeline.toJSONObjectFields pipeline.(Pipeline.Type)
            # toMap { type = JSON.string "exec", steps = JSON.array steps }

in  { Step, Type = ExecPipeline, default, toJSONObjectFields }
