let Prelude = ./Prelude.dhall

let JSON = Prelude.JSON

let Enums = ./enums.dhall

let Pipeline = ./pipeline_common.dhall

let BaseStep = ./step.dhall

let Step =
      { Type =
            BaseStep.Type
          ⩓ { command : Optional (List Text)
            , detach : Optional Bool
            , entrypoint : Optional (List Text)
            , image : Text
            , network_mode : Optional Text
            , privileged : Optional Bool
            , pull : Optional Enums.Pull
            , user : Optional Text
            }
      , default =
            BaseStep.default
          ∧ { command = None (List Text)
            , detach = None Bool
            , entrypoint = None (List Text)
            , network_mode = None Text
            , privileged = None Bool
            , pull = None Enums.Pull
            , user = None Text
            }
      }

let Step/toJSON
    : Step.Type → JSON.Type
    = λ(step : Step.Type) →
        let fields =
                BaseStep.toJSONObjectFields step.(BaseStep.Type)
              # toMap { image = JSON.string step.image }

        in  JSON.object fields

let DockerPipeline = Pipeline.Type ⩓ { steps : List Step.Type }

let default = Pipeline.default ∧ {=}

let toJSONObjectFields
    : DockerPipeline → List { mapKey : Text, mapValue : JSON.Type }
    = λ(pipeline : DockerPipeline) →
        let steps =
              Prelude.List.map Step.Type JSON.Type Step/toJSON pipeline.steps

        in    Pipeline.toJSONObjectFields pipeline.(Pipeline.Type)
            # toMap { type = JSON.string "docker", steps = JSON.array steps }

in  { Step, Type = DockerPipeline, default, toJSONObjectFields }
