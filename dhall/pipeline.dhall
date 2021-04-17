let Prelude = ./Prelude.dhall

let DockerPipeline = ./pipeline_docker.dhall

let ExecPipeline = ./pipeline_exec.dhall

let Pipeline = < docker : DockerPipeline.Type | exec : ExecPipeline.Type >

let toJSON
    : Pipeline → Prelude.JSON.Type
    = λ(pipeline : Pipeline) →
        let JSONObjectFields =
              merge
                { docker =
                    λ(pipeline : DockerPipeline.Type) →
                      DockerPipeline.toJSONObjectFields pipeline
                , exec =
                    λ(pipeline : ExecPipeline.Type) →
                      ExecPipeline.toJSONObjectFields pipeline
                }
                pipeline

        let allFields =
              JSONObjectFields # toMap { kind = Prelude.JSON.string "pipeline" }

        in  Prelude.JSON.object allFields

in  { Type = Pipeline, toJSON }
