let Prelude = ./Prelude.dhall

let DockerPipeline = ./pipeline_docker.dhall

let ExecPipeline = ./pipeline_exec.dhall

let Pipeline = ./pipeline.dhall

let Resource = ./resource.dhall

let Secret = ./secret.dhall

let render
    : List Resource.Type → List Prelude.JSON.Type
    = λ(resources : List Resource.Type) →
        Prelude.List.map
          Resource.Type
          Prelude.JSON.Type
          Resource.toJSON
          resources

in  { Pipeline =
      { Docker =
        { Type = DockerPipeline.Type, default = DockerPipeline.default }
      , Exec = { Type = ExecPipeline.Type, default = ExecPipeline.default }
      }
    , Resource =
      { Pipeline =
        { Docker =
            λ(pipeline : DockerPipeline.Type) →
              Resource.Type.pipeline (Pipeline.Type.docker pipeline)
        , Exec =
            λ(pipeline : ExecPipeline.Type) →
              Resource.Type.pipeline (Pipeline.Type.exec pipeline)
        }
      , Secret = λ(secret : Secret.Type) → Resource.Type.secret secret
      }
    , Step = { Docker = DockerPipeline.Step, Exec = ExecPipeline.Step }
    , Secret = λ(secret : Secret.Type) → Resource.Type.secret secret
    , StepType = DockerPipeline.StepType
    , render
    }
