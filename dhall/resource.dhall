let JSON = (./Prelude.dhall).JSON

let Pipeline = ./pipeline.dhall

let Secret = ./secret.dhall

let Resource = < pipeline : Pipeline.Type | secret : Secret.Type >

let toJSON
    : Resource → JSON.Type
    = λ(resource : Resource) →
        merge
          { pipeline = λ(pipeline : Pipeline.Type) → Pipeline.toJSON pipeline
          , secret = λ(secret : Secret.Type) → Secret.toJSON secret
          }
          resource

in  { Type = Resource, toJSON }
