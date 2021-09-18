let Prelude = ./Prelude.dhall

let Enums = ./enums.dhall

let Misc = ./misc.dhall

let Pipeline =
      { name : Text
      , platform : Optional Misc.Platform
      , workspace : Optional Misc.Workspace
      , clone : Optional Misc.Clone
      , trigger : Optional Misc.Conditions.Type
      }

let default =
      { platform = None Misc.Platform
      , workspace = None Misc.Workspace
      , clone = None Misc.Clone
      , trigger = None Misc.Conditions.Type
      }

let toJSONObjectFields
    : Pipeline → List { mapKey : Text, mapValue : Prelude.JSON.Type }
    = λ(pipeline : Pipeline) →
        toMap { name = Prelude.JSON.string pipeline.name }

in  { Type = Pipeline, default, toJSONObjectFields }
