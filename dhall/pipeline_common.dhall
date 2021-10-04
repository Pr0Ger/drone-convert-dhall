let Prelude = ./Prelude.dhall

let Optional/map = Prelude.Optional.map

let Enums = ./enums.dhall

let Misc = ./misc.dhall

let Utils = ./utils.dhall

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
        let fields
            : Prelude.Map.Type Text (Optional Prelude.JSON.Type)
            = toMap
                { name = Some (Prelude.JSON.string pipeline.name)
                , platform =
                    Optional/map
                      Misc.Platform
                      Prelude.JSON.Type
                      Misc.Platform/toJSON
                      pipeline.platform
                , workspace =
                    Optional/map
                      Misc.Workspace
                      Prelude.JSON.Type
                      Misc.Workspace/toJSON
                      pipeline.workspace
                , clone =
                    Optional/map
                      Misc.Clone
                      Prelude.JSON.Type
                      Misc.Clone/toJSON
                      pipeline.clone
                , trigger =
                    Optional/map
                      Misc.Conditions.Type
                      Prelude.JSON.Type
                      Misc.Conditions/toJSON
                      pipeline.trigger
                }

        in  Utils.dropNones Text Prelude.JSON.Type fields

in  { Type = Pipeline, default, toJSONObjectFields }
