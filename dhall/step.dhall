let Prelude = ./Prelude.dhall

let JSON = Prelude.JSON

let Optional/map = Prelude.Optional.map

let Misc = ./misc.dhall

let Enums = ./enums.dhall

let dropNones = ./utils.dhall

let Step =
      { name : Text
      , failure : Optional Enums.Failure
      , environment : Optional Misc.KVParams
      , when : Optional Misc.Conditions
      , depends_on : Optional (List Text)
      }

let default =
      { failure = None Enums.Failure
      , environment = None Misc.KVParams
      , when = None Misc.Conditions
      , depends_on = None (List Text)
      }

let toJSONObjectFields
    : Step → List { mapKey : Text, mapValue : JSON.Type }
    = λ(step : Step) →
        let stringsArray
            : List Text → JSON.Type
            = λ(xs : List Text) →
                JSON.array (Prelude.List.map Text JSON.Type JSON.string xs)

        let everything
            : Prelude.Map.Type Text (Optional JSON.Type)
            = toMap
                { name = Some (JSON.string step.name)
                , failure =
                    Optional/map
                      Enums.Failure
                      JSON.Type
                      Enums.Failure/toJSON
                      step.failure
                , environment =
                    Optional/map
                      Misc.KVParams
                      JSON.Type
                      Misc.KVParams/toJSON
                      step.environment
                , when =
                    Optional/map
                      Misc.Conditions
                      JSON.Type
                      Misc.Conditions/toJSON
                      step.when
                , depends_on =
                    Optional/map
                      (List Text)
                      JSON.Type
                      stringsArray
                      step.depends_on
                }

        in  dropNones Text JSON.Type everything

in  { Type = Step, default, toJSONObjectFields }
