let Prelude = ./Prelude.dhall

let JSON = Prelude.JSON

let Map = Prelude.Map

let Optional/map = Prelude.Optional.map

let Enums = ./enums.dhall

let dropNones = ./utils.dhall

let Platform =
    -- The Platform object defines the target os and architecture for the pipeline.
      { os : Optional Enums.OS
      , arch : Optional Enums.Arch
      , variant : Optional Text
      , version : Optional Text
      }

let Platform/toJSON
    : Platform → JSON.Type
    = λ(platform : Platform) →
        let everything
            : Map.Type Text (Optional JSON.Type)
            = toMap
                { os =
                    Optional/map Enums.OS JSON.Type Enums.OS/toJSON platform.os
                , arch =
                    Optional/map
                      Enums.Arch
                      JSON.Type
                      Enums.Arch/toJSON
                      platform.arch
                , variant =
                    Optional/map Text JSON.Type JSON.string platform.variant
                , version =
                    Optional/map Text JSON.Type JSON.string platform.version
                }

        in  JSON.object (dropNones Text JSON.Type everything)

let Clone =
    -- The Clone object defines the clone behavior for the pipeline.
      { depth : Optional Natural, disable : Optional Bool }

let Clone/toJSON
    : Clone → JSON.Type
    = λ(clone : Clone) →
        let everything
            : Map.Type Text (Optional JSON.Type)
            = toMap
                { depth =
                    Optional/map Natural JSON.Type JSON.natural clone.depth
                , disable = Optional/map Bool JSON.Type JSON.bool clone.disable
                }

        in  JSON.object (dropNones Text JSON.Type everything)

let Constraint =
    -- The Constraint object defines pattern matching criteria.
    -- If the pattern matching evaluates to false, the parent object is skipped.
      { exclude : Optional (List Text), include : Optional (List Text) }

let Constraint/toJSON
    : Constraint → JSON.Type
    = λ(constraint : Constraint) →
        let stringsArray
            : List Text → JSON.Type
            = λ(xs : List Text) →
                JSON.array (Prelude.List.map Text JSON.Type JSON.string xs)

        let everything
            : Map.Type Text (Optional JSON.Type)
            = toMap
                { exclude =
                    Optional/map
                      (List Text)
                      JSON.Type
                      stringsArray
                      constraint.exclude
                , include =
                    Optional/map
                      (List Text)
                      JSON.Type
                      stringsArray
                      constraint.include
                }

        in  JSON.object (dropNones Text JSON.Type everything)

let Conditions =
    -- The Conditions object defines a set of conditions.
    -- If any condition evaluates to true its parent object is skipped.
      { action : < Constraint : Constraint | actions : List Text >
      , branch : < Constraint : Constraint | actions : List Text >
      , cron : < Constraint : Constraint | actions : List Text >
      , event : < Constraint : Constraint | actions : List Enums.Event >
      , instance : < Constraint : Constraint | actions : List Text >
      , ref : < Constraint : Constraint | actions : List Text >
      , repo : < Constraint : Constraint | actions : List Text >
      , status : < Constraint : Constraint | actions : List Enums.Status >
      , target : < Constraint : Constraint | actions : List Text >
      }

let Conditions/toJSON
    : Conditions → JSON.Type
    = λ(conditions : Conditions) →
        let fields = toMap { todo = JSON.string "TODO" } in JSON.object fields

let Secret =
    -- The Secret defines the named source of a secret.
      { from_secret : Text }

let Secret/toJSON
    : Secret → JSON.Type
    = λ(secret : Secret) →
        JSON.object (toMap { from_secret = JSON.string secret.from_secret })

let Param = < plain : JSON.Type | from_secret : Secret >

let KVParams = Map.Type Text Param

let KVParams/toJSON
    : KVParams → JSON.Type
    = λ(environment : KVParams) →
        let convert =
              λ(val : Param) →
                merge
                  { plain = λ(value : JSON.Type) → value
                  , from_secret = λ(secret : Secret) → Secret/toJSON secret
                  }
                  val

        let data = Map.map Text Param JSON.Type convert environment

        in  JSON.object data

let Concurrency =
    -- The Concurrency object defines the concurrency limits for the named pipeline
      { limit : Natural }

let Concurrency/toJSON
    : Concurrency → JSON.Type
    = λ(concurrency : Concurrency) →
        JSON.object (toMap { limit = JSON.natural concurrency.limit })

let Workspace =
    -- The Workspace object defines the path to which the source code is cloned (non-normative)
    -- and the default working directory for each pipeline step (non-normative).
      { path : Text }

let Workspace/toJSON
    : Workspace → JSON.Type
    = λ(workspace : Workspace) →
        JSON.object (toMap { path = JSON.string workspace.path })

in  { Clone
    , Clone/toJSON
    , Concurrency
    , Concurrency/toJSON
    , Conditions
    , Conditions/toJSON
    , Constraint
    , Constraint/toJSON
    , Param
    , KVParams
    , KVParams/toJSON
    , Platform
    , Platform/toJSON
    , Secret
    , Secret/toJSON
    , Workspace
    , Workspace/toJSON
    }
