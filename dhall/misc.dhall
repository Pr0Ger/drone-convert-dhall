let Prelude = ./Prelude.dhall

let JSON = Prelude.JSON

let Map = Prelude.Map

let Optional/map = Prelude.Optional.map

let Enums = ./enums.dhall

let Utils = ./utils.dhall

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

        in  JSON.object (Utils.dropNones Text JSON.Type everything)

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

        in  JSON.object (Utils.dropNones Text JSON.Type everything)

let Constraint =
    -- The Constraint object defines pattern matching criteria.
    -- If the pattern matching evaluates to false, the parent object is skipped.
      { Type =
          { exclude : Optional (List Text), include : Optional (List Text) }
      , default = { exclude = None (List Text), include = None (List Text) }
      }

let Constraint/toJSON
    : Constraint.Type → JSON.Type
    = λ(constraint : Constraint.Type) →
        let fields =
              toMap
                { exclude =
                    Optional/map
                      (List Text)
                      JSON.Type
                      Utils.stringsArray
                      constraint.exclude
                , include =
                    Optional/map
                      (List Text)
                      JSON.Type
                      Utils.stringsArray
                      constraint.include
                }

        in  JSON.object (Utils.dropNones Text JSON.Type fields)

let ConstraintOrText = < constraint : Constraint.Type | actions : List Text >

let ConstraintOrText/toJSON
    : ConstraintOrText → JSON.Type
    = λ(condition : ConstraintOrText) →
        merge
          { constraint =
              λ(constraint : Constraint.Type) → Constraint/toJSON constraint
          , actions = λ(actions : List Text) → Utils.stringsArray actions
          }
          condition

let ConstraintOrEvent =
      < constraint : Constraint.Type | events : List Enums.Event >

let ConstraintOrEvent/toJSON
    : ConstraintOrEvent → JSON.Type
    = λ(condition : ConstraintOrEvent) →
        merge
          { constraint =
              λ(constraint : Constraint.Type) → Constraint/toJSON constraint
          , events =
              λ(events : List Enums.Event) →
                JSON.array
                  ( Prelude.List.map
                      Enums.Event
                      JSON.Type
                      Enums.Event/toJSON
                      events
                  )
          }
          condition

let ConstraintOrStatus =
      < constraint : Constraint.Type | statuses : List Enums.Status >

let ConstraintOrStatus/toJSON
    : ConstraintOrStatus → JSON.Type
    = λ(condition : ConstraintOrStatus) →
        merge
          { constraint =
              λ(constraint : Constraint.Type) → Constraint/toJSON constraint
          , statuses =
              λ(statuses : List Enums.Status) →
                JSON.array
                  ( Prelude.List.map
                      Enums.Status
                      JSON.Type
                      Enums.Status/toJSON
                      statuses
                  )
          }
          condition

let Conditions =
    -- The Conditions object defines a set of conditions.
    -- If any condition evaluates to true its parent object is skipped.
      { Type =
          { action : Optional ConstraintOrText
          , branch : Optional ConstraintOrText
          , cron : Optional ConstraintOrText
          , event : Optional ConstraintOrEvent
          , instance : Optional ConstraintOrText
          , ref : Optional ConstraintOrText
          , repo : Optional ConstraintOrText
          , status : Optional ConstraintOrStatus
          , target : Optional ConstraintOrText
          }
      , default =
        { action = None ConstraintOrText
        , branch = None ConstraintOrText
        , cron = None ConstraintOrText
        , event = None ConstraintOrEvent
        , instance = None ConstraintOrText
        , ref = None ConstraintOrText
        , repo = None ConstraintOrText
        , status = None ConstraintOrStatus
        , target = None ConstraintOrText
        }
      }

let Conditions/toJSON
    : Conditions.Type → JSON.Type
    = λ(conditions : Conditions.Type) →
        let fields =
              toMap
                { action =
                    Optional/map
                      ConstraintOrText
                      JSON.Type
                      ConstraintOrText/toJSON
                      conditions.action
                , branch =
                    Optional/map
                      ConstraintOrText
                      JSON.Type
                      ConstraintOrText/toJSON
                      conditions.branch
                , cron =
                    Optional/map
                      ConstraintOrText
                      JSON.Type
                      ConstraintOrText/toJSON
                      conditions.cron
                , event =
                    Optional/map
                      ConstraintOrEvent
                      JSON.Type
                      ConstraintOrEvent/toJSON
                      conditions.event
                , instance =
                    Optional/map
                      ConstraintOrText
                      JSON.Type
                      ConstraintOrText/toJSON
                      conditions.instance
                , ref =
                    Optional/map
                      ConstraintOrText
                      JSON.Type
                      ConstraintOrText/toJSON
                      conditions.ref
                , repo =
                    Optional/map
                      ConstraintOrText
                      JSON.Type
                      ConstraintOrText/toJSON
                      conditions.repo
                , status =
                    Optional/map
                      ConstraintOrStatus
                      JSON.Type
                      ConstraintOrStatus/toJSON
                      conditions.status
                , target =
                    Optional/map
                      ConstraintOrText
                      JSON.Type
                      ConstraintOrText/toJSON
                      conditions.target
                }

        in  JSON.object (Utils.dropNones Text JSON.Type fields)

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
    , ConstraintOrText
    , ConstraintOrText/toJSON
    , ConstraintOrEvent
    , ConstraintOrEvent/toJSON
    , ConstraintOrStatus
    , ConstraintOrStatus/toJSON
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
