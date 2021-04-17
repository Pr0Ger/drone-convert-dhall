let Prelude = ./Prelude.dhall

let JSON = Prelude.JSON

let Event =
    -- The Event enum provides a list of pipeline events. This value represents the event that triggered the pipeline.
      < Cron | Custom | Promote | Pull_request | Push | Rollback | Tag >

let Event/toJSON
    : Event → JSON.Type
    = λ(event : Event) →
        JSON.string
          ( merge
              { Cron = "cron"
              , Custom = "custom"
              , Promote = "promote"
              , Pull_request = "pull_request"
              , Push = "push"
              , Rollback = "rollback"
              , Tag = "tag"
              }
              event
          )

let Status =
    -- The Status enum provides a list of pipeline statuses.
    -- The default pipeline state is success, even if the pipeline is still running.
      < Failure | Success >

let Status/toJSON
    : Status → JSON.Type
    = λ(status : Status) →
        JSON.string (merge { Failure = "failure", Success = "success" } status)

let Pull =
    -- The Pull enum defines if and when a docker image should be pull from the registry.
      < Always | Never | IfNotExists >

let Pull/toJSON
    : Pull → JSON.Type
    = λ(pull : Pull) →
        JSON.string
          ( merge
              { Always = "always"
              , Never = "never"
              , IfNotExists = "if-not-exists"
              }
              pull
          )

let Failure =
    -- The Failure enum defines a list of failure behaviors.
    -- The value always indicates a failure will fail the parent process.
    -- The value ignore indicates the failure is silently ignored.
      < Always | Ignore >

let Failure/toJSON
    : Failure → JSON.Type
    = λ(failure : Failure) →
        JSON.string (merge { Always = "always", Ignore = "ignore" } failure)

let OS =
    -- The OS enum provides a list of supported operating systems.
      < Darwin
      | DragonFly
      | FreeBSD
      | Linux
      | NetBSD
      | OpenBSD
      | Solaris
      | Windows
      >

let OS/toJSON
    : OS → JSON.Type
    = λ(os : OS) →
        JSON.string
          ( merge
              { Darwin = "darwin"
              , DragonFly = "dragonfly"
              , FreeBSD = "freebsd"
              , Linux = "linux"
              , NetBSD = "netbsd"
              , OpenBSD = "openbsd"
              , Solaris = "solaris"
              , Windows = "windows"
              }
              os
          )

let Arch =
    -- The Arch enum provides a list of supported chip architectures.
      < `386` | amd64 | arm64 | arm >

let Arch/toJSON
    : Arch → JSON.Type
    = λ(arch : Arch) →
        JSON.string
          ( merge
              { `386` = "386", amd64 = "amd64", arm64 = "arm64", arm = "arm" }
              arch
          )

in  { Event
    , Event/toJSON
    , Status
    , Status/toJSON
    , Pull
    , Pull/toJSON
    , Failure
    , Failure/toJSON
    , OS
    , OS/toJSON
    , Arch
    , Arch/toJSON
    }
