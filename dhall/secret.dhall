let JSON = (./Prelude.dhall).JSON

let Secret
    : Type
    = { name : Text, get : { path : Text, name : Text } }

let toJSON
    : Secret → JSON.Type
    = λ(secret : Secret) →
        JSON.object
          ( toMap
              { kind = JSON.string "secret"
              , get =
                  JSON.object
                    ( toMap
                        { path = JSON.string secret.get.path
                        , name = JSON.string secret.get.name
                        }
                    )
              }
          )

in  { Type = Secret, toJSON }
