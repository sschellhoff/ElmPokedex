module Lazy exposing (Lazy(..))

type Lazy a
    = Loading
    | Finished a
    | Error String
