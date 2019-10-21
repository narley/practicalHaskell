{-# LANGUAGE TemplateHaskell #-}

import Data.Proxy
import Elm.Module (makeElmModule, makeModuleContentWithAlterations, DefineElm(..), recAlterType, defaultAlterations)
import Elm.Derive (deriveElmDef, defaultOptions)
import Elm.TyRep

import Schema
import SchemaTypes

deriveElmDef defaultOptions ''User
deriveElmDef defaultOptions ''Article
deriveElmDef defaultOptions ''Comment

main :: IO ()
main = writeFile "frontend/src/SchemaTypes.elm" moduleString
  where
    moduleString = moduleHeader "SchemaTypes" ++ moduleContent
    moduleContent = makeModuleContentWithAlterations
      (defaultAlterations . alterKeyTypes)
      [ DefineElm (Proxy :: Proxy User)
      , DefineElm (Proxy :: Proxy Article)
      , DefineElm (Proxy :: Proxy Comment)
      ]

alterKeyTypes :: ETypeDef -> ETypeDef
alterKeyTypes = recAlterType $ \t -> case t of
  -- Key User -> Int, Key Article -> Int
  ETyApp (ETyCon (ETCon "Key")) _ -> ETyCon (ETCon "Int")
  _ -> t

moduleHeader :: String -> String
moduleHeader moduleName = unlines
  [ "module " ++ moduleName ++ " exposing(..)"
  , ""
  , "import Json.Decode"
  , "import Json.Encode exposing (Value)"
  , "-- The following module comes from bartavelle/json-helpers"
  , "import Json.Helpers exposing (..)"
  , "import Dict exposing (Dict)"
  , "import Set exposing (Set)"
  , "import Time exposing (..)"
  , ""
  , ""
  ]
