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
deriveElmDef defaultOptions ''LoginInfo
deriveElmDef defaultOptions ''LoginResponse
deriveElmDef defaultOptions ''ArticleReaction
deriveElmDef defaultOptions ''ReactionType
deriveElmDef defaultOptions ''Metadata

main :: IO ()
main = writeFile "frontend/src/SchemaTypes.elm" moduleString
  where
    moduleString = moduleHeader "SchemaTypes" ++ moduleContent
    moduleContent = makeModuleContentWithAlterations
      (defaultAlterations . alterKeyTypes)
      [ DefineElm (Proxy :: Proxy User)
      , DefineElm (Proxy :: Proxy Article)
      , DefineElm (Proxy :: Proxy Comment)
      , DefineElm (Proxy :: Proxy LoginInfo)
      , DefineElm (Proxy :: Proxy LoginResponse)
      , DefineElm (Proxy :: Proxy ArticleReaction)
      , DefineElm (Proxy :: Proxy ReactionType)
      , DefineElm (Proxy :: Proxy Metadata)
      ]

alterKeyTypes :: ETypeDef -> ETypeDef
alterKeyTypes = recAlterType $ \t -> case t of
  -- Key User -> Int, Key Article -> Int
  ETyApp (ETyCon (ETCon "Key")) _ -> ETyCon (ETCon "Int")
  ETyCon (ETCon "Int64") -> ETyCon (ETCon "Int")
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
  , "import PosixJson exposing (..)"
  , ""
  , ""
  , "type alias Entity a ="
  , "  { entityKey : Int"
  , "  , entityValue : a"
  , "  }"
  , ""
  ]
