{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Core where

import Data.List
import Data.Aeson
import GHC.Generics
import GHC.RTS.Flags (MiscFlags(installSignalHandlers))
import Numeric
import Data.Foldable
import qualified Data.Text as T
import Text.Printf

newtype Tag = Tag Int deriving (Eq, Generic)

tagToInt :: Tag -> Int
tagToInt (Tag t) = t

nextTag :: Tag -> Tag
nextTag (Tag t) = Tag (t + 1)

instance Show Tag where
    show (Tag n) = printf "%05x" n

ofHex :: String -> Tag
ofHex s = case readHex s of
          [(t, _)] -> Tag t
          _ -> error "invalid hex string"

instance ToJSON Tag where
    toJSON n = String $ T.pack $ show n

instance FromJSON Tag where
    parseJSON = withText "Tag" (\x -> case readHex (T.unpack x) of
                                        [(t, _)] -> return $ Tag t
                                        _ -> fail "Invalid tag")

data Obj =
    Label {
        course :: String,
        page :: Int,
        tag :: Tag,
        typ :: String,
        name :: String,
        description :: String
    }
    | Ref {
        course :: String,
        page :: Int,
        tag :: Tag,
        destination :: Tag
    } deriving (Generic, Show)

instance ToJSON Obj
instance FromJSON Obj where
    parseJSON = withObject "label or ref" $ \o -> asum [
        Label <$> o .: "course" <*> o .: "page" <*> o .: "tag" <*> o .: "typ" <*> o .: "name" <*> o .: "description",
        Ref <$> o .: "course" <*> o .: "page" <*> o .: "tag" <*> o .: "destination"]

-- Given a list of objects, return the one with the given tag.
findByTag :: [Obj] -> Tag -> Maybe Obj
findByTag [] t                                     = Nothing
findByTag (x@Label{tag=t}:xs) tg | t == tg = Just x
findByTag (x@Ref{tag=t}:xs) tg        | t == tg = Just x
findByTag (x : xs) tg                              = findByTag xs tg

-- Given a list of objects, return all Refs which link to the given tag.
findReferenced :: [Obj] -> Tag -> [Obj]
findReferenced [] t = []
findReferenced (x@Ref{destination=t}:xs) tg | t == tg = x : findReferenced xs tg
findReferenced (x : xs) tg = findReferenced xs tg

-- Given a list of objects, return all Labels with the given name (exact match)
findByNameExact :: [Obj] -> String -> Maybe Obj
findByNameExact [] t = Nothing
findByNameExact (x@Label{name=n}:xs) nm | nm == n = Just x
findByNameExact (x : xs) nm = findByNameExact xs nm

-- Given a list of objects, return all Labels where the input is a substring of the name
findByNameSub :: [Obj] -> String -> [Obj]
findByNameSub [] t = []
findByNameSub (x@Label{name=n}:xs) nm | nm `isInfixOf` n = x : findByNameSub xs nm
findByNameSub (x : xs) nm = findByNameSub xs nm

-- Given a list of objects, return all labels where the description contains the input
findByDescrSub :: [Obj] -> String -> [Obj]
findByDescrSub [] t = []
findByDescrSub (x@Label{description=n}:xs) d | d `isInfixOf` n = x : findByDescrSub xs d
findByDescrSub (x : xs) d = findByDescrSub xs d
