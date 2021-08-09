{-# LANGUAGE LambdaCase #-}
module Lib.IO where

import Lib.Core
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Aeson

getData :: IO [Obj]
getData = do
    f <- B.readFile "index.json"
    let Just d = decode (BL.fromStrict f)
    return d

getLastTag :: IO Tag
getLastTag = do foldl (\(Tag t) x -> Tag (max t (tagToInt $ tag x))) (Tag 0) <$> getData

addObj :: Obj -> IO ()
addObj o = do
    d <- getData
    B.writeFile "index.json" $ BL.toStrict $ encode (d ++ [o])

addLabel :: String -> String -> String -> String -> String -> IO ()
addLabel co pg ty nm de = do
    t <- nextTag <$> getLastTag
    addObj $ Label { course = co, page = read pg, tag = t, typ = ty, name = nm, description = de }
    putStrLn $ "Label added. Tag: " ++ show t

addRef :: String -> String -> String -> IO ()
addRef co pg de = do
    t <- nextTag <$> getLastTag
    addObj $ Ref { course = co, page = read pg, tag = t, destination = ofHex de }
    putStrLn $ "Reference added. Tag: " ++ show t

findTag :: String -> IO ()
findTag t = do
    d <- getData
    case findByTag d (ofHex t) of
      Nothing -> putStrLn "No such tag found."
      Just obj -> print obj

deleteTag :: String -> IO ()
deleteTag t = do
    d <- getData
    case findByTag d (ofHex t) of
      Nothing -> putStrLn "No such tag found."
      Just obj -> do 
        putStrLn $ "Object found: " ++ show obj ++ "\nConfirm Delete y/[n]:"
        c <- getLine
        if c == "y" then do
            putStrLn "Tag deleted. Delete references to this tag [y]/n?"
            c' <- getLine
            d' <-
                if c' == "n" then do
                    putStrLn "No references deleted. Note that there may now be references that point to nonexistent tags."
                    return $ filter (\o -> tag o /= ofHex t) d
                else do
                    return $ filter (\o -> tag o /= ofHex t && (case o of
                        Label{} -> True
                        Ref{destination = tg} -> tg /= ofHex t)) d
            B.writeFile "index.json" $ BL.toStrict $ encode d'
        else
            putStrLn "Deletion aborted."
