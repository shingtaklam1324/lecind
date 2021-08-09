module Lib.IO where

import Lib.Core
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Aeson

getLastTag :: IO Tag
getLastTag = do
    f <- B.readFile "index.json"
    print f
    case decode (BL.fromStrict f) :: Maybe [Obj] of
        Just l -> return $ foldl (\(Tag t) x -> Tag (max t (tagToInt $ tag x))) (Tag 0) l
        Nothing -> return $ Tag (-255)

addObj :: Obj -> IO ()
addObj o = do
    f <- B.readFile "index.json"
    let Just d = decode (BL.fromStrict f) :: Maybe [Obj]
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
    f <- B.readFile "index.json"
    let Just d = decode (BL.fromStrict f) :: Maybe [Obj]
    case findByTag d (ofHex t) of
      Nothing -> putStrLn "No such tag found."
      Just obj -> print obj

deleteTag :: String -> IO ()
deleteTag t = do    
    f <- B.readFile "index.json"
    let Just d = decode (BL.fromStrict f) :: Maybe [Obj]
    case findByTag d (ofHex t) of
      Nothing -> putStrLn "No such tag found."
      Just obj -> do 
        putStrLn $ "Object found: " ++ show obj ++ "\nConfirm Delete y/[n]:"
        c <- getLine
        if c == "y" then do
            let d' = filter (\o -> tag o /= ofHex t) d
            B.writeFile "index.json" $ BL.toStrict $ encode d'
            putStrLn $ "Tag deleted."
        else
            putStrLn "Deletion aborted."
