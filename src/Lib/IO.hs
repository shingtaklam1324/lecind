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

editCourse :: String -> String -> IO ()
editCourse tg co = do
    d <- getData
    let d' = map (\x -> if tag x == ofHex tg then x { course = co } else x) d
    B.writeFile "index.json" $ BL.toStrict $ encode d'

editPage :: String -> String -> IO ()
editPage tg pg = do
    d <- getData
    let d' = map (\x -> if tag x == ofHex tg then x { page = read pg } else x) d
    B.writeFile "index.json" $ BL.toStrict $ encode d'

editName :: String -> String -> IO ()
editName tg nm = do
    d <- getData
    let d' = map (\x -> if tag x == ofHex tg then x { name = nm } else x) d
    B.writeFile "index.json" $ BL.toStrict $ encode d'

editDescr :: String -> String -> IO ()
editDescr tg de = do
    d <- getData
    let d' = map (\x -> if tag x == ofHex tg then x { description = de } else x) d
    B.writeFile "index.json" $ BL.toStrict $ encode d'

editDest :: String -> String -> IO ()
editDest tg de = do
    d <- getData
    let d' = map (\x -> if tag x == ofHex tg then x { destination = ofHex de } else x) d
    B.writeFile "index.json" $ BL.toStrict $ encode d'

findTag :: String -> IO ()
findTag t = do
    d <- getData
    case findByTag d (ofHex t) of
      Nothing -> putStrLn "No such tag found."
      Just obj -> print obj

findName :: String -> IO ()
findName n = do
    d <- getData
    case findByNameSub d n of
      [] -> putStrLn "No matches found."
      l -> print l

findNameExact :: String -> IO ()
findNameExact n = do
    d <- getData
    case findByNameExact d n of
      Nothing -> putStrLn "No matches found."
      Just obj -> print obj

findDescr :: String -> IO ()
findDescr de = do
    d <- getData
    case findByDescrSub d de of
      [] -> putStrLn "No matches found."
      l -> print l

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
