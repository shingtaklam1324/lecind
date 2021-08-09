module Lib where

import Lib.Core
import Lib.IO
import System.Environment (getArgs)
import Data.Aeson

mainAux :: [String] -> IO ()
mainAux ["al", co, pg, ty, nm, de] = addLabel co pg ty nm de
mainAux ["ar", co, pg, de] = addRef co pg de
mainAux ["ft", tg] = findTag tg
mainAux ["d", tg] = deleteTag tg
mainAux _ = fail "unrecognized command"

mainFunc :: IO ()
mainFunc = do
    l <- getArgs
    mainAux l
