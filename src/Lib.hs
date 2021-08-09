module Lib where

import Lib.IO
import System.Environment (getArgs)

mainAux :: [String] -> IO ()
-- a[dd]
mainAux ["al", co, pg, ty, nm, de] = addLabel co pg ty nm de
mainAux ["ar", co, pg, de] = addRef co pg de
-- e[dit]
mainAux ["ec", tg, co] = editCourse tg co
mainAux ["ep", tg, pg] = editPage tg pg
mainAux ["en", tg, nm] = editName tg nm
mainAux ["ed", tg, de] = editDescr tg de
mainAux ["et", tg, nm] = editDest tg nm
-- f[ind]
mainAux ["ft", tg] = findTag tg
mainAux ["fn", nm] = findName nm
mainAux ["fne", nm] = findNameExact nm
mainAux ["fd", de] = findDescr de
-- d[elete]
mainAux ["d", tg] = deleteTag tg
mainAux _ = fail "unrecognized command"

mainFunc :: IO ()
mainFunc = do
    l <- getArgs
    mainAux l
