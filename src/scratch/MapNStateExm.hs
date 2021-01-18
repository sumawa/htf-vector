module MapNStateExm where

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Control.Lens
import Control.Monad.State

import Data.Map as M

import Data.Typeable

type Code = [Int]



-- begin original code ------

type EnvState = State Env (Int, Code)
type Env = M.Map String Int

fStatBlock :: [EnvState] -> EnvState
fStatBlock block = do origEnv <- get
                      xs      <- sequence block -- prelude sequence
                      newEnv  <- get
                      put origEnv
                      return (M.size newEnv, concatMap snd xs)

-- end original code -------


setValue :: String -> Int -> EnvState
setValue name value = state (\env -> ((0,[]), insert name value env))

mapStateExm = do
    let env = fromList [("x", 5), ("y", 10)]
        fsb = fStatBlock [setValue "a" 15]
    print (typeOf fsb)
    print (runState fsb env)
--    print $ fst $ fst $ runState fsb env


example :: State (Map String Int) Int
example = do
    -- set value
    at "pennywise" ?= 16
    at "krusty" ?= 18
    -- get value
    krustMaybe <- use $ at "krusty1"
    case krustMaybe of
      Just kr -> pure kr
      Nothing -> pure 0
--    pure krusty

exMain :: IO ()
exMain = do
    let r = evalState example empty
    print r