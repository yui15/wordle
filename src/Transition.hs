{-# LANGUAGE EmptyDataDecls #-}
module Transition
    ( responser
    ) where

import Data.Maybe
import Interact 

{- | 
任意の入力列をそれぞれの入力文字列を"なんか関数"に変換
>>> putStr $ unlines $ responser undefined ["Hi.", "お元気ですか？"]
なんか関数
なんか関数
-}

responser :: String -> [String] -> [String]
responser extra = mapMaybe output . eval . initial extra

data MachineState
    = MachineState 
    { inChan :: [String]
    , output :: Maybe String
    , innerState :: InnerState
    }

data InnerState

initial :: String -> [String] -> MachineState
initial extra inputs
    = MachineState
    { inChan = inputs
    , output = Nothing
    , innerState = error "initial InnerState is not implemented" extra
    }

eval :: MachineState -> [MachineState]
eval state = state : rests
    where
        rests | isFinal state = []
              | otherwise     = eval (step state)

isFinal :: MachineState -> Bool
isFinal state = case state of
    MachineState { inChan = [] } -> True
    _                            -> False

step :: MachineState -> MachineState
step state = case state of
    MachineState { inChan = i : is
                 , innerState = instate
                 } -> state { inChan = is
                            , innerState = undefined
                            , output = Just "なんか関数"
                            }
