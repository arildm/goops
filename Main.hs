module Main where

import Language
import Build
import Control.Monad.State

demo :: State Type String
demo = do
	put (Class "Fruit" [] [] [])
	c <- get
	put $ addImmutableField (Class "Color" [] [] []) "color" c
	c <- get
	put $ addImmutableField Integer "length" c
	c <- get
	put $ addMutableField Boolean "eaten" c
	c <- get
	return $ show c

main :: IO ()
main = putStrLn $ evalState demo Void
