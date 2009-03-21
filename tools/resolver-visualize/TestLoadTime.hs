#!/usr/bin/env runhaskell

module Main where

import Control.Monad
import Resolver.Log
import System.Environment
import System.IO

main = do args <- getArgs
          when (length args > 1) (error "Too many arguments: expected exactly one.")
          when (length args < 1) (error "Too few arguments: expected exactly one.")
          let [fn] = args
          h <- openFile fn ReadMode
          logFile <- loadLogFile h fn (\_ _ -> return ())
          return ()
