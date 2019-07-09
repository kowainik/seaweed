module Main (main) where

import System.IO (hSetEncoding, utf8)

import Seaweed (runSeaweed)


main :: IO ()
main = hSetEncoding stdout utf8 >> runSeaweed
