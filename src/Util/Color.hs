module Util.Color where

red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[39m"

green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[39m"

yellow :: String -> String
yellow s = "\ESC[33m" ++ s ++ "\ESC[39m"
