module Lib
    ( libMain
    ) where

import Criterion.Main
import Data.Char (toUpper)
import System.Random
import Text.Regex.TDFA
import Text.Parsec.String
import Text.Parsec

libMain :: IO ()
libMain = defaultMain
    [ env setUpEnv $ \ input -> bgroup "matching"
        [ bench "parsec" $ whnf (matchParsec input) (thatcherParser "dead")
        , bench "tdfa" $ whnf (matchTDFA input) (thatcherRE "[dD]ead")
        ]
    ]

setUpEnv :: IO String
setUpEnv = do
    gen <- getStdGen
    let x = randomRs (' ','z') gen -- from 32 to 122
    return $ take 2000 x

thatcherRE :: String -> String
thatcherRE verb = "thatcher('s *| *[Ii]s) *" <> verb

matchTDFA :: String -> String -> String
matchTDFA input regex =
    case input =~ regex of
        True -> "complete"
        False -> "whoops"

thatcherParser :: String -> Parser String
thatcherParser verb = do
    string "thatcher"
    (string "'s" <|> (many1 space >> (string "is" <|> string "Is")))
    many1 space
    (char (head verb) <|> char ((toUpper . head) verb))
    string (tail verb)
    eof
    pure ""

matchParsec :: String -> Parser String -> String
matchParsec input parser =
    case runParser parser () "" input of
        Left _ -> "whoops"
        Right x -> "complete"
