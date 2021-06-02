{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( libMain
    ) where

import Control.Applicative ((<|>))
import Criterion.Main
import Data.Char (toUpper, isSpace)
import qualified Data.Text as T
import System.Random
import Text.Regex.TDFA
import qualified Text.Parsec.String as P
import qualified Text.Parsec as P
import qualified Data.Attoparsec.Text as A
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Data.Void (Void)

libMain :: IO ()
libMain = defaultMain
    [ env setUpEnv $ \ ~(input, textinput) -> bgroup "matching"
        [ bench "parsec" $ whnf (matchParsec input) (thatcherParser "dead")
        , bench "attoparsec" $ whnf (matchAttoparsec textinput) (thatcherAtto "dead")
        , bench "megaparsec" $ whnf (matchMegaparsec textinput) (thatcherMega "dead")
        , bench "tdfa" $ whnf (matchTDFA input) (thatcherRE "[dD]ead")
        ]
    ]

setUpEnv :: IO (String, T.Text)
setUpEnv = do
    gen <- getStdGen
    let x = randomRs (' ','z') gen -- from 32 to 122
    return $ (take 2000 x, T.pack $ take 2000 x)

thatcherRE :: String -> String
thatcherRE verb = "thatcher('s *| *[Ii]s) *" <> verb

matchTDFA :: String -> String -> String
matchTDFA input regex =
    case input =~ regex of
        True -> "complete"
        False -> "whoops"

thatcherParser :: String -> P.Parser String
thatcherParser verb = do
    P.string "thatcher"
    (P.string "'s" <|> (P.many1 P.space >> (P.string "is" <|> P.string "Is")))
    P.many1 P.space
    (P.char (head verb) <|> P.char ((toUpper . head) verb))
    P.string (tail verb)
    P.eof
    pure ""

matchParsec :: String -> P.Parser String -> String
matchParsec input parser =
    case P.runParser parser () "" input of
        Left _ -> "whoops"
        Right x -> "complete"

thatcherAtto :: T.Text -> A.Parser T.Text
thatcherAtto verb = do
    A.string "thatcher"
    (A.string "'s" <|> (A.takeWhile1 A.isHorizontalSpace >> (A.string "is" <|> A.string "Is")))
    A.takeWhile1 A.isHorizontalSpace
    (A.char (T.head verb) <|> A.char ((toUpper . T.head) verb))
    A.string $ T.tail verb
    A.endOfInput
    pure ""

matchAttoparsec :: T.Text -> A.Parser T.Text -> String
matchAttoparsec input parser =
    case A.parseOnly parser input of
        Left _ -> "whoops"
        Right x -> "complete"

thatcherMega :: T.Text -> M.Parsec Void T.Text T.Text
thatcherMega verb = do
    M.string "thatcher"
    (M.string "'s" <|> (M.takeWhile1P (Just "space") isSpace >> (M.string "is" <|> M.string "Is")))
    M.takeWhile1P Nothing isSpace
    (M.single (T.head verb) <|> M.single ((toUpper . T.head) verb))
    M.string $ T.tail verb
    M.eof
    pure ""


matchMegaparsec :: T.Text -> M.Parsec Void T.Text T.Text -> String
matchMegaparsec input parser =
    case M.runParser parser "" input of
        Left _ -> "whoops"
        Right x -> "complete"
