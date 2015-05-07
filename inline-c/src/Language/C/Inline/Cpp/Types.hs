{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.C.Inline.Cpp.Types (parseCppDeclaration) where

import           Control.Applicative ((<*))
import           Control.Monad (void)
import           Data.Functor ((<$>))
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Data.Char (isSpace)

import           Language.C.Types
import           Language.C.Types.Parse

parseCppDeclaration :: CParser m => m (Id, String)
parseCppDeclaration = do
  void $ char '('
  chunk <- ('(' :) <$> go [')']
  getId $ tail $ init chunk
  where
    go [] = do
      return ""
    go stack = do
      ch <- oneOf acceptable
      rest <- case (opening ch, closing ch, stack) of
        (Just closedBy, _, _) -> go (closedBy : stack)
        (_, True, closedBy : stack') ->
          if ch == closedBy
            then go stack'
            else fail $ "Unexpected closing characther " ++ show ch
        (_, True, []) ->
          fail $ "Unexpected closing characther " ++ show ch
        (_, _, _) -> go stack
      return $ ch : rest

    acceptable = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "<>()[]:* \t_"

    matching =
      [ ('(', ')')
      , ('[', ']')
      , ('<', '>')
      ]

    opening ch = lookup ch matching
    closing ch = ch `elem` map snd matching

    getId s = do
      let (revCId, revRest) = break (== '*') $ reverse s
      let (cId, rest) = (reverse (dropWhile isSpace revCId), reverse revRest)
      if null rest
        then fail $ "Expecting pointer type, instead of `" ++ s ++ "`"
        else case runCParser (const False) "parseCppDeclaration" cId (identifier_no_lex <* eof) of
          Right cId' -> return (cId', rest)
          Left _ -> fail $ "Invalid C identifier " ++ cId
