{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A code generation template haskell. Everything is taken as literal text,
-- with ~var~ variable interpolation.
module Yesod.Util.CodeGen (codegen, codegenDir) where

import Foundation hiding (takeWhile)
import Control.Applicative
import Language.Haskell.TH.Syntax
-- import Text.ParserCombinators.Parsec
import Data.Attoparsec.Text hiding (take)
import qualified Data.Text as DT
import Filesystem
import qualified Data.Char as DC
import Filesystem.Path.CurrentOS hiding (concat)
-- import qualified Data.Text.Lazy as LT
-- import qualified Data.Text.Lazy.Encoding as LT

data Token = VarToken Text | LitToken Text | EmptyToken

codegenDir :: FilePath -> FilePath -> Q Exp
codegenDir dir fp = do
  s <- qRunIO . readTextFile $ addExtension (dir ++ fp) "cg"
  -- let s = LT.unpack $ LT.decodeUtf8 s'
      -- _GUIDE makeFields l = mapM (\t -> checkRes (parse parseField t)) l
      -- _GUIDE  where
      -- _GUIDE   checkRes (Done _ r) = return r
      -- _GUIDE   checkRes (Partial f) = checkRes $  f DT.empty 
      -- _GUIDE   checkRes r = throwError $ "parse error: " ++ show r
  concat' <- [|(concat)|]

  case parse (many parseToken) s of
    -- Left e -> error $ show e
    -- Right tokens' -> do
    Partial f -> case f DT.empty of
      Done _ tokens' ->  do
        let tokens'' = map toExp tokens'
        return $ concat' `AppE` ListE tokens''
      _ -> error ("partial didn't return" :: Text)
    Done _ tokens' ->  do
      let tokens'' = map toExp tokens'
      return $ concat' `AppE` ListE tokens''
    _ -> error ("parse failure" :: Text)

codegen :: FilePath -> Q Exp
codegen fp = codegenDir "codegen" fp

toExp :: Token -> Exp
toExp (LitToken s) = LitE $ StringL $ DT.unpack s
toExp (VarToken s) = VarE $ mkName $ DT.unpack s
toExp EmptyToken = LitE $ StringL ""

parseToken :: Parser Token
parseToken =
  parseVar <|> parseLit
  where
  parseVar = do
    _ <- char '~'
    s <- takeWhile DC.isAlphaNum
    _ <- char '~'
    return $ if length s == 0 then EmptyToken else VarToken s
  parseLit = do
    s <- takeWhile1 (/= '~')
    -- s <- many1 $ noneOf "~"
    return $ LitToken s
