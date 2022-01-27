{-# LANGUAGE CPP #-}

-- |
-- Module      : BenchShow.Internal.Pretty
-- Copyright   : (c) 2022 Composewell Technologies
--
-- License     : BSD-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module BenchShow.Internal.Pretty
    ( Doc
    , dullred
    , dullgreen
    , (<+>)
    , vcat
    , fill
    , indent
    , text
    , putDoc
    ) where

#ifndef NO_COLORS

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

#else

type Doc = String

dullred :: Doc -> Doc
dullred = id

dullgreen :: Doc -> Doc
dullgreen = id

(<+>) :: Doc -> Doc -> Doc
(<+>) x y = x ++ " " ++ y

vcat :: [Doc] -> Doc
vcat = unlines

putDoc :: Doc -> IO ()
putDoc = putStrLn

fill :: Int -> Doc  -> Doc
fill i x =
    let len = length x
     in x ++ replicate (i - len) ' '

indent :: Int -> Doc -> Doc
indent i = unlines . map (replicate i ' ' ++) . lines

text :: String -> Doc
text = id

#endif
