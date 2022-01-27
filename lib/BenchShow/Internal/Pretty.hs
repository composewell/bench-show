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

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
