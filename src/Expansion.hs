{-# LANGUAGE NoMonomorphismRestriction #-}
module Expansion (
    primaryExpansion, allExpansions, isUnexpandable,
    rewriteTerm, rewriteFormula
) where

import Prelude hiding ((/))
import Data.Maybe

import RobotM
import Types
import Match

----------------------------------------------------------------------------------------------------

--TODO: consider moving primaryExpansion', isUnexpandable into the RobotM monad

primaryExpansion' :: Formula -> RobotM (Maybe (Matching, Formula))
primaryExpansion' f = do
    et <- askLibraryExpansionTable
    return . listToMaybe $ do
        (patt, expansion) <- et
        matching <- maybeToList $ match patt f
        return (matching, expansion)
--    return $ transform matching expansion

isUnexpandable :: Formula -> RobotM Bool
isUnexpandable f = do
    _et <- askLibraryExpansionTable
    pe <- primaryExpansion' f
    return $ isNothing pe

primaryExpansion :: Formula -> RobotM Formula
primaryExpansion f = do
    _et <- askLibraryExpansionTable
    _rt <- askLibraryRewriteTable
    pe <- primaryExpansion' f
    (matching, expansion) <- oneOf $ maybeToList pe
    expansion' <- renameFormula expansion
    --rewriteFormulaIfPossible <$> renameFormula f'
    rewriteFormulaIfPossible $ transform matching expansion'

allExpansions :: Formula -> RobotM Formula
allExpansions = primaryExpansion

----------------------------------------------------------------------------------------------------

--NB: this term rewriting code does not use renameFormula  -- so DO NOT ever put quantifiers on RHS
--      of the rewrite trable.
--  (This is only relevant if we introduce sets, etc., so that formulae can be inside terms.)

rewriteTerm :: Term -> RobotM (Maybe Term)
rewriteTerm t = do
    rt <- askLibraryRewriteTable
    return . listToMaybe $ do
        (patt, vs, rewriteTo) <- rt
        matching <- maybeToList $ matchTerm patt vs t
        return $ transformTerm matching rewriteTo

rewriteTermIfPossible :: Term -> RobotM Term
rewriteTermIfPossible t = do
    rewritten <- rewriteTerm t
    return . fromMaybe t $ rewritten

rewriteFormulaIfPossible :: Formula -> RobotM Formula
rewriteFormulaIfPossible = mapTermInFormulaM rewriteTermIfPossible

rewriteFormula :: Formula -> RobotM (Maybe Formula)
rewriteFormula f = do f' <- mapTermInFormulaM rewriteTermIfPossible f
                      return $ if f == f' then Nothing else Just f'

----------------------------------------------------------------------------------------------------

