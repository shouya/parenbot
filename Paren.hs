module Paren (
  matchParen
  ) where

import qualified Data.Foldable as F

parenList :: String
parenList = "()[]{}（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»" ++
            "「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙" ++
            "｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱" ++
            "❲❳⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸"

parseList :: String -> [(Char, Char)]
parseList []       = []
parseList (l:r:xs) = (l,r) : parseList xs
parseList _        = error "invalid parentheses list"


matchParen :: String -> Maybe String
matchParen input =
  do stack <- F.foldlM process "" input
     if null stack
       then Nothing
       else mapM (`lookup` parenMap) stack
  where leftParens  = map fst parenMap
        rightParens = map snd parenMap
        process st c =
          case True of
           _ | c `elem` leftParens  -> return (c:st)
           _ | c `elem` rightParens -> mst
             where mst | null st   = Nothing
                       | otherwise = do
                           r <- lookup (head st) parenMap
                           if r == c
                             then return (tail st)
                             else Nothing
           _                        -> return st
        parenMap = parseList parenList
