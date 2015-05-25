{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

import Control.Applicative
import Control.Monad
(<||>) = liftA2 (||)

-- Section 4.1

-- 's' is the "source" information for the term
-- (this isn't used here)
data Term s
	= TmTrue s
	| TmFalse s
	| TmIf s (Term s) (Term s) (Term s)
	| TmZero s
	| TmSucc s (Term s)
	| TmPred s (Term s)
	| TmIsZero s (Term s)
	deriving (Eq, Show)

-- pattern matchers which ignore the 'source' information
pattern TTrue <- TmTrue _
pattern TFalse <- TmFalse _
pattern TIf b x y <- TmIf _ b x y
pattern TPred t <- TmPred _ t
pattern TSucc t <- TmSucc _ t
pattern TZero <- TmZero _
pattern TIsZero t <- TmIsZero _ t

isNumeric TZero = True
isNumeric (TSucc t) = isNumeric t
isNumeric _ = False

isBoolean TTrue = True
isBoolean TFalse = True
isBoolean _ = False

isVal = isBoolean <||> isNumeric

-- Section 4.2

eval1 (TIf TTrue t _) = pure t
eval1 (TIf TFalse _ f) = pure f
eval1 (TmIf s tb tt tf) = (\tb' -> TmIf s tb' tt tf) <$> eval1 tb
eval1 (TmSucc s t) = TmSucc s <$> eval1 t
eval1 (TPred TZero) = pure $ TmZero Nothing
eval1 (TPred (TSucc nv)) | isNumeric nv = pure nv
eval1 (TmPred s t) = TmPred s <$> eval1 t
eval1 (TIsZero TZero) = pure $ TmTrue Nothing
eval1 (TIsZero (TSucc nv)) | isNumeric nv = pure $ TmFalse Nothing
eval1 (TmIsZero s t) = TmIsZero s <$> eval1 t
eval1 _ = empty

eval t = (eval1 t >>= eval) <|> pure t

-- Exercise 4.2.2
big (TIf (big -> TTrue) t _) = big t
big (TIf (big -> TFalse) _ f) = big f
big (TSucc (big -> nv)) | isNumeric nv = TmSucc Nothing nv
big (TPred (big -> TZero)) = TmZero Nothing
big (TPred (big -> TSucc nv)) | isNumeric nv = nv
big (TIsZero (big -> TZero)) = TmTrue Nothing
big (TIsZero (big -> TSucc nv)) | isNumeric nv = TmFalse Nothing
big v = v