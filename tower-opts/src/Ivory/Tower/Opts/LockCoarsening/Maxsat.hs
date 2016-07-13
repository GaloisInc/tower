module Ivory.Tower.Opts.LockCoarsening.Maxsat where

import Data.List
import Data.Int

-- | The datatype of a PWMS instance.
data PWMS = PWMS { comments     :: [Comment]
                 , hard_clauses :: [HardClause]
                 , soft_clauses :: [SoftClause]
                 }


-- | Comments rendered in the output file.
type Comment = String

-- | Each hard clause is a list of litterals, of which one should be true.
type HardClause = [Litteral]

-- | Each soft clause is a list of litterals, of which one should be true, 
--   and a weight (integer) for the clause.
type SoftClause = (Integer,[Litteral])

-- | Positive litteral or negative litterals. The absolute value indicates 
--   the atom of the litteral.
type Litteral = Integer

-- | The weight for the hard clauses : we set it to Int32 max - 10
hardWeight :: Integer
hardWeight = toInteger $ (maxBound::Int32) - 10

-- | Compute the number of variables in one PWMS isntance.
nbVar :: PWMS -> Integer
nbVar (PWMS _ hards softs) =
  toInteger $ length $ nub $ map abs $ concat $ concat [hards, map snd softs]

-- | Given soft clauses, it computes the maximum weight of one soft clause.
maxWeight :: [SoftClause] -> Integer
maxWeight softs = 
  maximum $ map fst softs

-- | If the maximum weight is too big for the maxsat instance (risk of integer 
--   overflow), it will replace the weights by smaller ones using some math.
replaceWeights :: Integer -> Integer -> [SoftClause] -> [SoftClause]
replaceWeights _ _ [] = []
replaceWeights maxValue threshold (a:b) =
  if (maxValue <= threshold)
    then a:(replaceWeights maxValue threshold b)
    else 
      let newWeight = quot ((fst a)*threshold+maxValue-1) maxValue in
      ((newWeight, snd a):(replaceWeights maxValue threshold b))

-- | Outputs a list of clauses to the right format (add 0 at the end)
outputList :: [Integer] -> String
outputList l = concat $ intersperse " " $ map show (l++[0])

-- | Transforms the comments into one string.
renderComments :: [Comment] -> [String]
renderComments com =
  map (\x -> "c "++x) com

renderHardClauses :: [HardClause] -> [String]
renderHardClauses hards = 
  map (\x -> (show hardWeight) ++ " " ++ (outputList x)) hards

renderSoftClauses :: [SoftClause] -> [String]
renderSoftClauses softs = 
  let scaledSofts = replaceWeights (maxWeight softs) threshold softs
      filteredSofts = filter (\(x,_) -> x /= 0) scaledSofts in
  map (\(w,l) -> (show w) ++ " " ++ (outputList l)) filteredSofts
  where
    threshold = quot hardWeight $ toInteger $ length softs


renderPWMS :: PWMS -> String
renderPWMS (pwms@(PWMS com hards softs)) = 
  commentsPWMS ++ header ++ hardClauses ++ softClauses
  where 
    commentsPWMS = concat $ intersperse "\n" $ renderComments com
    header = "p wcnf "++(show $ nbVar pwms)++" "++
             (show $ (length hards) + (length softs))++" "++
             (show hardWeight)
    hardClauses = concat $ intersperse "\n" $ renderHardClauses hards
    softClauses = concat $ intersperse "\n" $ renderSoftClauses softs