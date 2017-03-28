module LearningLib
    ( last4
    , cutFirstPart
    , mytake
    , wordsWhen
    , getExtensionHelper
    , getFnameHelper
    , right
    , cnv
    ) where


last4 x = reverse (take 4 (reverse x))

cutFirstPart [] = []
cutFirstPart (x:xs) =
 if x=='.'
  then xs
  else cutFirstPart xs

wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
mytake m ys               = case (m,ys) of
                             (0,_) ->  0
                             (_,1) ->  1
                             (n,_) ->  2
getExtensionHelper n (x:xs) =
 if (x:xs)!!n=='.' || n>=length xs
  then take (n+1) (x:xs)
  else getExtensionHelper (n+1) (x:xs)

getFnameHelper n (x:xs) =
 if (x:xs)!!n=='.' || n>=length xs
  then take (length (x:xs)-(n+1)) ( reverse  (x:xs) )
  else getFnameHelper (n+1) (x:xs)

right n x = drop (length x - n) x

cnv = map (\ x -> ("getM4a"++ x, "getTxt" ++ x))
