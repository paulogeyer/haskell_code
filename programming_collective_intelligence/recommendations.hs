import Data.List (intersect)
import qualified Data.Map as Map

prefs = Map.fromList
        [("Lisa Rose", Map.fromList [("Lady in the Water", 2.5)
                                    , ("Snakes on a Plane", 3.5)
                                    , ("Just My Luck", 3.0)
                                    , ("Superman Returns", 3.5)
                                    , ("You, Me and Dupree", 2.5)
                                    , ("The Night Listener", 3.0)])
        , ("Gene Seymour", Map.fromList [("Lady in the Water", 3.0)
                                        , ("Snakes on a Plane", 3.5)
                                        , ("Just My Luck", 1.5)
                                        , ("Superman Returns", 5.0)
                                        , ("The Night Listener", 3.0)
                                        , ("You, Me and Dupree", 3.5)])
        ]

-- sim_pearson :: Map [Char] a -> String -> String -> Double
sim_pearson prefs p1 p2 =
  if siLength > 0 && den /= 0
  then r
  else 0.0
  where p1Movies = prefs Map.! p1
        p2Movies = prefs Map.! p2
        p1Movies_rates = Map.elems $ Map.intersection p1Movies p2Movies
        p2Movies_rates = Map.elems $ Map.intersection p2Movies p1Movies
        si :: [String]
        si = Map.keys p1Movies `intersect` Map.keys p2Movies
        siLength :: Double
        siLength = fromIntegral $ length si
        sum1 :: Double
        sum1 = sum p1Movies_rates
        sum2 :: Double
        sum2 = sum p2Movies_rates
        sum1sq = sum $ map (**2) p1Movies_rates
        sum2sq = sum $ map (**2) p2Movies_rates
        pSum = sum $ zipWith (*) p1Movies_rates p2Movies_rates
        num = pSum - ((sum1 * sum2) / siLength)
        den = sqrt $ (sum1sq - sum1 ** 2 / siLength) * (sum2sq - sum2 ** 2 / siLength)
        r = num / den
