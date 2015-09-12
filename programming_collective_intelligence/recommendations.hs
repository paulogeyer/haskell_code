import Data.List (intersect, sort)
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
        , ("Michael Phillips", Map.fromList [("Lady in the Water", 2.5)
                                            ,("Snakes on a Plane", 3.0)
                                            ,("Superman Returns", 3.5)
                                            ,("The Night Listener", 4.0)])
        , ("Claudia Puig", Map.fromList [("Snakes on a Plane", 3.5)
                                        ,("Just My Luck", 3.0)
                                        ,("The Night Listener", 4.5)
                                        ,("Superman Returns", 4.0)
                                        ,("You, Me and Dupree", 2.5)])
        , ("Mick LaSalle", Map.fromList [("Lady in the Water", 3.0)
                                        ,("Snakes on a Plane", 4.0)
                                        ,("Just My Luck", 2.0)
                                        ,("Superman Returns", 3.0)
                                        ,("The Night Listener", 3.0)
                                        ,("You, Me and Dupree", 2.0)])
        , ("Jack Matthews", Map.fromList [("Lady in the Water", 3.0)
                                         ,("Snakes on a Plane", 4.0)
                                         ,("The Night Listener", 3.0)
                                         ,("Superman Returns", 5.0)
                                         ,("You, Me and Dupree", 3.5)])
        , ("Toby", Map.fromList [("Snakes on a Plane", 4.5)
                                ,("You, Me and Dupree", 1.0)
                                ,("Superman Returns", 4.0)])
        ]

sim_distance :: [Double] -> [Double] -> Double
sim_distance p1ranks p2ranks =
  if n /= 0
  then r
  else 0.0
  where n = fromIntegral $ length p1ranks
        sum_of_squares = sum $ zipWith (\a b -> (a - b)**2) p1ranks p2ranks
        r = 1 / (1 + sum_of_squares)


sim_pearson :: [Double] -> [Double] -> Double
sim_pearson p1ranks p2ranks =
  if n /= 0
  then r
  else 0.0
  where n = fromIntegral $ length p1ranks
        sum1 = sum p1ranks
        sum2 = sum p2ranks
        sum1sq = sum $ map (**2) p1ranks
        sum2sq = sum $ map (**2) p2ranks
        pSum = sum $ zipWith (*) p1ranks p2ranks
        num = pSum - ((sum1 * sum2) / n)
        den = sqrt $ (sum1sq - sum1 ** 2 / n) * (sum2sq - sum2 ** 2 / n)
        r = num / den

calc_sim f p1 p2 =
  f p1_ranks p2_ranks
  where p1_movies = prefs Map.! p1
        p2_movies = prefs Map.! p2
        p1_ranks = Map.elems $ Map.intersection p1_movies p2_movies
        p2_ranks = Map.elems $ Map.intersection p2_movies p1_movies

topMatches prefs person n =
  take n (reverse $ sort scores)
  where others = filter ((/=) person) (Map.keys prefs)
        scores = [(calc_sim sim_pearson person other, other) | other <- others]

