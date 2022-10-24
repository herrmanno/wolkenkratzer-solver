{-# LANGUAGE TypeApplications #-}
import Data.List (permutations, transpose)
import Control.Monad (forM)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

{-
    Termina:
    - high: a digit at index i is 'high', if it greater than all digits at index [0..i - 1]
            respectevely
    - low: a digit at index i is 'low', if there is some digit at index [0..i - 1], that is greater
           than the digit in question
    - maxima: a series of digits has exactly n maxima, if it contains n high digits
-}

type Puzzle = Map (Int,Int) (Set Int)

-- | (Left,Right) / (Upper,Lower) constraint
type Constraint = (Int,Int)

type Permutation = [Int]

-- | Checks if array has exactly `n` maxima
maxima n (0:b:c:d:e:f:_) = maxima' n [b,c,d,e,f]
maxima n (a:0:c:d:e:f:_) = maxima' n [a,c,d,e,f]
maxima n (a:b:0:d:e:f:_) = maxima' n [a,b,d,e,f]
maxima n (a:b:c:0:e:f:_) = maxima' n [a,b,c,e,f]
maxima n (a:b:c:d:0:f:_) = maxima' n [a,b,c,d,f]
maxima n (a:b:c:d:e:0:_) = maxima' n [a,b,c,d,e]
maxima n _ = False

-- | Checks if array has exactly `n` maxima, ignoring a possible zero
maxima' n xs = case n of
    1 ->
        -- HLLLL
        l 2 && l 3 && l 4 && l 5
    2 ->
        -- HHLLL, HLHLL, HLLHL, HLLLH
        h 1 && (
            h 2 && (
                l 3 && l 4 && l 5
            ) ||
            l 2 && (
                h 3 && (
                    l 4 && l 5
                ) ||
                l 3 && (
                    (
                        (h 4 && l 5) ||
                        (l 4 && h 5)
                    )
                )
            )
        )
    3 ->
        -- HHHLL, HHLHL, HHLLH, HLHLH, HLHH, HLLHH
        h 1 && (
            h 2 && (
                h 3 && (
                    (l 4 && l 5)
                ) ||
                l 3 && (
                    (h 4 && l 5) ||
                    (l 4 && h 5)
                )
            ) ||
            l 2 && (
                (h 3 && l 4 && h 5) ||
                (h 3 && h 4 && l 5) ||
                (l 3 && h 4 && h 5)
            )
        )
    4 ->
        -- HHHHL, HHHLH, HHLHH, HLHHH
        h 1 && (
          h 2 && (
            h 3 && (
                (h 4 && l 5) ||
                (l 4 && h 5)
            ) ||
            (l 3 && h 4 && h 5)
          ) ||
          (l 2 && h 3 && h 4 && h 5)
        )
    5 ->
        -- HHHHH
        h 1 && h 2 && h 3 && h 4 && h 5
    _ -> False
    where
        h i = is i H xs
        l i = is i L xs

-- | A property a digit at a specific index can have
data Height = H | L

-- | Checks if a digit at a specific location is 'high' or 'low'
is 1 _ _ = True 
is 2 H (a:b:_) = b > a
is 2 L (a:b:_) = b < a
is 3 H (a:b:c:_) = c > a && c > b
is 3 L (a:b:c:_) = c < a || c < b
is 4 H (a:b:c:d:_) = d > a && d > b && d > c
is 4 L (a:b:c:d:_) = d < a || d < b || d < c
is 5 H (a:b:c:d:e:_) = e > a && e > b && e > c && e > d
is 5 L (a:b:c:d:e:_) = e < a || e < b || e < c || e < d

-- | Solves a puzzle
solve n (cVertical, cHorizontal) =
    let
        candidates = S.fromList [0..n]
        map = M.fromList $ [((x,y), candidates) | x <- [0..n], y <- [0..n]]
        map' = (go n (Y 0) cHorizontal) . (go n (X 0) cVertical) $ map
    in map'
    where
        go = filterCandidates [filterByConstraint]
        filterByConstraint _ (c1, c2) _ xs =
            maxima c1 xs && maxima c2 (reverse xs)

-- | Reduces candidates in a puzzle
reduce n constraints map =
    let map' = reduceByConstraints n constraints
             . reduceChoices n
             . reduceFinites n
             $ map
    in if map' == map then map' else reduce n constraints map'

-- Reduces candidates by eliminating numbers from rows and columns where one cell is already finite
reduceFinites n map =
    let finites = M.filter ((==1) . S.size) map
        map' =
            foldr
                (\((x,y), set) map ->
                    let map' = foldr (\y' map -> M.adjust (S.\\ set) (x,y') map) map [y' | y' <- [0..n], y' /= y]
                        map'' = foldr (\x' map -> M.adjust (S.\\ set) (x',y) map) map' [x' | x' <- [0..n], x' /= x]
                    in map''
                )
                map
                (M.toList finites)
    in map'

-- | Reduces candidates by making those digits finite, that only occur in a single cell
reduceChoices n map =
    let f b (x,y) map =
            let others =
                    if b
                        then S.unions [map M.! (x', y) | x' <- [0..n], x' /= x]
                        else S.unions [map M.! (x, y') | y' <- [0..n], y' /= y]
                remaining = (map M.! (x,y)) S.\\ others
            in
                if 1 == S.size remaining
                    then M.insert (x,y) remaining map
                    else map
        map' = foldr (f True) map [(x,y) | x <- [0..n], y <- [0..n]]
        map'' = foldr (f False) map' [(x,y) | x <- [0..n], y <- [0..n]]
    in map''

-- | Reduces candidates by overlaying valid permutations
reduceByConstraints n (cVertical, cHorizontal) map =
    (go n (Y 0) cHorizontal) . (go n (X 0) cVertical) $ map
    where
        go = filterCandidates [filterByConstraint, filterByPermutation]
        filterByConstraint _ (c1, c2) _ xs =
            maxima c1 xs && maxima c2 (reverse xs)
        filterByPermutation counter _ map xs =
            case counter of
                Y y ->
                    let row = [map M.! (x,y) | x <- [0..n]]
                    in and $ zipWith S.member xs row
                X x -> 
                    let column = [map M.! (x,y) | y <- [0..n]]
                    in and $ zipWith S.member xs column

type PermutationFilter = Counter Int -> Constraint -> Puzzle -> Permutation -> Bool

-- | Filters a puzzle by limiting the possible permutation for each row and column
filterCandidates :: [PermutationFilter] -> Int -> Counter Int -> [Constraint] -> Puzzle -> Puzzle
filterCandidates _ n _ [] map = map
filterCandidates filters n counter (constraint@(c_left, c_right):cs) map =
    let xss = permutations [0..n]
        candidatePerms = foldr filter xss (fmap (\f -> f counter constraint map) filters)
        candidates = S.fromList <$> (transpose candidatePerms)
        restrictions = case counter of
            Y y ->
                M.fromList [((x,y), set) | (set, x) <- zip candidates [0..]]
            X x ->
                M.fromList [((x,y), set) | (set, y) <- zip candidates [0..]]
        map' = M.unionWith S.intersection map restrictions
    in
        filterCandidates filters n ((+1) <$> counter) cs map'

-- | A container for an x or y value
data Counter a = X a | Y a deriving Functor

-- | Checks if a result is unambiguously
toResult map =
    let solved = M.foldr (&&) True . M.map ((==1) . S.size) $ map
        map' = M.map (head . S.toList) map
    in
        if solved
            then Just map'
            else Nothing

-- | Pretty prints result map
showResult n (cv, ch) map =
    case toResult map of
        Just map' -> header <> toString map' <> footer
        Nothing -> "No result"
    where
        toString map = unlines $
            [ (show (fst (ch !! y)) <> "|")
              ++ concat
                [ show (map M.! (x,y))
                | x <- [0..n]
                ]
              ++ ("|" <> show (snd (ch !! y)))
            | y <- [0..n]
            ]
        header = unlines
            [ "  " <> concat [ show x | (x,_) <- cv]
            , "  " <> replicate (n + 1) '-'
            ]
        footer = unlines
            [ "  " <> replicate (n + 1) '-'
            , "  " <> concat [ show x | (_,x) <- cv]
            ]

-- | Parses input
readInput = do
    n <- readLn @Int
    constraintsVertical <- readConstraints n
    constraintsHorizontal <- readConstraints n
    return (n, (constraintsVertical, constraintsHorizontal))
    where
        readConstraints n = forM [1..(n + 1)] $ \_ -> do
            c1 <- readLn @Int
            c2 <- readLn @Int
            return (c1,c2)

main = do
    (n, constraints) <- readInput
    let result = reduce n constraints $ solve n constraints
    putStrLn $ showResult n constraints result
