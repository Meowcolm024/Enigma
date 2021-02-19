module Enigma where

import           Control.Monad.State
import           Data.Function                  ( (&) )
import qualified Data.Vector                   as V
import           System.Random
import           System.Random.Shuffle          ( shuffle' )
import           Data.List                      ( sortBy )

type Table = (V.Vector Int, V.Vector Int)

data Enigma = Enigma
  { rotors    :: (Table, Table, Table)
  , reflector :: V.Vector Int
  , pointer   :: (Int, Int, Int)
  }
  deriving Show

type EnigmaST = State Enigma

-- helper function
assoc :: [Int] -> Table
assoc xs =
  ( V.fromList xs
  , V.fromList $ map snd $ sortBy (\(a, b) (c, d) -> a `compare` c) $ zip
    xs
    [0 ..]
  )

-- constructors
enigma :: ([Int], [Int], [Int]) -> [Int] -> (Int, Int, Int) -> Enigma
enigma (a, b, c) r = Enigma (assoc a, assoc b, assoc c) (V.fromList r)

enigmaR :: Int -> (Int, Int, Int) -> Enigma
enigmaR seed = Enigma (genAlpha x, genAlpha y, genAlpha x)
                      (V.fromList [0 .. 25] V.// genRef)
 where
  genAlpha = assoc . shuffle' [0 .. 25] 26
  genRef   = ap (++) (map (uncurry $ flip (,))) $ uncurry zip $ splitAt
    13
    (shuffle' [0 .. 25] 26 w)
  [x, y, z, w] = map mkStdGen $ take 4 (randoms (mkStdGen seed) :: [Int])

enigmaR' :: Int -> Enigma
enigmaR' seed =
  let [a, b, c] = map (\x -> (abs x) `mod` 26)
        $ take 3 (randoms (mkStdGen seed) :: [Int])
  in  enigmaR seed (a, b, c)

-- rotor steps
step :: Enigma -> Enigma
step (Enigma rts rfl (a, b, c)) = Enigma rts rfl (x, y, z)
 where
  (c1, z) = (c + 1) `divMod` 26
  (c2, y) = (b + c1) `divMod` 26
  x       = (a + c2) `mod` 26

-- major cipher step
cipher :: Int -> Enigma -> (Int, Enigma)
cipher p e@(Enigma (a, b, c) r (x, y, z)) = (out, step e)
 where
  ind l s t = (l V.! ((s + t) `mod` 26) - s) `mod` 26
  out =
    p
      & ind (fst c) z
      & ind (fst b) y
      & ind (fst a) x
      & ind r       0
      & ind (snd a) x
      & ind (snd b) y
      & ind (snd c) z

cipherST :: Int -> EnigmaST Int
cipherST p = state $ \s -> cipher p s

cipherString :: String -> EnigmaST String
cipherString p = (fmap intToChar) <$> mapM (cipherST . charToInt) p
 where
  charToInt c = fromEnum c - 65
  intToChar c = toEnum (c + 65)

-- entry point
doEnigma :: Enigma -> String -> String
doEnigma e p = evalState (cipherString p) e
