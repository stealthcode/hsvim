import Data.Char
import Data.Monoid

main = do 
	f <- acceptCommand (Sum 1) 
	putStrLn . show $ f vim
	where vim = Vim (1,1) "this is tricky"

acceptCommand :: Sum Int -> IO (Vim -> Vim)
acceptCommand s = do
	c <- getChar
	preprocess c s

preprocess :: Char -> Sum Int -> IO (Vim -> Vim)
preprocess c s
	| isAlpha  c = return $ runCommand c (getSum s)
	| isNumber c = acceptCommand (Sum (digitToInt c))
	| otherwise  = return $ id

runCommand :: Char -> Int -> (Vim -> Vim)
runCommand 'h' x = move id ((-) x)
runCommand 'j' x = move id (+ x)
runCommand 'u' x = move (+ x) id
runCommand 'n' x = move ((-) x) id
runCommand  _  _ = id

data Vim = Vim {cursor :: (Int, Int), content :: String} deriving (Show, Eq)

move :: (Int -> Int) -> (Int -> Int) -> (Vim -> Vim)
move rm cm (Vim (r,c) content) = Vim (r',c') content
	where 	
		c' = max (min 1 (cm c)) (length $ (lines content) !! (r-1))
		r' = max (min 1 (rm r)) (length (lines content))