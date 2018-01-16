import System.Environment (getArgs, getProgName)
import Data.Maybe (listToMaybe)

bold :: String -> String
bold text = "\ESC[1m" ++ text ++ "\ESC[0m"

underline :: String -> String
underline text = "\ESC[4m" ++ text ++ "\ESC[0m"

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ bold progName ++ " " ++ underline "number" ++ " " ++ underline "number"

main = getArgs >>= takeOne

takeOne :: [String] -> IO ()
takeOne []            = usage
takeOne (number:args) = case maybeRead number of Nothing -> usage
                                                 Just p  -> takeTwo p args

takeTwo :: Integer -> [String] -> IO ()
takeTwo p [number] = case maybeRead number of Nothing -> usage
                                              Just q  -> keygen p q
takeTwo _ _        = usage

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

keygen :: Integer -> Integer -> IO ()
keygen p q
    | isPrime p && isPrime q && p /= q = do
        let n   = p * q
            phi = n - p - q + 1
            e   = head . filter ((== 1) . gcd phi) $ dropWhile (> phi) [65537,257,17,5,3]
            d   = (`div` e) . head . filter ((== 0) . (`mod` e)) $ map ((+1) . (* phi)) [e+1..]
        putStrLn $ "n: " ++ show n
        putStrLn $ "e: " ++ show e
        putStrLn $ "d: " ++ show d
    | otherwise = usage

isPrime :: Integer -> Bool
isPrime n = not $ any ((== 0) . mod n) [2..isqrt n]

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral