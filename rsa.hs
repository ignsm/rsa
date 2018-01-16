import System.Environment (getArgs, getProgName)
import Data.Maybe (listToMaybe)
import Data.Char (ord, chr)

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
                                              Just q  -> rsa p q
takeTwo _ _        = usage

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

rsa :: Integer -> Integer -> IO ()
rsa e n = interact $ map (\c -> character $ (ordinal c ^ e) `mod` n)

ordinal :: Char -> Integer
ordinal = toInteger . ord

character :: Integer -> Char
character = chr . fromIntegral
