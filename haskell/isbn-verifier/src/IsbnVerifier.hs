module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

parts :: String -> Maybe (String, Char)
parts xs = if length xs == 10
           then let (digits, checkDigit:_) = splitAt 9 xs in Just (digits, checkDigit)
           else Nothing

validateDigits :: String -> Maybe [Int]
validateDigits xs = if all isDigit xs then Just . map digitToInt $ xs else Nothing

validateCheckDigit :: Char -> Maybe Int
validateCheckDigit 'X' = Just 10
validateCheckDigit x | isDigit x = Just . digitToInt $ x
                     | otherwise = Nothing

values :: String -> Maybe [Int]
values str = do
  let chars = filter (\c -> isDigit c || c == 'X') str
  (digits, checkDigit) <- parts chars
  validDigits <- validateDigits digits
  validCheckDigit <- validateCheckDigit checkDigit
  Just $ validDigits ++ [validCheckDigit]

isbn :: String -> Bool
isbn = maybe False ((==0) . flip mod 11 . sum . zipWith (*) [10,9..1]) . values
