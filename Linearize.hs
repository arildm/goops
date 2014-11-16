module Linearize where

import Language
import Data.List (intercalate)
import Data.Char (isSpace, toUpper)

--
-- Show instances for data structures
--

instance Show Type where
	show (Class n ps fs ms)
		= blocko ("class" +++ n) (nnjoin
			  [ nnjoin $ map show fs
		    , case ps of
						[] -> "" -- Only print constructor if it has parameters
						ps -> show $ Method Public Void ps "__construct" "\n"
				, nnjoin $ map show ms
				]
			)
	show Integer = "int"
	show Boolean = "bool"
	show String = "string"
	show Void = ""

instance Show Field where
	show (Field v t n) = show v +++ dol n ++ ";\n"

instance Show Method where
	show (Method v _ ps n b) = block (show v +++ "function" +++ n ++ "(" ++ comma (map show ps) ++ ")") b

instance Show Parameter where
	show (Parameter (Class t _ _ _) n) = t +++ dol n
	show (Parameter _ n) = dol n

instance Show Visibility where
	show Private = "private"
	show Protected = "protected"
	show Public = "public"

--
-- String operations
--

comma :: [String] -> String
comma = intercalate ", "

join :: String -> [String] -> String
join glue = intercalate glue . filter ([] /=) . map ntrim

ntrim :: String -> String
ntrim = dropStereoWhile ('\n' ==)

dropStereoWhile :: (a -> Bool) -> [a] -> [a]
dropStereoWhile p = dropWhile p . dropEndWhile p []
  where
		dropEndWhile :: (a -> Bool) -> [a] -> [a] -> [a]
		dropEndWhile p _ [] = []
		dropEndWhile p bin (a:as) | p a = dropEndWhile p (bin ++ [a]) as
		                          | otherwise = bin ++ a : dropEndWhile p [] as

nnjoin :: [String] -> String
nnjoin = join "\n\n"

indents :: String -> String
indents = nend . unlines . map indent . lines

indent :: String -> String
indent [] = []
indent l = "  " ++ l

dol :: String -> String
dol = (++) "$"

this :: String -> String
this = dol . arw "this"

arw :: String -> String -> String
arw a b = a ++ "->" ++ b

block :: String -> String -> String
block head body = head +++ "{\n" ++ indents body ++ "}\n"

blocko :: String -> String -> String
blocko head body = head +++ "{\n\n" ++ indents body ++ "\n}\n"

(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b

nend :: String -> String
nend [] = []
nend [a] = if (a == '\n') then "\n" else a:"\n"
nend (a:as) = a : nend as

cap :: String -> String
cap (a:as) = toUpper a : as
