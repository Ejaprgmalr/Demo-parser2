module ParserImpl where

import Absyn
-- other imports
import Text.ParserCombinators.ReadP as RP
import Data.Char
-- import Data.List
import Control.Applicative


main = do 
    text <- readFile "../examples/components.cr"
    let Right x = parseString text
    putStr $ show x


parseString :: String -> Either String IDB
parseString str = let result = filter (\x -> snd x == "") (readP_to_S pIDB str) in
                  case result of
                     [] -> Left ("parse error on " ++ str)
                     (x:_) -> Right (fst x)

      



token :: ReadP a -> ReadP a
token p = do
          skipSpaces
          skipMany pComment
          skipSpaces
          a <- p
          return a


pComment :: ReadP String
pComment = between (char '{') (char '}') (pComment +++ (munch (/= '}')))

-- pCommentNest :: ReadP a -> ReadP a
-- pCommentNest p = munch (/= '}')
--                +++  pComment


-- token :: ReadP a -> ReadP a
-- token p = do
--           skipMany (char ' ') <|> skipMany1 (char '\n') <|> skipMany1 (char '\t')
--           rp <- p
--           skipMany (char ' ') <|> skipMany1 (char '\n') <|> skipMany1 (char '\t')
--           return rp



symbol:: String  -> ReadP String
symbol str =  token $ string str


asciiLetter = ['A'..'Z'] ++ ['a'..'z']

asciiLetterDigit = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']



letterFound :: String -> String -> Bool
letterFound _ [] = False
letterFound ascls (x:xs) = x `elem` ascls || letterFound ascls xs



nameLengthCheck :: String -> Bool
nameLengthCheck str = length str <= 32


-- get inspiration from Internet:
-- https://stackoverflow.com/questions/9220986/is-there-any-haskell-function-to-concatenate-list-with-separator
joinBy :: String -> [String] -> String
joinBy sep cont = drop (length sep) $ concatMap (\w -> sep ++ w) cont



pWord :: ReadP String
pWord = token $
        do s <- munch1 (`elem` asciiLetterDigit)
           if letterFound asciiLetter s 
           then return s
           else fail "word should contain at least one letter"


pName :: ReadP String
pName = token $
        do n <- sepBy1 pWord (symbol "-")
           let str = joinBy "-" n
           if nameLengthCheck str
           then return str
           else fail "Name cannot be longer than 32 characters"
      

pNum :: ReadP Int
pNum  = do n <- munch1 isDigit
           if read n <= 999999 
           then return $ read n
           else fail "numeric value must be no more than 999999"
        

{--

pRSpec :: ReadP RSpec
pRSpec = (do r1 <- token pRSpec1
             symbol "|"
             r  <- token pRSpec
             pRSpec_sep $ RSOr r1 r)
      +++token pRSpec1

pRSpec_sep :: RSpec -> ReadP RSpec
pRSpec_sep a = (do
                   _ <- symbol "|"
                   b <- token pRSpec1
                   return $ RSOr a b)
                <++ return a

pRSpec1 :: ReadP RSpec
pRSpec1 = (do r2 <- token pRSpec2
              symbol ","
              r1 <- token pRSpec1
              pRSpec1_sep $ RSAnd r2 r1)
       +++token pRSpec2

pRSpec1_sep :: RSpec -> ReadP RSpec
pRSpec1_sep a = (do
                   _ <- symbol ","
                   b <- token pRSpec2
                   return $ RSAnd a b)
                <++ return a


pRSpec2 :: ReadP RSpec
pRSpec2 = between (symbol "(") (symbol ")") (token pRSpec)
      <++ token (fmap RSRes pName)
      <++ (do n <- token pNum
              r <- token pRSpec
              return $ RSNum n r)

--}

infixOp :: String -> (a -> a -> a) -> ReadP (a -> a -> a)
infixOp x f = symbol x >> return f


pRSpec :: ReadP RSpec
pRSpec = pRSpec_term `chainl1` (infixOp "|" RSOr)

pRSpec_term :: ReadP RSpec
pRSpec_term = pRSpec_clause `chainl1` (infixOp "," RSAnd)

pRSpec_clause :: ReadP RSpec
pRSpec_clause = token (between (char '(') (char ')') pRSpec)
            +++ (do
                   n <- token pName
                   return $ RSRes n) 
            +++ (do 
                   n <-  token pNum
                   rspec <- token pRSpec_clause
                   return $ RSNum n rspec)
             
-- p x str= filter (\x -> snd x == "") (readP_to_S x str)          

pClause :: ReadP Clause
pClause = (do symbol "provides"
              r <- token pRSpec
              return (CKProvides, r))
       +++(do symbol "uses"
              r <- token pRSpec
              return (CKUses, r))
       +++(do symbol "requires"
              r <- token pRSpec
              return (CKRequires, r))


pClauses :: ReadP [Clause]
pClauses = sepBy1 pClause (symbol ";")



pRNames :: ReadP [RName]
pRNames = do
            symbol "resource"
            a <- sepBy1 pName (symbol ",")
            symbol "."
            return a


pIComp :: ReadP IComp
pIComp = do symbol "component"
            cn <- pName
            symbol ":"
            cl <- pClauses
            symbol "."
            return $ IC cn cl

pIComps :: ReadP [IComp]
pIComps = sepBy1 pIComp (symbol "")


pNameOrRes :: ReadP IDB
pNameOrRes = (do
                name <- token pRNames
                return (name, []))
          +++(do
                com <- token pIComp
                return ([], [com]))



pIDB :: ReadP IDB
pIDB = do 
          a <- token $ RP.many pNameOrRes 
          skipSpaces
          skipMany pComment
          skipSpaces
          return $ merge a


merge :: [IDB] -> IDB
merge a = case a of
            ((name, com) : rest) -> let from_rest = merge (rest) in
                                    (name ++ (fst from_rest), com ++ (snd from_rest))
            [] -> ([],[])