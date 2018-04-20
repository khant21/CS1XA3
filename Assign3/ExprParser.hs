module ExprParser (parseExprD, parseExprOtherD, parseExprF, parseExprOtherF) where

import ExprType
import Text.Parsec
import Text.Parsec.String


{- parseExprD
 - ---------------------------------
 - Takes a string of format:
 -     (number/variable binary-operator number/variable ...... )
 -     Ex. Ex. parseExprD "2+2" => Add (Const 2) (Const 2)
  -	   NOTE: Spaces are not accepted 
 - Parses the string into BINARY Expression data types (Add, Mult, Div, Exponent)
 - 		NOTE: ParseExprD cannot take unary operators
 -}
parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

--exprD separates terms (1, 2, +, etc) based on the main operations defined in setOp
exprD :: Parser (Expr Double)
exprD = termD `chainl1` setOp

termD :: Parser (Expr Double)
termD = (negOp factorD) <|> factorD

factorD :: Parser (Expr Double)
factorD = try numParseD <|> varParse


{- parseExprOtherD
 - ---------------------------------
 - Takes a string of format:
 -     (unary-operator number)
 -	   NOTE: Spaces are not accepted 
 - 	   NOTE: This function cannot take nested expressions
 -     Ex. parseExprOtherD "ln2"  =>  Ln (Const 2) 
 - Parses the string into UNARY Expression data types (Cos, Sin, Exp, Ln)
 - 		NOTE: ParseExprOtherD cannot take binary operators
 -}
parseExprOtherD :: String -> Expr Double
parseExprOtherD ss = case parse exprOtherD "" ss of
                Left err  -> error "This is invalid input for parsing variable strings"
                Right expr -> expr

exprOtherD :: Parser (Expr Double)
exprOtherD = let
             opRest = do {op <- setOpOther;
                                    spaces;
                                    term <- termOtherD;
                                    spaces;
                                    return (op term)} in try opRest <|> termOtherD

termOtherD :: Parser (Expr Double)
termOtherD = (negOp factorOtherD) <|> factorOtherD

factorOtherD :: Parser (Expr Double)
factorOtherD = try numParseD <|> varParse



{- parseExprF
 - ---------------------------------
 - Takes a string of format:
 -     (number/variable binary-operator number/variable ...... )
 -     Ex. Ex. parseExprD "2+2" => Add (Const 2.0) (Const 2.0)
  -	   NOTE: Spaces are not accepted 
 - Parses the string into BINARY Expression data types (Add, Mult, Div, Exponent)
 - this parser turns integer numbers into floating numbers when parsed 
 - 		NOTE: ParseExprF cannot take unary operators
 -}
parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

--exprD separates terms (1, 2, +, etc) based on the main operations defined in setOp
exprF :: Parser (Expr Float)
exprF = termF `chainl1` setOp

termF :: Parser (Expr Float)
termF = (negOp factorF) <|> factorF

factorF :: Parser (Expr Float)
factorF = try numParseF <|> varParse


{- parseExprOtherF
 - ---------------------------------
 - Takes a string of format:
 -     (unary-operator number)
 -	   NOTE: Spaces are not accepted 
 - 	   NOTE: This function cannot take nested expressions 
 -     Ex. parseExprOtherF "ln2"  =>  Ln (Const 2.0) 
 - Parses the string into UNARY Expression data types (Cos, Sin, Exp, Ln)
 - 		NOTE: ParseExprOtherF cannot take binary operators
 -}
parseExprOtherF :: String -> Expr Float
parseExprOtherF ss = case parse exprOtherF "" ss of
                Left err  -> error "This is invalid input for parsing variable strings"
                Right expr -> expr

exprOtherF :: Parser (Expr Float)
exprOtherF = let
             opRest = do {op <- setOpOther;
                                    spaces;
                                    term <- termOtherF;
                                    spaces;
                                    return (op term)} in try opRest <|> termOtherF

termOtherF :: Parser (Expr Float)
termOtherF = (negOp factorOtherF) <|> factorOtherF

factorOtherF :: Parser (Expr Float)
factorOtherF = try numParseF <|> varParse


-- setOp parses the binary operators
setOp :: Parser (Expr a -> Expr a-> Expr a)
setOp = (do { symbol "+"; return Add })
     <|> do { symbol "*"; return Mult } 
     <|> do { symbol "^"; return Exponent}
     <|> do { symbol "/"; return Div}

-- setOpOther parses the unary operators
setOpOther :: Parser (Expr a -> Expr a)
setOpOther = (do { string "e"; return Exp })
    <|> do { string "ln"; return Ln } 
    <|> do { string "cos"; return Cos}
    <|> do { string "sin"; return Sin}

-- negOp finds a negative symbol and wraps the term in the Neg Expression type
negOp :: Parser (Expr a) -> Parser (Expr a)
negOp p = do { symbol "-" ;
               exp <- p ;
               return $ Neg exp }

-- numParseD wraps a double with the Const Expression type
numParseD :: Parser (Expr Double)
numParseD = do {i <- double;
                return (Const i)}

-- numParseF wraps a float with the Const Expression type
numParseF :: Parser (Expr Float)
numParseF = do {i <- float;
                return (Const i)}

-- varParse wraps the Var Expression type around the variable parsed
varParse :: Parser (Expr a)
varParse = do { s <- many1 letter;
                return (Var s)}


-- UTILITY COMBINATORS:

-- parses symbols such as: ^,*,+, etc.
symbol :: String -> Parser String
symbol ss = let
    symbol' :: Parser String
    symbol' = do { ss' <- string ss;
                    return ss' } in try symbol'

-- parses doubles only
double :: Parser Double
double = fmap read $ doubleDigits

-- parses floats only
float:: Parser Float
float = fmap read $ doubleDigits

doubleDigits :: Parser String
doubleDigits = do { ds <- try negDigits <|> digits ;
                    rs <- try decimalDigits <|> return "" ;
                    return $ ds ++ rs }

decimalDigits :: Parser String
decimalDigits = do { d <- char '.' ;
                     rm <- digits ;
                     return $ d:rm }


{-This deals with parsing negative digits that begin with a "-" -}
negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                    dig <- digits ;
                    return (neg ++ dig) }

-- parses many instance of a certain digit (ex. 11111)
digits :: Parser String
digits = many1 digit