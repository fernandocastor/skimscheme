module SSParser(readExpr) where
import LispVal
import Text.ParserCombinators.Parsec hiding ( spaces )
import Control.Monad


-----------------------------------------------------------
--                         PARSER                        --
-----------------------------------------------------------

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

-- We have to lift the List constructor because it takes a [LispVal] 
-- and yields a LispVal. However, the result that endBy yield is in the
-- Parser monad. 
-- The tutorial originally recommends the use of sepBy, 
-- instead of endBy. Howecer, I coulnd't make the following example
-- work using setBy: 
-- ~/src/scheme $ ./simple_parser "(6  )"
-- The parser gets confused by the empty space between the '6' and 
-- the ')'. It assumes that another list element will appear but none 
-- does. On the other hand, using endBy rejects examples such as "(6)".
-- sepEndBy, which combines sepBy and endBy seems to solve the problem.
parseList :: Parser LispVal
parseList = liftM List $ sepEndBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = 
  do {
    exprs <- many (parseExpr >>= \p -> spaces >> return p);
    lastExpr <- (char '.' >> spaces >> parseExpr);
    return $ DottedList exprs lastExpr;
  }


-- Parses a String. It Looks for a double quote followed by a sequence of 
-- characters among which the backslash is not and then another double quote. 
parseString :: Parser LispVal 
parseString = 
  do { char '"';
       x <- many (noneOf "\"\\" <|> escaped);
       char '"';
       return $ String x
     }

escaped = char '\\' >> choice (zipWith escapeChar codes replacements)
escapeChar code replacement = char code >> return replacement
codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseAtom :: Parser LispVal
parseAtom = 
  do { first <- letter <|> symbol;
       rest <- many ( letter <|> digit <|> symbol);
       let atom = [first] ++ rest in
         return $ case atom of
                   "#t" -> Bool True
                   "true" -> Bool True
                   "#f" -> Bool False 
                   "false" -> Bool False 
                   otherwise -> Atom atom;
     }

parseQuoted :: Parser LispVal 
parseQuoted = 
  do { char '\'';
       x <- parseExpr;
       return $ List [Atom "quote", x];
     }
        
parseNumber :: Parser LispVal
parseNumber =
  do { digits <- (char '-' >> (many1 digit) >>= \num -> return ('-':num)) <|> (many1 digit);
       return $ (Number . read) digits;
     }
-- Alternate implementation for parseNumber. I used the
-- one above because I can understand it better. The
-- liftM function gets an (a -> b) function and makes it 
-- into an (m a -> m b) function, where m is of the Monad
-- class. The monad to which the function is lifted depends
-- on the monad on which liftM was executed (in this case, 
-- Parser LispVal). To use liftM, we had to import 
-- Control.Monad.
-- parseNumber : : Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal 
parseExpr = try parseNumber <|> 
            try parseAtom   <|> 
            try parseString <|>
            parseQuoted     <|>
            do { 
              char '(' >> spaces1;
              -- The try function below implements backtracking. 
              -- It is necessary because lists start with the same
              -- syntax all the way until a "." is found. 
              l <- (try parseDottedList) <|> parseList;
              spaces1 >> char ')';
              return l;
            }

spaces :: Parser ()
spaces = skipMany1 space

spaces1 :: Parser ()
spaces1 = skipMany space

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "Scheme" input of
                  Left err  -> Error (show err) 
                  Right val -> val

 