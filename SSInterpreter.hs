{-

A basic interpreter for a purely functional subset of Scheme named SkimScheme.
Part of this interpreter has been derived from the "Write Yourself a Scheme in
48 Hours - An Introduction to Haskell through Example", by Jonathan Tang. It
does not implement a number of Scheme's constructs. Moreover, it uses a
different approach to implement mutable state within the language.

The name "SkimScheme" refers to the stripped down nature of this interpreter.
According to the New Oxford American Dictionary, "skim" can mean:

(as a verb) ... read (something) quickly or cursorily so as to note only
the important points.

(as a noun) ... an act of reading something quickly or superficially. 

"skimmed/skim milk" is milk from which the cream has been removed. 

The name emphasizes that we do not want to cover the entire standard, small as
it may be. Instead, we want to focus on some of the important aspects, taking a
language implementer's point of view, with the goal of using it as a teaching
tool. Many, many, many aspects of Scheme standards are not covered (it does not
even support recursion!).

Written by Fernando Castor
Started at: August 28th 2012
Last update: December 17th 2012

-}

module Main where
import System.Environment
import Control.Monad
import Data.Map as Map
import LispVal
import SSParser
import SSPrettyPrinter

-----------------------------------------------------------
--                      INTERPRETER                      --
-----------------------------------------------------------
eval :: StateT -> LispVal -> StateTransformer LispVal
eval st val@(String _) = return val
eval st val@(Atom var) = stateLookup st var 
eval st val@(Number _) = return val
eval st val@(Bool _) = return val
eval st (List [Atom "quote", val]) = return val
eval st (List (Atom "begin":[v])) = eval st v
eval st (List (Atom "begin": l: ls)) = eval st l >> eval st (List (Atom "begin": ls))
eval st (List (Atom "begin":[])) = return (List [])
eval st lam@(List (Atom "lambda":(List formals):body:[])) = return lam
-- The following line is slightly more complex because we are addressing the
-- case where define is redefined by the user (whatever is the user's reason
-- for doing so. The problem is that redefining define does not have
-- the same semantics as redefining other functions, since define is not
-- stored as a regular function because of its return type.
eval st (List (Atom "define": args)) = maybe (define st args) (\v -> return v) (Map.lookup "define" state)
eval st (List (Atom "set!": args)) = maybe (setVar st args) (\v -> return v) (Map.lookup "set!" state)
eval st (List (Atom func : args)) = mapM (eval st) args >>= apply st func 
eval st (Error s)  = return (Error s)
eval st form = return (Error ("Could not eval the special form: " ++ (show form)))

stateLookup :: StateT -> String -> StateTransformer LispVal
stateLookup st var = ST $ 
  (\s -> 
    (maybe (Error "variable does not exist.") 
           id (Map.lookup var (union s st) 
    ), s))


-- Because of monad complications, define is a separate function that is not
-- included in the state of the program. This saves  us from having to make
-- every predefined function return a StateTransformer, which would also
-- complicate state management. The same principle applies to set!. We are still
-- not talking about local definitions. That's a completely different
-- beast.
define :: StateT -> [LispVal] -> StateTransformer LispVal
define st [(Atom id), val] = defineVar st id val
define st [(List [Atom id]), val] = defineVar st id val
define st args = return (Error "wrong number of arguments")

defineVar :: StateT -> String -> LispVal -> StateTransformer LispVal
defineVar env id val = 
  ST (\s -> let (ST f)    = eval env val
                (result, newState) = f s
            in (result, (insert id result newState))
     )

---------------------------------------------------
--SET!

setVar :: StateT -> [LispVal] -> StateTransformer LispVal
setVar st [(Atom id), val] = setVarAux st id val
setVar st [(List [Atom id]), val] = setVarAux st id val
setVar st args = return (Error "wrong number of arguments")

setVarAux :: StateT -> String -> LispVal -> StateTransformer LispVal
setVarAux env id val = 
  ST (\s -> let (ST f)    = eval env val
                (result, newState) = f s
            in (result, (insert id result newState))
     )


-- The maybe function yields a value of type b if the evaluation of 
-- its third argument yields Nothing. In case it yields Just x, maybe
-- applies its second argument f to x and yields (f x) as its result.
-- maybe :: b -> (a -> b) -> Maybe a -> b
apply :: StateT -> String -> [LispVal] -> StateTransformer LispVal
apply st func args =  
                  case (Map.lookup func state) of
                      Just (Native f)  -> return (f args)
                      Just (NativeComp f)  -> return (f args)
                      otherwise -> 
                        (stateLookup st func >>= \res -> 
                          case res of 
                            List (Atom "lambda" : List formals : body:l) -> lambda st formals body args 
                            otherwise -> return (Error "not a function.")
                        )
 
-- The lambda function is an auxiliary function responsible for
-- applying user-defined functions, instead of native ones. We use a very stupid 
-- kind of dynamic variable (parameter) scoping that does not even support
-- recursion. This has to be fixed in the project.
lambda :: StateT -> [LispVal] -> LispVal -> [LispVal] -> StateTransformer LispVal
lambda st formals body args = 
  let dynEnv = Prelude.foldr (\(Atom f, a) m -> Map.insert f a m) st (zip formals args)
  in  eval dynEnv body


-- Initial state of the programs. Maps identifiers to vaues. 
-- Initially, maps function names to function values, but there's 
-- nothing stopping it from storing general values (e.g., well-known
-- constants, such as pi). The initial state includes all the functions 
-- that are available for programmers.
state :: Map String LispVal
state =   
            insert "number?"        (Native predNumber)
          $ insert "boolean?"       (Native predBoolean)
          $ insert "list?"          (Native predList)
          $ insert "eqv?"           (Native equivalence)
          $ insert "+"              (Native numericSum) 
          $ insert "*"              (Native numericMult) 
          $ insert "-"              (Native numericSub)
          $ insert "/"              (Native integerDiv)
          $ insert "mod"            (Native numericMod)
          $ insert "car"            (Native car)           
          $ insert "cdr"            (Native cdr)
          $ insert "<"              (NativeComp lessThan)
          $ insert ">"              (NativeComp biggerThan)
          $ insert "<="             (NativeComp lessOrEqual)
          $ insert ">="             (NativeComp biggerOrEqual)
          $ insert "="              (NativeComp equal)
          $ insert "and"            (NativeComp andOp)
          $ insert "or"             (NativeComp orOp)
          $ insert "not"            (NativeComp notOp)
          $ insert "if"             (Native ifThenElse)
          $ insert "cons"           (Native concatenation)
          $ insert "length"         (Native lengthList)
            empty

type StateT = Map String LispVal

-- StateTransformer is a data type that embodies computations
-- that transform the state of the interpreter (add new (String, LispVal)
-- pairs to the state variable). The ST constructor receives a function
-- because a StateTransformer gets the previous state of the interpreter 
-- and, based on that state, performs a computation that might yield a modified
-- state (a modification of the previous one). 
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
  return x = ST (\s -> (x, s))
  (>>=) (ST m) f = ST (\s -> let (v, newS) = m s
                                 (ST resF) = f v
                             in  resF newS
                      )
    
-----------------------------------------------------------
--          HARDWIRED PREDEFINED LISP FUNCTIONS          --
-----------------------------------------------------------

-- Includes some auxiliary functions. Does not include functions that modify
-- state. These functions, such as define and set!, must run within the
-- StateTransformer monad.

car :: [LispVal] -> LispVal
car [List (a:as)] = a
car [DottedList (a:as) _] = a
car ls = Error "invalid list."

cdr :: [LispVal] -> LispVal
cdr (List (a:as) : ls) = List as
cdr (DottedList (a:[]) c : ls) = c
cdr (DottedList (a:as) c : ls) = DottedList as c
cdr ls = Error "invalid list."

predNumber :: [LispVal] -> LispVal
predNumber (Number _ : []) = Bool True
predNumber (a:[]) = Bool False
predNumber ls = Error "wrong number of arguments."

predBoolean :: [LispVal] -> LispVal
predBoolean (Bool _ : []) = Bool True
predBoolean (a:[]) = Bool False
predBoolean ls = Error "wrong number of arguments."

predList :: [LispVal] -> LispVal
predList (List _ : []) = Bool True
predList (a:[]) = Bool False
predList ls = Error "wrong number of arguments."

numericSum :: [LispVal] -> LispVal
numericSum [] = Number 0
numericSum l = numericBinOp (+) l

numericMult :: [LispVal] -> LispVal
numericMult [] = Number 1
numericMult l = numericBinOp (*) l

numericSub :: [LispVal] -> LispVal
numericSub [] = Error "wrong number of arguments."
numericSub [x] = if onlyNumbers [x]
                 then (\num -> (Number (- num))) (unpackNum x)
                 else Error "not a number."
numericSub l = numericBinOp (-) l

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op args = if onlyNumbers args 
                       then Number $ foldl1 op $ Prelude.map unpackNum args 
                       else Error "not a number."
                       
onlyNumbers :: [LispVal] -> Bool
onlyNumbers [] = True
onlyNumbers (Number n:ns) = onlyNumbers ns
onlyNumbers ns = False   

onlyBools :: [LispVal] -> Bool
onlyBools [] = True
onlyBools (Bool n:ns) = onlyBools ns
onlyBools ns = False           
                       
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
--- unpackNum a = ... -- Should never happen!!!!

unpackBool :: LispVal -> Bool
unpackBool (Bool n) = n

---------------------------------------------------
--DIVISÃO INTEIRA

integerDiv :: [LispVal] -> LispVal
integerDiv [] = Error "wrong number of arguments." 
integerDiv (Number n:[]) = Error "wrong number of arguments."
integerDiv (Number n:Number m:[]) = Number (div n m)
integerDiv (Number n:Number m:l) = Error "wrong number of arguments."

---------------------------------------------------
--MÓDULO

numericMod :: [LispVal] -> LispVal
numericMod [] = Error "wrong number of arguments."
numericMod (Number n:[]) = Error "wrong number of arguments."
numericMod (Number n:Number m:[]) = Number (mod n m)
numericMod (Number n:Number m:l) = Error "wrong number of arguments."

-- We have not implemented division. Also, notice that we have not 
-- addressed floating-point numbers.

---------------------------------------------------
--EQV?

igual :: LispVal -> LispVal -> Bool
igual (Number n) (Number m) = n == m
igual (Bool n) (Bool m) = n == m
igual (String n) (String m) = n == m
igual (List n) (List m) = n == m

instance Eq LispVal where
   (==) n m = igual n m

equivalence :: [LispVal] -> LispVal
equivalence [] = Error "wrong number of arguments."
equivalence (n:[]) = Error "wrong number of arguments."
equivalence (n:m:[]) = Bool (n == m)
equivalence (n:m:l) = Error "wrong number of arguments."

---------------------------------------------------
--MENOR QUE

lessThan :: [LispVal]  -> LispVal
lessThan ((Number a):(Number b):[]) = Bool ((<) a b)
lessThan ls = Error "wrong number of arguments."

---------------------------------------------------
--MAIOR QUE

biggerThan :: [LispVal]  -> LispVal
biggerThan ((Number a):(Number b):[]) = Bool ((>) a b)
biggerThan ls = Error "wrong number of arguments."

---------------------------------------------------
--MENOR OU IGUAL

lessOrEqual :: [LispVal]  -> LispVal
lessOrEqual ((Number a):(Number b):[]) = Bool ((<=) a b)
lessOrEqual ls = Error "wrong number of arguments."

---------------------------------------------------
--MAIOR OU IGUAL
biggerOrEqual :: [LispVal]  -> LispVal
biggerOrEqual ((Number a):(Number b):[]) = Bool ((>=) a b)
biggerOrEqual ls = Error "wrong number of arguments."

---------------------------------------------------
--IGUAL
equal :: [LispVal]  -> LispVal
equal ((Number a):(Number b):[]) = Bool ((==) a b)
equal ls = Error "wrong number of arguments."

---------------------------------------------------
--AND

andOp :: [LispVal] -> LispVal
andOp list = if onlyBools list
             then Bool (and (Prelude.map unpackBool list))
             else Error "not a boolean."

---------------------------------------------------
--OR

orOp :: [LispVal] -> LispVal
orOp list = if onlyBools list
             then Bool (or (Prelude.map unpackBool list))
             else Error "not a boolean."

---------------------------------------------------
--NOT

notOp :: [LispVal] -> LispVal
notOp ((Bool a):[]) = Bool (not a)
notOp ls = Error "wrong number of arguments."

---------------------------------------------------
--IF THEN ELSE

ifThenElse :: [LispVal] -> LispVal
ifThenElse ((Bool predicate):body1:body2:_) = if predicate then body1 else body2
ifThenElse l = Error "wrong number of arguments."

---------------------------------------------------
--CONS

concatenation :: [LispVal] -> LispVal
concatenation (element: (List l):_) = List (element:l)
concatenation l = Error "wrong number of arguments."

---------------------------------------------------
--LENGTH

lengthList :: [LispVal] -> LispVal
lengthList ((List l):_) = Number (toInteger (length l))
lengthList ls = Error "wrong number of arguments."

-----------------------------------------------------------
--                     main FUNCTION                     --
-----------------------------------------------------------

showResult :: (LispVal, StateT) -> String
showResult (val, defs) = show val ++ "\n" ++ show (toList defs)

getResult :: StateTransformer LispVal -> (LispVal, StateT)
getResult (ST f) = f state

---------------------------------------------------
--COMMENTS

clean :: LispVal -> LispVal
clean (List a) = (List (cleanAux a))
clean n = n

cleanAux :: [LispVal] -> [LispVal]
cleanAux [] = []
cleanAux ((Atom f):args:ls) | f == "--" = (cleanAux ls)
                            | otherwise = ((Atom f):(clean args):(cleanAux ls))
cleanAux ((List args):ls) = ((clean (List args)):(cleanAux ls))
cleanAux (n:ls) = (n:(cleanAux ls))

trim::String->String
trim = Prelude.filter (\x->(not (x `elem` "\r\t\n")))

main :: IO ()
main = do args <- getArgs
          sourceCode <- (readFile (head args))
          putStr $ showResult $ getResult $ eval state $ clean $ readExpr $ trim sourceCode
          

