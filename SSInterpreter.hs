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
eval :: LispVal -> StateTransformer LispVal
eval val@(String _) = return val
eval val@(Atom var) = stateLookup var 
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom "begin":[v])) = eval v
eval (List (Atom "begin": l: ls)) = eval l >> eval (List (Atom "begin": ls))
eval (List (Atom "begin":[])) = return (List [])
eval lam@(List (Atom "lambda":(List formals):body:[])) = return lam
eval ourLet@(List (Atom "let":(List bindings):body:[])) = ST (\s a -> 
																	let	(ST m) = let' bindings body
																		(result, newS, newA) = m s a
																	in (result,newS, a))
-- The following line is slightly more complex because we are addressing the
-- case where define is redefined by the user (whatever is the user's reason
-- for doing so. The problem is that redefining define does not have
-- the same semantics as redefining other functions, since define is not
-- stored as a regular function because of its return type.
eval (List (Atom "define": args)) = maybe (define args) (\v -> return v) (Map.lookup "define" state)
eval (List (Atom "set!": args)) = maybe (setVar args) (\v -> return v) (Map.lookup "set!" state)
eval (List (Atom "do": args)) = maybe (doFunc (List args)) (\v -> return v) (Map.lookup "do" state)
{-
eval (List (Atom "list-comp": var:(Atom v):result:condition:[])) = ST (\s a -> let (ST m1) = stateLookup v
                                                                                   (r1, s1, a1) = m1 s a
                                                                                   (ST m2) = eval (List (Atom "list-comp": var:r1:result:condition:[])) 
                                                                               in m2 s1 a1
                                                                            )
eval (List (Atom "list-comp": args)) = maybe (return (List (listComp args))) (\v -> return v) (Map.lookup "list-comp" state)
-}

eval (List (Atom "list-comp": var:listEvaluation:result:condition:[])) = listComp (var:listEvaluation:result:condition:[])

{-
eval (List (Atom "list-comp": var:listEvaluation:result:condition:[])) = ST (\s a -> let (ST m1) = eval listEvaluation
                                                                                         (r1, s1, a1) = m1 s a
                                                                                         r2 = listComp (var:r1:result:condition:[]) 
                                                                                     in  (List r2, s, a)
                                                                            )
eval (List (Atom "list-comp": args)) = maybe (return (List (listComp args))) (\v -> return v) (Map.lookup "list-comp" state)
-}


eval (List (Atom func : args)) = mapM eval args >>= apply func 
eval val@(List _) = return val
eval (Error s)  = return (Error s)
eval form = return (Error ("Could not eval the special form: " ++ (show form)))



stateLookup :: String -> StateTransformer LispVal
stateLookup var = ST $ 
  (\s a -> 
    (maybe (Error "variable does not exist.") id (Map.lookup var (union a s)), s, a)
	)
	



-- Because of monad complications, define is a separate function that is not
-- included in the state of the program. This saves  us from having to make
-- every predefined function return a StateTransformer, which would also
-- complicate state management. The same principle applies to set!. We are still
-- not talking about local definitions. That's a completely different
-- beast.
define :: [LispVal] -> StateTransformer LispVal
define [(Atom id), val] = defineGlobalVar id val
define [(List [Atom id]), val] = defineGlobalVar id val
define args = return (Error "wrong number of arguments. define")

defineGlobalVar :: String -> LispVal -> StateTransformer LispVal
defineGlobalVar id val = 
  ST (\s a -> let (ST f)    = eval val
                  (result, newState, newAmbient) = f s a
              in (result, (insert id result newState), newAmbient)
     )
	 
	 
defineLocalVar :: String -> LispVal -> StateTransformer LispVal
defineLocalVar id val = 
  ST (\s a -> let (ST f)    = eval val
                  (result, newState, newAmbient) = f s a
              in (result, newState, (insert id result newAmbient))
     )
	 
---------------------------------------------------
--LET
--nossoLet::StateT->StateT->[LispVal]-> StateTransformer LispVal
let' :: [LispVal] -> LispVal -> StateTransformer LispVal
let' ((List ((Atom id):val:[])):[]) body = defineLocalVar id val >> eval body
let' ((List ((Atom id):val:[])):xs) body = defineLocalVar id val >> let' xs body
let' _ body = return (Error "wrong number of the goddamn arguments. let")



---------------------------------------------------
--SET!

setVar :: [LispVal] -> StateTransformer LispVal
setVar [(Atom id), val] = setVarAux id val
setVar [(List [Atom id]) , val] = setVarAux id val
setVar args = return (Error "wrong number of arguments. setVar")


--Se a variável nao existir, ela é criada. TODO: Consertar isso.
setVarAux :: String -> LispVal -> StateTransformer LispVal
setVarAux id val = 
  ST (\s a -> let (ST f)    = eval val
                  (result, newState, newAmbient) = f s a
              in if ( id `member` newAmbient ) then (result, newState, (insert id result newAmbient)) else (result, (insert id result newState), newAmbient )
     )

---------------------------------------------------
--LIST-COMP
{-}
listComp :: [LispVal] -> [LispVal]
listComp ((Atom var):(List []):result:condition:[]) = []
listComp ((Atom var):(List (x:xs)):result:condition:[]) = if (test == (Bool True)) then (xResult:(listComp ((Atom var):(List xs):result:condition:[]))) else (listComp ((Atom var):(List xs):result:condition:[]))
                                                            where (ST m) = defineLocalVar var x >> eval condition
                                                                  (test, newS, newA) = m Map.empty Map.empty --pegar o resultado da condição
                                                                  (ST m2) = (ST m) >> eval result
                                                                  (xResult, newS2, newA2) = m2 Map.empty Map.empty --pegar o resultado da operação sobre o elemento que será inserido na lista
listComp _ = [Error "wrong number of arguments. listComp"]-}

listCompRec :: [LispVal] -> StateTransformer LispVal
listCompRec ((Atom var):(List (x:xs)):result:condition:[]) = setVar [Atom var,x] >> ST (\s a -> let (ST m1) = eval condition
                                                                                                    ((Bool test), newS1, newA1) = m1 s a -- Pegando o resultado da condição (test)
                                                                                                    (ST m2) = eval result 
                                                                                                    (r2, newS2, newA2) = m2 s a -- Calculamos a função aplicada ao elemento (result)
                                                                                                    (ST m3) = listCompRec ((Atom var):(List xs):result:condition:[]) -- Chamamos para o resto (mesmo sem saber se vai usar o resto ou nao)
                                                                                                    (List r3, newS3, newA3) = m3 newS2 newA2 -- Pegamos o resultado dessa chamada recursiva acima /\
                                                                                                    r4 = if (test) then (r2:r3) else r3 -- Se tiver passado no test, entra no resultado final, senao, fica apenas o resultado da chamada recursiva
                                                                                                in (List r4, s, a)
                                                                                              )
listCompRec ((Atom var):(List []):result:condition:[]) = ST (\s a->(List [],s,a))
listCompRec _ = return (Error "wrong number of arguments. listComp")

listComp :: [LispVal] -> StateTransformer LispVal
listComp args@((Atom var):list:result:condition:[]) = ST (\s a -> let (ST m1) = eval list
                                                                      (r1, newS1, newA1) = m1 s a -- Precisamos evaluar a lista porque ela pode não ser dada diretamente, e.g. pode ser uma funcao ou variável que retorna uma lista, etc.
                                                                      (ST m) = defineLocalVar var (Number 0) >> listCompRec ((Atom var):r1:result:condition:[]) --Definimos a variável local e processamos a listComp recursiva
                                                                      (r,newS,newA) = m s a
                                                                  in (r,s,a)
                                                          ) 
listComp _ = return (Error "wrong number of arguments. listComp")

---------------------------------------------------
--DO

doVar :: [LispVal] ->StateTransformer LispVal
doVar ((Atom var):initial:step:[]) = 
  ST (\s a -> let (ST m) = eval initial
                  (resultInit, newStateI, newAmbientI) = m s a --Avaliando o valor inicial da variavel
                  (ST m2) = eval step
                  (resultStep, newStateS, newAmbientS) = m2 s a --Avaliando o valor da variável aplicando a expressão do passo (step)
              in if (var `member` a) then (resultStep, newStateS, (insert var resultStep newAmbientS)) else (resultInit, newStateS, (insert var resultInit newAmbientI))
      )

doVarAux :: LispVal -> StateTransformer LispVal
doVarAux (List ((List initials):[])) = doVar initials
doVarAux (List ((List initials):ls)) = doVar initials >> doVarAux (List ls)
doVarAux _ = return (Error "wrong number of arguments. doVarAux")

doExpr :: [LispVal] -> StateTransformer LispVal
doExpr (expr:[]) = eval expr
doExpr (expr:exps) = eval expr >> doExpr exps

doFunc :: LispVal -> StateTransformer LispVal
doFunc (List ((List initials):(List (condition:exps)):[])) = doVarAux (List initials) >> 
                                                                            ST (\s a -> let (ST m) = eval condition
                                                                                            (resultCond, newS, newA) = m s a
                                                                                            (ST m2) = doExpr exps
                                                                                            (ST m3) = doFunc (List ((List initials):(List (condition:exps)):[]))
                                                                                        in if (resultCond == (Bool True)) then (m2 s a) else (m3 s a)
                                                                                )
doFunc (List ((List initials):(List (condition:exps)):command:[])) = doVarAux (List initials) >> 
                                                                            ST (\s a -> let (ST m) = eval condition
                                                                                            (resultCond, newS, newA) = m s a
                                                                                            (ST m2) = doExpr exps
                                                                                            (ST m3) = eval command >> doFunc (List ((List initials):(List (condition:exps)):command:[]))
                                                                                        in if (resultCond == (Bool True)) then (m2 s a) else (m3 s a)
                                                                                )
doFunc _ = return (Error "wrong number of arguments. doFunc")


-- The maybe function yields a value of type b if the evaluation of 
-- its third argument yields Nothing. In case it yields Just x, maybe
-- applies its second argument f to x and yields (f x) as its result.
-- maybe :: b -> (a -> b) -> Maybe a -> b
apply :: String -> [LispVal] -> StateTransformer LispVal
apply func args =  
                  case (Map.lookup func state) of
                      Just (Native f)  -> return (f args)
                      Just (NativeComp f)  -> return (f args)
                      otherwise -> 
                        (stateLookup func >>= \res -> 
                          case res of 
                            List (Atom "lambda" : List formals : body:l) -> lambda formals body args
                            otherwise -> return (Error "not a function.")
                        )
 
-- The lambda function is an auxiliary function responsible for
-- applying user-defined functions, instead of native ones. We use a very stupid 
-- kind of dynamic variable (parameter) scoping that does not even support
-- recursion. This has to be fixed in the project.
defineLambdaVar :: String -> LispVal -> StateTransformer LispVal
defineLambdaVar id val = 
  ST (\s a -> let (ST f)    = eval val
                  (result, newState, newAmbient) = f s a
              in (result, newState, newAmbient)
     )

lambdaVar :: [LispVal] -> [LispVal] -> StateTransformer LispVal
lambdaVar ((Atom var):[]) (arg:[]) = defineLocalVar var arg
lambdaVar ((Atom var):vars) (arg:args) = defineLocalVar var arg >> lambdaVar vars args
lambdaVar _ _ = return (Error "wrong number of arguments lol")


lambda :: [LispVal] -> LispVal -> [LispVal] -> StateTransformer LispVal
{-
lambda formals body args = ST (\s a ->
  let dynEnv = Prelude.foldr (\(Atom f, a) m -> Map.insert f a m) state (zip formals args)
      (ST m) = eval dynEnv body
      (result, newState, newAmb) = m dynEnv Map.empty
  in (result, s, a)
  )

lambda formals body args = lambdaVar formals args >> ST (\s a -> let (ST m) = eval body
                                                                           (result, newState, newAmbient) = m s a
                                                                       in (result, s, a)
                                                              )
-}
lambda formals body args = ST (\s a -> let (ST m) = lambdaVar formals args >> eval body
                                           (result, newS, newA) = m s a
                                        in (result, s, a))

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
          $ insert "concList"       (Native concList)
            empty

type StateT = Map String LispVal

-- StateTransformer is a data type that embodies computations
-- that transform the state of the interpreter (add new (String, LispVal)
-- pairs to the state variable). The ST constructor receives a function
-- because a StateTransformer gets the previous state of the interpreter 
-- and, based on that state, performs a computation that might yield a modified
-- state (a modification of the previous one). 
data StateTransformer t = ST (StateT -> StateT -> (t, StateT, StateT))

instance Monad StateTransformer where
  return x = ST (\s a -> (x, s, a))
  (>>=) (ST m) f = ST (\s a -> let (v, newS, newA) = m s a
                                   (ST resF) = f v
                               in  resF newS newA
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
predNumber ls = Error "wrong number of arguments. predNumber"

predBoolean :: [LispVal] -> LispVal
predBoolean (Bool _ : []) = Bool True
predBoolean (a:[]) = Bool False
predBoolean ls = Error "wrong number of arguments. predBoolean"

predList :: [LispVal] -> LispVal
predList (List _ : []) = Bool True
predList (a:[]) = Bool False
predList ls = Error "wrong number of arguments. predList"

numericSum :: [LispVal] -> LispVal
numericSum [] = Number 0
numericSum l = numericBinOp (+) l

numericMult :: [LispVal] -> LispVal
numericMult [] = Number 1
numericMult l = numericBinOp (*) l

numericSub :: [LispVal] -> LispVal
numericSub [] = Error "wrong number of arguments. numericSub"
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
integerDiv [] = Error "wrong number of arguments. integerDiv" 
integerDiv (Number n:[]) = Error "wrong number of arguments. integerDiv"
integerDiv (Number n:Number m:[]) = Number (div n m)
integerDiv (Number n:Number m:l) = Error "wrong number of arguments. integerDiv"

---------------------------------------------------
--MÓDULO

numericMod :: [LispVal] -> LispVal
numericMod [] = Error "wrong number of arguments. numericMod"
numericMod (Number n:[]) = Error "wrong number of arguments. numericMod"
numericMod (Number n:Number m:[]) = Number (mod n m)
numericMod (Number n:Number m:l) = Error "wrong number of arguments. numericMod"

-- We have not implemented division. Also, notice that we have not 
-- addressed floating-point numbers.

---------------------------------------------------
--EQV?
igualList :: LispVal -> LispVal -> Bool
igualList (List []) (List []) = True
igualList (List l) (List []) = False
igualList (List []) (List l) = False
igualList (List (n:ns)) (List (m:ms)) = (n == m) && (igualList (List ns) (List ms))

igualDottedList :: LispVal -> LispVal -> Bool
igualDottedList (DottedList [] n) (DottedList [] m) = n == m
igualDottedList (DottedList [] n) (DottedList k m) = False
igualDottedList (DottedList l n) (DottedList [] m) = False
igualDottedList (DottedList (l:ls) n) (DottedList (k:ks) m) = (l == k) && (igualDottedList (DottedList ls n) (DottedList ks m))

igual :: LispVal -> LispVal -> Bool
igual (Atom n) (Atom m) = n == m
igual (Number n) (Number m) = n == m
igual (Bool n) (Bool m) = n == m
igual (String n) (String m) = n == m
igual (List n) (List m) = igualList (List n) (List m)
igual (DottedList l n) (DottedList k m) = igualDottedList (DottedList l n) (DottedList k m)
igual n m = False

instance Eq LispVal where
   (==) n m = igual n m

equivalence :: [LispVal] -> LispVal
equivalence [] = Error "wrong number of arguments. equivalence"
equivalence (n:[]) = Error "wrong number of arguments. equivalence"
equivalence (n:m:[]) = Bool (n == m)
equivalence (n:m:l) = Error "wrong number of arguments. equivalence"

---------------------------------------------------
--MENOR QUE

lessThan :: [LispVal]  -> LispVal
lessThan ((Number a):(Number b):[]) = Bool ((<) a b)
lessThan ls = Error "wrong number of arguments. lessThan"

---------------------------------------------------
--MAIOR QUE

biggerThan :: [LispVal]  -> LispVal
biggerThan ((Number a):(Number b):[]) = Bool ((>) a b)
biggerThan ls = Error "wrong number of arguments. biggerThan"

---------------------------------------------------
--MENOR OU IGUAL

lessOrEqual :: [LispVal]  -> LispVal
lessOrEqual ((Number a):(Number b):[]) = Bool ((<=) a b)
lessOrEqual ls = Error "wrong number of arguments. lessOrEqual"

---------------------------------------------------
--MAIOR OU IGUAL
biggerOrEqual :: [LispVal]  -> LispVal
biggerOrEqual ((Number a):(Number b):[]) = Bool ((>=) a b)
biggerOrEqual ls = Error "wrong number of arguments. biggerOrEqual"

---------------------------------------------------
--IGUAL
equal :: [LispVal]  -> LispVal
equal ((Number a):(Number b):[]) = Bool ((==) a b)
equal ls = Error ( "wrong number of arguments. Equal args = " ++ show ls)

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
notOp ls = Error "wrong number of arguments. NotOP"

---------------------------------------------------
--IF THEN ELSE

ifThenElse :: [LispVal] -> LispVal
ifThenElse ((Bool predicate):body1:body2:_) = if predicate then body1 else body2
ifThenElse ((Bool predicate):body1:_) = if predicate then body1 else Error "Expression Unspecified"
ifThenElse l = Error ("wrong number of arguments. ifThenElse args = " ++ show(l))

---------------------------------------------------
--CONS

concatenation :: [LispVal] -> LispVal
concatenation (element: (List l):_) = List (element:l)
concatenation (element1:element2:_) = DottedList [element1] element2
concatenation l = Error "wrong number of arguments. concatenation"

---------------------------------------------------
--LENGTH

lengthList :: [LispVal] -> LispVal
lengthList ((List l):_) = Number (toInteger (length l))
lengthList ls = Error ("wrong number of arguments. length args = " ++ show ls)


---------------------------------------------------
--COMMENTS

clean :: LispVal -> LispVal
clean (List a) = (List (cleanAux a))
clean n = n

cleanAux :: [LispVal] -> [LispVal]
cleanAux [] = []
cleanAux ((Atom "comment"):(String c):ls) = cleanAux ls
cleanAux ((Atom f):args:ls) = ((Atom f):(clean args):(cleanAux ls))
cleanAux ((List args):ls) = ((clean (List args)):(cleanAux ls))
cleanAux (n:ls) = (n:(cleanAux ls))

---------------------------------------------------
--CONCATENACAO DE LISTA
concList :: [LispVal] -> LispVal
concList ((List list1):(List list2):[]) = (List (list1 ++ list2))
concList l = Error "wrong number of arguments. ConcList"

-----------------------------------------------------------
--                     main FUNCTION                     --
-----------------------------------------------------------

showResult :: (LispVal, StateT, StateT) -> String
showResult (val, defs, local) = show val ++ "\nGlobal: " ++ show (toList defs) ++ "\nLocal: " ++ show (toList local)

getResult :: StateTransformer LispVal -> (LispVal, StateT, StateT)
getResult (ST f) = f state Map.empty

trim::String->String
trim = Prelude.filter (\x->(not (x `elem` "\r\t\n")))

main :: IO ()
main = do args <- getArgs
          sourceCode <- (readFile (head args))
          putStr $ showResult $ getResult $ eval $ clean $ readExpr $ trim sourceCode
          

