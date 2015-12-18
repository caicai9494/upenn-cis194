module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

--extend :: State -> String -> Int -> State
--extend st str i = if st str == i then st else empty
--
extend :: State -> String -> Int -> State
extend s x v y = if x == y then v else s y
--     f s i s 

empty :: State
empty _ = 0 

-- Exercise 2 -----------------------------------------
intToBool :: Int -> Bool
intToBool 0 = False
intToBool _ = True

boolToInt :: Bool -> Int
boolToInt True = 1 
boolToInt False = 0 

evalE :: State -> Expression -> Int
evalE s (Var str) = s str 
evalE s (Val i) = i
evalE s (Op elhs bop erhs) = 
    case bop of
        Plus -> vlhs + vrhs
        Minus -> vlhs - vrhs
        Times -> vlhs * vrhs
        Divide -> vlhs `div` vrhs
        Gt -> boolToInt $ vlhs > vrhs 
        Ge -> boolToInt $ vlhs >= vrhs 
        Lt -> boolToInt $  vlhs < vrhs 
        Le -> boolToInt $ vlhs <= vrhs 
        Eql -> boolToInt $ vlhs == vrhs 
        where vlhs, vrhs :: Int
              vlhs = evalE s elhs
              vrhs = evalE s erhs
                            
           


-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar Skip                            = DSkip
desugar (Assign s exp)                  = DAssign s exp 
desugar (If exp stm1 stm2)              = DIf exp (desugar stm1) (desugar stm2) 
desugar (While exp stm)                 = DWhile exp (desugar stm)
desugar (Sequence stm1 stm2)            = DSequence (desugar stm1) (desugar stm2)
--desugar (For astm ifexp incstm loopstm) = desugar (Sequence astm (While ifexp (Sequence loopstm incstm)))  
desugar (For astm ifexp incstm loopstm) = DSequence (desugar astm) (DWhile ifexp (DSequence (desugar loopstm) (desugar incstm)))  
desugar (Incr str)                      = DAssign str (Op (Var str) Plus (Val 1))


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s1 (DAssign str exp) = extend s1 str $ evalE s1 exp 
evalSimple s1 (DIf eif stm1 stm2) = if intToBool $ evalE s1 eif then evalSimple s1 stm1 else evalSimple s1 stm2 
evalSimple s1 dw @ (DWhile exp stm) = if intToBool $ evalE s1 exp then evalSimple (evalSimple s1 stm) dw else s1   
evalSimple s1 (DSequence stm1 stm2) = evalSimple (evalSimple s1 stm1) stm2
evalSimple s1 DSkip = s1

run :: State -> Statement -> State
run s1 stm = evalSimple s1 $ desugar stm  

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
