-- APPLICATIVE PARSER FOR INFIX ARITHMETIC EXPRESSIONS WITHOUT ANY
-- DEPENDENCY ON HACKAGE. BUILDS AN EXPLICIT REPRESENTATION OF THE
-- SYNTAX TREE TO FOLD OVER USING CLIENT-SUPPLIED SEMANTICS.
MODULE PARSER (PARSEEXP) WHERE
IMPORT CONTROL.APPLICATIVE HIDING (CONST)
IMPORT CONTROL.ARROW
IMPORT DATA.CHAR
IMPORT DATA.MONOID
IMPORT DATA.LIST (FOLDL')

-- BUILDING BLOCK OF A COMPUTATION WITH SOME STATE OF TYPE @S@
-- THREADED THROUGH IT, POSSIBLY RESULTING IN A VALUE OF TYPE @R@
-- ALONG WITH SOME UPDATED STATE.
NEWTYPE STATE S R = STATE (S -> MAYBE (R, S))

-- EXPRESSIONS
DATA EXPR = CONST INTEGER
          | ADD EXPR EXPR
          | MUL EXPR EXPR
            DERIVING SHOW

INSTANCE FUNCTOR (STATE S) WHERE
    FMAP F (STATE G) = STATE $ FMAP (FIRST F) . G

INSTANCE APPLICATIVE (STATE S) WHERE
    PURE X = STATE $ \S -> JUST (X, S)
    STATE F <*> STATE G = STATE $ \S ->
                          CASE F S OF
                            NOTHING -> NOTHING
                            JUST (R, S') -> FMAP (FIRST R) . G $ S'

INSTANCE ALTERNATIVE (STATE S) WHERE
    EMPTY = STATE $ CONST NOTHING
    STATE F <|> STATE G = STATE $ \S -> MAYBE (G S) JUST (F S)

-- A PARSER THREADS SOME 'STRING' STATE THROUGH A COMPUTATION THAT
-- PRODUCES SOME VALUE OF TYPE @A@.
TYPE PARSER A = STATE STRING A

-- PARSE ONE NUMERICAL DIGIT.
DIGIT :: PARSER INTEGER
DIGIT = STATE $ PARSEDIGIT
    WHERE PARSEDIGIT [] = NOTHING
          PARSEDIGIT S@(C:CS)
              | ISDIGIT C = JUST (FROMINTEGRAL $ DIGITTOINT C, CS)
              | OTHERWISE = NOTHING

-- PARSE AN INTEGER. THE INTEGER MAY BE PREFIXED WITH A NEGATIVE SIGN.
NUM :: PARSER INTEGER
NUM = MAYBE ID (CONST NEGATE) <$> OPTIONAL (CHAR '-') <*> (TOINTEGER <$> SOME DIGIT)
    WHERE TOINTEGER = FOLDL' ((+) . (* 10)) 0

-- PARSE A SINGLE WHITE SPACE CHARACTER.
SPACE :: PARSER ()
SPACE = STATE $ PARSESPACE
    WHERE PARSESPACE [] = NOTHING
          PARSESPACE S@(C:CS)
              | ISSPACE C = JUST ((), CS)
              | OTHERWISE = NOTHING

-- CONSUME ZERO OR MORE WHITE SPACE CHARACTERS.
EATSPACE :: PARSER ()
EATSPACE = CONST () <$> MANY SPACE

-- PARSE A SPECIFIC CHARACTER.
CHAR :: CHAR -> PARSER CHAR
CHAR C = STATE PARSECHAR
    WHERE PARSECHAR [] = NOTHING
          PARSECHAR (X:XS) | X == C = JUST (C, XS)
                           | OTHERWISE = NOTHING

-- PARSE ONE OF OUR TWO SUPPORTED OPERATOR SYMBOLS.
OP :: PARSER (EXPR -> EXPR -> EXPR)
OP = CONST ADD <$> (CHAR '+') <|> CONST MUL <$> (CHAR '*')

-- SUCCEED ONLY IF THE END OF THE INPUT HAS BEEN REACHED.
EOF :: PARSER ()
EOF = STATE PARSEEOF
    WHERE PARSEEOF [] = JUST ((),[])
          PARSEEOF _  = NOTHING

-- PARSE AN INFIX ARITHMETIC EXPRESSION CONSISTING OF INTEGERS, PLUS
-- SIGNS, MULTIPLICATION SIGNS, AND PARENTHESES.
PARSEEXPR :: PARSER EXPR
PARSEEXPR = EATSPACE *>
            ((BUILDOP <$> NONOP <*> (EATSPACE *> OP) <*> PARSEEXPR) <|> NONOP)
    WHERE BUILDOP X OP Y = X `OP` Y
          NONOP = CHAR '(' *> PARSEEXPR <* CHAR ')' <|> CONST <$> NUM

-- RUN A PARSER OVER A 'STRING' RETURNING THE PARSED VALUE AND THE
-- REMAINING 'STRING' DATA.
EXECPARSER :: PARSER A -> STRING -> MAYBE (A, STRING)
EXECPARSER (STATE F) = F

-- RUN A PARSER OVER A 'STRING' RETURNING THE PARSED VALUE.
EVALPARSER :: PARSER A -> STRING -> MAYBE A
EVALPARSER = (FMAP FST .) . EXECPARSER

-- PARSE AN ARITHMETIC EXPRESSION USING THE SUPPLIED SEMANTICS FOR
-- INTEGRAL CONSTANTS, ADDITION, AND MULTIPLICATION.
PARSEEXP :: (INTEGER -> A) -> (A -> A -> A) -> (A -> A -> A) -> STRING -> MAYBE A
PARSEEXP CON ADD MUL = (CONVERT <$>) . EVALPARSER (PARSEEXPR <* EOF)
    WHERE CONVERT (CONST X) = CON X
          CONVERT (ADD X Y) = ADD (CONVERT X) (CONVERT Y)
          CONVERT (MUL X Y) = MUL (CONVERT X) (CONVERT Y)
