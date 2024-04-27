{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Turtle.Frontend.Parser where

import Language.Turtle.Frontend.Lexer (Alex(..), AlexState(..), Token(..), alexGetUserState, runAlex, lexwrap)
import Language.Turtle.Frontend.Range (Ranged(..), ranges)
import Language.Turtle.Frontend.ParsedAST 
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (sconcat)
import qualified Data.List.NonEmpty as NE
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Ranged Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,81) ([12288,42,41728,2,0,0,16384,0,0,1024,0,0,0,0,12288,10,41728,0,2608,0,0,32768,0,0,0,256,0,256,0,4,41728,0,10800,0,0,0,0,0,0,0,0,163,12288,298,0,0,16384,0,0,12288,42,0,0,0,2,16384,32768,0,41728,18,10800,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_program","Program","Block","BlockOrSingleStatement","Statements","StatementsNonEmpty","Statement","Expression","Identifier","comma_separated__Expression__","id","num","'='","':'","','","'('","')'","'['","']'","if","else","eof","indent","unindent","newline","%eof"]
        bit_start = st Prelude.* 28
        bit_end = (st Prelude.+ 1) Prelude.* 28
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..27]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (13) = happyShift action_6
action_0 (14) = happyShift action_7
action_0 (18) = happyShift action_8
action_0 (20) = happyShift action_9
action_0 (22) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (7) = happyGoto action_2
action_0 (9) = happyGoto action_3
action_0 (10) = happyGoto action_4
action_0 (11) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (13) = happyShift action_6
action_1 (14) = happyShift action_7
action_1 (18) = happyShift action_8
action_1 (20) = happyShift action_9
action_1 (22) = happyShift action_10
action_1 (7) = happyGoto action_2
action_1 (9) = happyGoto action_3
action_1 (10) = happyGoto action_4
action_1 (11) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (27) = happyShift action_18
action_3 _ = happyReduce_5

action_4 _ = happyReduce_11

action_5 (15) = happyShift action_17
action_5 _ = happyReduce_13

action_6 _ = happyReduce_16

action_7 _ = happyReduce_12

action_8 (13) = happyShift action_6
action_8 (14) = happyShift action_7
action_8 (18) = happyShift action_8
action_8 (20) = happyShift action_9
action_8 (10) = happyGoto action_16
action_8 (11) = happyGoto action_13
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (13) = happyShift action_6
action_9 (14) = happyShift action_7
action_9 (18) = happyShift action_8
action_9 (20) = happyShift action_9
action_9 (10) = happyGoto action_14
action_9 (11) = happyGoto action_13
action_9 (12) = happyGoto action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (13) = happyShift action_6
action_10 (14) = happyShift action_7
action_10 (18) = happyShift action_8
action_10 (20) = happyShift action_9
action_10 (10) = happyGoto action_12
action_10 (11) = happyGoto action_13
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (28) = happyAccept
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (16) = happyShift action_24
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_13

action_14 (17) = happyShift action_23
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (21) = happyShift action_22
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (19) = happyShift action_21
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (13) = happyShift action_6
action_17 (14) = happyShift action_7
action_17 (18) = happyShift action_8
action_17 (20) = happyShift action_9
action_17 (10) = happyGoto action_20
action_17 (11) = happyGoto action_13
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (13) = happyShift action_6
action_18 (14) = happyShift action_7
action_18 (18) = happyShift action_8
action_18 (20) = happyShift action_9
action_18 (22) = happyShift action_10
action_18 (7) = happyGoto action_19
action_18 (9) = happyGoto action_3
action_18 (10) = happyGoto action_4
action_18 (11) = happyGoto action_5
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_6

action_20 _ = happyReduce_9

action_21 _ = happyReduce_14

action_22 _ = happyReduce_15

action_23 (13) = happyShift action_6
action_23 (14) = happyShift action_7
action_23 (18) = happyShift action_8
action_23 (20) = happyShift action_9
action_23 (10) = happyGoto action_14
action_23 (11) = happyGoto action_13
action_23 (12) = happyGoto action_29
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (13) = happyShift action_6
action_24 (14) = happyShift action_7
action_24 (18) = happyShift action_8
action_24 (20) = happyShift action_9
action_24 (22) = happyShift action_10
action_24 (25) = happyShift action_28
action_24 (5) = happyGoto action_25
action_24 (6) = happyGoto action_26
action_24 (9) = happyGoto action_27
action_24 (10) = happyGoto action_4
action_24 (11) = happyGoto action_5
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_3

action_26 (23) = happyShift action_32
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_4

action_28 (13) = happyShift action_6
action_28 (14) = happyShift action_7
action_28 (18) = happyShift action_8
action_28 (20) = happyShift action_9
action_28 (22) = happyShift action_10
action_28 (8) = happyGoto action_30
action_28 (9) = happyGoto action_31
action_28 (10) = happyGoto action_4
action_28 (11) = happyGoto action_5
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_17

action_30 (26) = happyShift action_35
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (27) = happyShift action_34
action_31 _ = happyReduce_7

action_32 (16) = happyShift action_33
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (13) = happyShift action_6
action_33 (14) = happyShift action_7
action_33 (18) = happyShift action_8
action_33 (20) = happyShift action_9
action_33 (22) = happyShift action_10
action_33 (25) = happyShift action_28
action_33 (5) = happyGoto action_25
action_33 (6) = happyGoto action_37
action_33 (9) = happyGoto action_27
action_33 (10) = happyGoto action_4
action_33 (11) = happyGoto action_5
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (13) = happyShift action_6
action_34 (14) = happyShift action_7
action_34 (18) = happyShift action_8
action_34 (20) = happyShift action_9
action_34 (22) = happyShift action_10
action_34 (8) = happyGoto action_36
action_34 (9) = happyGoto action_31
action_34 (10) = happyGoto action_4
action_34 (11) = happyGoto action_5
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_2

action_36 _ = happyReduce_8

action_37 _ = happyReduce_10

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 :: [Ranged (Statement Ranged)]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 :: NonEmpty (Ranged (Statement Ranged))
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 :| []
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 ([ happy_var_1 ]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 :| []
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 `NE.cons` happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn9
		 (Ranged (Assignment happy_var_1 happy_var_3) (happy_var_1.range <> happy_var_3.range)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 7 9 happyReduction_10
happyReduction_10 ((HappyAbsSyn6  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Ranged (If happy_var_2 happy_var_4 happy_var_7) (happy_var_2.range <> ranges happy_var_4 <> ranges happy_var_7)
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Ranged (StatementExpression happy_var_1) happy_var_1.range
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (let TNumber num = happy_var_1.value in Ranged (ELiteral (NumLit num)) happy_var_1.range
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (EIdentifier `fmap` happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  10 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn12  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (Ranged (EList (fmap value happy_var_2)) (sconcat (happy_var_1.range :| fmap (.range) happy_var_2))
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (let Identifier val = happy_var_1.value in Ranged (Ident val) happy_var_1.range
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Ranged { value = EOF } -> action 28 28 tk (HappyState action) sts stk;
	Ranged { value = Identifier _ } -> cont 13;
	Ranged { value = TNumber num } -> cont 14;
	Ranged { value = TAssign } -> cont 15;
	Ranged { value = TColon } -> cont 16;
	Ranged { value = TComma } -> cont 17;
	Ranged { value = TParenL } -> cont 18;
	Ranged { value = TParenR } -> cont 19;
	Ranged { value = TBracketL } -> cont 20;
	Ranged { value = TBracketR } -> cont 21;
	Ranged { value = TIf } -> cont 22;
	Ranged { value = TElse } -> cont 23;
	Ranged { value = EOF } -> cont 24;
	Ranged { value = TIndent _ } -> cont 25;
	Ranged { value = TUnindent } -> cont 26;
	Ranged { value = TNewline } -> cont 27;
	_ -> happyError' (tk, [])
	})

happyError_ explist 28 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Alex a
happyReturn = (Prelude.return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Ranged Token), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, explist) -> happyError) tk
program = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: Alex a
happyError = Alex $ \alexState -> do 
  Left $ "Unspecified parser error at (char, line, col) "  ++ show alexState.alex_pos
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
