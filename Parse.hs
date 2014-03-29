{-# OPTIONS_GHC -w #-}
module Parse where

import Lexer
import Debug.Trace
import Types
import Keiko (Op(..))

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 (Stmt)
	| HappyAbsSyn6 ([Stmt])
	| HappyAbsSyn8 ([([Int], Stmt)])
	| HappyAbsSyn9 (([Int], Stmt))
	| HappyAbsSyn10 ([Int])
	| HappyAbsSyn12 (Expr)
	| HappyAbsSyn16 (Name)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84 :: () => Int -> ({-HappyReduction (Alex) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40 :: () => ({-HappyReduction (Alex) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

action_0 (36) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (36) = happyShift action_2
action_1 _ = happyFail

action_2 (17) = happyShift action_8
action_2 (34) = happyShift action_9
action_2 (40) = happyShift action_10
action_2 (43) = happyShift action_11
action_2 (44) = happyShift action_12
action_2 (45) = happyShift action_13
action_2 (46) = happyShift action_14
action_2 (48) = happyShift action_15
action_2 (49) = happyShift action_16
action_2 (5) = happyGoto action_4
action_2 (6) = happyGoto action_5
action_2 (7) = happyGoto action_6
action_2 (16) = happyGoto action_7
action_2 _ = happyReduce_5

action_3 (50) = happyAccept
action_3 _ = happyFail

action_4 (39) = happyShift action_33
action_4 _ = happyFail

action_5 _ = happyReduce_2

action_6 (23) = happyShift action_32
action_6 _ = happyReduce_3

action_7 (31) = happyShift action_31
action_7 _ = happyFail

action_8 _ = happyReduce_40

action_9 (17) = happyShift action_8
action_9 (18) = happyShift action_24
action_9 (22) = happyShift action_25
action_9 (26) = happyShift action_26
action_9 (29) = happyShift action_27
action_9 (12) = happyGoto action_30
action_9 (13) = happyGoto action_20
action_9 (14) = happyGoto action_21
action_9 (15) = happyGoto action_22
action_9 (16) = happyGoto action_23
action_9 _ = happyFail

action_10 (17) = happyShift action_8
action_10 (18) = happyShift action_24
action_10 (22) = happyShift action_25
action_10 (26) = happyShift action_26
action_10 (29) = happyShift action_27
action_10 (12) = happyGoto action_29
action_10 (13) = happyGoto action_20
action_10 (14) = happyGoto action_21
action_10 (15) = happyGoto action_22
action_10 (16) = happyGoto action_23
action_10 _ = happyFail

action_11 (17) = happyShift action_8
action_11 (18) = happyShift action_24
action_11 (22) = happyShift action_25
action_11 (26) = happyShift action_26
action_11 (29) = happyShift action_27
action_11 (12) = happyGoto action_28
action_11 (13) = happyGoto action_20
action_11 (14) = happyGoto action_21
action_11 (15) = happyGoto action_22
action_11 (16) = happyGoto action_23
action_11 _ = happyFail

action_12 (17) = happyShift action_8
action_12 (18) = happyShift action_24
action_12 (22) = happyShift action_25
action_12 (26) = happyShift action_26
action_12 (29) = happyShift action_27
action_12 (12) = happyGoto action_19
action_12 (13) = happyGoto action_20
action_12 (14) = happyGoto action_21
action_12 (15) = happyGoto action_22
action_12 (16) = happyGoto action_23
action_12 _ = happyFail

action_13 _ = happyReduce_8

action_14 (17) = happyShift action_8
action_14 (34) = happyShift action_9
action_14 (40) = happyShift action_10
action_14 (43) = happyShift action_11
action_14 (44) = happyShift action_12
action_14 (45) = happyShift action_13
action_14 (46) = happyShift action_14
action_14 (48) = happyShift action_15
action_14 (49) = happyShift action_16
action_14 (5) = happyGoto action_18
action_14 (6) = happyGoto action_5
action_14 (7) = happyGoto action_6
action_14 (16) = happyGoto action_7
action_14 _ = happyReduce_5

action_15 (17) = happyShift action_8
action_15 (34) = happyShift action_9
action_15 (40) = happyShift action_10
action_15 (43) = happyShift action_11
action_15 (44) = happyShift action_12
action_15 (45) = happyShift action_13
action_15 (46) = happyShift action_14
action_15 (48) = happyShift action_15
action_15 (49) = happyShift action_16
action_15 (5) = happyGoto action_17
action_15 (6) = happyGoto action_5
action_15 (7) = happyGoto action_6
action_15 (16) = happyGoto action_7
action_15 _ = happyReduce_5

action_16 _ = happyReduce_15

action_17 (39) = happyShift action_48
action_17 _ = happyFail

action_18 (47) = happyShift action_47
action_18 _ = happyFail

action_19 (21) = happyShift action_37
action_19 _ = happyReduce_7

action_20 (20) = happyShift action_45
action_20 (29) = happyShift action_46
action_20 _ = happyReduce_28

action_21 (19) = happyShift action_44
action_21 _ = happyReduce_30

action_22 _ = happyReduce_33

action_23 _ = happyReduce_35

action_24 (17) = happyShift action_8
action_24 (18) = happyShift action_24
action_24 (22) = happyShift action_25
action_24 (26) = happyShift action_26
action_24 (29) = happyShift action_27
action_24 (15) = happyGoto action_43
action_24 (16) = happyGoto action_23
action_24 _ = happyFail

action_25 _ = happyReduce_36

action_26 (17) = happyShift action_8
action_26 (18) = happyShift action_24
action_26 (22) = happyShift action_25
action_26 (26) = happyShift action_26
action_26 (29) = happyShift action_27
action_26 (12) = happyGoto action_42
action_26 (13) = happyGoto action_20
action_26 (14) = happyGoto action_21
action_26 (15) = happyGoto action_22
action_26 (16) = happyGoto action_23
action_26 _ = happyFail

action_27 (17) = happyShift action_8
action_27 (18) = happyShift action_24
action_27 (22) = happyShift action_25
action_27 (26) = happyShift action_26
action_27 (29) = happyShift action_27
action_27 (15) = happyGoto action_41
action_27 (16) = happyGoto action_23
action_27 _ = happyFail

action_28 (21) = happyShift action_37
action_28 (37) = happyShift action_40
action_28 _ = happyFail

action_29 (21) = happyShift action_37
action_29 (42) = happyShift action_39
action_29 _ = happyFail

action_30 (21) = happyShift action_37
action_30 (35) = happyShift action_38
action_30 _ = happyFail

action_31 (17) = happyShift action_8
action_31 (18) = happyShift action_24
action_31 (22) = happyShift action_25
action_31 (26) = happyShift action_26
action_31 (29) = happyShift action_27
action_31 (12) = happyGoto action_36
action_31 (13) = happyGoto action_20
action_31 (14) = happyGoto action_21
action_31 (15) = happyGoto action_22
action_31 (16) = happyGoto action_23
action_31 _ = happyFail

action_32 (17) = happyShift action_8
action_32 (34) = happyShift action_9
action_32 (40) = happyShift action_10
action_32 (43) = happyShift action_11
action_32 (44) = happyShift action_12
action_32 (45) = happyShift action_13
action_32 (46) = happyShift action_14
action_32 (48) = happyShift action_15
action_32 (49) = happyShift action_16
action_32 (6) = happyGoto action_35
action_32 (7) = happyGoto action_6
action_32 (16) = happyGoto action_7
action_32 _ = happyReduce_5

action_33 (24) = happyShift action_34
action_33 _ = happyFail

action_34 _ = happyReduce_1

action_35 _ = happyReduce_4

action_36 (21) = happyShift action_37
action_36 _ = happyReduce_6

action_37 (17) = happyShift action_8
action_37 (18) = happyShift action_24
action_37 (22) = happyShift action_25
action_37 (26) = happyShift action_26
action_37 (29) = happyShift action_27
action_37 (13) = happyGoto action_60
action_37 (14) = happyGoto action_21
action_37 (15) = happyGoto action_22
action_37 (16) = happyGoto action_23
action_37 _ = happyFail

action_38 (22) = happyShift action_59
action_38 (38) = happyReduce_18
action_38 (39) = happyReduce_18
action_38 (8) = happyGoto action_56
action_38 (9) = happyGoto action_57
action_38 (10) = happyGoto action_58
action_38 _ = happyReduce_22

action_39 (17) = happyShift action_8
action_39 (34) = happyShift action_9
action_39 (40) = happyShift action_10
action_39 (43) = happyShift action_11
action_39 (44) = happyShift action_12
action_39 (45) = happyShift action_13
action_39 (46) = happyShift action_14
action_39 (48) = happyShift action_15
action_39 (49) = happyShift action_16
action_39 (5) = happyGoto action_55
action_39 (6) = happyGoto action_5
action_39 (7) = happyGoto action_6
action_39 (16) = happyGoto action_7
action_39 _ = happyReduce_5

action_40 (17) = happyShift action_8
action_40 (34) = happyShift action_9
action_40 (40) = happyShift action_10
action_40 (43) = happyShift action_11
action_40 (44) = happyShift action_12
action_40 (45) = happyShift action_13
action_40 (46) = happyShift action_14
action_40 (48) = happyShift action_15
action_40 (49) = happyShift action_16
action_40 (5) = happyGoto action_54
action_40 (6) = happyGoto action_5
action_40 (7) = happyGoto action_6
action_40 (16) = happyGoto action_7
action_40 _ = happyReduce_5

action_41 _ = happyReduce_38

action_42 (21) = happyShift action_37
action_42 (27) = happyShift action_53
action_42 _ = happyFail

action_43 _ = happyReduce_37

action_44 (17) = happyShift action_8
action_44 (18) = happyShift action_24
action_44 (22) = happyShift action_25
action_44 (26) = happyShift action_26
action_44 (29) = happyShift action_27
action_44 (15) = happyGoto action_52
action_44 (16) = happyGoto action_23
action_44 _ = happyFail

action_45 (17) = happyShift action_8
action_45 (18) = happyShift action_24
action_45 (22) = happyShift action_25
action_45 (26) = happyShift action_26
action_45 (29) = happyShift action_27
action_45 (14) = happyGoto action_51
action_45 (15) = happyGoto action_22
action_45 (16) = happyGoto action_23
action_45 _ = happyFail

action_46 (17) = happyShift action_8
action_46 (18) = happyShift action_24
action_46 (22) = happyShift action_25
action_46 (26) = happyShift action_26
action_46 (29) = happyShift action_27
action_46 (14) = happyGoto action_50
action_46 (15) = happyGoto action_22
action_46 (16) = happyGoto action_23
action_46 _ = happyFail

action_47 (17) = happyShift action_8
action_47 (18) = happyShift action_24
action_47 (22) = happyShift action_25
action_47 (26) = happyShift action_26
action_47 (29) = happyShift action_27
action_47 (12) = happyGoto action_49
action_47 (13) = happyGoto action_20
action_47 (14) = happyGoto action_21
action_47 (15) = happyGoto action_22
action_47 (16) = happyGoto action_23
action_47 _ = happyFail

action_48 _ = happyReduce_14

action_49 (21) = happyShift action_37
action_49 _ = happyReduce_13

action_50 (19) = happyShift action_44
action_50 _ = happyReduce_32

action_51 (19) = happyShift action_44
action_51 _ = happyReduce_31

action_52 _ = happyReduce_34

action_53 _ = happyReduce_39

action_54 (39) = happyShift action_70
action_54 _ = happyFail

action_55 (38) = happyShift action_67
action_55 (39) = happyShift action_68
action_55 (41) = happyShift action_69
action_55 (11) = happyGoto action_66
action_55 _ = happyFail

action_56 (38) = happyShift action_64
action_56 (39) = happyShift action_65
action_56 _ = happyFail

action_57 (30) = happyShift action_63
action_57 _ = happyReduce_19

action_58 (25) = happyShift action_62
action_58 _ = happyFail

action_59 (28) = happyShift action_61
action_59 _ = happyReduce_23

action_60 (20) = happyShift action_45
action_60 (29) = happyShift action_46
action_60 _ = happyReduce_29

action_61 (22) = happyShift action_59
action_61 (10) = happyGoto action_77
action_61 _ = happyReduce_22

action_62 (17) = happyShift action_8
action_62 (34) = happyShift action_9
action_62 (40) = happyShift action_10
action_62 (43) = happyShift action_11
action_62 (44) = happyShift action_12
action_62 (45) = happyShift action_13
action_62 (46) = happyShift action_14
action_62 (48) = happyShift action_15
action_62 (49) = happyShift action_16
action_62 (5) = happyGoto action_76
action_62 (6) = happyGoto action_5
action_62 (7) = happyGoto action_6
action_62 (16) = happyGoto action_7
action_62 _ = happyReduce_5

action_63 (22) = happyShift action_59
action_63 (38) = happyReduce_18
action_63 (39) = happyReduce_18
action_63 (8) = happyGoto action_75
action_63 (9) = happyGoto action_57
action_63 (10) = happyGoto action_58
action_63 _ = happyReduce_22

action_64 (17) = happyShift action_8
action_64 (34) = happyShift action_9
action_64 (40) = happyShift action_10
action_64 (43) = happyShift action_11
action_64 (44) = happyShift action_12
action_64 (45) = happyShift action_13
action_64 (46) = happyShift action_14
action_64 (48) = happyShift action_15
action_64 (49) = happyShift action_16
action_64 (5) = happyGoto action_74
action_64 (6) = happyGoto action_5
action_64 (7) = happyGoto action_6
action_64 (16) = happyGoto action_7
action_64 _ = happyReduce_5

action_65 _ = happyReduce_16

action_66 (39) = happyShift action_73
action_66 _ = happyFail

action_67 (17) = happyShift action_8
action_67 (34) = happyShift action_9
action_67 (40) = happyShift action_10
action_67 (43) = happyShift action_11
action_67 (44) = happyShift action_12
action_67 (45) = happyShift action_13
action_67 (46) = happyShift action_14
action_67 (48) = happyShift action_15
action_67 (49) = happyShift action_16
action_67 (5) = happyGoto action_72
action_67 (6) = happyGoto action_5
action_67 (7) = happyGoto action_6
action_67 (16) = happyGoto action_7
action_67 _ = happyReduce_5

action_68 _ = happyReduce_9

action_69 (17) = happyShift action_8
action_69 (18) = happyShift action_24
action_69 (22) = happyShift action_25
action_69 (26) = happyShift action_26
action_69 (29) = happyShift action_27
action_69 (12) = happyGoto action_71
action_69 (13) = happyGoto action_20
action_69 (14) = happyGoto action_21
action_69 (15) = happyGoto action_22
action_69 (16) = happyGoto action_23
action_69 _ = happyFail

action_70 _ = happyReduce_12

action_71 (21) = happyShift action_37
action_71 (42) = happyShift action_80
action_71 _ = happyFail

action_72 (39) = happyShift action_79
action_72 _ = happyFail

action_73 _ = happyReduce_11

action_74 (39) = happyShift action_78
action_74 _ = happyFail

action_75 _ = happyReduce_20

action_76 _ = happyReduce_21

action_77 _ = happyReduce_24

action_78 _ = happyReduce_17

action_79 _ = happyReduce_10

action_80 (17) = happyShift action_8
action_80 (34) = happyShift action_9
action_80 (40) = happyShift action_10
action_80 (43) = happyShift action_11
action_80 (44) = happyShift action_12
action_80 (45) = happyShift action_13
action_80 (46) = happyShift action_14
action_80 (48) = happyShift action_15
action_80 (49) = happyShift action_16
action_80 (5) = happyGoto action_81
action_80 (6) = happyGoto action_5
action_80 (7) = happyGoto action_6
action_80 (16) = happyGoto action_7
action_80 _ = happyReduce_5

action_81 (38) = happyShift action_83
action_81 (41) = happyShift action_69
action_81 (11) = happyGoto action_82
action_81 _ = happyReduce_25

action_82 _ = happyReduce_26

action_83 (17) = happyShift action_8
action_83 (34) = happyShift action_9
action_83 (40) = happyShift action_10
action_83 (43) = happyShift action_11
action_83 (44) = happyShift action_12
action_83 (45) = happyShift action_13
action_83 (46) = happyShift action_14
action_83 (48) = happyShift action_15
action_83 (49) = happyShift action_16
action_83 (5) = happyGoto action_84
action_83 (6) = happyGoto action_5
action_83 (7) = happyGoto action_6
action_83 (16) = happyGoto action_7
action_83 _ = happyReduce_5

action_84 _ = happyReduce_27

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Program happy_var_2
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (mkSeq happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  7 happyReduction_5
happyReduction_5  =  HappyAbsSyn5
		 (Skip
	)

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn5
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Print happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn5
		 (Newline
	)

happyReduce_9 = happyReduce 5 7 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfStmt happy_var_2 happy_var_4 Skip
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 7 7 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfStmt happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 6 7 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfStmt happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 7 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (WhileStmt happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 7 happyReduction_13
happyReduction_13 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (RepeatStmt happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (LoopStmt happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn5
		 (Exit
	)

happyReduce_16 = happyReduce 5 7 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (CaseStmt happy_var_2 happy_var_4 Skip
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 7 7 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (CaseStmt happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_0  8 happyReduction_18
happyReduction_18  =  HappyAbsSyn8
		 ([]
	)

happyReduce_19 = happySpecReduce_1  8 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  8 happyReduction_20
happyReduction_20 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  9 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ((happy_var_1, happy_var_3)
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_0  10 happyReduction_22
happyReduction_22  =  HappyAbsSyn10
		 ([]
	)

happyReduce_23 = happySpecReduce_1  10 happyReduction_23
happyReduction_23 (HappyTerminal (NUMBER happy_var_1))
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  10 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (NUMBER happy_var_1))
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_0  11 happyReduction_25
happyReduction_25  =  HappyAbsSyn5
		 (Skip
	)

happyReduce_26 = happyReduce 5 11 happyReduction_26
happyReduction_26 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfStmt happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 6 11 happyReduction_27
happyReduction_27 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfStmt happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  12 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  12 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal (RELOP happy_var_2))
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Binop happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  13 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  13 happyReduction_31
happyReduction_31 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal (ADDOP happy_var_2))
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Binop happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  13 happyReduction_32
happyReduction_32 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Binop Minus happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  14 happyReduction_33
happyReduction_33 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  14 happyReduction_34
happyReduction_34 (HappyAbsSyn12  happy_var_3)
	(HappyTerminal (MULOP happy_var_2))
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Binop happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  15 happyReduction_35
happyReduction_35 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn12
		 (Variable happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  15 happyReduction_36
happyReduction_36 (HappyTerminal (NUMBER happy_var_1))
	 =  HappyAbsSyn12
		 (Number happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  15 happyReduction_37
happyReduction_37 (HappyAbsSyn12  happy_var_2)
	(HappyTerminal (MONOP happy_var_1))
	 =  HappyAbsSyn12
		 (Monop happy_var_1 happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  15 happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Monop Uminus happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  15 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happyMonadReduce 1 16 happyReduction_40
happyReduction_40 ((HappyTerminal (IDENT happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( do {a <- getPosn; return $ Name happy_var_1 "" (fst a)})
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	EOF -> action 50 50 tk (HappyState action) sts stk;
	IDENT happy_dollar_dollar -> cont 17;
	MONOP happy_dollar_dollar -> cont 18;
	MULOP happy_dollar_dollar -> cont 19;
	ADDOP happy_dollar_dollar -> cont 20;
	RELOP happy_dollar_dollar -> cont 21;
	NUMBER happy_dollar_dollar -> cont 22;
	SEMI -> cont 23;
	DOT -> cont 24;
	COLON -> cont 25;
	LPAR -> cont 26;
	RPAR -> cont 27;
	COMMA -> cont 28;
	MINUS -> cont 29;
	VBAR -> cont 30;
	ASSIGN -> cont 31;
	EOF -> cont 32;
	BADTOK -> cont 33;
	CASE -> cont 34;
	OF -> cont 35;
	BEGIN -> cont 36;
	DO -> cont 37;
	ELSE -> cont 38;
	END -> cont 39;
	IF -> cont 40;
	ELSIF -> cont 41;
	THEN -> cont 42;
	WHILE -> cont 43;
	PRINT -> cont 44;
	NEWLINE -> cont 45;
	REPEAT -> cont 46;
	UNTIL -> cont 47;
	LOOP -> cont 48;
	EXIT -> cont 49;
	_ -> happyError' tk
	})

happyError_ 50 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (>>=)
happyReturn :: () => a -> Alex a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> Alex a
happyError' tk = happyError tk

parseFile = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


getPosn :: Alex (Int,Int)
getPosn = do
  (AlexPn _ l c,_,_,_) <- alexGetInput
  return (l,c)

happyError :: Token -> Alex a
happyError t = do
  (l,c) <- getPosn
  fail (show l ++ ":" ++ show c ++ ": Parse error on Token: " ++ show t ++ "\n")  

parse s = runAlex s parseFile
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
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
