module Arbitrary () where

import Core
import Data.Text qualified as Text
import Stage.CVar qualified as CVar
import Stage.LInt qualified as LInt
import Stage.LVar qualified as LVar
import Stage.X86Int qualified as X86Int
import Test.Tasty.QuickCheck

instance Arbitrary Name where
  arbitrary = MkName . Text.pack <$> ((:) <$> elements first <*> listOf (elements rest))
   where
    first = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "-_!$%&*+/:<=>?@^~"
    rest = first ++ ['0' .. '9']

instance Arbitrary Label where
  arbitrary = MkLabel . Text.pack <$> ((:) <$> elements first <*> listOf (elements rest))
   where
    first = ['a' .. 'z'] <> ['A' .. 'Z'] <> "_"
    rest = first <> ['0' .. '9']

instance Arbitrary Platform where arbitrary = elements [Linux, Darwin]

instance Arbitrary NulOp where arbitrary = pure Read
instance Arbitrary UnOp where arbitrary = pure Neg
instance Arbitrary BinOp where arbitrary = elements [Add, Sub]

instance Arbitrary Reg where
  arbitrary = elements [RSP, RBP, RAX, RBX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15]

instance Arbitrary LInt.Expr where
  -- NOTE: Generation of @Prim Read []@ is excluded because it makes tests halt.
  arbitrary = sized \n ->
    if n <= 0 then
      LInt.Lit <$> arbitrary
    else
      oneof
        [ LInt.Lit <$> arbitrary
        , LInt.neg <$> resize (n `div` 2) arbitrary
        , do
            a <- resize (n `div` 2) arbitrary
            b <- resize (n `div` 2) arbitrary
            elements [LInt.add a b, LInt.sub a b]
        ]

instance Arbitrary LVar.Expr where
  arbitrary = sized \n ->
    if n <= 0 then
      oneof [LVar.Lit <$> arbitrary, LVar.Var <$> arbitrary]
    else
      oneof
        [ LVar.Lit <$> arbitrary
        , LVar.Var <$> arbitrary
        , LVar.Let <$> arbitrary <*> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
        , LVar.neg <$> resize (n `div` 2) arbitrary
        , do
            a <- resize (n `div` 2) arbitrary
            b <- resize (n `div` 2) arbitrary
            elements [LVar.add a b, LVar.sub a b]
        ]

instance Arbitrary Atom where
  arbitrary = oneof [Lit <$> arbitrary, Var <$> arbitrary]

instance Arbitrary CVar.Expr where
  arbitrary = sized \n ->
    if n <= 0 then
      CVar.Atom <$> arbitrary
    else
      oneof
        [ CVar.Atom <$> arbitrary
        , pure (CVar.NulApp Read)
        , CVar.UnApp Neg <$> arbitrary
        , CVar.BinApp <$> elements [Add, Sub] <*> arbitrary <*> arbitrary
        ]

instance Arbitrary CVar.Stmt where
  arbitrary = CVar.Assign <$> arbitrary <*> arbitrary

instance Arbitrary CVar.Tail where
  arbitrary = sized \n ->
    if n <= 0 then
      CVar.Return <$> arbitrary
    else
      frequency
        [ (1, CVar.Return <$> arbitrary)
        , (4, CVar.Seq <$> arbitrary <*> resize (n `div` 2) arbitrary)
        ]

instance Arbitrary X86Int.Arg where
  arbitrary =
    oneof
      [ X86Int.Imm <$> arbitrary
      , X86Int.Reg <$> arbitrary
      , X86Int.Deref <$> arbitrary <*> arbitrary
      ]
