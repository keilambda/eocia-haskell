{-# LANGUAGE QuasiQuotes #-}

module X86Int (module X86Int) where

import Data.HashMap.Strict (HashMap, foldlWithKey')
import Data.Kind (Type)
import Data.List (List)
import GHC.Records (HasField (getField))
import PyF (fmt)

import X86

type Arg :: Type
data Arg = Imm Int | Reg Reg | Deref Int Reg

instance Show Arg where
  show = \case
    Imm n -> "$" ++ show n
    Reg r -> "%" ++ show r
    Deref n r -> show n ++ "(%" ++ show r ++ ")"

type Instr :: Type
type Instr = InstrF Arg

type Frame :: Type
data Frame = MkFrame {env :: HashMap String Arg, offset :: Int}

instance HasField "size" Frame Int where
  getField MkFrame{offset} = let n = negate offset in (n `mod` 16) + n

type Block :: Type
newtype Block = MkBlock (List Instr)

instance Show Block where
  show (MkBlock xs) = foldl' (\acc x -> [fmt|{acc}\t{show x}\n|]) mempty xs

type Program :: Type
data Program = MkProgram {globl :: String, blocks :: HashMap String Block}

instance Show Program where
  show MkProgram{globl, blocks} = [fmt|.globl {globl}\n{body}|]
   where
    body = foldlWithKey' (\acc lbl block -> [fmt|{acc}{lbl}:\n{show block}|]) mempty blocks
