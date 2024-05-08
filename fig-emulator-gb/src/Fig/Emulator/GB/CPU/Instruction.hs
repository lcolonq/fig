module Fig.Emulator.GB.CPU.Instruction where

import Fig.Prelude
import Prelude (Integral, fromIntegral, error)

import Data.Word (Word8, Word16)

import Fig.Emulator.GB.Utils
import Fig.Emulator.GB.Bus as Bus

newtype DecodeError = DecodeError Text
  deriving Show
instance Exception DecodeError
instance Pretty DecodeError where
  pretty (DecodeError b) = mconcat
    [ "instruction decoding error: "
    , b
    ]

class ExtractFromOpcode t where
  ext :: Integral i => i -> t

data R8 = R8B | R8C | R8D | R8E | R8H | R8L | R8MemHL | R8A
  deriving (Show)
instance ExtractFromOpcode R8 where
  ext i = case (fromIntegral i :: Int) of
    0 -> R8B
    1 -> R8C
    2 -> R8D
    3 -> R8E
    4 -> R8H
    5 -> R8L
    6 -> R8MemHL
    7 -> R8A
    _ -> error "unreachable"

data R16 = R16BC | R16DE | R16HL | R16SP
  deriving (Show)
instance ExtractFromOpcode R16 where
  ext i = case (fromIntegral i :: Int) of
    0 -> R16BC
    1 -> R16DE
    2 -> R16HL
    3 -> R16SP
    _ -> error "unreachable"

data R16Stk = R16StkBC | R16StkDE | R16StkHL | R16StkAF
  deriving (Show)
instance ExtractFromOpcode R16Stk where
  ext i = case (fromIntegral i :: Int) of
    0 -> R16StkBC
    1 -> R16StkDE
    2 -> R16StkHL
    3 -> R16StkAF
    _ -> error "unreachable"

data R16Mem = R16MemBC | R16MemDE | R16MemHLPlus | R16MemHLMinus
  deriving (Show)
instance ExtractFromOpcode R16Mem where
  ext i = case (fromIntegral i :: Int) of
    0 -> R16MemBC
    1 -> R16MemDE
    2 -> R16MemHLPlus
    3 -> R16MemHLMinus
    _ -> error "unreachable"

data Cond = CondNz | CondZ | CondNc | CondC
  deriving (Show)
instance ExtractFromOpcode Cond where
  ext i = case (fromIntegral i :: Int) of
    0 -> CondNz
    1 -> CondZ
    2 -> CondNc
    3 -> CondC
    _ -> error "unreachable"

newtype B3 = B3 Int
  deriving (Show)
instance ExtractFromOpcode B3 where
  ext i = B3 $ fromIntegral i

newtype Tgt3 = Tgt3 Int
  deriving (Show)
instance ExtractFromOpcode Tgt3 where
  ext i = Tgt3 $ fromIntegral i

newtype Imm8 = Imm8 Word8
  deriving (Show)
readImm8 :: Bus.Bus -> Addr -> IO Imm8
readImm8 b a = Bus.read b a >>= \case
  Just i -> pure $ Imm8 i
  Nothing -> throwM . DecodeError $ mconcat
    [ "failed to read immediate at unmapped address"
    , pretty a
    ]

newtype Imm16 = Imm16 Word16
  deriving (Show)
readImm16 :: Bus.Bus -> Addr -> IO Imm16
readImm16 b a = do
  mlo <- Bus.read b a
  mhi <- Bus.read b $ a + 1
  case (mlo, mhi) of
    (Just lo, Just hi) -> pure . Imm16 $ w8w8 hi lo
    _otherwise -> throwM . DecodeError $ mconcat
      [ "failed to read 16-bit immediate at unmapped address"
      , pretty a
      ]

data Instruction
  -- Block 0
  = Nop

  | LdR16Imm16 !R16 !Imm16
  | LdR16MemA !R16Mem
  | LdAR16Mem !R16Mem
  | LdImm16Sp !Imm16

  | IncR16 !R16
  | DecR16 !R16
  | AddHlR16 !R16

  | IncR8 !R8
  | DecR8 !R8

  | LdR8Imm8 !R8 !Imm8

  | Rlca
  | Rrca
  | Rla
  | Rra
  | Daa
  | Cpl
  | Scf
  | Ccf

  | JrImm8 !Imm8
  | JrCondImm8 !Cond !Imm8

  | Stop

  -- Block 1
  | LdR8R8 !R8 !R8

  | Halt

  -- Block 2
  | AddAR8 !R8
  | AdcAR8 !R8
  | SubAR8 !R8
  | SbcAR8 !R8
  | AndAR8 !R8
  | XorAR8 !R8
  | OrAR8 !R8
  | CpAR8 !R8

  -- Block 3
  | AddAImm8 !Imm8
  | AdcAImm8 !Imm8
  | SubAImm8 !Imm8
  | SbcAImm8 !Imm8
  | AndAImm8 !Imm8
  | XorAImm8 !Imm8
  | OrAImm8 !Imm8
  | CpAImm8 !Imm8

  | RetCond !Cond
  | Ret
  | Reti
  | JpCondImm16 !Cond !Imm16
  | JpImm16 !Imm16
  | JpHl
  | CallCondImm16 !Cond !Imm16
  | CallImm16 !Imm16
  | RstTgt3 !Tgt3

  | PopR16Stk !R16Stk
  | PushR16Stk !R16Stk

  | LdhCA
  | LdhImm8A !Imm8
  | LdImm16A !Imm16
  | LdhAC
  | LdhAImm8 !Imm8
  | LdAImm16 !Imm16

  | AddSpImm8 !Imm8
  | LdHlSpPlusImm8 !Imm8
  | LdSpHl

  | Di
  | Ei

  -- 0xcb prefixed 16-bit instructions
  | CbRlcR8 !R8
  | CbRrcR8 !R8
  | CbRlR8 !R8
  | CbRrR8 !R8
  | CbSlaR8 !R8
  | CbSraR8 !R8
  | CbSwapR8 !R8
  | CbSrlR8 !R8

  | CbBitB3R8 !B3 !R8
  | CbResB3R8 !B3 !R8
  | CbSetB3R8 !B3 !R8
  deriving (Show)

readInstruction ::
  Bus.Bus -> Addr ->
  IO (Instruction, Addr)
readInstruction b a = do
  op <- Bus.read b a >>= \case
    Just o -> pure o
    Nothing -> throwM . DecodeError $ mconcat
      [ "failed to read opcode at unmapped address "
      , pretty a
      ]
  let blk = w8bits2 7 op
  let bot3 = w8bits3 2 op
  let bot4 = w8bits4 3 op
  let no i = pure (i, a + 1)
  let imm8 f = do
        x <- readImm8 b $ a + 1
        pure (f x, a + 2)
  let imm16 f = do
        x <- readImm16 b $ a + 1
        pure (f x, a + 3)
  case (op, blk, bot4, bot3) of
    -- Block 0
    (0b00000000, _, _, _) -> {-# SCC "DecodeNop" #-} no Nop

    (_, 0b00, 0b0001, _) -> {-# SCC "DecodeLdR16Imm16" #-} imm16 $ LdR16Imm16 (ext $ w8bits2 5 op)
    (_, 0b00, 0b0010, _) -> {-# SCC "DecodeLdR16MemA" #-} no $ LdR16MemA (ext $ w8bits2 5 op)
    (_, 0b00, 0b1010, _) -> {-# SCC "DecodeLdAR16Mem" #-} no $ LdAR16Mem (ext $ w8bits2 5 op)
    (0b00001000, _, _, _) -> {-# SCC "DecodeLdImm16Sp" #-} imm16 LdImm16Sp

    (_, 0b00, 0b0011, _) -> {-# SCC "DecodeIncR16" #-} no $ IncR16 (ext $ w8bits2 5 op)
    (_, 0b00, 0b1011, _) -> {-# SCC "DecodeDecR16" #-} no $ DecR16 (ext $ w8bits2 5 op)
    (_, 0b00, 0b1001, _) -> {-# SCC "DecodeAddHlR16" #-} no $ AddHlR16 (ext $ w8bits2 5 op)

    (_, 0b00, _, 0b100) -> {-# SCC "DecodeIncR8" #-} no $ IncR8 (ext $ w8bits3 5 op)
    (_, 0b00, _, 0b101) -> {-# SCC "DecodeDecR8" #-} no $ DecR8 (ext $ w8bits3 5 op)

    (_, 0b00, _, 0b110) -> {-# SCC "DecodeLdR8Imm8" #-} imm8 $ LdR8Imm8 (ext $ w8bits3 5 op)

    (0b00000111, _, _, _) -> {-# SCC "DecodeRlca" #-} no Rlca
    (0b00001111, _, _, _) -> {-# SCC "DecodeRrca" #-} no Rrca
    (0b00010111, _, _, _) -> {-# SCC "DecodeRla" #-} no Rla
    (0b00011111, _, _, _) -> {-# SCC "DecodeRra" #-} no Rra
    (0b00100111, _, _, _) -> {-# SCC "DecodeDaa" #-} no Daa
    (0b00101111, _, _, _) -> {-# SCC "DecodeCpl" #-} no Cpl
    (0b00110111, _, _, _) -> {-# SCC "DecodeScf" #-} no Scf
    (0b00111111, _, _, _) -> {-# SCC "DecodeCcf" #-} no Ccf

    (0b00011000, _, _, _) -> {-# SCC "DecodeJrImm8" #-} imm8 JrImm8
    (0b00010000, _, _, _) -> {-# SCC "DecodeStop" #-} no Stop
    (_, 0b00, _, 0b000) -> {-# SCC "DecodeJrCondImm8" #-} imm8 $ JrCondImm8 (ext $ w8bits2 4 op)

    -- Block 1
    (0b01110110, _, _, _) -> {-# SCC "DecodeHalt" #-} no Halt
    (_, 0b01, _, _) -> {-# SCC "DecodeLdR8R8" #-} no $ LdR8R8 (ext $ w8bits3 5 op) (ext $ w8bits3 2 op)

    -- Block 2
    (_, 0b10, _, _) -> do
      let ins = case w8bits3 5 op of
            0b000 -> {-# SCC "DecodeAddAR8" #-} AddAR8
            0b001 -> {-# SCC "DecodeAdcAR8" #-} AdcAR8
            0b010 -> {-# SCC "DecodeSubAR8" #-} SubAR8
            0b011 -> {-# SCC "DecodeSbcAR8" #-} SbcAR8
            0b100 -> {-# SCC "DecodeAndAR8" #-} AndAR8
            0b101 -> {-# SCC "DecodeXorAR8" #-} XorAR8
            0b110 -> {-# SCC "DecodeOrAR8" #-} OrAR8
            0b111 -> {-# SCC "DecodeCpAR8" #-} CpAR8
            _ -> error "unreachable"
      no $ ins (ext $ w8bits3 2 op)

    -- Block 3
    (0b11000110, _, _, _) -> {-# SCC "DecodeAddAImm8" #-} imm8 AddAImm8
    (0b11001110, _, _, _) -> {-# SCC "DecodeAdcAImm8" #-} imm8 AdcAImm8
    (0b11010110, _, _, _) -> {-# SCC "DecodeSubAImm8" #-} imm8 SubAImm8
    (0b11011110, _, _, _) -> {-# SCC "DecodeSbcAImm8" #-} imm8 SbcAImm8
    (0b11100110, _, _, _) -> {-# SCC "DecodeAndAImm8" #-} imm8 AndAImm8
    (0b11101110, _, _, _) -> {-# SCC "DecodeXorAImm8" #-} imm8 XorAImm8
    (0b11110110, _, _, _) -> {-# SCC "DecodeOrAImm8" #-} imm8 OrAImm8
    (0b11111110, _, _, _) -> {-# SCC "DecodeCpAImm8" #-} imm8 CpAImm8

    (0b11001001, _, _, _) -> {-# SCC "DecodeRet" #-} no Ret
    (0b11011001, _, _, _) -> {-# SCC "DecodeReti" #-} no Reti
    (0b11000011, _, _, _) -> {-# SCC "DecodeJpImm16" #-} imm16 JpImm16
    (0b11101001, _, _, _) -> {-# SCC "DecodeJpHl" #-} no JpHl
    (0b11001101, _, _, _) -> {-# SCC "DecodeCallImm16" #-} imm16 CallImm16

    (0b11100010, _, _, _) -> {-# SCC "DecodeLdhCA" #-} no LdhCA
    (0b11100000, _, _, _) -> {-# SCC "DecodeLdhImm8A" #-} imm8 LdhImm8A
    (0b11101010, _, _, _) -> {-# SCC "DecodeLdImm16A" #-} imm16 LdImm16A
    (0b11110010, _, _, _) -> {-# SCC "DecodeLdhAC" #-} no LdhAC
    (0b11110000, _, _, _) -> {-# SCC "DecodeLdhAImm8" #-} imm8 LdhAImm8
    (0b11111010, _, _, _) -> {-# SCC "DecodeLdAImm16" #-} imm16 LdAImm16

    (0b11101000, _, _, _) -> {-# SCC "DecodeAddSpImm8" #-} imm8 AddSpImm8
    (0b11111000, _, _, _) -> {-# SCC "DecodeLdHlSpPlusImm8" #-} imm8 LdHlSpPlusImm8
    (0b11111001, _, _, _) -> {-# SCC "DecodeLdSpHl" #-} no LdSpHl

    (0b11110011, _, _, _) -> {-# SCC "DecodeDi" #-} no Di
    (0b11111011, _, _, _) -> {-# SCC "DecodeEi" #-} no Ei

    (0b11001011, _, _, _) -> {-# SCC "DecodeCBPrefix" #-} do
      -- 0xcb prefix
      op2 <- Bus.read b (a + 1) >>= \case
        Just o -> pure o
        Nothing -> throwM . DecodeError $ mconcat
          [ "failed to read (0xCB-prefixed) opcode at unmapped address "
          , pretty $ a + 1
          ]
      case w8bits2 7 op2 of

        0b00 -> case w8bits3 5 op2 of
          0b000 -> {-# SCC "DecodeCbRlcR8" #-} pure (CbRlcR8 $ ext $ w8bits3 2 op2, a + 2)
          0b001 -> {-# SCC "DecodeCbRrcR8" #-} pure (CbRrcR8 $ ext $ w8bits3 2 op2, a + 2)
          0b010 -> {-# SCC "DecodeCbRlR8" #-} pure (CbRlR8 $ ext $ w8bits3 2 op2, a + 2)
          0b011 -> {-# SCC "DecodeCbRrR8" #-} pure (CbRrR8 $ ext $ w8bits3 2 op2, a + 2)
          0b100 -> {-# SCC "DecodeCbSlaR8" #-} pure (CbSlaR8 $ ext $ w8bits3 2 op2, a + 2)
          0b101 -> {-# SCC "DecodeCbSraR8" #-} pure (CbSraR8 $ ext $ w8bits3 2 op2, a + 2)
          0b110 -> {-# SCC "DecodeCbSwapR8" #-} pure (CbSwapR8 $ ext $ w8bits3 2 op2, a + 2)
          0b111 -> {-# SCC "DecodeCbSrlR8" #-} pure (CbSrlR8 $ ext $ w8bits3 2 op2, a + 2)
          _ -> error "unreachable"
        0b01 -> {-# SCC "DecodeCbBitB3R8" #-} pure (CbBitB3R8 (ext $ w8bits3 5 op2) $ ext $ w8bits3 2 op2, a + 2)
        0b10 -> {-# SCC "DecodeCbResB3R8" #-} pure (CbResB3R8 (ext $ w8bits3 5 op2) $ ext $ w8bits3 2 op2, a + 2)
        0b11 -> {-# SCC "DecodeCbSetB3R8" #-} pure (CbSetB3R8 (ext $ w8bits3 5 op2) $ ext $ w8bits3 2 op2, a + 2)
        _ -> error "unreachable"

    (_, 0b11, _, 0b000) -> {-# SCC "DecodeRetCond" #-} no $ RetCond (ext $ w8bits2 4 op)
    (_, 0b11, _, 0b010) -> {-# SCC "DecodeJpCondImm16" #-} imm16 $ JpCondImm16 (ext $ w8bits2 4 op)
    (_, 0b11, _, 0b100) -> {-# SCC "DecodeCallCondImm16" #-} imm16 $ CallCondImm16 (ext $ w8bits2 4 op)
    (_, 0b11, _, 0b111) -> {-# SCC "DecodeRstTgt3" #-} no $ RstTgt3 (ext $ w8bits3 5 op)
    (_, 0b11, 0b0001, _) -> {-# SCC "DecodePopR16Stk" #-} no $ PopR16Stk (ext $ w8bits2 5 op)
    (_, 0b11, 0b0101, _) -> {-# SCC "DecodePushR16Stk" #-} no $ PushR16Stk (ext $ w8bits2 5 op)

    _unknown -> do
      log $ "unknown opcode: " <> tshow op
      no Nop
