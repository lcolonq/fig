{-# Language TemplateHaskell #-}
module Fig.Emulator.GB.CPU
  ( CPU(..)
  , Registers(..), initialRegs
  , Emulating, runEmulating
  , running, regs, bus, regPC, regSP
  , regA, regB, regC, regD, regE, regH, regL
  , regFlagZ, regFlagN, regFlagH, regFlagC
  , updateComps
  , decode
  , step
  ) where

import Control.Lens.TH (makeLenses)

import Fig.Prelude

import Control.Lens ((.=), use, (^.))
import Control.Monad (when)
import Control.Monad.State.Strict (StateT(..))

import Data.Word (Word8, Word16, Word32)
import Data.Int (Int8)
import Data.Bits

import Fig.Emulator.GB.Utils
import Fig.Emulator.GB.Bus (Bus(..), Addr(..))
import qualified Fig.Emulator.GB.Bus as Bus
import Fig.Emulator.GB.CPU.Instruction

newtype CPUError = CPUError Text
  deriving Show
instance Exception CPUError
instance Pretty CPUError where
  pretty (CPUError b) = mconcat
    [ "CPU error: "
    , b
    ]

data Registers = Registers
  { _regA :: !Word8
  , _regB :: !Word8, _regC :: !Word8
  , _regD :: !Word8, _regE :: !Word8
  , _regH :: !Word8, _regL :: !Word8
  , _regSP :: !Word16
  , _regPC :: !Word16
  , _regFlagZ :: !Bool, _regFlagC :: !Bool
  , _regFlagN :: !Bool, _regFlagH :: !Bool
  , _regFlagIME :: !Bool
  }
makeLenses 'Registers

initialRegs :: Registers
initialRegs = Registers
  { _regA = 0x01
  , _regB = 0x00, _regC = 0x13
  , _regD = 0x00, _regE = 0xd8
  , _regH = 0x01, _regL = 0x4d
  , _regSP = 0xfffe
  , _regPC = 0x0100
  , _regFlagZ = True, _regFlagC = True
  , _regFlagN = False, _regFlagH = True
  , _regFlagIME = False
  }

data CPU = CPU
  { _lastPC :: Word16
  , _lastIns :: Instruction
  , _running :: Bool
  , _regs :: Registers
  , _bus :: Bus
  }
makeLenses 'CPU

newtype Emulating a = Emulating { runEmulating :: StateT CPU IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CPU, MonadThrow)

-- logCPUState :: Emulating m => m ()
-- logCPUState = do
--   rs <- use regs
--   let pc = rs ^. regPC
--   b <- use bus
--   m0 <- fromJust <$> liftIO (Bus.read b $ Addr pc)
--   m1 <- fromJust <$> liftIO (Bus.read b $ Addr pc + 1)
--   m2 <- fromJust <$> liftIO (Bus.read b $ Addr pc + 2)
--   m3 <- fromJust <$> liftIO (Bus.read b $ Addr pc + 3)
--   liftIO . hPutStrLn ?log $ mconcat 
--     [ "A:", rreg8 $ rs ^. regA
--     , " F:", rreg8 $ flagsw8 (rs ^. regFlagZ) (rs ^. regFlagN) (rs ^. regFlagH) (rs ^. regFlagC)
--     , " B:", rreg8 $ rs ^. regB
--     , " C:", rreg8 $ rs ^. regC
--     , " D:", rreg8 $ rs ^. regD
--     , " E:", rreg8 $ rs ^. regE
--     , " H:", rreg8 $ rs ^. regH
--     , " L:", rreg8 $ rs ^. regL
--     , " SP:", rreg16 $ rs ^. regSP
--     , " PC:", rreg16 pc
--     , " PCMEM:", rreg8 m0, ",", rreg8 m1, ",", rreg8 m2, ",", rreg8 m3
--     ]
--   where
--     rreg8 = pack . Pr.printf "%02X"
--     rreg16 = pack . Pr.printf "%04X"

-- | Inform all components that the given number of t-cycles have passed
updateComps :: Word16 -> Emulating ()
updateComps t = do
  b <- use bus
  b' <- liftIO $ Bus.update t b
  bus .= b'

decode :: Emulating Instruction
decode = do
  updateComps 4
  b <- use bus
  pc <- use $ regs . regPC
  lastPC .= pc
  (ins, Addr a) <- liftIO $ readInstruction b $ Addr pc
  lastIns .= ins
  regs . regPC .= a
  pure ins

cond :: Cond -> Emulating Bool
cond CondNz = not <$> use (regs . regFlagZ)
cond CondZ = use (regs . regFlagZ)
cond CondNc = not <$> use (regs . regFlagC)
cond CondC = use (regs . regFlagC)

read8 :: Addr -> Emulating Word8
read8 a = do
  updateComps 4
  b <- use bus
  pc <- use lastPC
  ins <- use lastIns
  liftIO (Bus.read b a) >>= \case
    Just v -> pure v
    Nothing -> throwM . CPUError $ mconcat
      [ "read from unmapped address "
      , pretty a
      , " while executing instruction "
      , tshow ins
      , " (at "
      , pretty $ Addr pc
      , ")"
      ]

read16 :: Addr -> Emulating Word16
read16 a = do
  lo <- read8 a
  hi <- read8 $ a + 1
  pure $ w8w8 hi lo

write8 :: Addr -> Word8 -> Emulating ()
write8 a v = do
  updateComps 4
  b <- use bus
  b' <- liftIO $ Bus.write b a v
  bus .= b'

write16 :: Addr -> Word16 -> Emulating ()
write16 a v = do
  write8 a $ w16lo v
  write8 (a + 1) $ w16hi v

r8 :: R8 -> Emulating Word8
r8 R8B = use $ regs . regB
r8 R8C = use $ regs . regC
r8 R8D = use $ regs . regD
r8 R8E = use $ regs . regE
r8 R8H = use $ regs . regH
r8 R8L = use $ regs . regL
r8 R8MemHL = do
  hl <- r16 R16HL
  read8 $ Addr hl
r8 R8A = use $ regs . regA

setR8 :: R8 -> Word8 -> Emulating ()
setR8 R8B v = regs . regB .= v
setR8 R8C v = regs . regC .= v
setR8 R8D v = regs . regD .= v
setR8 R8E v = regs . regE .= v
setR8 R8H v = regs . regH .= v
setR8 R8L v = regs . regL .= v
setR8 R8MemHL v = do
  hl <- r16 R16HL
  write8 (Addr hl) v
setR8 R8A v = regs . regA .= v

r16 :: R16 -> Emulating Word16
r16 R16BC = w8w8 <$> r8 R8B <*> r8 R8C
r16 R16DE = w8w8 <$> r8 R8D <*> r8 R8E
r16 R16HL = w8w8 <$> r8 R8H <*> r8 R8L
r16 R16SP = use $ regs . regSP

setR16 :: R16 -> Word16 -> Emulating ()
setR16 R16BC v = do
  regs . regB .= w16hi v
  regs . regC .= w16lo v
setR16 R16DE v = do
  regs . regD .= w16hi v
  regs . regE .= w16lo v
setR16 R16HL v = do
  regs . regH .= w16hi v
  regs . regL .= w16lo v
setR16 R16SP v = regs . regSP .= v

r16Stk :: R16Stk -> Emulating Word16
r16Stk R16StkBC = r16 R16BC
r16Stk R16StkDE = r16 R16DE
r16Stk R16StkHL = r16 R16HL
r16Stk R16StkAF = do
  hi <- r8 R8A
  z <- use $ regs . regFlagZ
  n <- use $ regs . regFlagN
  h <- use $ regs . regFlagH
  c <- use $ regs . regFlagC
  pure . w8w8 hi $ flagsw8 z n h c

setR16Stk :: R16Stk -> Word16 -> Emulating ()
setR16Stk R16StkBC v = setR16 R16BC v
setR16Stk R16StkDE v = setR16 R16DE v
setR16Stk R16StkHL v = setR16 R16HL v
setR16Stk R16StkAF v = do
  setR8 R8A $ w16hi v
  let lo = w16lo v
  regs . regFlagZ .= w8bit 7 lo
  regs . regFlagN .= w8bit 6 lo
  regs . regFlagH .= w8bit 5 lo
  regs . regFlagC .= w8bit 4 lo

r16Mem :: R16Mem -> Emulating Word16
r16Mem R16MemBC = r16 R16BC
r16Mem R16MemDE = r16 R16DE
r16Mem R16MemHLPlus = do
  hl <- r16 R16HL
  setR16 R16HL $ hl + 1
  pure hl
r16Mem R16MemHLMinus = do
  hl <- r16 R16HL
  setR16 R16HL $ hl - 1
  pure hl

step :: Instruction -> Emulating ()
step ins = do
  let
    sub8 :: (Word16 -> Word16 -> Word16) -> Word8 -> Word8 -> Emulating Word8
    sub8 op x y = do
      let res = op (sext x) (sext y)
      regs . regFlagH .= (w8bits4 3 y > w8bits4 3 x)
      regs . regFlagC .= (y > x)
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= True
      pure $ trunc res
    bitwise8 :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> Emulating Word8
    bitwise8 op x y = do
      let res = op x y
      regs . regFlagH .= False
      regs . regFlagC .= False
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= False
      pure res
  case ins of
    Nop -> {-# SCC "Nop" #-} pure ()
    LdR16Imm16 r (Imm16 i) -> {-# SCC "LdR16Imm16" #-} setR16 r i
    LdR16MemA r -> {-# SCC "LdR16MemA" #-} do
      addr <- r16Mem r
      a <- r8 R8A
      write8 (Addr addr) a
    LdAR16Mem r -> {-# SCC "LdAR16Mem" #-} do
      addr <- r16Mem r
      v <- read8 $ Addr addr
      setR8 R8A v
    LdImm16Sp (Imm16 addr) -> {-# SCC "LdImm16Sp" #-} do
      sp <- r16 R16SP
      write8 (Addr addr) $ w16lo sp
      write8 (Addr addr + 1) $ w16hi sp
    IncR16 r -> {-# SCC "IncR16" #-} do
      v <- r16 r
      setR16 r $ v + 1
    DecR16 r -> {-# SCC "DecR16" #-} do
      v <- r16 r
      setR16 r $ v - 1
    AddHlR16 r -> {-# SCC "AddHlR16" #-} do
      x <- r16 R16HL
      y <- r16 r
      let
        resl :: Word32
        resl = fromIntegral x + fromIntegral y
        res :: Word16
        res = fromIntegral resl
      regs . regFlagH .= (shiftR ((x .&. 0xfff) + (y .&. 0xfff)) 12 .&. 0b1 == 0b1)
      regs . regFlagC .= (shiftR resl 16 .&. 0b1 == 0b1)
      regs . regFlagN .= False
      setR16 R16HL res
    IncR8 r -> {-# SCC "IncR8" #-} do
      v <- r8 r
      let (res, _) = addC False v 1
      regs . regFlagH .= addH False v 1
      regs . regFlagZ .= (res == 0)
      regs . regFlagN .= False
      setR8 r res
    DecR8 r -> {-# SCC "DecR8" #-} do
      v <- r8 r
      let
        res :: Word8
        res = v - 1
      regs . regFlagH .= subH False v 1
      regs . regFlagZ .= (res == 0)
      regs . regFlagN .= True
      setR8 r res
    LdR8Imm8 r (Imm8 i) -> {-# SCC "LdR8Imm8" #-} setR8 r i
    Rlca -> {-# SCC "Rlca" #-} do
      v <- r8 R8A
      regs . regFlagH .= False
      regs . regFlagZ .= False
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 7 v
      setR8 R8A $ rotateL v 1
    Rrca -> {-# SCC "Rrca" #-} do
      v <- r8 R8A
      regs . regFlagH .= False
      regs . regFlagZ .= False
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 0 v
      setR8 R8A $ rotateR v 1
    Rla -> {-# SCC "Rla" #-} do
      v <- r8 R8A
      c <- use $ regs . regFlagC
      regs . regFlagH .= False
      regs . regFlagZ .= False
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 7 v
      setR8 R8A $ shiftL v 1 .|. if c then 1 else 0
    Rra -> {-# SCC "Rra" #-} do
      v <- r8 R8A
      c <- use $ regs . regFlagC
      regs . regFlagH .= False
      regs . regFlagZ .= False
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 0 v
      setR8 R8A $ shiftR v 1 .|. if c then 0b10000000 else 0
    Daa -> {-# SCC "Daa" #-} do
      v <- r8 R8A
      halfcarry <- use $ regs . regFlagH
      carry <- use $ regs . regFlagC
      subtract <- use $ regs . regFlagN
      let
        o0 :: Word8
        o0 = if (not subtract && v .&. 0xf > 0x09) || halfcarry then 0x06 else 0x00
        c = (not subtract && v > 0x99) || carry
        o1 :: Word8
        o1 = if c then o0 .|. 0x60 else o0
        res = if subtract then v - o1 else v + o1
      regs . regA .= res
      regs . regFlagH .= False
      regs . regFlagZ .= (res == 0)
      regs . regFlagC .= c
      pure ()
    Cpl -> {-# SCC "Cpl" #-} do
      v <- r8 R8A
      regs . regFlagH .= True
      regs . regFlagN .= True
      setR8 R8A $ complement v
    Scf -> {-# SCC "Scf" #-} do
      regs . regFlagH .= False
      regs . regFlagN .= False
      regs . regFlagC .= True
    Ccf -> {-# SCC "Ccf" #-} do
      c <- use $ regs . regFlagC
      regs . regFlagH .= False
      regs . regFlagN .= False
      regs . regFlagC .= not c
    JrImm8 (Imm8 i) -> {-# SCC "JrImm8" #-} do
      pc <- use $ regs . regPC
      regs . regPC .= pc + sext i
    JrCondImm8 c (Imm8 i) -> {-# SCC "JrCondImm8" #-} do
      b <- cond c
      when b do
        pc <- use $ regs . regPC
        regs . regPC .= pc + sext i
    Stop -> {-# SCC "Stop" #-} unimplemented
    LdR8R8 dst src -> {-# SCC "LdR8R8" #-} do
      v <- r8 src
      setR8 dst v
    Halt -> {-# SCC "Halt" #-} unimplemented
    AddAR8 i -> {-# SCC "AddAR8" #-} do
      x <- r8 R8A
      y <- r8 i
      let (res, carry) = addC False x y
      regs . regFlagH .= addH False x y
      regs . regFlagC .= carry
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= False
      regs . regA .= res
    AdcAR8 i -> {-# SCC "AdcAR8" #-} do
      x <- r8 R8A
      y <- r8 i
      c <- use $ regs . regFlagC
      let (res, carry) = addC c x y
      regs . regFlagH .= addH c x y
      regs . regFlagC .= carry
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= False
      regs . regA .= res
    SubAR8 i -> {-# SCC "SubAR8" #-} do
      x <- r8 R8A
      y <- r8 i
      let (res, carry) = subC False x y
      regs . regFlagH .= subH False x y
      regs . regFlagC .= carry
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= True
      regs . regA .= res
    SbcAR8 i -> {-# SCC "SbcAR8" #-} do
      x <- r8 R8A
      y <- r8 i
      c <- use $ regs . regFlagC
      let (res, carry) = subC c x y
      regs . regFlagH .= subH c x y
      regs . regFlagC .= carry
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= True
      regs . regA .= res
    AndAR8 i -> {-# SCC "AndAR8" #-} do
      x <- r8 R8A
      y <- r8 i
      res <- bitwise8 (.&.) x y
      regs . regFlagH .= True
      regs . regA .= res
    XorAR8 i -> {-# SCC "XorAR8" #-} do
      x <- r8 R8A
      y <- r8 i
      res <- bitwise8 xor x y
      regs . regA .= res
    OrAR8 i -> {-# SCC "OrAR8" #-} do
      x <- r8 R8A
      y <- r8 i
      res <- bitwise8 (.|.) x y
      regs . regA .= res
    CpAR8 i -> {-# SCC "CpAR8" #-} do
      x <- r8 R8A
      y <- r8 i
      let (res, carry) = subC False x y
      regs . regFlagH .= subH False x y
      regs . regFlagC .= carry
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= True
    AddAImm8 (Imm8 y) -> {-# SCC "AddAImm8" #-} do
      x <- r8 R8A
      let (res, carry) = addC False x y
      regs . regFlagH .= addH False x y
      regs . regFlagC .= carry
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= False
      regs . regA .= res
    AdcAImm8 (Imm8 y) -> {-# SCC "AdcAImm8" #-} do
      x <- r8 R8A
      c <- use $ regs . regFlagC
      let (res, carry) = addC c x y
      regs . regFlagH .= addH c x y
      regs . regFlagC .= carry
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= False
      regs . regA .= res
    SubAImm8 (Imm8 y) -> {-# SCC "SubAImm8" #-} do
      x <- r8 R8A
      let (res, carry) = subC False x y
      regs . regFlagH .= subH False x y
      regs . regFlagC .= carry
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= True
      regs . regA .= res
    SbcAImm8 (Imm8 y) -> {-# SCC "SbcAImm8" #-} do
      x <- r8 R8A
      c <- use $ regs . regFlagC
      let (res, carry) = subC c x y
      regs . regFlagH .= subH c x y
      regs . regFlagC .= carry
      regs . regFlagZ .= (res .&. 0xff == 0)
      regs . regFlagN .= True
      regs . regA .= res
    AndAImm8 (Imm8 y) -> {-# SCC "AndAImm8" #-} do
      x <- r8 R8A
      res <- bitwise8 (.&.) x y
      regs . regFlagH .= True
      regs . regA .= res
    XorAImm8 (Imm8 y) -> {-# SCC "XorAImm8" #-} do
      x <- r8 R8A
      res <- bitwise8 xor x y
      regs . regA .= res
    OrAImm8 (Imm8 y) -> {-# SCC "OrAImm8" #-} do
      x <- r8 R8A
      res <- bitwise8 (.|.) x y
      regs . regA .= res
    CpAImm8 (Imm8 y) -> {-# SCC "CpAImm8" #-} do
      x <- r8 R8A
      void $ sub8 (-) x y
    RetCond c -> {-# SCC "RetCond" #-} do
      b <- cond c
      when b do
        sp <- r16 R16SP
        v <- read16 $ Addr sp
        setR16 R16SP $ sp + 2
        regs . regPC .= v
    Ret -> {-# SCC "Ret" #-} do
      sp <- r16 R16SP
      v <- read16 $ Addr sp
      setR16 R16SP $ sp + 2
      regs . regPC .= v
    Reti -> {-# SCC "Reti" #-} do
      regs . regFlagIME .= True
      sp <- r16 R16SP
      v <- read16 $ Addr sp
      setR16 R16SP $ sp + 2
      regs . regPC .= v
    JpCondImm16 c (Imm16 i) -> {-# SCC "JpCondImm16" #-} do
      b <- cond c
      when b do
        regs . regPC .= i
    JpImm16 (Imm16 i) -> {-# SCC "JpImm16" #-} do
      regs . regPC .= i
    JpHl -> {-# SCC "JpHl" #-} do
      hl <- r16 R16HL
      regs . regPC .= hl
    CallCondImm16 c (Imm16 i) -> {-# SCC "CallCondImm16" #-} do
      b <- cond c
      when b do
        next <- use $ regs . regPC
        sp <- (\x -> x - 2) <$> r16 R16SP
        setR16 R16SP sp
        write16 (Addr sp) next
        regs . regPC .= i
    CallImm16 (Imm16 i) -> {-# SCC "CallImm16" #-} do
      next <- use $ regs . regPC
      sp <- (\x -> x - 2) <$> r16 R16SP
      setR16 R16SP sp
      write16 (Addr sp) next
      regs . regPC .= i
    RstTgt3 (Tgt3 v) -> {-# SCC "RstTgt3" #-} do
      next <- use $ regs . regPC
      sp <- (\x -> x - 2) <$> r16 R16SP
      setR16 R16SP sp
      write16 (Addr sp) next
      regs . regPC .= shiftL (fromIntegral v) 3
    PopR16Stk r -> {-# SCC "PopR16Stk" #-} do
      sp <- r16 R16SP
      v <- read16 $ Addr sp
      setR16 R16SP $ sp + 2
      setR16Stk r v
    PushR16Stk r -> {-# SCC "PushR16Stk" #-} do
      sp <- (\x -> x - 2) <$> r16 R16SP
      v <- r16Stk r
      setR16 R16SP sp
      write16 (Addr sp) v
    LdhCA -> {-# SCC "LdhCA" #-} do
      c <- r8 R8C
      a <- r8 R8A
      write8 (Addr $ 0xff00 + zext c) a
    LdhImm8A (Imm8 i) -> {-# SCC "LdhImm8A" #-} do
      a <- r8 R8A
      write8 (Addr $ 0xff00 + zext i) a
    LdImm16A (Imm16 i) -> {-# SCC "LdImm16A" #-} do
      a <- r8 R8A
      write8 (Addr i) a
    LdhAC -> {-# SCC "LdhAC" #-} do
      c <- r8 R8C
      v <- read8 (Addr $ 0xff00 + zext c)
      setR8 R8A v
    LdhAImm8 (Imm8 i) -> {-# SCC "LdhAImm8" #-} do
      v <- read8 (Addr $ 0xff00 + zext i)
      setR8 R8A v
    LdAImm16 (Imm16 i) -> {-# SCC "LdAImm16" #-} do
      v <- read8 (Addr i)
      setR8 R8A v
    AddSpImm8 (Imm8 y) -> {-# SCC "AddSpImm8" #-} do
      x <- r16 R16SP
      let
        res :: Word16
        res = x + sext y
        (_, carry) = addC False (w16lo x) y
      regs . regFlagH .= addH False (w16lo x) y
      regs . regFlagC .= carry
      regs . regFlagZ .= False
      regs . regFlagN .= False
      setR16 R16SP res
    LdHlSpPlusImm8 (Imm8 y) -> {-# SCC "LdHlSpPlusImm8" #-} do
      x <- r16 R16SP
      let
        res :: Word16
        res = x + sext y
        (_, carry) = addC False (w16lo x) y
      regs . regFlagH .= addH False (w16lo x) y
      regs . regFlagC .= carry
      regs . regFlagZ .= False
      regs . regFlagN .= False
      setR16 R16HL res
    LdSpHl -> {-# SCC "LdSpHl" #-} do
      v <- r16 R16HL
      setR16 R16SP v
    Di -> {-# SCC "Di" #-} regs . regFlagIME .= False
    Ei -> {-# SCC "Ei" #-} regs . regFlagIME .= True
    CbRlcR8 r -> {-# SCC "CbRlcR8" #-} do
      v <- r8 r
      let res = rotateL v 1
      regs . regFlagH .= False
      regs . regFlagZ .= (res == 0)
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 7 v
      setR8 r res
    CbRrcR8 r -> {-# SCC "CbRrcR8" #-} do
      v <- r8 r
      let res = rotateR v 1
      regs . regFlagH .= False
      regs . regFlagZ .= (res == 0)
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 0 v
      setR8 r res
    CbRlR8 r -> {-# SCC "CbRlR8" #-} do
      v <- r8 r
      c <- use $ regs . regFlagC
      let res = shiftL v 1 .|. if c then 0b1 else 0
      regs . regFlagH .= False
      regs . regFlagZ .= (res == 0)
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 7 v
      setR8 r res
    CbRrR8 r -> {-# SCC "CbRrR8" #-} do
      v <- r8 r
      c <- use $ regs . regFlagC
      let rizz = shiftR v 1 .|. if c then 0b10000000 else 0
      regs . regFlagH .= False
      regs . regFlagZ .= (rizz == 0)
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 0 v
      setR8 r rizz
    CbSlaR8 r -> {-# SCC "CbSlaR8" #-} do
      v <- r8 r
      let res = shiftL v 1
      regs . regFlagZ .= (res == 0)
      regs . regFlagH .= False
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 7 v
      setR8 r res
    CbSraR8 r -> {-# SCC "CbSraR8" #-} do
      v <- r8 r
      let
        vs :: Int8
        vs = fromIntegral v
        ress = shiftR vs 1
        res :: Word8
        res = fromIntegral ress
      regs . regFlagZ .= (res == 0)
      regs . regFlagH .= False
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 0 v
      setR8 r res
    CbSwapR8 r -> {-# SCC "CbSwapR8" #-} do
      v <- r8 r
      let res = rotate v 4
      regs . regFlagZ .= (res == 0)
      regs . regFlagH .= False
      regs . regFlagN .= False
      regs . regFlagC .= False
      setR8 r res
    CbSrlR8 r -> {-# SCC "CbSrlR8" #-} do
      v <- r8 r
      let res = shiftR v 1
      regs . regFlagZ .= (res == 0)
      regs . regFlagH .= False
      regs . regFlagN .= False
      regs . regFlagC .= w8bit 0 v
      setR8 r res
    CbBitB3R8 (B3 idx) r -> {-# SCC "CbBitB3R8" #-} do
      v <- r8 r
      regs . regFlagH .= True
      regs . regFlagN .= False
      regs . regFlagZ .= not (w8bit idx v)
    CbResB3R8 (B3 idx) r -> {-# SCC "CbResB3R8" #-} do
      v <- r8 r
      setR8 r $ v .&. (0xff .^. shiftL 0b1 idx)
    CbSetB3R8 (B3 idx) r -> {-# SCC "CbSetB3R8" #-} do
      v <- r8 r
      setR8 r $ v .|. shiftL 0b1 idx
  where
    unimplemented :: Emulating ()
    unimplemented = do
      a <- use lastPC
      throwM . CPUError $ mconcat
        [ "unimplemented instruction (at "
        , pretty $ Addr a
        , "): "
        , tshow ins
        ]
