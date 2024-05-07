{-# Language TemplateHaskell, ApplicativeDo #-}
module Fig.Emulator.GB.Test.Instr where

import Control.Lens.TH (makeLenses)

import Control.Lens ((^.))
import Control.Monad.State.Strict (StateT(..))

import Data.Word (Word8, Word16)
import qualified Data.Aeson as Aeson

import qualified Text.Printf as Pr

import Fig.Prelude
import Fig.Emulator.GB.Utils
import Fig.Emulator.GB.CPU
import Fig.Emulator.GB.CPU.Instruction
import Fig.Emulator.GB.Bus (Bus(..), Addr(..))
import qualified Fig.Emulator.GB.Bus as Bus
import Fig.Emulator.GB.Component.RAM

newtype InstrTestError = InstrTestError Text
  deriving Show
instance Exception InstrTestError
instance Pretty InstrTestError where
  pretty (InstrTestError b) = mconcat
    [ "Instruction test error: "
    , b
    ]

data TestVals = TestVals
  { _testvalsA :: !Word8
  , _testvalsB :: !Word8
  , _testvalsC :: !Word8
  , _testvalsD :: !Word8
  , _testvalsE :: !Word8
  , _testvalsF :: !Word8
  , _testvalsH :: !Word8
  , _testvalsL :: !Word8
  , _testvalsPC :: !Word16
  , _testvalsSP :: !Word16
  , _testvalsRAM :: ![(Word16, Word8)]
  }
makeLenses 'TestVals
instance Aeson.FromJSON TestVals where
  parseJSON = Aeson.withObject "TestVals" $ \v -> do
    _testvalsA <- v Aeson..: "a"
    _testvalsB <- v Aeson..: "b"
    _testvalsC <- v Aeson..: "c"
    _testvalsD <- v Aeson..: "d"
    _testvalsE <- v Aeson..: "e"
    _testvalsF <- v Aeson..: "f"
    _testvalsH <- v Aeson..: "h"
    _testvalsL <- v Aeson..: "l"
    _testvalsPC <- v Aeson..: "pc"
    _testvalsSP <- v Aeson..: "sp"
    _testvalsRAM <- v Aeson..: "ram"
    pure TestVals{..}

data Testcase = Testcase
  { _testcaseName :: !Text
  , _testcaseInitial :: !TestVals
  , _testcaseFinal :: !TestVals
  }
makeLenses 'Testcase

instance Aeson.FromJSON Testcase where
  parseJSON = Aeson.withObject "Testcase" $ \v -> do
    _testcaseName <- v Aeson..: "name"
    _testcaseInitial <- v Aeson..: "initial"
    _testcaseFinal <- v Aeson..: "final"
    pure Testcase {..}

readTestcases :: (MonadIO m, MonadThrow m) => FilePath -> m [Testcase]
readTestcases p = liftIO (Aeson.decodeFileStrict p) >>= \case
  Just ts -> pure ts
  Nothing -> throwM . InstrTestError $ "failed to read testcases at " <> pack p

cpuInstrTest :: (MonadIO m, MonadThrow m) => TestVals -> m (CPU m)
cpuInstrTest vs = do
  let
    (z, n, h, c) = w8flags $ vs ^. testvalsF
    initialBus = Bus [compWRAM 0x0000 $ 64 * 1024]
  finalBus <- foldM (\b (addr, v) -> Bus.write b (Addr addr) v) initialBus $ vs ^. testvalsRAM
  pure CPU
    { _lastPC = 0x0
    , _lastIns = Nop
    , _running = True
    , _regs = initialRegs
      { _regA = vs ^. testvalsA
      , _regB = vs ^. testvalsB
      , _regC = vs ^. testvalsC
      , _regD = vs ^. testvalsD
      , _regE = vs ^. testvalsE
      , _regH = vs ^. testvalsH
      , _regL = vs ^. testvalsL
      , _regPC = vs ^. testvalsPC - 1
      , _regSP = vs ^. testvalsSP
      , _regFlagZ = z
      , _regFlagN = n
      , _regFlagH = h
      , _regFlagC = c
      }
    , _bus = finalBus
    }

checkCPU :: forall m. (MonadIO m, MonadThrow m) => Text -> TestVals -> CPU m -> CPU m -> m ()
checkCPU tnm vs initial c = do
  let
    flag f = if f then "1" else "0"
    rreg8 = pack . Pr.printf "%02X"
    rreg16 = pack . Pr.printf "%04X"
    dumpRegs :: Text -> Registers -> Text
    dumpRegs prefix r = mconcat
      [ prefix, " registers:\t"
      , "A: ", rreg8 $ r ^. regA
      , ", B: ", rreg8 $ r ^. regB
      , ", C: ", rreg8 $ r ^. regC
      , ", D: ", rreg8 $ r ^. regD
      , ", E: ", rreg8 $ r ^. regE
      , ", H: ", rreg8 $ r ^. regH
      , ", L: ", rreg8 $ r ^. regL
      , ", PC: ", rreg16 $ r ^. regPC
      , ", SP: ", rreg16 $ r ^. regSP
      ]
    dumpFlags :: Text -> Registers -> Text
    dumpFlags prefix r = mconcat
      [ prefix, " flags:\t"
      , "Z: ", flag $ r ^. regFlagZ
      , ", N: ", flag $ r ^. regFlagN
      , ", H: ", flag $ r ^. regFlagH
      , ", C: ", flag $ r ^. regFlagC
      ]
    check :: (Eq a) => (a -> Text) -> Text -> a -> a -> m ()
    check pr nm eval aval = if eval == aval
      then pure ()
      else throwM . InstrTestError $ mconcat
      [ "while running test ", tnm, ":\n"
      , nm <> " mismatch: expected "
      , pr eval, ", got ", pr aval
      , "\n", dumpRegs "Initial" $ initial ^. regs
      , "\n", dumpRegs "Final" $ c ^. regs
      , "\n", dumpFlags "Initial" $ initial ^. regs
      , "\n", dumpFlags "Final" $ c ^. regs
      ]
  check rreg8 "register A" (vs ^. testvalsA) (c ^. regs . regA)
  check rreg8 "register B" (vs ^. testvalsB) (c ^. regs . regB)
  check rreg8 "register C" (vs ^. testvalsC) (c ^. regs . regC)
  check rreg8 "register D" (vs ^. testvalsD) (c ^. regs . regD)
  check rreg8 "register E" (vs ^. testvalsE) (c ^. regs . regE)
  check rreg8 "register H" (vs ^. testvalsH) (c ^. regs . regH)
  check rreg8 "register L" (vs ^. testvalsL) (c ^. regs . regL)
  check rreg16 "PC" (vs ^. testvalsPC - 1) (c ^. regs . regPC)
  check rreg16 "SP" (vs ^. testvalsSP) (c ^. regs . regSP)
  let (fz, fn, fh, fc) = w8flags $ vs ^. testvalsF
  check flag "flag Z" fz (c ^. regs . regFlagZ)
  check flag "flag N" fn (c ^. regs . regFlagN)
  check flag "flag H" fh (c ^. regs . regFlagH)
  check flag "flag C" fc (c ^. regs . regFlagC)
  forM_ (vs ^. testvalsRAM) \(Addr -> addr, eval) -> do
    Bus.read (c ^. bus) addr >>= \case
      Nothing -> throwM . InstrTestError $ "failed to read expected address: " <> pretty addr
      Just aval -> check rreg8 ("memory address " <> pretty addr) eval aval

runTestcase :: (MonadIO m, MonadThrow m) => Testcase -> m ()
runTestcase tc = liftIO do
  initial <- cpuInstrTest $ tc ^. testcaseInitial
  let
    body :: forall m'. Emulating m' => m' Instruction
    body = do
      ins <- decode
      step ins
      pure ins
  (ins, final) <- runStateT body initial
  checkCPU (tc ^. testcaseName <> " (" <> tshow ins <> ")") (tc ^. testcaseFinal) initial final
