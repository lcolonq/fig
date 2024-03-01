module Main where

import Prelude

import Control.Monad.State (class MonadState, get, modify_, put, runStateT)
import Data.Array (concat, cons, head, length, mapWithIndex, range, uncons, zip, (!!))
import Data.Int (ceil, floor, rem, round, toNumber)
import Data.Map (Map, delete, empty, filter, fromFoldable, insert, isEmpty, lookup, member, toUnfoldable, union, unions)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Random (randomInt)
import Effect.Ref (Ref, new, read, write)
import Graphics.Canvas (CanvasElement, Context2D, clearRect, fillRect, fillText, getCanvasElementById, getContext2D, setCanvasDimensions, setFillStyle, setFont)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (Window, innerHeight, innerWidth, open, requestAnimationFrame)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (fromEvent, pageX, pageY)

type Context =
  { window :: Window
  , canvas :: CanvasElement
  , render :: Context2D
  , width :: Number
  , height :: Number
  , cellDim :: Number
  , widthCells :: Int
  , heightCells :: Int
  , events :: Array Event
  }

main :: Effect Unit
main = do
  -- d <- toDocument <$> document w
  -- getElementById "foo" (toNonElementParentNode d) >>= case _ of
  --   Nothing -> log "failed to find foo"
  --   Just e -> do
  --     l <- eventListener \_e ->
  --       log "click"
  --     addEventListener click l false $ toEventTarget e
  w <- window
  newContext >>= case _ of
    Nothing -> log "failed to find canvas"
    Just ictx -> do
      rc <- new ictx
      lresize <- eventListener \_e -> do
        newContext >>= case _ of
          Nothing -> log "failed to find canvas"
          Just newctx -> write newctx rc
      addEventListener (EventType "resize") lresize false $ Window.toEventTarget w
      let lmouse h = eventListener \e -> case fromEvent e of
            Nothing -> pure unit
            Just me -> do
              let px = toNumber $ pageX me
              let py = toNumber $ pageY me
              ctx <- read rc
              write (ctx { events = cons (h (floor $ px / ctx.cellDim) (floor $ py / ctx.cellDim)) ctx.events }) rc
      lmouseclick <- lmouse EventMouseClick
      addEventListener (EventType "click") lmouseclick false $ Window.toEventTarget w
      lmousemove <- lmouse EventMouseMove
      addEventListener (EventType "mousemove") lmousemove false $ Window.toEventTarget w
      loop rc initialState
  pure unit

newContext :: Effect (Maybe Context)
newContext = do
  w <- window
  getCanvasElementById "lcolonq-canvas" >>= case _ of
    Nothing -> pure Nothing
    Just canvas -> do
      width <- toNumber <$> innerWidth w
      height <- toNumber <$> innerHeight w
      setCanvasDimensions canvas { width, height }
      render <- getContext2D canvas
      setFont render "bold 0.8vw Iosevka Comfy"
      let widthCells = 100.0
      let cellDim = toNumber $ ceil $ width / widthCells
      pure $ Just
        { window: w
        , canvas
        , render
        , width
        , height
        , cellDim
        , widthCells: ceil widthCells
        , heightCells: ceil $ height / cellDim
        , events: [EventGfx GfxWhiteout]
        }

newtype Cell = Cell
  { fg :: String
  , bg :: String
  , char :: String
  , click :: State -> Effect State
  }

drawCell :: forall t. Context -> State -> Cell -> { x :: Int, y :: Int | t } -> Effect Unit
drawCell ctx st (Cell c) pos = do
  let x = toNumber pos.x * ctx.cellDim 
  let y = toNumber pos.y * ctx.cellDim
  let fg = if member { x: pos.x, y: pos.y } st.inverse then c.bg else c.fg
  let bg = if member { x: pos.x, y: pos.y } st.inverse then c.fg else c.bg
  setFillStyle ctx.render bg
  fillRect ctx.render
    { x
    , y
    , width: ctx.cellDim
    , height: ctx.cellDim
    }
  setFillStyle ctx.render fg
  fillText ctx.render c.char (x + ctx.cellDim / 4.0) (y + ctx.cellDim / 1.4)

type Cells = Map { x :: Int, y :: Int } Cell

drawCells :: Context -> State -> Cells -> Effect Unit
drawCells ctx st cells = do
  void $ for (toUnfoldable cells :: Array _) \(Tuple pos c) -> do
    drawCell ctx st c pos

type Picker = Cells -> Effect (Maybe (Tuple { x :: Int, y :: Int} Cell))

pickRandom :: Picker
pickRandom cells = do
  let arr = toUnfoldable cells
  idx <- randomInt 0 $ length arr - 1
  case arr !! idx of
    Nothing -> pure Nothing
    Just c -> pure $ Just c

type Transition =
  { cells :: Cells 
  , speed :: Int
  , cadence :: Int
  , picker :: Picker
  }

type State =
  { tick :: Int
  , cells :: Cells
  , inverse :: Map { x :: Int, y :: Int } Int
  , transitions :: Array Transition
  }

initialState :: State
initialState =
  { tick: 0
  , cells: empty
  , inverse: empty
  , transitions: []
  }

data Event
  = EventGfx Gfx
  | EventMouseClick Int Int
  | EventMouseMove Int Int

data Gfx
  = GfxWhiteout

gfxTransitions :: Context -> Gfx -> Array Transition
gfxTransitions ctx GfxWhiteout =
  let
    bg = fromFoldable $ concat $ flip map (range 0 ctx.widthCells) \x ->
      flip map (range 0 ctx.heightCells) \y ->
      Tuple
      { x, y }
      $ Cell
      { bg: "white"
      , fg: "black"
      , click: pure
      , char:
        let base = "LCOLONQ"
        in case String.charAt (rem (x + y) (String.length base)) base of
          Nothing -> "?"
          Just c -> String.singleton c
      }
    link :: Int -> String -> String -> String -> String -> Cells
    link y fgc bgc str url =
      fromFoldable $ mapWithIndex
      (\i c ->
        Tuple { x: i, y }
        $ Cell
        { bg: bgc, fg: fgc, char: String.singleton c
        , click: \st -> do
          void $ open url "_blank" "" ctx.window
          pure st
        }
      )
      $ toCharArray str
    fg =
      unions
      [ link 0 "purple" "white" "twitch.tv/lcolonq" "https://twitch.tv/lcolonq"
      , link 1 "blue" "white" "twitter.com/lcolonq" "https://twitter.com/lcolonq"
      ]
    cells = union fg bg
  in
   [ { cells
     , speed: 20
     , cadence: 1
     , picker: pickRandom
     }
   ]

tick :: forall m. MonadState State m => m Unit
tick = modify_ \st ->
  st
  { tick = st.tick + 1
  , transitions = case uncons st.transitions of
    Nothing -> st.transitions
    Just { head, tail } -> if isEmpty head.cells then tail else st.transitions
  , inverse = filter (st.tick <= _) st.inverse
  }

pullEvents :: forall m. MonadState State m => MonadEffect m => Ref Context -> m Unit
pullEvents rc = do
  ctx <- liftEffect $ read rc
  st <- get
  st' <- case uncons ctx.events of
    Nothing -> pure st
    Just { head, tail } -> do
      liftEffect $ write (ctx { events = tail }) rc
      case head of
        EventGfx gfx -> pure $ st { transitions = st.transitions <> gfxTransitions ctx gfx }
        EventMouseClick mx my ->
          case lookup { x: mx, y: my } st.cells of
            Nothing -> pure st
            Just (Cell c) -> liftEffect $ c.click st
        EventMouseMove mx my -> do
          let inv = fromFoldable $ concat $ flip map (range (mx - 1) (mx + 1)) \x ->
                flip map (range (my - 1) (my + 1)) \y -> Tuple { x, y } (st.tick + 30)
          pure st { inverse = union inv st.inverse }
  put st'

pickCell :: forall m. MonadState State m => MonadEffect m => m Unit
pickCell = do
  st <- get
  case uncons st.transitions of
    Nothing -> pure unit
    Just { head: trans, tail } ->
      liftEffect (trans.picker trans.cells) >>= case _ of
        Nothing -> pure unit
        Just (Tuple pk pv) ->
          put st { cells = insert pk pv st.cells, transitions = cons (trans { cells = (delete pk trans.cells) }) tail }

loop :: Ref Context -> State -> Effect Unit
loop rc st = do
  ctx <- read rc
  -- render
  clearRect ctx.render { x: 0.0, y: 0.0, width: ctx.width, height: ctx.height }
  drawCells ctx st st.cells
  -- update
  Tuple _ st' <- flip runStateT st do
    tick
    pullEvents rc
    case head st.transitions of
      Nothing -> pure unit
      Just trans -> do
        when (rem st.tick trans.cadence == 0) do
          void $ for (range 0 trans.speed) \_ -> pickCell
  void $ requestAnimationFrame (loop rc st') ctx.window
