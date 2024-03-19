module Main where

import Prelude

import Config as Config
import Control.Monad.State (class MonadState, get, modify_, put, runStateT)
import Data.Array (concat, cons, delete, filter, foldM, foldr, fromFoldable, head, length, mapWithIndex, null, range, uncons, updateAt, (!!))
import Data.Int (ceil, floor, quot, rem, toNumber)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
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
  , cellWidth :: Number
  , cellHeight :: Number
  , widthCells :: Int
  , heightCells :: Int
  , events :: Array Event
  }

lookupPos :: forall m a t. MonadEffect m => Context -> {x :: Int, y :: Int | t} -> Array a -> m a
lookupPos ctx pos a = do
  let idx = pos.y * ctx.widthCells + pos.x
  case a !! idx of
    Nothing -> liftEffect $ throw "index out of bounds"
    Just x -> pure x

updatePos :: forall m a t. MonadEffect m => Context -> {x :: Int, y :: Int | t} -> a -> Array a -> m (Array a)
updatePos ctx pos v a = do
  let idx = pos.y * ctx.widthCells + pos.x
  case updateAt idx v a of
    Nothing -> liftEffect $ throw "index out of bounds"
    Just a' -> pure a'

main :: Effect Unit
main = do
  -- d <- toDocument <$> document w
  -- getElementById "foo" (toNonElementParentNode d) >>= case _ of
  --   Nothing -> log "failed to find foo"
  --   Just e -> do
  --     l <- eventListener \_e ->
  --       log "click"
  --     addEventListener click l false $ toEventTarget e
  log $ Config.apiServer
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
              write (ctx { events = cons (h (floor $ px / ctx.cellWidth) (floor $ py / ctx.cellHeight)) ctx.events }) rc
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
      let widthCells = 200.0
      let cellWidth = toNumber $ ceil $ width / widthCells
      let cellHeight = cellWidth * 2.0
      pure $ Just
        { window: w
        , canvas
        , render
        , width
        , height
        , cellHeight
        , cellWidth
        , widthCells: ceil widthCells
        , heightCells: ceil $ height / cellHeight
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
  let x = toNumber pos.x * ctx.cellWidth
  let y = toNumber pos.y * ctx.cellHeight
  inv <- lookupPos ctx pos st.inverse
  let fg = if inv /= 0 then c.bg else c.fg
  let bg = if inv /= 0 then c.fg else c.bg
  setFillStyle ctx.render bg
  fillRect ctx.render
    { x
    , y
    , width: ctx.cellWidth
    , height: ctx.cellHeight
    }
  setFillStyle ctx.render fg
  fillText ctx.render c.char (x + ctx.cellWidth / 4.0) (y + ctx.cellHeight / 1.4)

drawCells :: Context -> State -> Array Cell -> Effect Unit
drawCells ctx st cells = do
  void $ for (range 0 ctx.widthCells) \x -> do
    for (range 0 ctx.heightCells) \y -> do
      let pos = { x, y }
      c <- lookupPos ctx pos cells
      drawCell ctx st c pos

type Picker = Context -> Array Cell -> Effect (Maybe (Tuple { x :: Int, y :: Int} Cell))

pickRandom :: Picker
pickRandom ctx cells = do
  idx <- randomInt 0 $ length cells - 1
  case cells !! idx of
    Nothing -> pure Nothing
    Just c ->
      pure
      $ Just
      $ Tuple { x: rem idx ctx.widthCells, y: quot idx ctx.widthCells } c

type Transition =
  { cells :: Array Cell
  , speed :: Int
  , cadence :: Int
  , picker :: Picker
  }

type State =
  { tick :: Int
  , cells :: Array Cell
  , inverse :: Array Int
  , transitions :: Array Transition
  , redraw :: Boolean
  }

initialState :: State
initialState =
  { tick: 0
  , cells: []
  , inverse: []
  , transitions: []
  , redraw: true
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
    link :: Int -> String -> String -> String -> String -> Array Cell
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
    linkRight :: Int -> String -> String -> String -> String -> Array Cell
    linkRight y fgc bgc str url =
      fromFoldable $ mapWithIndex
      (\i c ->
        Tuple { x: (ctx.widthCells - String.length str - 8) + i, y }
        $ Cell
        { bg: bgc, fg: fgc, char: String.singleton c
        , click: \st -> do
          void $ open url "_blank" "" ctx.window
          pure st
        }
      )
      $ toCharArray str
    -- fg =
    --   unions
    --   [ link 0 "purple" "white" "twitch.tv/lcolonq" "https://twitch.tv/lcolonq"
    --   , link 1 "blue" "white" "twitter.com/lcolonq" "https://twitter.com/lcolonq"
    --   , link (ctx.heightCells - 1) "white" "black" "the previous one" "https://pub.colonq.computer/~llll/cgi-bin/ring?me=llll&offset=-1"
    --   , linkRight (ctx.heightCells - 1) "white" "black" "the next one" "https://pub.colonq.computer/~llll/cgi-bin/ring?me=llll&offset=1"
    --   ]
    -- cells = union fg bg
    cells = bg
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
  , redraw = false
  , transitions = case uncons st.transitions of
    Nothing -> st.transitions
    Just { head, tail } -> if null head.cells then tail else st.transitions
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
          lookupPos ctx { x: mx, y: my } st.cells >>= case _ of
            Nothing -> pure st
            Just (Cell c) -> liftEffect $ c.click st
        EventMouseMove mx my -> do
          inv <- foldM (\inv pos -> updatePos ctx pos (st.tick + 30) inv) st.inverse $ map (\x -> map (\y -> { x, y }) (range (my - 1) (my + 1))) (range (mx - 1) (mx + 1))
          pure st { inverse = inv }
  put st'

pickCell :: forall m. MonadState State m => MonadEffect m => Context -> m Unit
pickCell ctx = do
  st <- get
  case uncons st.transitions of
    Nothing -> pure unit
    Just { head: trans, tail } ->
      liftEffect (trans.picker trans.cells) >>= case _ of
        Nothing -> pure unit
        Just (Tuple pk pv) -> do
          cells <- updatePos ctx pk pv st.cells
          put st { redraw = true, cells = cells, transitions = cons (trans { cells = (delete pk trans.cells) }) tail }

loop :: Ref Context -> State -> Effect Unit
loop rc st = do
  ctx <- read rc
  -- render
  when st.redraw do
    clearRect ctx.render { x: 0.0, y: 0.0, width: ctx.width, height: ctx.height }
    drawCells ctx st st.cells
  -- update
  -- Tuple _ st' <- flip runStateT st do
  --   tick
  --   pullEvents rc
  --   case head st.transitions of
  --     Nothing -> pure unit
  --     Just trans -> do
  --       when (rem st.tick trans.cadence == 0) do
  --         void $ for (range 0 trans.speed) \_ -> pickCell
  -- void $ requestAnimationFrame (loop rc st') ctx.window
  void $ requestAnimationFrame (loop rc st) ctx.window
