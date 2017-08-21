module Main where
import Prelude
import DOM.Event.MouseEvent
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.DOM (addEventListener, querySelector)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, Ref, modifyRef)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (mousemove)
import DOM.HTML.Types (Window, htmlElementToElement)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, beginPath, clearRect, closePath, fillRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke, translate, withContext)
import Math (pi, cos, sin)


ss = { x: 450.0
        , y: 450.0
        , z : 450.0
        , qx: pi / 4.0
        , qy: pi / 4.0
        , qz: pi / 4.0
        , loop:1.25}


rotateCUBEUp =let
  canvas = getCanvasElementById "thecanvas"
  in
      withAnimateContext "thecanvas" ss \ctx ss -> do
          -- ctx <- drawBackground ctx
          void $ drawCube ctx (Cube { x: 0.0, y: 0.0, z: 0.0, size: 200.0, color: "rgb(123,0,0)" }) (Angle3D { qx: ss.qx, qy: ss.qy, qz: ss.qz})
          pure $ ss { x = ss.x, y = ss.y, z = ss.z, qx = ss.qx, qy = ss.qy + 0.00, qz = ss.qz + ss.loop,loop=max(ss.loop-0.05) 0.00 }


rotateCUBEDown=let
  canvas = getCanvasElementById "thecanvas"
  in
      withAnimateContext "thecanvas" ss \ctx ss -> do
          -- ctx <- drawBackground ctx
          void $ drawCube ctx (Cube { x: 0.0, y: 0.0, z: 0.0, size: 200.0, color: "rgb(160,0,0)" }) (Angle3D { qx: ss.qx, qy: ss.qy, qz: ss.qz})
          pure $ ss { x = ss.x, y = ss.y, z = ss.z, qx = ss.qx+ss.loop, qy = ss.qy + 0.00, qz = ss.qz,loop=max (ss.loop-0.05) 0.00 }


rotateCUBELeft=let
  canvas = getCanvasElementById "thecanvas"
  in
      withAnimateContext "thecanvas" ss \ctx ss -> do
          -- ctx <- drawBackground ctx
          void $ drawCube ctx (Cube { x: 0.0, y: 0.0, z: 0.0, size: 200.0, color: "rgb(182,0,0)" }) (Angle3D { qx: ss.qx, qy: ss.qy, qz: ss.qz})
          pure $ ss { x = ss.x, y = ss.y, z = ss.z, qx = ss.qx+0.0000 , qy = ss.qy + ss.loop, qz = ss.qz,loop=max (ss.loop-0.05) 0.00 }


rotateCUBExy=let
  canvas = getCanvasElementById "thecanvas"
  in
      withAnimateContext "thecanvas" ss \ctx ss -> do
          -- ctx <- drawBackground ctx
          void $ drawCube ctx (Cube { x: 0.0, y: 0.0, z: 0.0, size: 200.0, color: "rgb(200,0,0)" }) (Angle3D { qx: ss.qx, qy: ss.qy, qz: ss.qz})
          pure $ ss { x = ss.x, y = ss.y, z = ss.z, qx = ss.qx+ss.loop , qy = ss.qy + ss.loop, qz = ss.qz,loop=max (ss.loop-0.05) 0.00 }

rotateCUBEyx=let
  canvas = getCanvasElementById "thecanvas"
  in
      withAnimateContext "thecanvas" ss \ctx ss -> do
          -- ctx <- drawBackground ctx
          void $ drawCube ctx (Cube { x: 0.0, y: 0.0, z: 0.0, size: 200.0, color: "rgb(194,0,0)" }) (Angle3D { qx: ss.qx, qy: ss.qy, qz: ss.qz})
          pure $ ss { x = ss.x, y = ss.y, z = ss.z, qx = ss.qx+ss.loop , qy = ss.qy, qz = ss.qz +0.0,loop=max (ss.loop-0.05) 0.00 }

rotateCUBEzx=let
  canvas = getCanvasElementById "thecanvas"
  in
      withAnimateContext "thecanvas" ss \ctx ss -> do
          -- ctx <- drawBackground ctx
          void $ drawCube ctx (Cube { x: 0.0, y: 0.0, z: 0.0, size: 200.0, color: "rgb(91,0,0)" }) (Angle3D { qx: ss.qx, qy: ss.qy, qz: ss.qz})
          pure $ ss { x = ss.x, y = ss.y, z = ss.z, qx = ss.qx+0.5 , qy = ss.qy, qz = ss.qz +0.75,loop=max (ss.loop-0.01) 0.00 }

rotateCUBEStop=let
  canvas = getCanvasElementById "thecanvas"
  in
      withAnimateContext "thecanvas" ss \ctx ss -> do
          -- ctx <- drawBackground ctx
          void $ drawCube ctx (Cube { x: 0.0, y: 0.0, z: 0.0, size: 200.0, color: "rgb(81,12,12)" }) (Angle3D { qx: ss.qx, qy: ss.qy, qz: ss.qz})
          pure $ ss { x = ss.x, y = ss.y, z = ss.z, qx = ss.qx , qy = ss.qy, qz = ss.qz}


-- main :: forall e. Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
main = do
  glob <- newRef 1
  rotateCUBEStop
  canvas<-getCanvasElementById "thecanvas"
  -- captureCoords canvas holdRef mouseCoordsRef
  node <- querySelector "#theCanvas"
  for_ node $ addEventListener "click" $ void  do
    modifyRef glob \ a->a+1
    a<-readRef glob
    modifyRef glob \a-> mod a 6
    log (show a)
    rotateCUBEStop
    a<-readRef glob
    if a==0
      then do
        log (show a)
        for_ node $ addEventListener "mousemove" $ void do
            log (show a)
            log "Mouse Moved Up"
            rotateCUBEUp
      else
        log "Mouse Clicked"

    if a==1
      then do
        log (show a)
        for_ node $ addEventListener "mousemove" $ void do
            log (show a)
            log "Mouse Moved Down"
            rotateCUBExy
      else
        log "Mouse Clicked"
    if a==2
      then do
        log (show a)
        for_ node $ addEventListener "mousemove" $ void do
            log (show a)
            log "Mouse Moved Left"
            rotateCUBEDown
      else
        log "Mouse Clicked"
    if a==3
      then do
        log (show a)
        for_ node $ addEventListener "mousemove" $ void do
            log (show a)
            log "Mouse Moved Right"
            rotateCUBELeft
      else
        log "Mouse Clicked"
    if a==4
      then do
        log (show a)
        for_ node $ addEventListener "mousemove" $ void do
            log (show a)
            log "Mouse Moved Right"
            rotateCUBEyx
      else
        log "Mouse Clicked"


--Template To Render Cube--
-- Define 3D and 2D Point objects
newtype Point3D = Point3D
  { x :: Number
  , y :: Number
  , z :: Number
  }

newtype Point2D = Point2D
  { x :: Number
  , y :: Number
  }

newtype Angle3D = Angle3D
  { qx :: Number
  , qy :: Number
  , qz :: Number
  }

-- Define cube object
newtype Cube = Cube
  { x :: Number
  , y :: Number
  , z :: Number
  , size :: Number
  , color :: String
  }

--| Function to project 3D point on 2D coordinate plane

project :: Point3D -> Angle3D -> Point2D
project (Point3D { x, y, z }) (Angle3D { qx, qy, qz }) =
  let xRotQz = x * (cos qz) + y * (sin qz)
      yRotQz = y * (cos qz) - x * (sin qz)
      yRotQzQx = yRotQz * (cos qx) + z * (sin qx)
      zRotQzQx = (z * (cos qx) - yRotQz * (sin qx))
      xRotQzQxQy = (xRotQz * (cos qy) + zRotQzQx * (sin qy))
  in
    Point2D { x: xRotQzQxQy+450.00, y: yRotQzQx+450.00 }

doTask :: forall e.
  Context2D ->
  String ->
  (Context2D -> Eff (canvas :: CANVAS | e) Context2D) ->
  Eff (canvas :: CANVAS | e) Context2D
doTask ctx color draw = withContext ctx do
  ctx <- setStrokeStyle color ctx
  ctx <- beginPath ctx
  ctx <- draw ctx
  ctx <- closePath ctx
  stroke ctx


drawEdge :: forall e. Context2D -> Point2D -> Point2D -> Eff (canvas :: CANVAS | e) Context2D
drawEdge ctx (Point2D from) (Point2D to) = do
  ctx <- moveTo ctx from.x from.y
  lineTo ctx to.x to.y


drawCube :: forall e. Context2D -> Cube -> Angle3D -> Eff (canvas :: CANVAS | e) Context2D
drawCube ctx (Cube { color, x, y, z, size }) (Angle3D { qx, qy, qz })= do
  let half = size / 2.0
  let v1 = project (Point3D { x: x - half, y: y - half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v2 = project (Point3D { x: x - half, y: y + half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v3 = project (Point3D { x: x - half, y: y - half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v4 = project (Point3D { x: x - half, y: y + half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v5 = project (Point3D { x: x + half, y: y - half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v6 = project (Point3D { x: x + half, y: y + half, z: z - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v7 = project (Point3D { x: x + half, y: y - half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  let v8 = project (Point3D { x: x + half, y: y + half, z: z + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
  doTask ctx color \ctx -> do
    ctx <- drawEdge ctx v1 v5
    ctx <- drawEdge ctx v5 v6
    ctx <- drawEdge ctx v6 v2
    ctx <- drawEdge ctx v2 v1
    ctx <- drawEdge ctx v3 v7
    ctx <- drawEdge ctx v7 v8
    ctx <- drawEdge ctx v8 v4
    ctx <- drawEdge ctx v4 v3

    ctx <- drawEdge ctx v1 v3
    ctx <- drawEdge ctx v5 v7
    ctx <- drawEdge ctx v6 v8
    -- void $ fillRect ctx {x:0.0,y:0.0,w:500.0,h:500.0}
    drawEdge ctx v2 v4


clearCanvas :: forall e. CanvasElement -> Eff (canvas :: CANVAS | e) Unit
clearCanvas canvas = do
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  ctx <- getContext2D canvas
  void $ clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }


recurseHere :: forall e state.
  Window ->
  Ref state ->
  state ->
  (state -> Eff (ref :: REF, dom :: DOM | e) state) ->
  Eff (ref :: REF, dom :: DOM | e) Unit
recurseHere window ref state step =
  void $ requestAnimationFrame
      do recurseHere window ref state step
         state <- readRef ref
         state <- step state
         writeRef ref state
      window


withAnimation :: forall e state.
  state ->
  (state -> Eff (ref :: REF, dom :: DOM | e) state) ->
  Eff (ref :: REF, dom :: DOM | e) Unit
withAnimation state step = do
  window <- window
  ref <- newRef state
  recurseHere window ref state step

withAnimateContext :: forall e state.
  String ->
  state ->
  (Context2D -> state -> Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) state) ->
  Eff (dom :: DOM, ref :: REF, canvas :: CANVAS | e) Unit
withAnimateContext name state draw = do
      canvas <- getCanvasElementById name
      case canvas of
        Just canvas -> do
          ctx <- getContext2D canvas
          ctx <- translate {translateX:0.0,translateY:0.0} ctx
          withAnimation state \state -> do
            clearCanvas canvas
            draw ctx state
        Nothing -> pure unit
