{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
module GUI where 


import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Utils (getDataFile)
import Debug.Trace


wk, wq :: Bitmap ()
wk    = bitmap $ getDataFile "WK.png"
wq    = bitmap $ getDataFile "WQ.png"

height, width :: Int
height   = 400
width    = 400

gui :: IO ()
gui = start $ do
    

    f       <- frame [text := "Counter"]

    t  <- timer f [ interval   := 500 ]

    --bup     <- button f [text := "Up"]
    --bdown   <- button f [text := "Down"]
    output  <- staticText f []    
    input <- entry f []
    playBtn   <- button f [text := "Play"]
    pp      <- panel f []
    
    --set f [layout := minsize (sz width height) $ margin 10 $
    --        column 5 [widget bup, widget bdown, widget output, widget pp]]
    set f [layout :=  margin 10 $ column 3 [ minsize (sz width height) ( widget pp ), margin 10 $ row 2 [minsize (sz 200 20) (widget input), widget playBtn ], widget output]]


    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
            etick  <- event0 t command

            --eup   <- event0 bup   command
            --edown <- event0 bdown command

            eplay <- event0 playBtn command
            moveIn <- behaviorText input ""

            ekey   <- event1 pp keyboard

            let memoryE = apply (const <$> moveIn) eplay

            let eleft  = filterE ((== KeyLeft ) . keyKey) ekey
                eright = filterE ((== KeyRight) . keyKey) ekey

            let
                bship :: Behavior t Int
                bship = accumB (width `div` 2) $
                    (goLeft <$ eleft) `union` (goRight <$ eright)
            
                goLeft  x = max 0          (x - 5)
                goRight x = min (width-30) (x + 5)
        
            sink output [text :== show <$> stepper "0" memoryE ] 

            sink pp [on paint :== stepper (\_dc _ -> return ()) $
                     (drawGameState <$> bship) <@ etick]
            reactimate $ repaint pp <$ etick

    network <- compile networkDescription    
    actuate network

drawBmp :: forall a. DC a -> Bitmap () -> Point -> IO ()
drawBmp dc bmp pos = drawBitmap dc bmp pos True []    

drawShip :: DC a -> Point -> IO ()
drawShip dc pos = trace ("Drawing at " ++ show pos) drawBmp dc wq pos

drawGameState :: Int -> DC a -> b -> IO ()
drawGameState ship dc _view = do
    let
        shipLocation = point ship (height `div` 2)
        --positions    = map head rocks
        --collisions   = map (collide shipLocation) positions

    drawShip dc shipLocation
    --mapM (drawRock dc) (zip positions collisions) 

    --when (or collisions) (play explode)
