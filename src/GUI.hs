{-# LANGUAGE ScopedTypeVariables #-}
module GUI where


import           Debug.Trace
import           Graphics.UI.WX     hiding (Event)
import           Reactive.Banana
import           Reactive.Banana.WX
import           Utils              (getDataFile)

import           Board              (Square, piecePosition)
import           MoveParser
import           State


wk, wq :: Bitmap ()
wk    = bitmap $ getDataFile "WK.png"
wq    = bitmap $ getDataFile "WQ.png"

height, width, pieceMargin, pieceSize, boardMargin :: Int
pieceSize = 64
pieceMargin = 8
boardMargin = 100
height   = 8 * (pieceSize + pieceMargin) + boardMargin
width    = height + boardMargin

gui :: IO ()
gui = start $ do
                f   <- frame [text := "Counter"]
                t   <- timer f [ interval   := 500 ]
                output  <- staticText f []
                input <- entry f []
                playBtn   <- button f [text := "Play"]
                pp      <- panel f []

                set f [layout :=  margin 10 $ column 3 [ minsize (sz width height) ( widget pp ),
                                                         margin 10 $ row 2 [minsize (sz 200 20) (widget input), widget playBtn ],
                                                         widget output]]

                let networkDescription :: forall t. Frameworks t => Moment t ()
                    networkDescription = do

                                            etick  <- event0 t command

                                            eplay <- event0 playBtn command
                                            moveIn <- behaviorText input ""

                                            let memoryE :: Event t String
                                                memoryE = apply (const <$> moveIn) eplay
                                                moveB :: Behavior t String
                                                moveB = stepper "0" memoryE

                                            let
                                                bship :: Behavior t State
                                                bship = accumB newState $ (playTurn <$> (moveIn <@ eplay))

                                                playTurn :: String -> State -> State
                                                playTurn move x = trace "playTurn" $ case (parseMove move) of Right r -> applyMove r x
                                                                                                              Left _ -> x

                                            sink output [text :== show <$> moveB ]

                                            sink pp [on paint :== stepper (\_dc _ -> return ()) $
                                                     (drawGameState <$> bship ) <@ (etick `union` eplay )]

                                            reactimate $ repaint pp <$ etick
                                            --reactimate $ repaint output <$ etick

                network <- compile networkDescription
                actuate network


drawBmp :: forall a. DC a -> Bitmap () -> Point -> IO ()
drawBmp dc bmp pos = drawBitmap dc bmp pos True []

drawPiece :: DC a -> (Point, Square) -> IO ()
drawPiece dc (pos, piece) = trace ("Drawing at " ++ show pos) drawBmp dc wq pos


drawGameState :: State -> DC a -> b -> IO ()
drawGameState state dc _view = do
    let
        board = getBoard state
        pieces = piecePosition board

        piecesToDraw = map (\ (x,y,p) -> (point (x*(pieceSize + pieceMargin)) (y* (pieceSize + pieceMargin)), p)) pieces

    sequence_ $ map (drawPiece dc) piecesToDraw
