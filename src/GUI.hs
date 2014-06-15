{-# LANGUAGE ScopedTypeVariables #-}
module GUI where


import           Debug.Trace
import           Graphics.UI.WX     hiding (Event)
import           Reactive.Banana
import           Reactive.Banana.WX
import           Utils              (getDataFile)

import           Board              (Square, piecePosition, Piece(..), PieceColor(..))
import           MoveParser
import           State


height, width, pieceMargin, pieceSize, boardMargin :: Int
pieceSize = 64
pieceMargin = 8
boardMargin = 100
height   = 8 * (pieceSize + pieceMargin) + boardMargin
width    = height + boardMargin

gui :: IO ()
gui = start $ do
                f   <- frame [text := "hsChess"]
                t   <- timer f [ interval   := 500 ]
                currentPlayer  <- staticText f []
                input <- entry f []
                playBtn   <- button f [text := "Play"]
                pp      <- panel f []

                set f [layout :=  margin 10 $ column 3 [ minsize (sz width height) ( widget pp ),
                                                         margin 10 $ row 3 [label "Move: ", minsize (sz 200 20) (widget input), widget playBtn ],
                                                         margin 10 $ row 2 [label "Player: ", widget currentPlayer]]]

                let networkDescription :: forall t. Frameworks t => Moment t ()
                    networkDescription = do

                                            etick  <- event0 t command

                                            eplay <- event0 playBtn command
                                            moveIn <- behaviorText input ""

                                            --let memoryE :: Event t String
                                            --    memoryE = apply (const <$> moveIn) eplay
                                                --moveB :: Behavior t String
                                                --moveB = stepper "0" memoryE

                                            let
                                                stateB :: Behavior t State
                                                stateB = accumB newState $ (playTurn <$> (moveIn <@ eplay))

                                                playTurn :: String -> State -> State
                                                playTurn move x = trace "playTurn" $ case (parseMove move) of Right r -> applyMove r x
                                                                                                              Left _ -> x

                                            sink currentPlayer [text :== (show . getPlayer) <$> stateB ]

                                            sink pp [on paint :== stepper (\_dc _ -> return ()) $
                                                     (drawGameState <$> stateB ) <@ (etick `union` eplay )]

                                            reactimate $ repaint pp <$ etick
                                            --reactimate $ repaint output <$ etick

                network <- compile networkDescription
                actuate network

pieceBitmapFor :: Piece -> Bitmap ()
pieceBitmapFor piece = case (pieceColor piece) of White -> bitmap $ getDataFile $ 'W':show (pieceType piece) ++ ".png"
                                                  Black -> bitmap $ getDataFile $ 'B':show (pieceType piece) ++ ".png"

drawBmp :: forall a. DC a -> Bitmap () -> Point -> IO ()
drawBmp dc bmp pos = drawBitmap dc bmp pos True []

drawPiece :: DC a -> (Point, Square) -> IO ()
drawPiece dc (pos, square) = maybe (return ()) doDraw square
            where doDraw piece = drawBmp dc (pieceBitmapFor piece) pos

drawBoard :: forall t a. DC a -> t -> IO ()
drawBoard dc _view = do 
                        let 
                            squaresSize :: Int
                            squaresSize = pieceSize + pieceMargin
                            boardBorder :: Int
                            boardBorder = boardMargin `div` 2
                            boardSize :: Int
                            boardSize = 8 * squaresSize
                            labelYx :: Int -> Int 
                            labelYx p = boardBorder + squaresSize `div` 2 + p * squaresSize
                            labelXy :: Int -> Int 
                            labelXy p = boardBorder + squaresSize `div` 2 + p * squaresSize
                            labelXx :: Int
                            labelXx = boardBorder `div` 2
                            labelYy :: Int
                            labelYy = boardBorder `div` 2

                            squares = [ drawRect dc (rect (point (pos2boardX x) (pos2boardY y)) (sz squaresSize squaresSize )) [brushKind := BrushSolid, brushColor := black] | x <- [0..7::Int], y <- [0..7::Int], (x + y) `mod` 2 /= 0]
                            -- X labels (letters) on the left
                            labelX = map (\(p,t) -> drawText dc (show t) (point labelXx (labelXy p)) []) (zip [0..7::Int] ['0'..])
                            -- X labels (letters) on the right
                            labelX2 = map (\(p,t) -> drawText dc (show t) (point (labelXx + boardSize + boardBorder) (labelXy p)) []) (zip [0..7::Int] ['0'..])
                            -- Y labels (numbers) on the top
                            labelY = map (\(p,t) -> drawText dc (show t) (point (labelYx p) labelYy) []) (zip [0..7::Int] ['a'..])
                            -- Y labels (numbers) on the bottom
                            labelY2 = map (\(p,t) -> drawText dc (show t) (point (labelYx p) (labelYy + boardSize + boardBorder)) []) (zip [0..7::Int] ['a'..])
                        
                        -- do draw the label and the squares all at once
                        sequence_ $ labelX ++ labelX2 ++ labelY ++ labelY2 ++ squares
                        -- add a border to the board
                        drawRect dc (rect (point boardBorder boardBorder) (sz boardSize boardSize)) []

                        
pos2boardX :: Int -> Int
pos2boardX x = x * (pieceSize + pieceMargin) + (boardMargin `div` 2)

pos2boardY :: Int -> Int
pos2boardY y = y * (pieceSize + pieceMargin) + (boardMargin `div` 2)

drawGameState :: State -> DC a -> b -> IO ()
drawGameState state dc _view = do
    let
        board = getBoard state
        pieces = piecePosition board

        piecesToDraw = map (\ (y,x,p) -> (point ((pos2boardX x) + pieceMargin `div` 2) ((pos2boardY y) + pieceMargin `div` 2), p)) pieces

    -- draw the board
    drawBoard dc _view
    -- draw the pieces on the board
    sequence_ $ map (drawPiece dc) piecesToDraw
