{-# LANGUAGE ScopedTypeVariables #-}
module GUI where


import           Data.Maybe         (fromMaybe)
import           Debug.Trace
import           Graphics.UI.WX     hiding (Event)
import           Reactive.Banana
import           Reactive.Banana.WX


import           Board              (Piece (..), PieceColor (..), Square,
                                     piecePosition)
import           GUIUtils
import           Minimax            as M
import           MinimaxAlphaBeta   as AB
import           MinimaxLazy        as ML
import           MoveParser
import           Options
import           State
import           Utils

height, width, pieceMargin, pieceSize, boardMargin, squaresSize :: Int
pieceSize   = 64
pieceMargin = 8
boardMargin = 100
height      = 8 * (pieceSize + pieceMargin) + boardMargin
width       = height + boardMargin
squaresSize = pieceSize + pieceMargin


playOpponentWithStrategy :: Maybe OpponentOption -> State -> State
playOpponentWithStrategy option state = case fromMaybe AB option of AB -> AB.doMove state
                                                                    ML -> ML.doMove state
                                                                    M -> M.doMove state


playTurn :: Options -> String -> State -> State
playTurn options move state = trace "playTurn" $ case parseMove move of Right r -> playOpponentWithStrategy (snd3 options) $ applyMove r state
                                                                        Left _ -> state

helpPlayer :: State -> State
helpPlayer state = let state' = AB.doMove state
                    in trace (show state') state'

gui :: Options -> IO ()
gui options = start $ do
                f               <- frame [ text := "hsChess"
                                         , resizeable := True ]
                t               <- timer f [ interval := 500 ]
                currentPlayer   <- staticText f []
                moveInput       <- entry f []
                playBtn         <- button f [ text := "Play" ]
                helpBtn         <- button f [ text := "Help me!!" ]
                boardPanel      <- panel f []
                moveList        <- singleListBox f []
                recommendation  <- staticText f []

                set f [ layout :=  margin 10 $ row 2 [margin 10 $ column 4 [ minsize (sz width height) ( widget boardPanel )
                                                                           , margin 10 $ row 4 [ label "Move: "
                                                                                               , minsize (sz 200 20) (widget moveInput)
                                                                                               , widget playBtn
                                                                                               , widget helpBtn ]
                                                                           , margin 10 $ row 2 [label "Player: ", widget currentPlayer ]
                                                                           , margin 10 $ row 2 [label "Recommendation: ", hfill $ widget recommendation ]]
                                                     , vfill $ minsize (sz 120 height) $ widget moveList ]]

                let networkDescription :: forall t. Frameworks t => Moment t ()
                    networkDescription = do

                                            tickE  <- event0 t command

                                            playE <- event0 playBtn command

                                            helpE <- event0 helpBtn command

                                            moveIn <- behaviorText moveInput "Enter your move here (e.g. a6a5)"

                                            -- This behavior holds the state of the game
                                            let
                                                stateB :: Behavior t State
                                                stateB = accumB newState $ playTurn options <$> (moveIn <@ playE)

                                            -- This behavior holds the recommendation
                                            -- The new recommendation is computed on helpE - help button clicked.
                                            -- But the play button hides the recommendation - which is again made visible (after it has been updated
                                            -- if we click again on 'help')
                                            sink recommendation [ text :== stepper "Nothing" $ (last . getMoveHistoryFromState . helpPlayer <$> (stateB <@ helpE))
                                                                , visible :== accumB True $
                                                                                            ((pure False) <$ playE) `union`
                                                                                            ((pure True) <$ helpE)]



                                            -- Take care of the current player widget
                                            sink currentPlayer [text :== (show . getPlayer) <$> stateB ]

                                            -- Take care of the board
                                            sink boardPanel [on paint :== stepper (\_dc _ -> return ()) $
                                                     (drawGameState <$> stateB ) <@ (tickE `union` playE )]

                                            -- Take care of the move history widget
                                            let
                                                moveHistoryE :: Event t [String]
                                                moveHistoryE = (getMoveHistoryFromState <$> stateB) <@ (tickE `union` playE)
                                                moveHistoryB :: Behavior t [String]
                                                moveHistoryB = stepper [] moveHistoryE

                                            sink moveList [ items :== moveHistoryB ]

                                            reactimate $ repaint boardPanel <$ tickE
                                            reactimate $ repaint currentPlayer <$ tickE
                                            reactimate $ repaint recommendation <$ ( tickE `union` playE `union` helpE)

                network <- compile networkDescription
                actuate network

pieceBitmapFor :: Piece -> Bitmap ()
pieceBitmapFor piece = case pieceColor piece of White -> bitmap $ getDataFile $ 'W':show (pieceType piece) ++ ".png"
                                                Black -> bitmap $ getDataFile $ 'B':show (pieceType piece) ++ ".png"

drawBmp :: forall a. DC a -> Bitmap () -> Point -> IO ()
drawBmp dc bmp pos = drawBitmap dc bmp pos True []

drawPiece :: DC a -> (Point, Square) -> IO ()
drawPiece dc (pos, square) = maybe (return ()) doDraw square
            where doDraw piece = drawBmp dc (pieceBitmapFor piece) pos

drawBoard :: forall t a. DC a -> t -> IO ()
drawBoard dc _view = do
                        let
                            boardBorder :: Int
                            boardBorder = boardMargin `div` 2
                            boardSize :: Int
                            boardSize   = 8 * squaresSize
                            labelYx :: Int -> Int
                            labelYx p   = boardBorder + squaresSize `div` 2 + p * squaresSize
                            labelXy :: Int -> Int
                            labelXy p   = boardBorder + squaresSize `div` 2 + p * squaresSize
                            labelXx :: Int
                            labelXx     = boardBorder `div` 2
                            labelYy :: Int
                            labelYy     = boardBorder `div` 2

                            squares = [ drawRect dc (rect (point (pos2boardX x) (pos2boardY y)) (sz squaresSize squaresSize )) [brushKind := BrushSolid, brushColor := black] | x <- [0..7::Int], y <- [0..7::Int], (x + y) `mod` 2 /= 0]
                            -- X labels (letters) on the left
                            labelX = map (\(p,t) -> drawText dc [t] (point labelXx (labelXy p)) []) (zip [0..7::Int] ['0'..])
                            -- X labels (letters) on the right
                            labelX2 = map (\(p,t) -> drawText dc [t] (point (labelXx + boardSize + boardBorder) (labelXy p)) []) (zip [0..7::Int] ['0'..])
                            -- Y labels (numbers) on the top
                            labelY = map (\(p,t) -> drawText dc [t] (point (labelYx p) labelYy) []) (zip [0..7::Int] ['a'..])
                            -- Y labels (numbers) on the bottom
                            labelY2 = map (\(p,t) -> drawText dc [t] (point (labelYx p) (labelYy + boardSize + boardBorder)) []) (zip [0..7::Int] ['a'..])

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

        piecesToDraw = map (\ (y,x,p) -> (point (pos2boardX x + pieceMargin `div` 2) (pos2boardY y + pieceMargin `div` 2), p)) pieces

    -- draw the board
    drawBoard dc _view
    -- draw the pieces on the board
    mapM_ (drawPiece dc) piecesToDraw
