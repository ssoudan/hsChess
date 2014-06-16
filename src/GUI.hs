{-# LANGUAGE ScopedTypeVariables #-}
module GUI where


import           Data.Maybe         (fromMaybe)
import           Debug.Trace
import           Graphics.UI.WX     hiding (Event)
import           Reactive.Banana
import           Reactive.Banana.WX


import           Board              (Piece (..), Square, piecePosition)
import           GUIUtils
import           Minimax            as M
import           MinimaxAlphaBeta   as AB
import           MinimaxLazy        as ML
import           MoveParser
import           Options
import           State
import           Utils

height, width, pieceMargin, pieceSize, boardMargin, squaresSize, boardBorder, boardSize :: Int
pieceSize   = 64
pieceMargin = 8
boardMargin = 100
boardSize   = 8 * squaresSize
squaresSize = pieceSize + pieceMargin

height      = boardSize + boardMargin
width       = height + boardMargin
boardBorder = boardMargin `div` 2


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
                                         , resizeable := False ]
                t               <- timer f [ interval := 500 ]
                currentPlayer   <- staticText f []
                moveInput       <- entry f []
                playBtn         <- button f [ text := "Play" ]
                helpBtn         <- button f [ text := "Help me!!" ]
                boardPanel      <- panel f []
                moveList        <- singleListBox f []
                recommendation  <- staticText f []

                set f [ layout :=  margin 10 $ row 2 [ margin 10 $ column 4 [ minsize (sz width height) ( widget boardPanel )
                                                                            , margin 10 $ row 4 [ label "Move: "
                                                                                                , minsize (sz 200 20) (widget moveInput)
                                                                                                , widget playBtn
                                                                                                , widget helpBtn ]
                                                                            , margin 10 $ row 2 [ label "Player: "
                                                                                                , widget currentPlayer ]
                                                                            , margin 10 $ row 2 [ label "Recommendation: "
                                                                                                , hfill $ widget recommendation ]]
                                                     , vfill $ minsize (sz 120 height) $ widget moveList ]]

                let networkDescription :: forall t. Frameworks t => Moment t ()
                    networkDescription = do

                                            tickE  <- event0 t command

                                            playE  <- event0 playBtn command

                                            helpE  <- event0 helpBtn command

                                            moveInB <- behaviorText moveInput ""
                                            
                                            -- Catch 'Return' key on 'moveInput' to later use it has an event similar to 'playE'
                                            moveInE <- event1 moveInput keyboard
                                            let moveInValidatedE = pure (const ()) <@> filterE ((== KeyReturn ) . keyKey) moveInE

                                            -- This behavior holds the state of the game
                                            let
                                                stateB :: Behavior t State
                                                stateB = accumB newState $ playTurn options <$> (moveInB <@ (playE `union` moveInValidatedE))

                                            -- This behavior holds the recommendation
                                            -- The new recommendation is computed on helpE - help button clicked.
                                            -- But the play button hides the recommendation - which is again made visible (after it has been updated
                                            -- if we click again on 'help')
                                            sink recommendation [ text :== stepper "Nothing" $ (last . getMoveHistoryFromState . helpPlayer <$> (stateB <@ helpE))
                                                                , visible :== accumB True $
                                                                                            (pure False <$ playE) `union`
                                                                                            (pure True <$ helpE)]

                                            -- Take care of the current player widget
                                            sink currentPlayer [ text :== (show . getPlayer) <$> stateB ]

                                            -- Take care of the board
                                            sink boardPanel [ on paint :== stepper (\_dc _ -> return ()) $
                                                     (drawGameState <$> stateB ) <@ (unions [ tickE, playE, moveInValidatedE ])]

                                            -- Take care of the move history widget
                                            let
                                                moveHistoryE :: Event t [String]
                                                moveHistoryE = (getMoveHistoryFromState <$> stateB) <@ (unions [ tickE, playE, moveInValidatedE ])
                                                moveHistoryB :: Behavior t [String]
                                                moveHistoryB = stepper [] moveHistoryE

                                            sink moveList [ items :== moveHistoryB ]

                                            reactimate $ repaint boardPanel <$ tickE
                                            reactimate $ repaint currentPlayer <$ tickE
                                            reactimate $ repaint recommendation <$ (unions [ tickE, playE, moveInValidatedE, helpE])

                network <- compile networkDescription
                actuate network

pieceBitmapFor :: Piece -> Bitmap ()
pieceBitmapFor piece = bitmap $ getDataFile $ show (pieceColor piece) ++ show (pieceType piece) ++ ".png"

drawBmp :: forall a. DC a -> Bitmap () -> Point -> IO ()
drawBmp dc bmp pos = drawBitmap dc bmp pos True []

drawPiece :: DC a -> (Point, Square) -> IO ()
drawPiece dc (pos, square) = maybe (return ()) doDraw square
            where doDraw piece = drawBmp dc (pieceBitmapFor piece) pos

-- | Draw the empty board
drawBoard :: forall t a. DC a -> t -> IO ()
drawBoard dc _view = do
                        let
                            labelPosition :: Int -> Int
                            labelPosition p   = boardBorder + squaresSize `div` 2 + p * squaresSize                            
                            labelFixedPosition :: Int
                            labelFixedPosition = boardBorder `div` 2                            
                            figures :: [(Int, Char)]
                            figures = zip [0..7::Int] ['0'..]
                            letters :: [(Int, Char)]
                            letters = zip [0..7::Int] ['a'..]

                            -- The board itself
                            squares = [ drawRect dc (rect (point (pos2Board x) (pos2Board y)) (sz squaresSize squaresSize )) [brushKind := BrushSolid, brushColor := black] | x <- [0..7::Int], y <- [0..7::Int], (x + y) `mod` 2 /= 0]
                            -- X labels (letters) on the left
                            labelX  = map (\(p,t) -> drawText dc [t] (point labelFixedPosition (labelPosition p)) []) figures
                            -- X labels (letters) on the right
                            labelX2 = map (\(p,t) -> drawText dc [t] (point (labelFixedPosition + boardSize + boardBorder) (labelPosition p)) []) figures
                            -- Y labels (numbers) on the top
                            labelY  = map (\(p,t) -> drawText dc [t] (point (labelPosition p) labelFixedPosition) []) letters
                            -- Y labels (numbers) on the bottom
                            labelY2 = map (\(p,t) -> drawText dc [t] (point (labelPosition p) (labelFixedPosition + boardSize + boardBorder)) []) letters

                        -- do draw the label and the squares all at once
                        sequence_ $ labelX ++ labelX2 ++ labelY ++ labelY2 ++ squares
                        -- add a border to the board
                        drawRect dc (rect (point boardBorder boardBorder) (sz boardSize boardSize)) []


pos2Board :: Int -> Int
pos2Board x = x * squaresSize + (boardMargin `div` 2)

drawGameState :: State -> DC a -> b -> IO ()
drawGameState state dc _view = do
    let
        board = getBoard state
        pieces = piecePosition board

        piecesToDraw = map (\ (y,x,p) -> (point (pos2Board x + pieceMargin `div` 2) (pos2Board y + pieceMargin `div` 2), p)) pieces

    -- draw the board
    drawBoard dc _view
    -- draw the pieces on the board
    mapM_ (drawPiece dc) piecesToDraw
