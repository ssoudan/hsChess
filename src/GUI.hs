{-# LANGUAGE ScopedTypeVariables #-}
module GUI where


import           Data.Maybe         (fromMaybe)
import           Debug.Trace
import           Graphics.UI.WX     hiding (Event)
import           Reactive.Banana
import           Reactive.Banana.WX


import           Board              (Piece (..), Square, piecePosition, Pos(..))
import           GUIUtils
import           Minimax            as M
import           MinimaxAlphaBeta   as AB
import           MinimaxLazy        as ML
import           MoveParser
import           Options
import           State
import           Utils
import           Move

height, width, pieceMargin, pieceSize, boardMargin, squaresSize, boardBorder, boardSize :: Int
pieceSize   = 64
pieceMargin = 8
boardMargin = 100
boardSize   = 8 * squaresSize
squaresSize = pieceSize + pieceMargin

height      = boardSize + boardMargin
width       = height + boardMargin
boardBorder = boardMargin `div` 2


playOpponentWithStrategy :: Maybe OpponentOption -> SuperState -> SuperState
playOpponentWithStrategy option state = case fromMaybe AB option of AB -> AB.doMove state
                                                                    ML -> ML.doMove state
                                                                    M -> M.doMove state


playTurn :: Options -> String -> SuperState -> SuperState
playTurn options move state = trace "playTurn" $ case parseMove move of Right r -> if validMove r state && not (isCurrentPlayerMate state)
                                                                                    then playOpponentWithStrategy (snd3 options) $ applyMove r (fst state) 
                                                                                    else state
                                                                        Left _ -> state

helpPlayer :: SuperState -> Maybe SuperState
helpPlayer state = if isCurrentPlayerMate state 
                    then Nothing 
                    else let state' = AB.doMove state
                          in trace (show state') $ Just state'

gui :: Options -> IO ()
gui options = start $ do
                f               <- frame [ text := "hsChess"
                                         , resizeable := False ]
                t               <- timer f [ interval := 500 ]
                currentPlayer   <- staticText f []
                currentPlayerStatus   <- staticText f []
                moveInput       <- entry f []
                playBtn         <- button f [ text := "Play" ]
                helpBtn         <- button f [ text := "Help me!!" ]
                boardPanel      <- panel f []
                moveList        <- singleListBox f []
                recommendation  <- staticText f []
                debug           <- staticText f []

                set f [ layout :=  margin 10 $ row 2 [ margin 10 $ column 6 [ minsize (sz width height) ( widget boardPanel )
                                                                            , margin 10 $ row 4 [ label "Move: "
                                                                                                , minsize (sz 200 20) (widget moveInput)
                                                                                                , widget playBtn
                                                                                                , widget helpBtn ]
                                                                            , margin 10 $ row 3 [ label "Player: "
                                                                                                , widget currentPlayer ]
                                                                            , margin 10 $ row 2 [ label "Status: "
                                                                                                , hfill $ widget currentPlayerStatus ]
                                                                            , margin 10 $ row 2 [ label "Recommendation: "
                                                                                                , hfill $ widget recommendation ]
                                                                            , margin 10 $ row 2 [ label "DEBUG: "
                                                                                                , hfill $ widget debug ]]
                                                     , vfill $ minsize (sz 120 height) $ widget moveList ]]

                let networkDescription :: forall t. Frameworks t => Moment t ()
                    networkDescription = do

                                            tickE  <- event0 t command

                                            playE  <- event0 playBtn command

                                            helpE  <- event0 helpBtn command

                                            moveInB <- behaviorText moveInput ""
                                                                                    
                                            -- mouse pointer position
                                            mouseE <- event1 boardPanel mouse   -- mouse events
                                            let 
                                                mouseB = stepper (point 0 0) (filterJust $ justMotion <$> mouseE)

                                                activeCellB :: Behavior t (Maybe Pos)
                                                activeCellB = cellFromPoint <$> mouseB

                                            sink debug [ text :== stepper "" $ show <$> (activeCellB <@ mouseE) ]

                                            -- Catch 'Return' key on 'moveInput' to later use it has an event similar to 'playE'
                                            moveInE <- event1 moveInput keyboard
                                            let moveInValidatedE = pure (const ()) <@> filterE ((== KeyReturn ) . keyKey) moveInE

                                            -- This behavior holds the state of the game
                                            let
                                                stateB :: Behavior t SuperState
                                                stateB = accumB newSuperState $ playTurn options <$> (moveInB <@ (playE `union` moveInValidatedE))

                                            -- This behavior holds the recommendation
                                            -- The new recommendation is computed on helpE - help button clicked.
                                            -- But the play button hides the recommendation - which is again made visible (after it has been updated
                                            -- if we click again on 'help')
                                            sink recommendation [ text :== stepper "Nothing" $ (maybe "Start a new game..." (last . getMoveHistoryFromState . fst) . helpPlayer <$> (stateB <@ helpE))
                                                                , visible :== accumB True $
                                                                                            (pure False <$ playE) `union`
                                                                                            (pure True <$ helpE)]

                                            -- Take care of the current player widget
                                            sink currentPlayer [ text :== (show . getPlayer . fst) <$> stateB ]

                                            -- Take case of the current player status widget
                                            sink currentPlayerStatus [ text :== (showPlayerState . getCurrentPlayerState ) <$> stateB ]

                                            -- Take care of the board
                                            sink boardPanel [ on paint :== stepper (\_dc _ -> return ()) $
                                                     (drawGameState <$> (GameState <$> stateB <*> activeCellB)) <@ unions [ tickE, playE, moveInValidatedE ]]

                                            -- Hide the play button in case the player is mate
                                            sink playBtn [ visible :== (not . isCurrentPlayerMate ) <$> stateB ]

                                            -- Take care of the move history widget
                                            let
                                                moveHistoryE :: Event t [String]
                                                moveHistoryE = (getMoveHistoryFromState . fst <$> stateB) <@ unions [ tickE, playE, moveInValidatedE ]
                                                moveHistoryB :: Behavior t [String]
                                                moveHistoryB = stepper [] moveHistoryE

                                            sink moveList [ items :== moveHistoryB ]

                                            reactimate $ repaint debug <$ tickE
                                            reactimate $ repaint boardPanel <$ tickE
                                            reactimate $ repaint currentPlayer <$ tickE
                                            reactimate $ repaint currentPlayerStatus <$ tickE
                                            reactimate $ repaint recommendation <$ unions [ tickE, playE, moveInValidatedE, helpE]

                network <- compile networkDescription
                actuate network

justMotion :: EventMouse -> Maybe Point
justMotion (MouseMotion p _) = Just p
justMotion _                 = Nothing

cellFromPoint :: Point -> Maybe Pos
cellFromPoint (Point x y) = let xx = (x - boardBorder) `div` squaresSize
                                yy = (y - boardBorder) `div` squaresSize
                             in if xx >= 0 && xx <= 7 && yy >= 0 && yy <= 7
                                  then Just $ Pos (yy,xx)
                                  else Nothing

showPlayerState :: PlayerState -> String
showPlayerState playerState | isCheckMate playerState = "CheckMate!"
                            | isCheck playerState = "Check!"
                            | otherwise = "So far so good"

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

data GameState = GameState { getSuperState :: SuperState, getMousePosition :: Maybe Pos }

-- | Draw a 'Move'
drawMove :: DC a -> Move -> IO ()
drawMove dc move = do 
                    let Pos (ys, xs) = getSource move
                        Pos (yd, xd) = getDestination move
                    line dc (point (pos2Board xs + squaresSize `div` 2) (pos2Board ys + squaresSize `div` 2))
                            (point (pos2Board xd + squaresSize `div` 2) (pos2Board yd + squaresSize `div` 2))
                            [color := red]

-- | Draw a list of 'Move'
drawPossibleMoves :: [Move] -> DC a -> IO ()
drawPossibleMoves possibleMoves dc = mapM_ (drawMove dc) possibleMoves

-- | Draw game state
drawGameState :: GameState -> DC a -> b -> IO ()
drawGameState state dc _view = trace "drawGameState" $ do
    let
        board = (getBoard . fst) (getSuperState state)
        pieces = piecePosition board

        piecesToDraw = map (\ (y,x,p) -> (point (pos2Board x + pieceMargin `div` 2) (pos2Board y + pieceMargin `div` 2), p)) pieces
        possibleMoves = case getMousePosition state of Nothing -> trace "out!" []
                                                       Just p -> trace "filtering" filter ((== p) . getSource) $ (snd . getSuperState) state

    -- draw the board
    drawBoard dc _view
    -- draw the pieces on the board
    mapM_ (drawPiece dc) piecesToDraw

    drawPossibleMoves possibleMoves dc
