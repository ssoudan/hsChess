{-
 GUI.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GUI where

import           Data.Maybe         (fromMaybe)
import           Debug.Trace
import           Graphics.UI.WX     hiding (Event)
import           Reactive.Banana
import           Reactive.Banana.WX

import           Control.Concurrent

import           Board              (Piece (..), Pos (..), Square,
                                     piecePosition)
import           GUIUtils
import           Minimax            as M
import           MinimaxAlphaBeta   as AB
import           MinimaxLazy        as ML
import           Move
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

---------------
-- Play turns
---------------

union :: [Event a] -> Event a
union xs = foldr1 (unionWith (curry fst)) xs

-- | Async computation of the moves (player + AI)
asyncPlayTurn :: (Handler SuperState) -> Options -> String -> SuperState -> IO ()
asyncPlayTurn fireS options m s = do
                                    _ <- ($)
                                        forkIO $ do
                                                    let !nS = playTurn options m s
                                                    trace "computation done" $ fireS nS
                                    trace "launched playTurn computation in a different thread" $ return ()

-- | Do the player's move plus the AI's move
playTurn :: Options -> String -> SuperState -> SuperState
playTurn options move state = trace "playTurn" $ case parseMove move of Right r -> if validMove r state
                                                                                    then let stateAfterWhiteTurn = applyMove r (fst state)
                                                                                          in if isCurrentPlayerMate stateAfterWhiteTurn
                                                                                              then stateAfterWhiteTurn
                                                                                              else playOpponentWithStrategy (snd3 options) stateAfterWhiteTurn
                                                                                    else state
                                                                        Left _ -> state
                        where -- Play AI move
                              playOpponentWithStrategy :: Maybe OpponentOption -> SuperState -> SuperState
                              playOpponentWithStrategy option s = case fromMaybe AB option of AB -> AB.doMove s
                                                                                              ML -> ML.doMove s
                                                                                              M  ->  M.doMove s


---------------
-- Helper
---------------

-- | Compute the recommendation for the player based on current state
helpPlayer :: SuperState -> Maybe SuperState
helpPlayer state = if isCurrentPlayerMate state
                    then Nothing
                    else let state' = AB.doMove state
                          in trace (show state') $ Just state'


---------------
-- The event network
---------------

-- | entry point for the UI - build and actuate the event network
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

                -- Define the layout of the main frame.
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

                -- Create a new handler for the async computation of the new state
                (addHandlerS, fireS) <- newAddHandler

                -- Define the network of events
                let networkDescription :: MomentIO ()
                    networkDescription = do

                                            -- Create a behavior from the handler
                                            -- This behavior holds the state of the game
                                            stateB  <- fromChanges newSuperState addHandlerS

                                            -- Clock ticks events
                                            tickE   <- event0 t command

                                            -- Clicks on play button events
                                            playE   <- event0 playBtn command

                                            -- Clicks on help button events
                                            helpE   <- event0 helpBtn command

                                            -- Text entered in move textarea
                                            moveInB <- behaviorText moveInput ""

                                            -- All mouse related events
                                            mouseE <- event1 boardPanel mouse

                                            -- Mouse pointer position behavior
                                            (mouseB :: Behavior Point) <- stepper (point 0 0) (filterJust $ justMotion <$> mouseE)

                                            let
                                                -- Behavior holding the currently hovered board cell (if relevant)
                                                activeCellB :: Behavior (Maybe Pos)
                                                activeCellB = posFromPoint <$> mouseB

                                            -- Drag and drop
                                            -- Current state of the DnD process
                                            (dndStateE :: Event DnD) <- accumE initialDnDState $ updateDnDFSM <$> mouseE
                                            let
                                                -- Only the completed DnD events
                                                completedDndE :: Event DnD
                                                completedDndE = filterE ((== Complete) . getDndState) dndStateE

                                            debugTextB <- stepper "" $ show <$> completedDndE 

                                            sink debug [ text :== debugTextB ]

                                            -- Catch 'Return' key on 'moveInput' to later use it has an event similar to 'playE'
                                            moveInE <- event1 moveInput keyboard
                                            let
                                                moveInValidatedE :: Event ()
                                                moveInValidatedE = pure (const ()) <@> filterE ((== KeyReturn ) . keyKey) moveInE

                                            -- This behavior holds the recommendation
                                            -- The new recommendation is computed on helpE - help button clicked.
                                            -- But the play button hides the recommendation - which is again made visible
                                            -- (after it has been updated if we click again on 'help')
                                            recommendationTextB <- stepper "Nothing" $ (maybe "Start a new game..." (last . getMoveHistoryFromState . fst) . helpPlayer <$> (stateB <@ helpE))
                                            recommendationVisibleB <- accumB True $  unions [(pure False <$ playE), (pure True  <$ helpE)]
                                            sink recommendation [ text :== recommendationTextB
                                                                , visible :== recommendationVisibleB
                                                                ]

                                            -- Take care of the current player widget
                                            sink currentPlayer [ text :== (show . getPlayer . fst) <$> stateB ]

                                            -- Take case of the current player status widget
                                            sink currentPlayerStatus [ text :== (showPlayerState . getCurrentPlayerState ) <$> stateB ]

                                            -- Take care of the board
                                            boardPanelOnPaintB <- stepper (\_dc _ -> return ()) $ (drawGameState <$> (GameState <$> stateB <*> activeCellB)) <@ union [ tickE, playE, moveInValidatedE ] 
                                            sink boardPanel [ on paint :== boardPanelOnPaintB]

                                            -- Hide the play button in case the player is mate
                                            sink playBtn [ visible :== (not . isCurrentPlayerMate ) <$> stateB ]

                                            -- Take care of the move history widget
                                            let
                                                moveHistoryE :: Event [String]
                                                moveHistoryE = (getMoveHistoryFromState . fst <$> stateB) <@ union [ playE
                                                                                                                    , moveInValidatedE
                                                                                                                    , tickE ]
                                                
                                            (moveHistoryB :: Behavior [String]) <- stepper [] moveHistoryE

                                            sink moveList [ items :== moveHistoryB ]

                                            -- Take care of the DnD driven moves
                                            let
                                                asyncPlayTurnFromDnD :: SuperState -> DnD -> IO ()
                                                asyncPlayTurnFromDnD s m = asyncPlayTurn fireS options (dndToMove m) s

                                                asyncPlayTurnFromDnDB :: Behavior (DnD -> IO ())
                                                asyncPlayTurnFromDnDB = asyncPlayTurnFromDnD <$> stateB

                                                asyncPlayTurnFromDnDE :: Event (IO ())
                                                asyncPlayTurnFromDnDE = asyncPlayTurnFromDnDB <@> completedDndE

                                            reactimate $ asyncPlayTurnFromDnDE

                                            -- Take care of the manually entered moves (still needed for castling)
                                            let
                                                asyncPlayTurnB :: Behavior (IO ())
                                                asyncPlayTurnB = asyncPlayTurn fireS options <$> moveInB <*> stateB

                                            reactimate $ asyncPlayTurnB <@ (union [playE, moveInValidatedE])

                                            reactimate $ repaint boardPanel             <$ union [ tickE, playE, moveInValidatedE ]
                                            reactimate $ repaint currentPlayer          <$ tickE
                                            reactimate $ repaint currentPlayerStatus    <$ tickE
                                            reactimate $ repaint recommendation         <$ union [ tickE, playE, moveInValidatedE, helpE]

                network <- compile networkDescription
                -- networkRepr <- showNetwork network
                -- putStrLn networkRepr
                actuate network

-- | Filters 'EventMouse' to keep only the 'MouseMotion'
justMotion :: EventMouse -> Maybe Point
justMotion (MouseMotion p _) = Just p
justMotion _                 = Nothing

-- | Convert the PlayerState to a proper text message
showPlayerState :: PlayerState -> String
showPlayerState playerState | isCheckMate playerState   = "CheckMate!"
                            | isCheck playerState       = "Check!"
                            | otherwise                 = "So far so good"

---------------
-- Draw the board
---------------

-- | Find the right 'Bitmap' to represent a 'Piece'
pieceBitmapFor :: Piece -> Bitmap ()
pieceBitmapFor piece = bitmap $ getDataFile $ show (pieceColor piece) ++ show (pieceType piece) ++ ".png"

-- | Draw a 'Bitmap' somewhere in the window
drawBmp :: forall a. DC a -> Bitmap () -> Point -> IO ()
drawBmp dc bmp pos = drawBitmap dc bmp pos True []

-- | Draw a 'Square' at a given 'Point'
drawPiece :: DC a -> (Point, Square) -> IO ()
drawPiece dc (pos, square) = maybe (return ()) doDraw square
            where doDraw piece = drawBmp dc (pieceBitmapFor piece) pos

data Position = N | E | S | W

-- | Draw the empty board
drawBoard :: forall t a. DC a -> t -> IO ()
drawBoard dc _view = do
                        let
                            labelPosition :: Int -> Int
                            labelPosition p   = boardBorder + squaresSize `div` 2 + p * squaresSize                            
                            labelFixedPosition :: Int
                            labelFixedPosition = boardBorder `div` 2
                            labelFixedPositionOtherSide :: Int
                            labelFixedPositionOtherSide = labelFixedPosition + boardSize + boardBorder
                            figures :: [(Int, Char)]
                            figures = zip [0..7::Int] ['0'..]
                            letters :: [(Int, Char)]
                            letters = zip [0..7::Int] ['a'..]

                            placeAt :: Position -> Int -> Point
                            placeAt GUI.W p = point labelFixedPosition (labelPosition p)
                            placeAt GUI.E p = point labelFixedPositionOtherSide (labelPosition p)
                            placeAt GUI.N p = point (labelPosition p) labelFixedPosition
                            placeAt GUI.S p = point (labelPosition p) labelFixedPositionOtherSide

                            -- The board itself
                            squares = [ drawRect dc (rect (point (pos2Board x) (pos2Board y)) (sz squaresSize squaresSize )) [brushKind := BrushSolid, brushColor := black] | x <- [0..7::Int], y <- [0..7::Int], (x + y) `mod` 2 /= 0]
                            -- X labels (letters) on the left
                            labelX  = map (\(p,t) -> drawText dc [t] (placeAt GUI.W p) []) figures
                            -- X labels (letters) on the right
                            labelX2 = map (\(p,t) -> drawText dc [t] (placeAt GUI.E p) []) figures
                            -- Y labels (numbers) on the top
                            labelY  = map (\(p,t) -> drawText dc [t] (placeAt GUI.N p) []) letters
                            -- Y labels (numbers) on the bottom
                            labelY2 = map (\(p,t) -> drawText dc [t] (placeAt GUI.S p) []) letters

                        -- do draw the label and the squares all at once
                        sequence_ $ labelX ++ labelX2 ++ labelY ++ labelY2 ++ squares

                        -- add a border to the board
                        drawRect dc (rect (point boardBorder boardBorder) (sz boardSize boardSize)) []

-- | Convert a 'Point' (coordinate in the window) to a 'Maybe Pos' (position on the board - if relevant)
posFromPoint :: Point -> Maybe Pos
posFromPoint (Point x y) = let xx = (x - boardBorder) `div` squaresSize
                               yy = (y - boardBorder) `div` squaresSize
                            in if xx >= 0 && xx <= 7 && yy >= 0 && yy <= 7
                                then Just $ Pos (yy, xx)
                                else Nothing

-- | Convert a Pos coordinate into a pixel position in the window corresponding to the corner of the square
pos2Board :: Int -> Int
pos2Board x = x * squaresSize + (boardMargin `div` 2)


---------------
-- Game state
---------------
data GameState = GameState { getSuperState    :: SuperState
                           , getMousePosition :: Maybe Pos
                           }

-- | Draw a 'Move' as a possible move.
drawPossibleMove :: DC a -> Move -> IO ()
drawPossibleMove dc move = do
                            let Pos (yd, xd) = getDestination move
                            circle dc
                                  (point (pos2Board xd + squaresSize `div` 2) 
                                         (pos2Board yd + squaresSize `div` 2))
                                  pieceMargin [ color := green
                                              , brush := BrushStyle BrushSolid green ]

-- | Draw a list of 'Move'
drawPossibleMoves :: [Move] -> DC a -> IO ()
drawPossibleMoves possibleMoves dc = mapM_ (drawPossibleMove dc) possibleMoves

-- | Draw game state
drawGameState :: GameState -> DC a -> b -> IO ()
drawGameState state dc _view = trace "drawGameState" $ do
    let
        board = (getBoard . fst) (getSuperState state)
        pieces = piecePosition board

        piecesToDraw = map (\ (y,x,p) -> (point (pos2Board x + pieceMargin `div` 2) (pos2Board y + pieceMargin `div` 2), p)) pieces
        possibleMoves = case getMousePosition state of Nothing -> []
                                                       Just p  -> filter ((== p) . getSource) $ (snd . getSuperState) state

    -- draw the board
    drawBoard dc _view

    -- draw the pieces on the board
    mapM_ (drawPiece dc) piecesToDraw

    -- draw legal moves
    drawPossibleMoves possibleMoves dc


---------------
-- Drag n Drop
---------------
data DnDState = Inactive
              | InProgress
              | Complete
              deriving (Show, Eq)

data DnD = DnD { getDndState       :: DnDState
               , getDndOrigin      :: Maybe Pos
               , getDnDDestination :: Maybe Pos
               } deriving Show


-- | Create a new initial DnD
initialDnDState :: DnD
initialDnDState = DnD Inactive Nothing Nothing

-- | FSM for the DnD
updateDnDFSM :: EventMouse -> DnD -> DnD
updateDnDFSM mouseEvent state = case (mouseEvent, getDndState state) of
                              (_, Complete)                 -> DnD Inactive                 -- get ready for a new DnD
                                                                       Nothing
                                                                       Nothing
                              (MouseLeftUp p _, InProgress) -> DnD Complete                 -- we change the state
                                                                       (getDndOrigin state) -- keep the source
                                                                       (posFromPoint p)     -- but update the destination
                              (MouseLeftDrag p _, Inactive) -> DnD InProgress               -- new DnD started
                                                                       (posFromPoint p)     -- record the origin
                                                                       Nothing
                              (_, _)                        -> state

-- | Convert a (complete) 'DnD' into the 'String' representing a 'Move'
dndToMove :: DnD -> String
dndToMove (DnD Complete (Just source) (Just dest)) = let m = makeMove source dest
                                                      in trace ("dnd move is " ++ show m) show m
dndToMove (DnD _ _ _) = ""
