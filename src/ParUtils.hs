{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
 ParUtils.hs

 Copyright (c) 2014 by Sebastien Soudan.
 Apache License Version 2.0, January 2004
-}
module ParUtils where

import           Board
import           Move
import           State

import           Control.DeepSeq
import           Control.Monad.Par

instance NFData PieceType where
	rnf _ = ()

instance NFData PieceColor where
	rnf Black = ()
	rnf White = ()

instance NFData Board.Piece where
  rnf piece = rnf (pieceType piece) `seq` rnf (pieceColor piece)

instance NFData Pos where
  rnf (Pos (x,y)) = rnf x `seq` rnf y

instance NFData Move where
  rnf (Move source destination) = rnf source `seq`  rnf destination
  rnf CastleBlackRight = ()
  rnf CastleBlackLeft = ()
  rnf CastleWhiteRight = ()
  rnf CastleWhiteLeft = ()


instance NFData PlayerState where
  rnf playerState = rnf (canCastleRight playerState) `seq` rnf (canCastleLeft playerState)
                                                     `seq` rnf (isCheck playerState)
                                                     `seq` rnf (isCheckMate playerState)


instance NFData State where
  rnf state = rnf (getBoard state) `seq` rnf (getHistory state)
                                   `seq` rnf (getPlayer state)
                                   `seq` rnf (getWhiteState state)
                                   `seq` rnf (getBlackState state)


myParMap :: NFData b => (a -> b) -> [a] -> Par [b]
myParMap = parMap

myRunPar :: forall a. Par a -> a
myRunPar = runPar
