hsChess
=======

@ssoudan

This is a basic implementation of a chess playing program in Haskell.

# Description

- based on a minimax with Alpha-Beta pruning AI
- UI is in reactive-banana-wx

# Build instructions

You'll need to install reactive-banana-wx and wxhaskell. Once that is done, cabal build will do the rest and build a Mac OSX app in dist/build/hsChess.app/ with all the dependencies included.

# TODO

- [x] add manual strategy
- [x] improve UX
- [x] Mac packaging
- [x] store the history in the State rather than the latest state (using DList)
- [x] add castling moves
- [x] web UI or GUI
- [x] check detection and move selection
- [x] mate detection
- [x] show possible moves
- [x] parallelize the evaluation of the options
- [x] async computation of moves and push notification on completion
- [x] enter moves with DnD
- [ ] split the current turn into each player turn so we can show the move of real player more quickly and pave the way to PvP
- [ ] show when the soft is busy computing a move and disable interactions
- [ ] stalemate detection
- [ ] test moves (shows best response moves of each player up to the horizon for a given option)
- [ ] non-linear game history with bookmarks
- [ ] pawn conversion
- [ ] save/restore games
- [ ] start from predefined states
- [ ] gamification: Achievements to learn chess

# License 

This program is distributed under APLv2. Check the LICENSE file for more details.