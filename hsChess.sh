#!/bin/bash

osascript <<END 
tell application "Terminal"
    do script "cd \"`dirname $0`/../MacOS\"; ./hsChess ; exit"
						end tell
						END
