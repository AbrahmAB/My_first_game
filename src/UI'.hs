module UI' (init_ui,shutdown_ui,make_board, get_event, do_changes)
  where
import Data'
import UI.HSCurses.Curses
import Input'
import Output'
import Board
init_ui :: IO (Vector, Vector)
init_ui = do w <- initScr
	     cBreak True
	     echo False
	     keypad w True
	     cursSet CursorInvisible
	     max_size


shutdown_ui :: IO ()
shutdown_ui = do _<-endWin
                 print "Thank you for playing :)"
                -- print score
                 return ()


	     
