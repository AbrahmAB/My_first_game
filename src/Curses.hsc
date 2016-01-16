{-# LANGUAGE ForeignFunctionInterface #-}
-- vim: set syntax=haskell tw=72:

-- Part of Hetris

#include <curses.h>

module Curses (PWindow,
               ChType,
               cERR,
               cKEY_UP,
               cKEY_DOWN,
               cKEY_LEFT,
               cKEY_RIGHT,
               cTRUE,
               cACS_BLOCK,
               initscr,
               cbreak,
               noecho,
               getch,
               nonl,
               halfdelay,
               intrflush,
               keypad,
               stdscr,
               timeout,
               curs_set,
               mvaddstr,
               mvaddch,
               addstr,
               refresh,
               endwin,
               getmaxyx,
               move,
               errI,
               errP,
               ) where

import Foreign
import Foreign.C

data Window = Window
type PWindow = Ptr Window
type NBool = #type bool
type ChType = #type chtype

cERR :: CInt
cERR = #const ERR
cKEY_UP, cKEY_DOWN, cKEY_LEFT, cKEY_RIGHT :: ChType
cKEY_UP = #const KEY_UP
cKEY_DOWN = #const KEY_DOWN
cKEY_LEFT = #const KEY_LEFT
cKEY_RIGHT = #const KEY_RIGHT
cTRUE :: NBool
cTRUE = #const TRUE
cACS_BLOCK :: ChType
cACS_BLOCK = #const ACS_BLOCK
foreign import ccall unsafe "static curses.h initscr" initscr :: IO PWindow
foreign import ccall unsafe "static curses.h cbreak" cbreak :: IO CInt
foreign import ccall unsafe "static curses.h noecho" noecho :: IO CInt
foreign import ccall unsafe "static curses.h getch" getch :: IO CInt
foreign import ccall unsafe "static curses.h nonl" nonl :: IO CInt
foreign import ccall unsafe "static curses.h halfdelay" halfdelay :: CInt -> IO CInt
foreign import ccall unsafe "static curses.h intrflush" intrflush :: PWindow -> CInt -> IO CInt
foreign import ccall unsafe "static curses.h keypad" keypad :: PWindow -> NBool -> IO CInt
foreign import ccall unsafe "static curses.h &stdscr" stdscr :: Ptr PWindow
foreign import ccall unsafe "static curses.h timeout" timeout :: CInt -> IO ()
foreign import ccall unsafe "static curses.h mvaddstr" mvaddstr :: CInt -> CInt -> CString -> IO ()
foreign import ccall unsafe "static curses.h mvaddch" mvaddch :: CInt -> CInt -> ChType -> IO ()
foreign import ccall unsafe "static curses.h addstr" addstr :: CString -> IO ()
foreign import ccall unsafe "static curses.h refresh" refresh :: IO CInt
foreign import ccall unsafe "static curses.h move" move :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "static curses.h curs_set" curs_set :: CInt -> IO CInt
foreign import ccall unsafe "static curses.h endwin" endwin :: IO CInt
foreign import ccall unsafe "static wrap.h w_getmaxyx" wgetmaxyx :: PWindow -> Ptr CInt -> Ptr CInt -> IO ()
getmaxyx :: PWindow -> IO (CInt, CInt)
getmaxyx w = alloca $ \py ->
             alloca $ \px ->
             do wgetmaxyx w py px
                y <- peek py
                x <- peek px
                return (y, x)

errI :: IO CInt -> IO ()
errI f = do r <- f
            if r == cERR then do _ <- endwin
                                 error "curses returned an error"
                         else return ()

errP :: IO (Ptr a) -> IO ()
errP f = do p <- f
            if p == nullPtr then do _ <- endwin
                                    error "curses returned an error"
                            else return ()
