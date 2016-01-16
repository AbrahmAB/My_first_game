module Input' (get_event)
  where

--import Curses
import UI.HSCurses.Curses
import Data'
import Foreign.C.Types
import System.Time
--import System.Timeout
import Data.Char
import Data.List


get_event :: Delay -> IO (Event, Delay)
get_event delay
    | delay' <= 0 = error "Input.get_event: delay <= 0"
    | otherwise = do timeout delay'
                     start <- getClockTime
                     c <- getch
                     end <- getClockTime
                     return (key_to_event c , elapsed_time start end)
    where delay' = fromIntegral delay

key_to_event :: CInt -> Event
key_to_event k = maybe None id (lookup k key_events)

key_events :: [(CInt,Event)]
key_events = [(cERR,Tick)]++ movement ++ rotations ++ control
    where to_event e = map (\ c -> (c,e))
          conv_char = fromIntegral . ord
          movement = lefts ++ rights ++ downs ++ drops
          rotations = rot_lefts ++ rot_rights
          control = quits
          lefts = to_event MLeft [conv_char 'j',fromIntegral cKEY_LEFT] 
          rights = to_event MRight [conv_char 'l',fromIntegral cKEY_RIGHT]
          downs = to_event MDown [conv_char 'k',fromIntegral cKEY_DOWN]
          drops = to_event Drop [conv_char ' ']
          rot_lefts = to_event RotR [conv_char 'u']
          rot_rights = to_event RotL [conv_char 'i'] 
          quits = to_event Quit [conv_char 'q']

elapsed_time :: ClockTime ->ClockTime -> Delay
elapsed_time start end = t `max` 0
    where t = case diffClockTimes end start of
                   (TimeDiff 0 0 0 0 0 secs psecs) -> let secs' = 1000 * fromIntegral secs
                                                          psecs' = fromIntegral (psecs `div` 1000000000)
                                                      in  secs' + psecs'
                   td                             -> error ("Input.elapsed_time: "++ show td)

          
