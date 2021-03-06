module Output' (max_size, make_board, do_changes, thankyou)
   where


import Data'
import UI.HSCurses.Curses
import Data.Char
import Foreign.Storable

border_width :: Vector
border_width = 2

border_height :: Vector
border_height = 2

max_size :: IO (Vector, Vector)
max_size = do
              (height, width) <- scrSize
              let width' = (fromIntegral width `div` 2)
                  height' = fromIntegral height
              return (width' - 2 * border_width,height' - 2 * border_height)

make_board :: Vector -> Vector -> IO ()
make_board width height = do let c =  fromIntegral $ ord 'X'
                             mapM_ (write1 c) border
                             thu width height
                             --do_changes (take sq (replicate Off (m,n)))
                where border = [(x,border_height -1)| x <- xs] ++ 
                               [(x,border_height+ height)| x <- xs] ++
                               [(border_width-1,y)| y<- ys] ++
                               [(border_width+width, y)| y <- ys]
                      xs = [(border_width-1) .. (border_width + width)]
                      ys = [(border_height-1) .. (border_height +height)]
                      sq=length xs * length ys
                      
do_changes :: [Change] -> IO ()
do_changes cs = do mapM_ do_change cs
                   refresh
                   return ()

do_change :: Change -> IO () 	
do_change (On x y)= paint_square_c x y '#'
do_change (Off x y)= paint_square_c x y ' '
do_change Delay   = do timeout 500
                       _ <-getch
                       return ()

--thankyou :: Vector -> Vector ->IO ()
thu width height= do mapM_ (paint_square_cs ' ') [(x,y)| x<-[0.. width-1] , y<-[0 .. height-1]]
thankyou width height = do paint_square_c 1 3 'T'
                           paint_square_c 2 3 'h'
                           paint_square_c 3 3'a'
                           paint_square_c 4 3 'n'
                           paint_square_c 5 3 'k'
                           paint_square_c 6 3 ' '
                           paint_square_c 7 3 'u'
                           paint_square_c 1 5 'p'               
                           paint_square_c 2 5 'r'
                           paint_square_c 3 5 'e'
                           paint_square_c 4 5 's'
                           paint_square_c 5 5 's'
                           paint_square_c 6 5 ' '
                           paint_square_c 7 5 'q'
                           paint_square_c 8 5 ' '
                           paint_square_c 9 5 't'
                           paint_square_c 10 5 '0'
                           paint_square_c 11 5 ' '
                           paint_square_c 12 5 'e'
                           paint_square_c 13 5 'x'
                           paint_square_c 14 5 'i'
                           paint_square_c 15 5 't'

write1 :: ChType -> (Vector,Vector)-> IO()
write1 c (x,y)= write x y c

write :: Vector -> Vector -> ChType -> IO ()
write x y c = do mvAddCh y' x' c
              --   mvAddCh y' (x'+1) c
      where y' = fromIntegral y
            x' = fromIntegral $ 2*x

paint_square_cs :: Char->(Vector,Vector) -> IO ()
paint_square_cs c (x,y)= paint_square_c x y c
paint_square_c :: Vector -> Vector -> Char -> IO ()
paint_square_c x y c = paint_square x y (fromIntegral $ ord c )

paint_square :: Vector -> Vector ->ChType -> IO ()
paint_square x y c = write (x+border_width ) (y+border_height) c


