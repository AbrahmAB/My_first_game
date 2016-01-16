module Board (Board,create_board,get_changes,can_down,next_piece)
 where

import Data'
import Pieces
import Data.Array

type Block = Bool
type PlayingArea = Array (Vector,Vector) Block
data Board = Board PlayingArea Piece Vector Vector

restricted_blocks :: PlayingArea -> Piece -> Vector -> Vector -> [(Vector,Vector)]
restricted_blocks a p x y = filter (inRange ( bounds a)) [(x+off_x,y+off_y) | (off_x,off_y) <- blocks p ]

alter_blocks :: (Vector -> Vector -> Change) -> PlayingArea -> Piece -> Vector -> Vector -> [Change]
alter_blocks f a p x y = map (uncurry f) (restricted_blocks a p x y)

on :: PlayingArea -> Piece -> Vector -> Vector -> [Change]
on = alter_blocks On

off :: PlayingArea -> Piece -> Vector -> Vector -> [Change]
off = alter_blocks Off

create_board :: Vector -> Vector -> Piece -> (Board,[Change])
create_board width height p = (b,on a p (width `div` 2) 0)
  where a = listArray ((0,0),(width-1,height-1)) (repeat False)
	b = Board a p (width `div` 2) 0

get_changes :: Board -> Event -> (Board,[Change])
get_changes b@(Board a p x y) MDown
	| can_down b = (Board a p x (y+1), off a p x y ++  on a p x (y+1) )
get_changes b@(Board a p x y) MLeft
	| can_left b = (Board a p (x-1) y,off a p x y ++ on a p (x-1) y)
get_changes b@(Board a p x y) MRight
	| can_right b= (Board a p (x+1) y, off a p x y ++ on a p (x+1) y)
get_changes b Tick= get_changes b MDown

get_changes b Drop 
	| can_down b = (b'',cs1++cs2)
	   where (b',cs1)= get_changes b MDown
	         (b'',cs2)= get_changes b' Drop

get_changes b RotL= rotate rot_left b
get_changes b RotR = rotate rot_right b

get_changes b _ = (b,[])

rotate :: (Piece -> Piece ) -> Board -> (Board,[Change])
rotate f (Board a p x y)
	| fits b' = (b' , off a p x y ++ on a p' x y)
	where p' = f p
	      b' = Board a p' x y
rotate _ b = (b,[])

fits :: Board -> Bool
fits b@(Board a p x y) = not_collides b && y + extent_down p <= (snd (snd (bounds a))) && x - extent_left p >= (fst (fst (bounds a))) && x+ extent_right p<= (fst (snd (bounds a)))

not_collides :: Board -> Bool
not_collides (Board a p x y)= not $ or $ map (a !) $ restricted_blocks a p x y

can_down (Board a p x y) = fits (Board a p x (y+1))
can_left (Board a p x y) = fits (Board a p (x-1) y)
can_right (Board a p x y) = fits (Board a p (x+1) y)

next_piece :: Board -> Piece -> (Maybe Board, [Change],Int)
next_piece (Board a p x y) p' = (if not_collides b' then Just b' else Nothing , cs ++ on a'' p' x' y',score)
	where a' = a // zip (restricted_blocks a p x y) (repeat True)
	      (a'' , cs,score) = drop_complete_lines a' score
	      b' = Board a'' p' x' y'
	      ((xmin,ymin),(xmax,_)) = bounds a
	      x' = (xmin+xmax) `div` 2
	      y' = ymin

drop_complete_lines :: PlayingArea ->Int-> (PlayingArea,[Change],Int)
drop_complete_lines a score= drop_complete_lines' xs (reverse ys) a score -- ys shows rows from top to bottom
	where ((xmin,ymin),(xmax,ymax)) = bounds a
	      xs = range (xmin,xmax)
	      ys = range (ymin,ymax)

drop_complete_lines' :: [Vector] -> [Vector] -> PlayingArea->Int -> (PlayingArea,[Change],Int)
drop_complete_lines' _ [] a score= (a,[],score)
drop_complete_lines' xs (y:ys) a score= if and [a! (x,y) | x<-xs]
				      then do (a''' , cs1 ++ [Delay]++ cs2 ++ cs3 ++ [Delay]++cs4,(score+1))
                                      else drop_complete_lines' xs ys a score

	where cs1 = [Off x y | x<-xs]
	      (a',cs2,_)= move_down a xs ys score
	      (a'' ,cs3,_) = empty_top_row a' xs score
	      (a''',cs4,_) = drop_complete_lines' xs (y:ys) a'' score

move_down :: PlayingArea -> [Vector] -> [Vector] ->Int-> (PlayingArea,[Change],Int)
move_down a xs ys score = (a',cs,score)
	where a' = a // [((x,y+1),a! (x,y))|y<-ys,x<-xs]
	      cs = [(if a! (x,y) then On else Off ) x (y+1) | x<-xs, y<-ys] 



empty_top_row :: PlayingArea -> [Vector]-> Int->(PlayingArea,[Change],Int)
empty_top_row a xs score = (a',cs,score)
	where cs= [Off x 0 | x<-xs]
	      a' = a // [((x,0),False)| x<-xs]





