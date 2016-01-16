module Pieces (Piece,rot_left,rot_right,blocks,extent_down,extent_left,extent_right,pieces)
 where

import Data'

data Piece = Piece [[(Vector,Vector)]]

blocks :: Piece -> [(Vector,Vector)]
blocks (Piece xss) =  head xss

extent_down :: Piece -> Vector
extent_down p = maximum $ map snd $ blocks p

extent_right :: Piece -> Vector
extent_right p = maximum $ map fst $ blocks p

extent_left :: Piece -> Vector
extent_left p = negate $ minimum $ map fst $ blocks p

rot_left :: Piece -> Piece
rot_left (Piece xss) =Piece (tail xss)

rot_right :: Piece -> Piece
rot_right (Piece xss) = Piece (drop 3 xss)

pieces :: [Piece]
pieces = [piece_I, piece_L,piece_J,piece_T,piece_2,piece_5,piece_O]

piece_I :: Piece
piece_I = Piece (cycle [p1,p2])
    where p1 = [(0,-1),(0,0),(0,1),(0,2)]
          p2 = [(-1,0),(0,0),(1,0),(2,0)]

piece_L :: Piece
piece_L = Piece ( cycle [p1,p2,p3,p4])
    where p1 = [(0,-1),(0,0),(0,1),(1,1)]
          p2 = [(-1,0),(0,0),(1,0),(1,-1)]
	  p3 = [(-1,-1),(0,-1),(0,0),(0,1)]
	  p4 = [(-1,1),(-1,0),(0,0),(1,0)]

piece_J :: Piece
piece_J = Piece (cycle [p1,p2,p3,p4])
    where p1 = [(0,-1),(0,0),(0,1),(-1,1)]
	  p2 = [(-1,0),(0,0),(1,0),(1,1)] 
	  p3 = [(1,-1),(0,-1),(0,0),(0,1)] 
	  p4 = [(-1,-1),(-1,0),(0,0),(1,0)]

piece_T :: Piece
piece_T = Piece (cycle [p1,p2,p3,p4])
    where p1 = [(-1,0),(0,0),(1,0),(0,1)]
	  p2 = [(0,-1),(0,0),(0,1),(1,0)]
	  p3 = [(-1,0),(0,0),(1,0),(0,-1)]
	  p4 = [(0,-1),(0,0),(0,1),(-1,0)]

piece_2 :: Piece
piece_2 = Piece (cycle [p1,p2])
    where p1 = [(1,0),(0,0),(0,-1),(-1,-1)]
	  p2 = [(0,0),(0,-1),(-1,0),(-1,1)]

piece_5 :: Piece
piece_5 = Piece (cycle [p1,p2])
    where p1 = [(-1,0),(0,0),(0,-1),(1,-1)]
	  p2 = [(0,0),(0,-1),(1,0),(1,1)]

piece_O :: Piece
piece_O = Piece (cycle [p1])
    where p1 = [(0,0),(0,1),(1,0),(1,1)]




