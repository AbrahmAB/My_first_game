module Data' (Delay,Vector,Event (..) ,Change (..))
 where

type Vector = Int
type Delay = Int

data Event = RotL
	   | RotR
	   | MDown
	   | MLeft
	   | MRight
	   | Drop
	   | Tick
	   | Quit 
	   | None
	deriving Eq

data Change = On Vector Vector
	    | Off Vector Vector
	    | Delay


