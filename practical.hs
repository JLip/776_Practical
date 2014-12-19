{-# LANGUAGE GADTs, KindSignatures #-}
module Main where

import Graphics.Blank
import Data.Text (Text)
import qualified Data.Text as Text

type Coord = (Double,Double)
type Color = Text
------------------------------------------------------
type State4ple = (Int, Int, Int, Int)

newFite :: State4ple
newFite = (100, 50,  100, 0, -1)

fetchMyHP 				::	State4ple -> Int
fetchMyHp (x, _, _, _, _)	=	x

fetchMyMp         ::  State4ple -> Int
fetchMyMp (_, x, _, _, _) = x

fetchYrHP         ::	State4ple -> Int
fetchYrHP (_, _, x, _, _)	=	x

fetchMenu 				::	State4ple -> Int
fetchMenu (_, _, _, x, _)	= x

fetchSubMenu 			::	State4ple -> Int
fetchSubMenu (_, _, _, _, x)	=	x

------------------------------------------------------

type GM a = State State4ple a

newtype State :: * -> (* -> *) where
  State_ :: (s-> (a,s)) -> State s a

instance Monad (State s) where
  return a = State_ (\ st0 -> (a,st0))
  (State f) >>= k = State (\ st0 ->
          let (a,st1) = f st0 in
          case (k a) of
            State g -> g st1)

                     
get :: State s s
get = State (\ st -> (st,st))

set :: s -> State s ()
set n = State (\ st -> ((),n))


runFT :: s -> State s a -> a
runST s (State f) = fst (f s)

-------------------------------------------------------
drawTriangle :: Coord -> Canvas ()
drawTriangle (x,y) = do
    beginPath()
    moveTo(x,y)
    lineTo(x-5,y-2.5)
    lineTo(x-5,y+2.5)
    fillStyle "black"
    fill()

drawRectangle :: Coord -> Coord -> Canvas ()
drawRectangle (x1,y1) (x2, y2) = do
    beginPath()
    moveTo(x1,y1)
    lineTo(x1,y2)
    lineTo(x2,y2)
    lineTo(x2,y1)
    lineTo(x1,y1)
    strokeStyle "black"
    stroke()

drawHPBar     :: Coord -> Int-> Canvas()
drawHPBar (x,y) val = do
    let color = 
        if val <= 30 then "red" else "black"
    beginPath()
    moveTo (x,y)
    lineTo(x, y-(val))
    strokeStyle color
    stroke()

-------------------------------------------------------

main = blankCanvas 3000 {events = ["keydown"] }$ \ context -> do
        runFT newFite
        loop battle

loop battle = do
    let (wide, high) = width context, height context)

    send context $ do
        beginPath()
        font "15pt Calibri"
        fillStyle "black"
        fillText ("Enemy Health", 100, 50)
        fillText ("My Health", (wide 'div' 2) + 100, 50)
        drawRectangle (0, high 'div' 2) (wide - 200, high)
        drawRectangle (wide - 200, high 'div' 2) (wide, high)
        fillText ("Attack", 75, (high 'div' 2) + 50)
        fillText ("Magic", wide -400, (high 'div' 2) + 50)
        fillText ("Meditate", 75, high- 50)
        fillText ("Magic", wide - 400, high - 50)
    event <- wait context
    case event of
        Nothing -> loop context
{-|     Enter   -> execute function based on submenu/menu location
                fs <- get
                let myHp = fetchMyHp fs
                    myMp = fetchMyMp fs
                    urHp = fetchYourHp fs
                    menu1 = fetchMenu fs
                    menu2 = fetchMenu2 fs
                if (menu2 < 0) then
                    if (menu1 == 1 OR menu1 == 2) then
                      set (myHp, myMp, urHp, menu1, 1)
                    else if (menu1 == 3) then
                      if (myHp < 6) then
                        launchGameOver("You Died!")
                      else
                        set (myHp-5, min(myMp+25, 50), urHp, menu1, menu2)
                    else if (menu1 == 4) then
                      launchGameOver("You ran away!")
                else if (menu2 == 1 ) then
                    if (menu1 == 1) then
                      if (urHp < 6) then
                        launchGameOver("You Won!")
                      if (myHp < 6) then
                        launchGameOver("You Died!")
                      else
                        set(myHp-5, myMp, urHp-5, menu1, menu2)
                    else if (menu1 == 2) then
                      if (urHp < 11) then
                        launchGameOver("You Won!")
                      if (myHp < 6) then
                        launchGameOver("You Died!")
                      else
                        set(myHp-5, myMp-10, urHp-10, menu1, menu2)
                else if (menu2 == 2)
                    if(menu1 == 1)
                      if (urHp < 16) then
                        launchGameOver("You Won!")
                      if (myHp < 11) then
                        launchGameOver("You Died!")
                      else
                        set(myHp-10, myMp, urHp-15, menu1, menu2)
                    else if (menu1 == 2) then
                      if (urHp < 31) then
                        launchGameOver("You Won!")
                      if (myHp < 11) then
                        launchGameOver("You Died!")
                      else
                        set(myHp-10, myMp-25, urHp-30, menu1, menu2)
                else if (menu2 == 3)
                    if (menu1 == 1) then
                      if (urHp < 36) then
                        launchGameOver("You Won!")
                      if (myHp < 26) then
                        launchGameOver("You Died!")
                      else
                        set(myHp-25, myMp, urHp-35, menu1, menu2)
                    else if (menu1 == 2) then
                        set(min(myHp+40, 100), myMp-15, urHp, menu1, menu2)
        Arrow   -> update state with menu item based on location
                if you have the available HP/MP to use the attack
|-}

{-|
      Enemy HP              My HP
         |                    |
         |                    |
         |                    |

    box                     box
      attack      magic        context sensitive
      meditate    run



    attack          magic
      standard        minor magic
      heavy           major magic
      berserk         heal
|-}
