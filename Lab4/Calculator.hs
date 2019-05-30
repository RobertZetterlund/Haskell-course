-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import Expr
import Data.Maybe
import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

minRange,maxRange, initialZoom :: Num a => a
minRange  = 1
maxRange = 100
initialZoom = 10

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas      <- mkCanvas canWidth canHeight              -- The drawing area
     fx          <- mkHTML "<i>f</i>(<i>x</i>)="             -- The text "f(x)="
     input       <- mkInput 20 "x"                           -- The formula input
     scaleslider <- mkSlider (minRange,maxRange) initialZoom -- The scaler input
     lowerB      <- mkHTML (show minRange)
     higherB     <- mkHTML (show maxRange)

     draw    <- mkButton "Draw graph"    -- The draw button
     diffBtn <- mkButton "Differeniate!" -- The diff button

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     scale <- row [pure lowerB, pure scaleslider, pure higherB]
     getBody window #+ [column
                        [pure canvas,
                         pure formula,
                         pure draw,
                         pure diffBtn,
                         pure scale]
                       ]

     -- Draw cross on canvas
     path "black" [(canWidth/2, 0), (canWidth/2, canHeight)] canvas
     path "black" [(0, canHeight/2), (canWidth, canHeight/2)] canvas

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input scaleslider canvas
     on UI.click     diffBtn  $ \ _ -> differentiateBTN input
     on UI.click     diffBtn  $ \ _ -> readAndDraw input scaleslider canvas

     on valueChange' scaleslider $ \ _ -> readAndDraw input scaleslider canvas


readAndDraw :: Element -> Element -> Canvas -> UI ()
readAndDraw input scaleslider canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Get the scaling value.
     scaleValue <- scaleslider # get value
     -- Prepare variables for drawing graph
     let scale = (fromIntegral (read scaleValue) :: Double)/canWidth
         expr = readExpr formula
         isExpr = (not . isNothing) expr
         myPoints = points (fromJust expr) scale (canWidth, canHeight)
         simpleformula = show (simplify (fromJust expr))
     -- Clear the canvas
     clearCanvas canvas
     -- Draw a cross section
     path "black" [(canWidth/2, 0), (canWidth/2, canHeight)] canvas
     path "black" [(0, canHeight/2), (canWidth, canHeight/2)] canvas

     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText ("f(x) = " ++ simpleformula) (canWidth/25,canHeight/20) canvas

     set UI.fillStyle (UI.solidColor (UI.RGB 200 0 0)) (pure canvas)
     if isExpr then
      path "blue" myPoints canvas
     else
      UI.fillText "Not an Expression" (150,150) canvas


-- Helper function to let Expr.differeniate affect the ui
differentiateBTN :: Element -> UI ()
differentiateBTN input = do formula <- get value input -- Get current equation from GUI.
                            let expr = readExpr formula -- Convert it into an expression
                            let isExpr = (not . isNothing) expr
                            let derivative = differeniate (fromJust expr) -- Differeniate expression
                            input # set' value (show derivative)

-- H

-- Function for translating an expressions to a list of points used by the GUI.
points :: Expr -> Double -> (Int, Int) -> [Point]
points expr scale (width, height) = zip xs ys
  where f = translateX width
        calculate = (toPixel scale . eval expr . toReal scale)
        xs = [0.0 .. fromIntegral width]
        translatedXs = map f xs
        translatedYs = map calculate translatedXs
        g = translateY height
        ys = map g translatedYs

-- Function for translating x onto mathematical representation of coordinate system (-150, 150) to (150, -150)
translateX :: Int -> Double -> Double
translateX width x = x - fromIntegral(div width 2)

-- Function for translating y onto GUI representation of coordinate system (0, 0) to (300, 300)
translateY :: Int -> Double -> Double
translateY height y = -(y - fromIntegral(div height 2))

-- (point/pixel) * pixel = point
toReal :: Double -> Double -> Double
toReal scale x = x * scale

-- (pixel/point) * point = pixel
toPixel :: Double -> Double -> Double
toPixel scale x = x * (1/scale)
