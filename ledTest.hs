import System.IO
import System.RaspberryPi.GPIO
import Data.List
import Control.Concurrent


main = withGPIO $ do
	setPinFunction Pin11 Output
	writePin Pin11 True
	setPinFunction Pin13 Output
	writePin Pin13 True
	sequence_ $ interleave pinList fibDelay


-- IO action lists

pinList :: [IO ()]
pinList = cycle $ map toggle $ take 2 $ intersperse Pin11 (repeat Pin13)

fibDelay :: [IO ()]
fibDelay = do
	cycle $ map threadDelay $ (take 32 fibs) ++ (reverse $ take 31 fibs)


-- some functions

interleave :: [IO ()] -> [IO ()] -> [IO ()]
interleave xs ys = concat $ transpose [xs, ys]

toggle :: Pin -> IO ()
toggle p = withGPIO $ do
	state <- readPin p
	writePin p $ not state

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
