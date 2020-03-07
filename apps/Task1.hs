import qualified Objectives
import qualified Utils

main :: IO ()
main = do
        Utils.plot Objectives.firstFunctionString 50 (-25) (-7.5) 7.5 (-7.5) 7.5 (-1) 0 (-25)