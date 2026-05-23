import System.Random 
import System.Environment
import qualified Data.Map as Map

allSchedules :: Map.Map String Schedule
allSchedules = Map.fromList 
  [ ("monday", mondaySchedule)
  , ("tuesday", tuesdaySchedule)
  
  ]

showArgs :: IO ()
showArgs = do 
  args <- getArgs
  mapM_ putStrLn args

data Lesson = Lesson
  {
    time :: String
    , subject :: String
    , room :: String
  } deriving Eq

instance Show Lesson where
  show l = 
    time l ++ " | " ++
    subject l ++ " | " ++ " (" ++ room l ++ ")"

data Schedule = Schedule String [Lesson]

instance Show Schedule where 
  show (Schedule day ls) =
    "Schedule for " ++ day ++ ":\n" ++ unlines (map show ls)

mondaySchedule :: Schedule
mondaySchedule = Schedule "monday"
  [Lesson "09:00" "DB" "305",
   Lesson "10:50" "FP" "412"]

tuesdaySchedule :: Schedule
tuesdaySchedule = Schedule "tuesday"
 [ Lesson "09:00" "DB" "305"
 , Lesson "10:50" "FP" "412"
 ]



helloUser :: String -> String
helloUser name = "Hi, " ++ name ++ "!"

classmates :: [String]
classmates = ["Daniel", "Roma", "Egor", "Anya", "Marina"]

pickPartner :: IO()
pickPartner = do
  let n = length classmates 
  i <- randomRIO (0, n - 1)
  putStrLn ("Your parner: " ++ classmates !! i)

sumTwoLines :: IO ()
sumTwoLines = do
  putStrLn "Enter two numbers:"
  a <- getLine
  b <- getLine
  let total = read a + read b :: Int
  putStrLn ("Sum is " ++ show total)


main :: IO ()
main = do
  args <- getArgs
  let day = if null args
            then "monday"
            else head args
  case Map.lookup day allSchedules of
    Nothing -> putStrLn ("Unknown day: " ++ day)
    Just s -> print s 
    
