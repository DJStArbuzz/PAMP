-- Синонимы типов
type Name     = String
type Satiety  = Int    
type Mood     = Int  
type Days     = Int
type Fussiness = Int   

-- Породы 
data Breed = Yard | Siamese | MaineCoon | Sphynx | British | Ginger
  deriving (Show, Eq) 

-- Функциональное представление кота 
data Cat = Cat
  { feed         :: Int -> Cat          -- покормить
  , pet          :: Int -> Cat          -- погладить
  , info         :: String              -- инфоблок о коте
  , getName      :: Name
  , getSatiety   :: Satiety
  , getMood      :: Mood
  , getDays      :: Days
  , getFussiness :: Fussiness
  , getBreed     :: Breed
  }

-- Создание кота (замыкание – все функции захватывают текущие параметры)
makeCat :: Name -> Satiety -> Mood -> Days -> Fussiness -> Breed -> Cat
makeCat n s m d f b = Cat
  { feed         = \amount -> makeCat n (min 100 (s + amount)) m d f b
  , pet          = \amount -> makeCat n s (min 100 (m + amount)) d f b
  , info         = n ++ ", сытость: " ++ show s ++ ", настроение: " ++ show m ++
                   ", дней в общаге: " ++ show d ++ ", привередливость: " ++ show f ++
                   ", порода: " ++ show b
  , getName      = n
  , getSatiety   = s
  , getMood      = m
  , getDays      = d
  , getFussiness = f
  , getBreed     = b
  }

-- Один день: падение характеристик. Если сытость или настроение <= 0, кот уходит.
nextDay :: Cat -> (Bool, Cat)
nextDay cat =
  let newSat = getSatiety cat - getFussiness cat
      newMood = getMood cat - getFussiness cat
  in if newSat <= 0 || newMood <= 0
     then (False, cat)
     else (True, makeCat (getName cat) newSat newMood (getDays cat + 1) (getFussiness cat) (getBreed cat))

-- симуляция выживания нескольких котов
-- Если сытость или настроение опускается до нуля,
-- то кот убегает на улицу и выбывает. Побеждает кот,
-- продержавшийся в общаге дольше всех.
simulate :: [Cat] -> String
simulate cats = go cats 1 ""
  where
    go [] _ acc = acc ++ "\nВсе коты убежали"
    go [c] day acc = acc ++ "\nДень " ++ show day ++ ": Победитель " ++ getName c ++
                     " (прожил " ++ show (getDays c) ++ " дней)\n"
    go alive day acc =
        let dayStr = "\nДень " ++ show day ++ "\n" ++ unlines (map info alive)
            (alive', acc') = foldl step ([], acc ++ dayStr) alive
        in go alive' (day + 1) acc'

    step :: ([Cat], String) -> Cat -> ([Cat], String)
    step (list, str) cat =
        case nextDay cat of
            (True, c)  -> (c : list, str)
            (False, _) -> (list, str ++ getName cat ++ " убежал\n")

main :: IO ()
main = do
    let kot1 = makeCat "Вещев" 50 60 0 10 Yard
        kot2 = makeCat "Семенов" 40 50 0 15 Siamese
        kot3 = makeCat "Мерзун" 70 70 0 5 MaineCoon
    putStrLn "Три кота в общежитии:"
    mapM_ (putStrLn . info) [kot1, kot2, kot3]
    putStr $ simulate [kot1, kot2, kot3]