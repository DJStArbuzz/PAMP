type Name      = String
type Satiety   = Int     
type Mood      = Int     
type Days      = Int
type Fussiness = Int

data Breed = Yard | Siamese | MaineCoon | Sphynx | British | Ginger
  deriving (Show, Eq)

data Cat = Cat
  { name      :: Name
  , satiety   :: Satiety
  , mood      :: Mood
  , days      :: Days
  , fussiness :: Fussiness
  , breed     :: Breed
  }

feed :: Int -> Cat -> Cat
feed amount cat = cat { satiety = min 100 (satiety cat + amount) }

pet :: Int -> Cat -> Cat
pet amount cat = cat { mood = min 100 (mood cat + amount) }

instance Show Cat where
  show cat = name cat ++ ", порода: " ++ show (breed cat) ++
             ", сытость: " ++ show (satiety cat) ++
             ", настроение: " ++ show (mood cat) ++
             ", дней: " ++ show (days cat) ++
             ", привередливость: " ++ show (fussiness cat)

nextDay :: Cat -> (Bool, Cat)
nextDay cat =
  let newSat = satiety cat - fussiness cat
      newMood = mood cat - fussiness cat
  in if newSat <= 0 || newMood <= 0
     then (False, cat)
     else (True, cat { satiety = newSat, mood = newMood, days = days cat + 1 })

simulate :: [Cat] -> String
simulate cats = go cats 1 ""
  where
    go [] _ acc = acc ++ "\nВсе коты убежали"
    go [c] day acc = acc ++ "\nДень " ++ show day ++ ": Победитель " ++ name c ++
                     " (прожил " ++ show (days c) ++ " дней)\n"
    go alive day acc =
      let dayStr = "\nДень " ++ show day ++ "\n" ++ unlines (map show alive)
          (alive', acc') = foldl step ([], acc ++ dayStr) alive
      in go alive' (day + 1) acc'

    step :: ([Cat], String) -> Cat -> ([Cat], String)
    step (list, str) cat =
      case nextDay cat of
        (True, c)  -> (c : list, str)
        (False, _) -> (list, str ++ name cat ++ " убежал\n")


main :: IO ()
main = do
  let kot1 = Cat "Вещев"     50 60 0 10 Yard
      kot2 = Cat "Семенов"   40 50 0 15 Siamese
      kot3 = Cat "Мерзун"    70 70 0 5  MaineCoon
  putStrLn "Три кота в общежитии:"
  mapM_ print [kot1, kot2, kot3]
  putStr $ simulate [kot1, kot2, kot3]