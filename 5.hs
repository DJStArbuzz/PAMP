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
  } deriving (Show, Eq)

feed :: Cat -> Int -> Cat
feed cat amount = cat { satiety = min 100 (satiety cat + amount) }

pet :: Cat -> Int -> Cat
pet cat amount = cat { mood = min 100 (mood cat + amount) }

info :: Cat -> String
info cat = 
  name cat ++ ", порода: " ++ show (breed cat) ++
  ", сытость: " ++ show (satiety cat) ++ ", настроение: " ++ show (mood cat) ++
  ", дней в общаге: " ++ show (days cat) ++ ", привередливость: " ++ show (fussiness cat)

battleOne :: Cat -> (Name, Days)
battleOne cat = go (satiety cat) (mood cat) 0
  where
    go s m d =
      let newS = s - fussiness cat
          newM = m - fussiness cat
          newD = d + 1
      in if newS <= 0 || newM <= 0
         then (name cat, newD)
         else go newS newM newD

battleAll :: [Cat] -> [(Name, Days)]
battleAll cats = map battleOne cats

winnerCats :: [Cat] -> String
winnerCats cats =
  case battleAll cats of
    [] -> "Котов не было"
    xs -> let (n, d) = foldr1 (\(n1,d1) (n2,d2) -> if d1 >= d2 then (n1,d1) else (n2,d2)) xs
          in n ++ " прожил " ++ show d ++ " дней"

main = do
  let kot1 = Cat { name = "Вещев", satiety = 50, mood = 60, days = 0, fussiness = 10, breed = Yard }
  let kot2 = Cat { name = "Семенов", satiety = 40, mood = 50, days = 0, fussiness = 15, breed = Siamese }
  let kot3 = Cat { name = "Мерзун", satiety = 40, mood = 50, days = 0, fussiness = 15, breed = MaineCoon }
  let allCats = [kot1, kot2, kot3]

  putStrLn "\nКормление"
  let fedKot1 = feed kot1 20
  putStrLn $ "Вещев после кормления: сытость " ++ show (satiety fedKot1)

  putStrLn "Три кота в общежитии"
  mapM_ (putStrLn . info) allCats

  let results = battleAll allCats
  mapM_ (\(n, d) -> putStrLn $ n ++ " прожил " ++ show d ++ " дней") results

  putStrLn "\nРезультат"
  putStrLn $ winnerCats allCats
