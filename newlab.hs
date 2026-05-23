type Name      = String
type Satiety   = Int
type Mood      = Int
type Days      = Int
type Fussiness = Int

data Breed = Yard | Siamese | MaineCoon | Sphynx | British | Ginger
  deriving (Show, Eq)

cat :: (Name, Satiety, Mood, Days, Fussiness, Breed)
    -> ((Name, Satiety, Mood, Days, Fussiness, Breed) -> a) -> a
cat state = \msg -> msg state

catName      (n,_,_,_,_,_) = n
catSatiety   (_,s,_,_,_,_) = s
catMood      (_,_,m,_,_,_) = m
catDays      (_,_,_,d,_,_) = d
catFussiness (_,_,_,_,f,_) = f
catBreed     (_,_,_,_,_,b) = b

name     someCat = someCat catName
satiety  someCat = someCat catSatiety
mood     someCat = someCat catMood
days     someCat = someCat catDays
fussiness someCat = someCat catFussiness
breed    someCat = someCat catBreed

feed :: ((Name, Satiety, Mood, Days, Fussiness, Breed) -> ((Name, Satiety, Mood, Days, Fussiness, Breed) -> a) -> a)
     -> Int -> a
feed someCat amount =
  someCat (\(n,s,m,d,f,b) -> cat (n, min 100 (s + amount), m, d, f, b))

pet :: ((Name, Satiety, Mood, Days, Fussiness, Breed) -> ((Name, Satiety, Mood, Days, Fussiness, Breed) -> a) -> a)
     -> Int -> a
pet someCat amount =
  someCat (\(n,s,m,d,f,b) -> cat (n, s, min 100 (m + amount), d, f, b))

info :: ((Name, Satiety, Mood, Days, Fussiness, Breed) -> String) -> String
info someCat =
  someCat (\(n,s,m,d,f,b) ->
    n ++ ", порода: " ++ show b ++
    ", сытость: " ++ show s ++ ", настроение: " ++ show m ++
    ", дней в общаге: " ++ show d ++ ", привередливость: " ++ show f)

battleOne :: ((Name, Satiety, Mood, Days, Fussiness, Breed) -> ((Name, Satiety, Mood, Days, Fussiness, Breed) -> (Name, Days)) -> (Name, Days))
          -> (Name, Days)
battleOne someCat =
  someCat (\(n,s,m,d,f,b) -> go n s m d f b)
  where
    go n s m d f b =
      let newS = s - f
          newM = m - f
          newD = d + 1
      in if newS <= 0 || newM <= 0
         then (n, newD)
         else go n newS newM newD f b

battleAll :: [((Name, Satiety, Mood, Days, Fussiness, Breed) -> ((Name, Satiety, Mood, Days, Fussiness, Breed) -> (Name, Days)) -> (Name, Days))]
          -> [(Name, Days)]
battleAll cats = map battleOne cats

winnerCats :: [((Name, Satiety, Mood, Days, Fussiness, Breed) -> ((Name, Satiety, Mood, Days, Fussiness, Breed) -> (Name, Days)) -> (Name, Days))]
           -> String
winnerCats cats =
  case battleAll cats of
    [] -> "Котов не было"
    xs -> let (n, d) = foldr1 (\(n1,d1) (n2,d2) -> if d1 >= d2 then (n1,d1) else (n2,d2)) xs
          in n ++ " прожил " ++ show d ++ " дней"

main :: IO ()
main = do
  let kot1 = cat ("Вещев", 50, 60, 0, 10, Yard)
  let kot2 = cat ("Семенов", 40, 50, 0, 15, Siamese)
  let kot3 = cat ("Мерзун", 40, 50, 0, 15, MaineCoon)
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
