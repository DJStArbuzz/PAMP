type Name = String
type Fullness = Int
type Mood = Int
type Days = Int
type Fussy = Int

data Breed = Yard | Siamese | MaineCoon | Sphynx | British | Ginger
  deriving (Show, Eq)

cat :: (Name, Fullness, Mood, Days, Fussy, Breed) -> ((Name, Fullness, Mood, Days, Fussy, Breed) -> a) -> a
cat state = \msg -> msg state

catName      (n,_,_,_,_,_ ) = n
catFullness  (_,f,_,_,_, _) = f
catMood      (_,_,m,_,_, _) = m
catDays      (_,_,_,d,_,_ ) = d
catFussiness (_,_,_,_,fs,_) = fs
catBreed     (_,_,_,_,_,b ) = b

name someCat = someCat catName 
fullness someCat = someCat catFullness
mood someCat = someCat catMood
days someCat = someCat catDays
fussiness someCat = someCat catFussiness
breed someCat = someCat catBreed

feed someCat amount = someCat (\(n,f,m,d,fs,b) -> cat (n, min 100 (f + amount), m, d, fs, b))

pet someCat amount = someCat (\(n,f,m,d,fs,b) -> cat (n, f, min 100 (m + amount), d, fs, b))

info someCat =
  someCat (\(n,f,m,d,fs,b) ->
    n ++ ", порода: " ++ show b ++
    ", сытость: " ++ show f ++ ", настроение: " ++ show m ++
    ", дней в общаге: " ++ show d ++ ", привередливость: " ++ show fs)

battleOne someCat =
  someCat (\(n,f,m,d,fs,b) -> go n f m d fs b)
  where
    go n f m d fs b =
      let newF = f - fs
          newM = m - fs
          newD = d + 1
      in if newF <= 0 || newM <= 0
         then (n, newD)
         else go n newF newM newD fs b

battleAll cats = map battleOne cats

winnerCats cats =
  case battleAll cats of
    [] -> "Котов не было"
    xs -> let (n, d) = foldr1 (\(n1,d1) (n2,d2) -> if d1 >= d2 then (n1,d1) else (n2,d2)) xs
          in n ++ " прожил " ++ show d ++ " дней"
          
main = do
  let kot1 = cat ("Вещев", 50, 60, 0, 10, Yard)
  let kot2 = cat ("Семенов", 40, 50, 0, 15, Siamese)
  let kot3 = cat ("Мерзун", 40, 50, 0, 15, MaineCoon)
  let allCats = [kot1, kot2, kot3]


  putStrLn "\nКормление"
  let fedKot1 = feed kot1 20
  putStrLn $ "Вещев после кормления: сытость " ++ show (fullness fedKot1)
  
  putStrLn "Три кота в общежитии"
  mapM_ (putStrLn . info) allCats

  let results = battleAll allCats
  mapM_ (\(n, d) -> putStrLn $ n ++ " прожил " ++ show d ++ " дней") results

  putStrLn "\nРезультат"
  putStrLn $ winnerCats allCats
