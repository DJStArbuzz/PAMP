import Data.List (maximumBy)
import Data.Function (on)

type CatName   = String
type Satiety   = Int
type Mood      = Int
type Days      = Int
type Fussiness = Int

data Breed = Yard
           | Siamese
           | MaineCoon
           | Sphynx
           | British
           | Ginger
           deriving (Eq)

showBreed :: Breed -> String
showBreed Yard     = "дворовый"
showBreed Siamese  = "сиамский"
showBreed MaineCoon = "мейн-кун"
showBreed Sphynx   = "сфинкс"
showBreed British  = "британский"
showBreed Ginger   = "рыжий"

data Cat = Cat
  { name      :: CatName
  , breed     :: Breed
  , satiety   :: Satiety
  , mood      :: Mood
  , daysLived :: Days
  , fussiness :: Fussiness
  } deriving (Eq)

showCat :: Cat -> String
showCat cat = unlines
  [ "Кличка: " ++ name cat
  , "Порода: " ++ showBreed (breed cat)
  , "Сытость: " ++ show (satiety cat)
  , "Настроение: " ++ show (mood cat)
  , "Прожито дней: " ++ show (daysLived cat)
  , "Привередливость: " ++ show (fussiness cat)
  , "------------------------"
  ]

makeCat :: CatName -> Breed -> Satiety -> Mood -> Days -> Fussiness -> Cat
makeCat n b s m d f = Cat n b (clamp 0 100 s) (clamp 0 100 m) d f

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

feed :: Int -> Cat -> Cat
feed amount cat = cat { satiety = clamp 0 100 (satiety cat + amount) }

pet :: Int -> Cat -> Cat
pet amount cat = cat { mood = clamp 0 100 (mood cat + amount) }

nextDay :: Cat -> (Bool, Cat)
nextDay cat =
  let f = fussiness cat
      newSat = satiety cat - f
      newMood = mood cat - f
      finalSat = max 0 newSat
      finalMood = max 0 newMood
      newCat = cat { satiety = finalSat, mood = finalMood, daysLived = daysLived cat + 1 }
      alive = newSat > 0 && newMood > 0
  in (alive, newCat)

simulate :: [Cat] -> [Cat]
simulate cats = go cats []
  where
    go :: [Cat] -> [Cat] -> [Cat]
    go [] dead = dead
    go current dead =
      let (survivors, todaysDead) = partitionCats current
      in go survivors (dead ++ todaysDead)

    partitionCats :: [Cat] -> ([Cat], [Cat])
    partitionCats [] = ([], [])
    partitionCats (c:cs) =
      let (survRest, deadRest) = partitionCats cs
          (alive, c') = nextDay c
      in if alive
         then (c' : survRest, deadRest)
         else (survRest, c' : deadRest)

winner :: [Cat] -> (CatName, Days)
winner cats =
  let allCats = simulate cats
      best    = maximumBy (compare `on` daysLived) allCats
  in (name best, daysLived best)

main :: IO ()
main = do
  let barsik = makeCat "Барсик" Siamese  80 70 0 10
      murka  = makeCat "Мурка"  British  90 85 0 5
      vaska  = makeCat "Васька" Yard     50 60 0 15
      cleo   = makeCat "Клео"   Sphynx   70 80 0 8
      simba  = makeCat "Симба"  MaineCoon 95 90 0 6
      ryzhik = makeCat "Рыжик"  Ginger    85 75 0 12

  putStrLn "=== Начальные параметры котов ==="
  mapM_ (putStrLn . showCat) [barsik, murka, vaska, cleo, simba, ryzhik]

  let results = simulate [barsik, murka, vaska, cleo, simba, ryzhik]
  putStrLn "\n=== Итоги (все выбывшие коты) ==="
  mapM_ (putStrLn . showCat) results

  let (winName, winDays) = winner [barsik, murka, vaska, cleo, simba, ryzhik]
  putStrLn $ "🏆 Победитель: " ++ winName ++ " прожил " ++ show winDays ++ " дней"
