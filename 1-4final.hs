import Data.List (maximumBy)
import Data.Function (on)

type Name = String
type Fullness = Int
type Mood = Int
type Days = Int
type Fussy = Int

data Breed = Yard | Siamese | MaineCoon | Sphynx | British | Ginger
  deriving (Show, Eq)

cat :: (Name, Fullness, Mood, Days, Fussy, Breed) -> ((Name, Fullness, Mood, Days, Fussy, Breed) -> a) -> a
cat state = \msg -> msg state

_name      (n,_,_,_,_,_ ) = n
_fullness  (_,f,_,_,_, _) = f
_mood      (_,_,m,_,_, _) = m
_days      (_,_,_,d,_,_ ) = d
_fussiness (_,_,_,_,fs,_) = fs
_breed     (_,_,_,_,_,b ) = b

name someCat = someCat _name
fullness someCat = someCat _fullness
mood someCat = someCat _mood
days someCat = someCat _days
fussiness someCat = someCat _fussiness
breed someCat = someCat _breed

feed someCat amount =
  someCat (\(n,f,m,d,fs,b) ->
    cat (n, min 100 (f + amount), m, d, fs, b))

pet someCat amount =
  someCat (\(n,f,m,d,fs,b) ->
    cat (n, f, min 100 (m + amount), d, fs, b))

info someCat =
  someCat (\(n,f,m,d,fs,b) ->
    n ++ ", порода: " ++ show b ++
    ", сытость: " ++ show f ++ ", настроение: " ++ show m ++
    ", дней в общаге: " ++ show d ++ ", привередливость: " ++ show fs)

simulateOne someCat =
  someCat (\(n,f,m,d,fs,b) ->
    go n f m d fs b)
  where
    go n f m d fs b =
      let newF = f - fs
          newM = m - fs
          newD = d + 1
      in if newF <= 0 || newM <= 0
         then (n, newD)
         else go n newF newM newD fs b

simulateAll cats = map simulateOne cats

winnerCats cats =
  let results = simulateAll cats
  in case results of
       [] -> "Котов не было"
       _  -> let (n, d) = maximumBy (compare `on` snd) results
             in n ++ " прожил " ++ show d ++ " дней"

main = do
  let kot1 = cat ("Вещев", 50, 60, 0, 10, Yard)
  let kot2 = cat ("Семенов", 40, 50, 0, 15, Siamese)
  let kot3 = cat ("Мерзун", 70, 70, 0, 5, MaineCoon)
  let allCats = [kot1, kot2, kot3]

  putStrLn "Три кота в общежитии"
  mapM_ (putStrLn . info) allCats

  putStrLn "\nСимуляция"
  let results = simulateAll allCats
  mapM_ (\(n, d) -> putStrLn $ n ++ " прожил " ++ show d ++ " дней") results

  putStrLn "\nРезультат"
  putStrLn $ winnerCats allCats

  putStrLn "\nКормление"
  let fedKot1 = feed kot1 20
  putStrLn $ "Вещев после кормления: сытость " ++ show (fullness fedKot1)
