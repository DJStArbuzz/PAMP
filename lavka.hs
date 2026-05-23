import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, 
                   fromMaybe, mapMaybe, catMaybes)

-- data Maybe a = Nothing | Just 
type ItemId = Int
data LavkaItem = LavkaItem 
  {
    itemName :: String
    , itemPrice :: Int
  } deriving (Show, Eq)

data Order = Order 
  {
    orderItems :: [ItemId]
    , orderTip :: Maybe Int
    , orderPromo :: Maybe String
  } deriving Show 

order1 :: Order 
order1 = Order 
  {
    orderItems = [101, 103, 106]
    , orderTip = Just 100
    , orderPromo = Nothing
  }

order2 :: Order 
order2 = Order 
  {
    orderItems = [102, 104]
    , orderTip = Nothing
    , orderPromo = Just "SUMMER26"
  }


basePrice :: [ItemId] -> Int
basePrice = sum . mapMaybe priceById

applyPromo :: Maybe String -> Int -> Int
applyPromo Nothing total = total
applyPromo (Just "SUMMER26") total = 
  total * 75 `div` 100
applyPromo (Just _) total = total

addTip :: Maybe Int -> Int -> Int
addTip tip total = total + fromMaybe 0 tip

orderTotal :: Order -> Int 
orderTotal o = 
  addTip (orderTip o)
    ((applyPromo (orderPromo o))
      (basePrice (orderItems o)))


allIds :: [ItemId]
allIds = [100..110]

allLookups :: [Maybe LavkaItem]
allLookups = map (\i ->Map.lookup i catalog) allIds

availableItems :: [LavkaItem]
availableItems = 
  mapMaybe (\i -> Map.lookup i catalog) allIds

catalog :: Map.Map ItemId LavkaItem 
catalog = Map.fromList
  [ (101, LavkaItem "Milk" 89)
  , (102, LavkaItem "Breed" 45)
  , (103, LavkaItem "Cheese" 240)
  , (104, LavkaItem "Tomato" 130)
  , (105, LavkaItem "Cucumber" 140)
  , (106, LavkaItem "Apple" 160)

  ]

showItem :: Maybe LavkaItem -> String
showItem Nothing = "Item not found"
showItem (Just item) =
  itemName item ++ ": " ++ show (itemPrice item) ++ "RUB"

priceById :: ItemId -> Maybe Int
priceById i = case Map.lookup i catalog of
  Nothing -> Nothing 
  Just item -> Just (itemPrice item)



data List a = Empty | Cons a (List a) deriving Show

myList :: List Int
myList = Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))

myMap :: (a -> b) -> List a -> List b 
myMap _ Empty = Empty
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

data Triple a = Triple a a a deriving (Show, Eq)
data Box a = Box a deriving Show

type Point3D = Triple Double
type Top3 = Triple String

pickupPoint :: Point3D
pickupPoint = Triple 55.7558 37.6137 0.0

topCouriers :: Top3
topCouriers = Triple "Damill" "Romul" "Egor"

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ y _) = y

third :: Triple a -> a
third (Triple _ _ z) = z

tripleToList :: Triple a -> [a]
tripleToList (Triple x y z) = [x, y, z]

transformTriple :: (a -> b) -> Triple a -> Triple b 
transformTriple f (Triple x y z) =
  Triple (f x) (f y) (f z)

warp :: a -> Box a
warp x = Box x

unwarp :: Box a -> a
unwarp (Box x) = x
