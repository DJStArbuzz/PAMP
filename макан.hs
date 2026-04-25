import Data.Monoid (Sum(..), Product(..), All(..))

newtype BadStats = BadStats {badValue :: Int} deriving (Show, Eq)

instance Semigroup BadStats where
  (BadStats a) <> (BadStats b) = BadStats (a - b)

instance Monoid BadStats where 
  mempty = BadStats 5


data Platform = YandexMusic | VKMusic | Zvuk deriving (Show, Eq)
data Genre = Rap | Pop | IndieFlok | Phonk | SoundCloudRap | Electro
  deriving (Show, Eq)
data MusicStyle = ClassicRap | TrapRap String | FolkRegional String | PopVariant Int deriving (Show, Eq)

data TrackSource = Online Platform String
                 | Offline FilePath Int  
  deriving (Show, Eq)

data Track = Track 
  { title :: String
  , artist :: String
  , duration :: Int
  , genre :: Genre
  , platform :: Platform 
  } deriving (Show, Eq)

data FullTrack = FullTrack
  { ftTitle :: String
  , ftSource :: TrackSource
  } deriving (Show, Eq)

data PlaylistStats = PlaylistStats
  { totalTracks :: Int
  , totalDuration :: Int 
  } deriving (Show, Eq)

instance Semigroup PlaylistStats where 
  a <> b = PlaylistStats {
    totalTracks = totalTracks a + totalTracks b 
    , totalDuration = totalDuration a + totalDuration b 
  }

instance Monoid PlaylistStats where 
  mempty = PlaylistStats 0 0
  
type Playlist = [Track]

macanPlayList :: Playlist
macanPlayList = [popolam]

indiePlayList :: Playlist 
indiePlayList = [kuhni]

macanStats :: PlaylistStats
macanStats = PlaylistStats 1 183 

indieStats :: PlaylistStats
indieStats = PlaylistStats 1 141

sourceLabel :: TrackSource -> String
sourceLabel (Online platform url) =  
  show platform ++ ": " ++ url
sourceLabel (Offline path _) =
  "locally at " ++ path

styleDescribe :: MusicStyle -> String
styleDescribe ClassicRap = "classic rap (90s origin)"
styleDescribe (TrapRap sub) = "trap rap: " ++ sub
styleDescribe (FolkRegional r) = "folk from " ++ r 
styleDescribe (PopVariant year) = "pop of " ++ show year ++ "s"

platformFee :: Platform -> Double
platformFee YandexMusic = 0.3
platformFee VKMusic = 0.35
platformFee Zvuk = 0.25

popolam :: Track
popolam = Track
  { title = "Popolam"
  , artist = "Macan"
  , duration = 183
  , genre = Rap
  , platform = YandexMusic
  }

kuhni :: Track
kuhni = Track
  { title = "kuhni"
  , artist = "Bond"
  , duration = 141
  , genre = IndieFlok
  , platform = YandexMusic
  }

totalDurationPlayList :: Playlist -> Int 
totalDurationPlayList tracks = getSum (foldMap (Sum . duration) tracks)

totalDurationRec :: Playlist -> Int 
totalDurationRec [] = 0
totalDurationRec (t:ts) = duration t + totalDurationRec ts 

allOnYandex :: PlayList -> Boool
allOnYandex tracks = getAll (foldMap (All . (== YandexMusic) . platform) tracks)
