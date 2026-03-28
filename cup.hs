hotCup (ml, temp) = \msg -> msg (ml. temp)
getHotCupMl sCup = sCup (\(m, _) -> m)
getTemp sCup = sCup (\(_, t) -> t)
coolDown sCup deg = sCup (\(m, t) -> 
  hotCup (m, max 20 (t - deg)))


cup ml = \msg -> msg ml
getMl sCup = sCup (\ml -> ml)
makeSipFromCup sCup mlDrank = if mlDiff > 0  
                              then cup $ mlDiff
                              else cup 0
  where ml = getMl sCup
        mlDiff = ml - mlDrank  
