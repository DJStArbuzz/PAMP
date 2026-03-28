tank (model, attack, hp) = \msg -> msg (model, attack, hp)
model (m, _, _) = m 
attack (_, a, _) = a 
health (_, _, h) = h  

getTankModel sTank = sTank model
getTankAttack sTank = sTank attack 
getTankHealth sTank = sTank health

setTankHealth sTank nHp = sTank (\(m, a, _) -> tank (m, a, nHp))
setTankAttack sTank nA = sTank (\(m, _, h) -> tank (m, nA, h))
setTankModel sTank nM = sTank (\(_, a, h) -> tank (nM, a, h))

printTankInfo sTank = sTank(\(m, a, h) -> 
  "Model: " ++ m ++ ", Attack: " ++ (show a) ++ 
    ", Health: " ++ (show h))
