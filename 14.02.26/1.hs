tripleMe x = x + x + x

tripleAs x y = tripleMe x + tripleMe y

quadSmall x = if x > 200
				then x
			 	else 4 * x

quadSmall' x = (if x > 200 then x else 4 * x) + 1 
