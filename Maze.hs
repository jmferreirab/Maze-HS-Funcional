--SAMPLE   [['3','1','1'],['5','i','f','x'],['3','5']]

data Direccion = Norte | Este | Sur | Oeste

move:: Direccion -> (Int,Int) -> (Int,Int)
move Norte (x,y) = (x,y-1)
move Este (x,y) = (x+1,y)
move Oeste (x,y) = (x-1,y)
move Sur (x,y) = (x,y+1)

hallarCharXY::[[Char]]->Char->(Int,Int)
hallarCharXY [] chr = (0,0)
hallarCharXY (x:xs) chr = if (elem chr x) then (0, hallarCharEnFila x chr)
			   else sumarTuplas (hallarCharXY xs chr) (1,0)

-- Halla la posicion en la que la x se encuentra para una fila dada
-- que se ha confirmado contiene la x
hallarCharEnFila::[Char]->Char->Int
hallarCharEnFila (x:xs) chr = if x==chr then 0
				 else 1 + hallarCharEnFila xs chr				 
				 
sumarTuplas::(Int,Int)->(Int,Int)->(Int,Int)
sumarTuplas (x,y) (w,z) = (x+w, y+z)

wtfIsAt::[[Char]]->(Int,Int)->Char
wtfIsAt maze (x,y) = head (drop y (head(drop x maze)))

--encontrarCamino  (x,y,with 0 yes/no 1/0)
--encontrarCamino lista = [hallarCharXY lista ]:[]
encontrarSiguiente::[[Char]]->(Int,Int)->(Int,Int)
encontrarSiguiente list (x,y) 
	| move Norte (x,y) == '0' = (move Norte (x,y))
	| move Este (x,y) == '0' = (move Este (x,y))
	| move Sur (x,y) == '0' = (move Sur (x,y))	
	| otherwise = (move Oeste (x,y))

--toma un laberinto y devuelve un arreglo de coordenadas que designa el camino solucion
resolverLabe::[[Char]]->(Int,Int)->[(Int,Int)]
resolverLabe [] = [(0,0)]	 



---------------------------[(1,4), ???] 

--busca la siguiente posicion de camino segun la posicion actual dada



--queda pendiente moverse y preguntarse si es camino, si no lo es, mirar en otra direccion
-- lo que se devuelve es un arreglo de coordenadas por las que se paso en una misma llamada o pila de llamadas

--wtfIsAt (izq hallarCharXY)  --actual call

--StrHallarCharF::


--toStrLista2D::


--registarPosicion::


printL::[[Char]]->IO()
printL [] = return ()
printL (x:xs) = do	
					print x
					printL xs --(xs:[])

--Tomar el laberinto raw y dar una fila (tipo lista) a ser operada
obtenerCabeza:: [[Int]] -> [Int]
obtenerCabeza [] = []
obtenerCabeza (x:xs) = x

{--

ghci> zip [5,3,2,6,2,7,2,5,4,6,6] ["soy","una","tortuga"]
[(5,"soy"),(3,"una"),(2,"tortuga")]

ghci> unzip [('3','3'),('1','2')]
("31","32")

ghci> take 1 [['3','1']]
["31"]
--}