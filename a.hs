fact n = product [1..n]

falling x n =
	if n < 0 then 0
	  else tmp x n where
		  tmp _ 0 = 1
		  tmp x n = x*(tmp (x-1) (n-1))

rising x n =
	if n < 0 then 0
	  else tmp x n where
		  tmp _ 0 = 1
		  tmp x n = x*(tmp (x+1) (n-1))

binom n k =
	if k < 0 then 0
	  else round $ (falling n k) / (fact k)

class Evaluable expr where
	eval :: expr -> String -> Integer -> expr
	getVars :: expr -> [String]
	getValue :: expr -> Maybe Integer

	fullEval :: expr -> [(String, Integer)] -> Either Integer expr
	fullEval e list =
		let fun ex (a,b) = eval ex a b in
			let tmp = foldl fun e list in
				case getValue tmp of
					Nothing -> Right tmp
					Just x -> Left x


data Arithmetic =
	Constant Integer |
	Variable String |
	Plus Arithmetic Arithmetic |
	Minus Arithmetic Arithmetic |
	Times Arithmetic Arithmetic

data CombExpr =
	Simple Arithmetic |
	Binom CombExpr CombExpr |
	Fact CombExpr CombExpr

data ComplexExpr =
	Combinatorial CombExpr |
	Sum String Integer Integer ComplexExpr
	--Prod
	--todo: bardziej zawansowane iteracjie niz prosty range, np: po parzystych

{--
	Div SimpleExpr SimpleExpr |
	--potegowanie
	--liczby Stirlinga
	--potegi gorne/dolne
	--liczby Bella
	--liczby harmoniczne
	--fibonacci, ogolnie jakies rekurencje
	--liczby Eulera
	--podloga
--}

{-Todo: wczytywanie, wypisywanie w texu
zmienne lokalne nie moga przeslaniac globalncyh!

-}
main = do
	putStrLn "Hello world"

	
	
