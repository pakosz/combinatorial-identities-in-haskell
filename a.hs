--test
fact :: Integer -> Integer
fact n = product [1..n]

falling :: Integer -> Integer -> Integer
falling x n =
	if n < 0 then 0
	  else tmp x n where
		  tmp _ 0 = 1
		  tmp x n = x*(tmp (x-1) (n-1))

rising :: Integer -> Integer -> Integer
rising x n =
	if n < 0 then 0
	  else tmp x n where
		  tmp _ 0 = 1
		  tmp x n = x*(tmp (x+1) (n-1))

binom :: Integer -> Integer -> Integer
binom n k =
	if k < 0 then 0
	  else div (falling n k) (fact k)

class Evaluable expr where
	eval :: expr -> String -> Integer -> expr
	reduce :: expr -> expr
	getVars :: expr -> [String]
	getValue :: expr -> Maybe Integer


fullEval e list =
	let fun ex (a,b) = eval ex a b in
		let tmp = foldl fun e list in
			case getValue tmp of
				Nothing -> Right tmp
				Just x -> Left x



{--todo:
	--dzielenie
	--potegowanie
	--liczby Stirlinga
	--potegi gorne/dolne
	--liczby Bella
	--liczby harmoniczne
	--fibonacci, ogolnie jakies rekurencje
	--liczby Eulera
	--podloga, sufit
--}
data OneArgType = Fact
oneArgFun Fact = fact

data TwoArgType = Plus | Minus | Times | Binom
twoArgFun Plus = (+)
twoArgFun Minus = (-)
twoArgFun Times = (*)
twoArgFun Binom = binom

data SimpleExpr =
	SimpleConstant Integer |
	SimpleVariable String |
	SimpleOneArg OneArgType SimpleExpr |
	SimpleTwoArg TwoArgType SimpleExpr SimpleExpr

data ComplexExpr =
	Constant Integer |
	Variable String |
	OneArg OneArgType ComplexExpr |
	TwoArg TwoArgType ComplexExpr ComplexExpr |
	Sum String Integer Integer ComplexExpr
	--Prod
	--todo: bardziej zawansowane iteracjie niz prosty range, np: po parzystych
	--todo: sumy ze zmienna liczba sigm

instance Evaluable SimpleExpr where
	getValue expr = case expr of
		      SimpleConstant t -> Just t
		      otherwise -> Nothing

	--todo: powinno zwracac unikalne rezultaty
	getVars expr =
		case expr of
			SimpleConstant _ -> []
			SimpleOneArg _ e -> getVars e
			SimpleTwoArg _ e1 e2 -> (getVars e1) ++ (getVars e2)
			SimpleVariable s -> [s]
	
	reduce expr =
		case expr of
			SimpleConstant x -> SimpleConstant x
			SimpleVariable s -> SimpleVariable s
			SimpleOneArg t e -> let u = reduce e in
				case u of
					SimpleConstant x -> SimpleConstant (oneArgFun t x)
					otherwise -> SimpleOneArg t u
			SimpleTwoArg t e1 e2 -> let (u1,u2) = (reduce e1, reduce e2) in
				case (u1, u2) of
					(SimpleConstant x1, SimpleConstant x2) -> SimpleConstant (twoArgFun t x1 x2)
					otherwise -> SimpleTwoArg t u1 u2

	eval e name val = reduce $ substitute e name val where
		substitute e name val =
			case e of
				SimpleVariable s -> if name == s then SimpleConstant val else SimpleVariable s
				SimpleOneArg t ex -> SimpleOneArg t $ substitute ex name val
				SimpleTwoArg t ex1 ex2 -> SimpleTwoArg t  (substitute ex1 name val) $ substitute ex2 name val
				x -> x

instance Evaluable ComplexExpr where
	reduce = undefined --todo
	eval = undefined --todo
	getVars = undefined --todo
	getValue = undefined --todo




{-Todo: wczytywanie, wypisywanie w texu
sprowadzanie wyrazenia do postaci kanonicznej (sigmy i pi wystepuja na poczatku)
zmienne lokalne nie moga przeslaniac globalncyh!

-}
main = do
	putStrLn "Hello world"
