{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Papers.RecursionSchemes where

import Control.Arrow
import Data.Foldable
import Data.Function
import Debug.Trace
import GHC.Natural

data ExprF a = Num Int | Add a a | Mul a a deriving (Eq, Functor)

newtype Fix f = Fix {out :: f (Fix f)}

type Expr = Fix ExprF

-----------------------------------------------------------
-- catamorphism
type Algebra f a = f a -> a

eval :: Algebra ExprF Int
eval (Num a) = a
eval (Add a b) = a + b
eval (Mul a b) = a * b

countOps (Num _) = 0
countOps (Add a b) = a + b + 1
countOps (Mul a b) = a + b + 1

cata :: (Functor f) => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out -- out >>> fmap (cata f) >>> f

numF a = Fix (Num a)

addF a b = Fix (Add a b)

mulF a b = Fix (Mul a b)

expr1 = mulF (numF 2) (addF (numF 1) (numF 3))

-- >>> cata eval expr1
-- >>> cata countOps expr1
-- 8
-- 2

------------------------------------------------------
-- anamorphism

showExpr :: Expr -> String
showExpr (Fix (Num a)) = show a
showExpr (Fix (Add a b)) = "(" ++ showExpr a ++ " + " ++ showExpr b ++ ")"
showExpr (Fix (Mul a b)) = "(" ++ showExpr a ++ " * " ++ showExpr b ++ ")"

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

nested :: Int -> Expr
nested n = ana go n
  where
    go :: Coalgebra ExprF Int
    go 0 = Num 1
    go n = Add (n - 1) (n - 1)

-- >>> showExpr $ nested 4
-- "((((1 + 1) + (1 + 1)) + ((1 + 1) + (1 + 1))) + (((1 + 1) + (1 + 1)) + ((1 + 1) + (1 + 1))))"
--

--------------------------------------------------------------------------------------------
-- paramorphisms
-- For example, though the function we pass to cata allows us to view the data we’re transformating, it loses information about the original structure

type RAlgebra f a = f (Fix f, a) -> a

para :: (Functor f) => RAlgebra f a -> Fix f -> a
para ralg = ralg . fmap fanout . out
  where
    fanout x = (x, para ralg x)

eval1 :: RAlgebra ExprF Int
eval1 (Num x) = x
eval1 (Add (a', a) (b', b)) = trace (showExpr a' ++ " " ++ showExpr b') a + b
eval1 (Mul (_, a) (_, b)) = a * b

expr2 = addF (numF 2) (numF 3)

x = para eval1 expr2

-- >>> x
-- 2 3
-- 5
--

type RAlgebra' f a = Fix f -> f a -> a

para' :: (Functor f) => RAlgebra' f a -> Fix f -> a
para' alg t = out t & fmap (para' alg) & alg t

eval1' :: RAlgebra' ExprF Int
eval1' y (Num x) = x
eval1' y (Add a b) = trace (showExpr y) a + b
eval1' y (Mul a b) = a * b

-- >>> para'  eval1' expr2
-- (2 + 3)
-- 5
--

----------------------------------------------------------
-- histomorphism

data Cofree f a = a :< (f (Cofree f a)) deriving (Foldable)

showAttr :: Cofree ExprF Int -> String
showAttr (a :< (Num x)) = "(" ++ show a ++ " -- Num" ++ show a ++ ")"
showAttr (a :< (Add x y)) = "(" ++ show a ++ " -- Add" ++ showAttr x ++ " " ++ showAttr y ++ ")"
showAttr (a :< (Mul x y)) = "(" ++ show a ++ " -- Mul" ++ showAttr x ++ " " ++ showAttr y ++ ")"

show2 (a :< Z) = show a ++ "Z"
show2 (a :< (S b)) = show a ++ show2 b

type CVAlgebra f a = f (Cofree f a) -> a

--histo :: Functor f => CVAlgebra f a -> Fix f -> a
--histo halg = halg . fmap go . out
--  where
--    --go :: Fix f -> Attr f a
--    go x@(Fix f) = histo halg x :< fmap go f
histo h = worker >>> (\(a :< b) -> a)
  where
    worker = out >>> fmap worker >>> (h &&& id) >>> mkAttr
    mkAttr (a, b) = a :< b

eval2 :: CVAlgebra ExprF Int
eval2 (Num a) = a
eval2 (Add x@(a :< _) y@(b :< _)) = trace (showAttr x ++ showAttr y) a + b
eval2 (Mul (a :< _) (b :< _)) = a * b

testHisto = histo eval2 expr2

data NatF a
  = Z
  | S a
  deriving (Functor, Foldable)

toNat 0 = Fix Z
toNat n = Fix (S (toNat (n - 1)))


fibAlg :: CVAlgebra NatF Integer
fibAlg Z = 1
fibAlg (S xs) = case toList xs of
  [x] -> x
  (a : b : as) ->   a + b

catalanAlg :: CVAlgebra NatF Integer
catalanAlg Z = 1
catalanAlg (S fs) =
  let xs = toList fs
      ys = reverse xs
   in sum $ zipWith (*) xs ys

-- >>> testHisto
-- (0.00 secs, 690,680 bytes)
-- (2 -- Num2)(3 -- Num3)
-- 5
-- (0.01 secs, 671,560 bytes)
--

-- >>> histo fibAlg (toNat 2000)
-- (0.00 secs, 701,776 bytes)
-- 6835702259575806647045396549170580107055408029365524565407553367798082454408054014954534318953113802726603726769523447478238192192714526677939943338306101405105414819705664090901813637296453767095528104868264704914433529355579148731044685634135487735897954629842516947101494253575869699893400976539545740214819819151952085089538422954565146720383752121972115725761141759114990448978941370030912401573418221496592822626
-- (0.01 secs, 3,402,480 bytes)
--

histoSpecialized = undefined

-- >>> cat 11
-- 16796
--

cat :: Integer -> Integer
cat 1 = 1
cat n = sum [cat i * cat (n - i) | i <- [1 .. n - 1]]

histo' :: ([a] -> a) -> Natural -> a
histo' f = head . go
  where
    go 0 = [f []]
    go x = let subvalues = go (x - 1) in f subvalues : subvalues

fibN :: Natural -> Integer
fibN = histo' $ \x -> case x of
  (x : y : _) -> x + y
  _ -> 1

-- >>> fibN 10000
-- 54438373113565281338734260993750380135389184554695967026247715841208582865622349017083051547938960541173822675978026317384359584751116241439174702642959169925586334117906063048089793531476108466259072759367899150677960088306597966641965824937721800381441158841042480997984696487375337180028163763317781927941101369262750979509800713596718023814710669912644214775254478587674568963808002962265133111359929762726679441400101575800043510777465935805362502461707918059226414679005690752321895868142367849593880756423483754386342639635970733756260098962462668746112041739819404875062443709868654315626847186195620146126642232711815040367018825205314845875817193533529827837800351902529239517836689467661917953884712441028463935449484614450778762529520961887597272889220768537396475869543159172434537193611263743926337313005896167248051737986306368115003088396749587102619524631352447499505204198305187168321623283859794627245919771454628218399695789223798912199431775469705216131081096559950638297261253848242007897109054754028438149611930465061866170122983288964352733750792786069444761853525144421077928045979904561298129423809156055033032338919609162236698759922782923191896688017718575555520994653320128446502371153715141749290913104897203455577507196645425232862022019506091483585223882711016708433051169942115775151255510251655931888164048344129557038825477521111577395780115868397072602565614824956460538700280331311861485399805397031555727529693399586079850381581446276433858828529535803424850845426446471681531001533180479567436396815653326152509571127480411928196022148849148284389124178520174507305538928717857923509417743383331506898239354421988805429332440371194867215543576548565499134519271098919802665184564927827827212957649240235507595558205647569365394873317659000206373126570643509709482649710038733517477713403319028105575667931789470024118803094604034362953471997461392274791549730356412633074230824051999996101549784667340458326852960388301120765629245998136251652347093963049734046445106365304163630823669242257761468288461791843224793434406079917883360676846711185597501
--

catalanHisto :: Natural -> Natural
catalanHisto = histo' $ \x -> case x of
  [] -> 1
  fs ->
    let ys = reverse fs
     in sum $ zipWith (*) fs ys

-- >>> catalanHisto 1000
-- 2046105521468021692642519982997827217179245642339057975844538099572176010191891863964968026156453752449015750569428595097318163634370154637380666882886375203359653243390929717431080443509007504772912973142253209352126946839844796747697638537600100637918819326569730982083021538057087711176285777909275869648636874856805956580057673173655666887003493944650164153396910927037406301799052584663611016897272893305532116292143271037140718751625839812072682464343153792956281748582435751481498598087586998603921577523657477775758899987954012641033870640665444651660246024318184109046864244732001962029120
--


