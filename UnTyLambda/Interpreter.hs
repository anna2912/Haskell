{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- � ������ ������� ��������� ����������� ������������� ���
-- ���������������� ������
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- �����-�� �������. ��������, ��� � ���� ������� ����
-- ������������ ������� Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- ����������� ��������� ��� ���������������� ������
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- ������ �� �� ���� ����������

-- ���� ������ ������ ������������ ����������� �������������, ��
-- � ��� ����� ������� ��� ������
-- (����� ������, ��� �� ����� ������� ����� ������ ��������,
-- ���� �������)

free (Var v)    = [ v ]
free (Lam v t)  = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

subst var what term = case term of
    Var v    -> if v == var then what else term
    Lam v t  -> if v == var then term else Lam v (subst var what t)
    App t t' -> App (subst var what t) (subst var what t')

newname fv = head . filter (not . flip elem fv) . iterate ('_':)

--- ...
betaReduct :: Variable -> Term -> Term -> Term
betaReduct var what term = subst var what $ newterm (free what) term
		where newterm vars subterm = 
				case subterm of 
						Var _ -> subterm
						Lam n t -> Lam nn newt 
								where 
										nn = if elem n vars then newname (vars ++ free t) n else n
										newt = if elem n vars then subst n (Var nn) t else t
						App t t' -> App (newterm vars t) (newterm vars t')
				  
				  
hasRedexes (Var _) = False
hasRedexes (App (Lam _ t) t') = True
hasRedexes (Lam v t) = hasRedexes t
hasRedexes (App t t') = hasRedexes t || hasRedexes t'

normalReduce term = case term of
		Var _ -> term
		Lam var subterm -> Lam var $ normalReduce subterm
		App (Lam var subterm) term' -> betaReduct var term' subterm
		App term term' -> if hasRedexes term
				then App (normalReduce term) term'
                else App term $ normalReduce term'

applicativeReduce term = case term of
		Var _ -> term
		Lam var subterm -> Lam var $ applicativeReduce subterm
		App term term' -> if hasRedexes term' 
				then App term $ applicativeReduce term' 
                else case term of
						Lam v subt -> betaReduct v term' subt
						_ -> App (applicativeReduce term) term'

inWeakHeadForm (Var _) = True
inWeakHeadForm (Lam _ _) = True
inWeakHeadForm (App (Lam _ t) t') = False
inWeakHeadForm (App t t') = inWeakHeadForm t

weakHeadReduce term = case term of
	App (Lam v t) t2 -> subst v t2 t
	App t1 t2 | not (inWeakHeadForm t1) -> App (weakHeadReduce t1) t2
	_ -> term
------------------------------------------------------------
-- �� ����������� ����, ��� ��������� ����������� ���������
-- ��������� ������������ (��� ��� ��������� ������������
-- ����� ����� �������������� � �������� ������� 
-- ��������� (n); ���� �� n ����� ������������� �� ������,
-- �� ������� ������� error, ������ ��� �������):

wh, no, wa, sa :: Integer -> Term -> Term

-- �������� ������������� ��������
sa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
sa n t = sa (n - 1) $ applicativeReduce t

-- ������������ ���������� ��������
no 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
no n t = no (n - 1) $ normalReduce t

-- �������� � ������ �������� ���������� �����
wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wh n t = wh (n - 1) $ weakHeadReduce t

-- (*) (�� �����������) �������� "������" ������������� ��������.
-- ���������� �� �������� �������������� ���, ��� �� ����� ������
-- ����� � ������ ����� ����������, ����� ��� ��������.
wa = undefined

-- ���������: c������� ������ ������ �������������� ���������� �� �����������,
-- ������ ����� ������������ ���� ���������� (� ��������� �� �����-���������)
-- ��� ��� ������������� ������ � ��������������� Term � ���� � �������.

-- ������������ ���� ���� �������� (� ������� �������� ��
-- �����������, ��)
orders =
    [ ("wh", wh)
    , ("no", no)
--    , ("wa", wa) -- ����� ����������������, ��
    , ("sa", sa) ]

------------------------------------------------------------
-- ����������� ���, ���� �������� ���������
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- ���� ����� ��������� �����
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- ������� ������������� ���������, ���� ��� ��� �������
--
-- ������� ���������� ��������, ��� ��������� � ����� �����
-- ��������� ���������� ��������, �� ��������� Haskell ��
-- ������ �� ��������� ����������������� ����������.
--
-- ����� ��� �������� ����������� � ������� ���� � �������
-- seq � ���������� ����� (���� ��������� ��� ��� ������ ��
-- �����������, �� �����-�� ����).
