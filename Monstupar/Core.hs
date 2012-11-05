-- Extremely simple but monstrously stupid (and slow) monadic parser
-- combinator library
module Monstupar.Core
    ( ParseError(..)
    , Monstupar, runParser
    , ok, isnot, eof, (<|>), like
    ) where

--------------------------------------------------------------------------------
-- �����������

-- ���� ����� ����������� ����� �������� �� ��, ��� ���������
data ParseError = ParseError
                deriving (Show) -- ���� �� show ���

newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a) }

instance Monad (Monstupar s) where
    return a = Monstupar $ \s -> Right (s , a)
    ma >>= f = Monstupar $ \s -> case runParser ma s of
				Left ParseError -> Left ParseError
				Right (xs, a) -> runParser (f a) xs 

--------------------------------------------------------------------------------
-- ����������� �������.
-- ����� � ��������� ������� ������ ������, ���� �����

-- �� ������
ok :: Monstupar s ()
ok = Monstupar $ \s -> Right (s , ())

-- �� ������ ��������� �������� p
isnot :: Monstupar s () -> Monstupar s ()
isnot p = Monstupar $ \s -> case runParser p s of
    Left e -> Right (s , ())
    Right _ -> Left ParseError

-- ����� �����
eof :: Monstupar s ()
eof = Monstupar $ \s -> case s of
    [] -> Right (s , ())
    _  -> Left ParseError

infixr 2 <|>
-- ������� ������ ������, ���� �� ��������, �� ������
(<|>) :: Monstupar s a -> Monstupar s a -> Monstupar s a
a <|> b = Monstupar $ \s -> case runParser a s of
					Left e -> runParser b s
					Right (xs, a) -> Right(xs, a)

-- � ������ ����� ������ �����, ��������������� p
like :: (s -> Bool) -> Monstupar s s
like p = Monstupar $ \s -> case s of
		[] -> Left ParseError
		(x:xs) -> if p x then Right (xs, x) else Left ParseError

-- ���� ����� ��������� ��� �����-�� ����������� �������
-- ���� ��� �����������

