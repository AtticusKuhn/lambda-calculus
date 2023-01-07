module Main where
-- import Data.Fix


import Data.List (intercalate, elemIndex)
import Text.ParserCombinators.Parsec
import Data.Maybe (fromMaybe)
-- import Lib
data LC = Var String
  | App LC LC
  | Lambda String LC

data LCN = Vr Int
  | Ap LCN LCN
  | Lam LCN

 -- `` deriving (Show, Eq)
data Program = Program {
  defs :: [(String, LC)],
  mainDef :: LC
                       }
--  deriving (Show, Eq)
sample_prog :: Program
sample_prog = Program [("K", Lambda "a" (Lambda "b" (Var "a")))] (App (App (Var "K") (Var "x")) (Var "y") )

instance Show LC where
  show (Var x) = x
  show (App x y) = "(" ++ (show x) ++ " " ++ (show y) ++ ")"
  show (Lambda v l) = "(λ" ++ v ++ ". " ++ (show l) ++ ")"

instance Show LCN where
  show (Vr i)  = show i
  show (Ap a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (Lam body) = "(λ" ++ ". " ++ (show body) ++ ")"
instance Show Program where
  show (Program defs mainDef) = (intercalate "\n" (map show defs)) ++ "\n" ++ show mainDef

prog_to_lc :: Program -> LC
prog_to_lc (Program mydefs mymainDef) = foldr  fold_prog_to_lc mymainDef  mydefs

fold_prog_to_lc :: (String, LC) ->   LC  -> LC
fold_prog_to_lc  (name, def) main = App (Lambda name (main)) (def)

lc_to_lcn :: LC -> LCN
lc_to_lcn  = lc_to_lcn_aux []

lc_to_lcn_aux :: [String] -> LC -> LCN
lc_to_lcn_aux names (Var s) = Vr $ fromMaybe 1000 $ elemIndex s names
lc_to_lcn_aux names (App f x) = Ap (lc_to_lcn_aux names f) (lc_to_lcn_aux names x)
lc_to_lcn_aux names (Lambda var body) = Lam (lc_to_lcn_aux (var : names) body)

eval :: LCN -> LCN
eval (Ap fun arg) = case eval fun of
  Lam body -> eval $ sub 0 body where
                sub n e = case e of
                  Ap e1 e2 -> Ap (sub n e1) (sub n e2)
                  Lam e' -> Lam $ sub (n+1) e'
                  Vr n' | n == n'    -> arg  -- substitute. arg has no free vars.
                          | otherwise  -> Vr n'
  other -> Ap other arg
eval x = x

beta_reduction :: LCN -> LCN -> Int -> LCN
beta_reduction (Lam body) rep level = Lam $ beta_reduction body rep (level+1)
beta_reduction (Ap f x) rep level = Ap (beta_reduction f rep level) (beta_reduction x rep level)
beta_reduction (Vr i) rep level = if i == level then rep else Vr i

parse_lc :: GenParser Char st LC
parse_lc = parse_app
  <|> (Var <$>  parse_var)
  <|> parse_lambda

parse_app :: GenParser Char st LC
parse_app  = App <$> (string "." *> parse_lc) <*> (string " " *> parse_lc)

parse_var :: GenParser Char st String
parse_var  = many1 (noneOf "\n ,λ=.")

parse_lambda :: GenParser Char st LC
parse_lambda  = Lambda <$> (string "λ" *> parse_var) <*> (string " " *> parse_lc)

parse_def :: GenParser Char st (String, LC)
parse_def = do
  name <- parse_var
  string " "
  args <- many (parse_var <* string " ")   --- (parse_var) `sepBy` (string " ")
  string "= "
  res <- parse_lc
  return $ (name, foldr Lambda res args)

test_lc :: String
test_lc = "K x y = x"


parse_program :: GenParser Char st Program
parse_program = Program <$> (many (parse_def <* string "\n")) <*> parse_lc

test_program_str :: String
test_program_str = "K x y = x\n..K a b"


main :: IO ()
main = do
  test_program <- readFile "./app/test"
  let prog = parse parse_program "errr" test_program
  case prog of
    Left err -> print err
    Right p -> do
      let comp = prog_to_lc p
      print comp
      let lcn = lc_to_lcn comp
      print lcn
      let res = eval lcn
--let res = evaluate_lc comp
      print $ res
