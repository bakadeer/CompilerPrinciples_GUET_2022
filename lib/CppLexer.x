{
module CppLexer (main) where
import System.IO
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$symbol = [\; \= \( \) \{ \} \< \> \" ]

tokens :-

  $white+          ; -- 忽略空格
  $digit+          { \s -> TokenNum s }
  $alpha+          { \s -> TokenWord s }
  $symbol          { \s -> TokenSymbol s }

{
data Token
  = TokenWord String
  | TokenNum String
  | TokenSymbol String
  deriving (Show)

main :: IO ()
main = do
  s <- getContents
  let tokens = alexScanTokens s
  mapM_ print tokens
}
