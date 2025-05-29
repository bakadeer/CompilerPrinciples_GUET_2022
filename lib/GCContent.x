{
module GCContent (main) where
import System.IO
import Text.Printf (printf)  -- 导入 printf 函数
}

%wrapper "basic"

$base = [ACGT]    -- 定义有效碱基

tokens :-
  $base+      { \s -> s }  -- 匹配一个或多个碱基
  $white+     ;            -- 忽略空白字符
  .           ;            -- 忽略其他字符

{
-- 计算GC比例
calcGC :: String -> Float
calcGC seq = let gcCount = length [c | c <- seq, c `elem` ['G', 'C']]
                 total = length seq
             in if total > 0 then fromIntegral gcCount / fromIntegral total else 0.0

main :: IO ()
main = do
  input <- getContents
  let sequences = words input  -- 按空白分割输入
  mapM_ (\seq -> printf "%.3f\n" (calcGC seq)) sequences
}
