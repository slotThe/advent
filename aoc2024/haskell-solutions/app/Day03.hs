{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-compat-unqualified-imports #-}
import Util
import Text.ParserCombinators.ReadP hiding (many)

type Ins :: Type
data Ins = Do | Dont | Mul{unMul :: !Int}

pMul :: ReadP (Maybe Ins)
pMul = (Just . Mul) .: (*) <$> ("mul(" *> pNum <* ",") <*> (pNum <* ")")

pGarbage :: ReadP (Maybe Ins)
pGarbage = (Nothing <$ munch1(`notElem`['d','m'])) <++ (Nothing <$ get)

main :: IO ()
main = do
  inp <- readFile' "../inputs/day03.txt"
  let pWith :: ReadP (Maybe Ins) -> [Ins] =
        \p -> catMaybes . fst . last . readP_to_S (many $ pMul <++ p <++ pGarbage) $ inp
      sumIfDo :: [Ins] -> (Bool, Int) =
        foldl' (\(e, acc) x -> case x of
                           Do -> (True, acc)
                           Dont -> (False, acc)
                           Mul m -> (e, acc + if e then m else 0))
               (True, 0)
  let one = sum . map unMul $ pWith empty
      two = snd . sumIfDo $ pWith (Just <$> (Dont <$ "don't()") <++ (Do <$ "do()"))
  day 3 (pure (one, two))
