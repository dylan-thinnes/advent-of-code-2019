{-# LANGUAGE MultiWayIf #-}
module D14 where

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!),(!?))
import Data.Maybe
import Data.Function ((&))
import Data.List.Split
import Debug.Trace

data Quantity = Quantity { qty :: Int, total :: Int } deriving Show
type Quantities = M.Map String Quantity
type Equations = M.Map String (Int, [(String, Int)])

test1 = toEquations "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL"
test2 = toEquations "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX"
input = toEquations "8 SPJN, 2 LJRB, 1 QMDTJ => 1 TFPRF\n111 ORE => 5 GCFP\n5 NGCKP => 6 QXQZ\n21 RGRLZ => 7 DKVN\n2 DCKF => 9 FCMVJ\n7 SGHSV, 4 LZPCS => 9 DQRCZ\n4 QNRH => 8 WGKHJ\n135 ORE => 6 BPLFB\n4 SPJN, 1 DCKF, 9 KJVZ, 1 DKVN, 4 ZKVPL, 11 TFPRF, 1 CWPVT => 8 BVMK\n8 TGPV, 4 MQPLD => 2 SPFZ\n11 QMDTJ, 15 LVPK, 5 LZPCS => 3 KJVZ\n2 RNXF, 3 MKMQ => 6 LJRB\n11 RKCXJ, 4 BJHW, 2 DKDST => 3 QNRH\n3 NZHP, 1 QMDTJ => 9 BCMKN\n10 DQRCZ, 1 GBJF => 7 RGRLZ\n2 WLKC, 1 GBJF, 7 SPJN => 5 GBWQT\n4 TGPV, 1 LTSB => 2 LZPCS\n6 LJRB => 4 LQHB\n3 LZPCS, 3 MDTZL, 12 DLHS => 2 CBTK\n1 TGPV, 1 CQPR => 9 XQZFV\n26 FSQBL => 8 HQPG\n9 LQHB => 1 GBJF\n7 NGCKP => 5 WLKC\n9 DKDST, 1 XQZFV => 9 TPZBM\n144 ORE => 9 RNXF\n1 LJRB => 6 CQPR\n9 MKMQ, 12 RNXF => 9 JWPLZ\n5 LZPCS, 28 QMDTJ, 1 QNRH => 5 LVPK\n5 TGPV, 1 HQPG => 6 FCBLK\n8 LVPK, 9 DQRCZ, 1 MDTZL => 6 DCKF\n1 RKCXJ, 2 LZPCS, 13 LJNJ => 1 QWFG\n4 DKDST, 1 XQZFV, 10 NSXFK => 4 JRDXQ\n7 QWFG, 1 BVMK, 4 BJHW, 21 QNSWJ, 3 FBTW, 3 FCBLK, 59 SPFZ, 4 GBWQT => 1 FUEL\n28 LZPCS, 17 NGCKP, 1 MQPLD => 5 MDTZL\n1 FCBLK, 5 WGKHJ => 7 ZKVPL\n7 LJNJ => 9 BLDJP\n11 FSQBL, 2 BCMKN, 1 CBTK => 9 CWPVT\n1 BJHW => 1 MQPLD\n11 SGHSV, 3 LJNJ => 1 NGCKP\n2 FSQBL, 7 FCBLK, 1 CQPR => 4 RKCXJ\n1 JRDXQ => 4 SGHSV\n107 ORE => 6 MKMQ\n1 DQRCZ, 3 QMDTJ, 9 XQZFV => 4 FZVH\n6 NSXFK, 1 MKMQ => 6 DLHS\n4 CQPR, 1 RNXF, 1 HQPG => 5 DKDST\n9 RNXF => 8 LTZTR\n1 LTSB, 8 BLDJP => 4 SPJN\n1 FCBLK => 4 LJNJ\n1 NGCKP => 3 NZHP\n11 LZPCS, 22 DQRCZ, 1 QWFG, 1 QXQZ, 6 DKVN, 16 FZVH, 3 MQPLD, 23 HQPG => 3 QNSWJ\n26 DLHS, 1 NSXFK => 9 BJHW\n3 FCBLK, 10 HQPG => 3 LTSB\n10 LTZTR, 13 JWPLZ, 16 FSQBL => 4 TGPV\n11 LTSB, 1 XQZFV, 3 DQRCZ => 4 CZCJ\n1 HQPG, 12 XQZFV, 17 TPZBM => 6 QMDTJ\n2 LTZTR => 7 FSQBL\n1 GCFP, 5 BPLFB => 1 NSXFK\n3 KJVZ, 1 QXQZ, 6 DKDST, 1 FCMVJ, 2 CZCJ, 1 QNRH, 7 WLKC => 4 FBTW"

toEquations :: String -> Equations
toEquations = M.fromList . map toEquation . lines
    where
        toEquation :: String -> (String, (Int, [(String, Int)]))
        toEquation s
          = let [reqStr, outStr] = splitOn " => " s
                f = (\[q,i] -> (i, read q))
                reqs = f . words <$> splitOn ", " reqStr
                (resource, qty) = f $ words outStr
             in (resource, (qty, reqs))

add :: Int -> Quantity -> Quantity
add i (Quantity x y) = Quantity (x + i) (y + i)

addM :: Int -> Maybe Quantity -> Quantity
addM i = add i . fromMaybe (Quantity 0 0)

getQty :: String -> Quantities -> Quantity
getQty resource = fromMaybe (Quantity 0 0) . (!? resource)

use :: Int -> Quantity -> Quantity
use i (Quantity x y) = Quantity (x - i) y

consume :: Equations -> (String, Int) -> Quantities -> Quantities
consume eqs (resource, needed) quantities
  = let existing = qty $ fromMaybe (Quantity 0 0) $ quantities !? resource
     in quantities
      & (if existing >= needed
           then id
           else provision eqs (resource, needed - existing)
        )
      & M.adjust (use needed) resource

provision :: Equations -> (String, Int) -> Quantities -> Quantities
provision eqs ("ORE", qty) quantities
  = M.alter (Just . addM qty) "ORE" quantities
provision eqs (resource, qty) quantities
  = let (product, requirements) = eqs ! resource
        multiples = ceiling $ fromIntegral qty / fromIntegral product
        totalManufactured = product * multiples
        f quantities spec
            = consume eqs spec quantities
     in M.alter (Just . addM totalManufactured) resource 
      $ foldl f quantities $ map (fmap (* multiples)) requirements

maxFuel :: Equations -> Int -> Int
maxFuel equations ore = f 0 ore
    where
        try i = total $ getQty "ORE" $ provision input ("FUEL", i) M.empty
        f guess offset =
            let res   = try guess
             in if | res == ore || offset == 0 -> guess
                   | res > ore  -> f (guess - offset) (offset `div` 2)
                   | res < ore  -> f (guess + offset) (offset `div` 2)

