module Main where

import qualified Circuit as C
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import Fast
import qualified Fast
import GateStruct
import QCParser
import Quipper
import Quipper.Libraries.QuipperASCIIParser
import QuipperParser
import System.CPUTime
import System.Environment
import System.IO
import System.Random
import TfcParser
import qualified ToFile as ToF
import qualified ToQC as QC
import Todd
import qualified ZX

run :: StdGen -> [(String, String)] -> [String] -> IO ()
run stdgen options [x] = do
  let file_name = x
  str <- readFile x

  let ext = reverse $ take 3 (reverse file_name)

  --  putStrLn ext

  (ivq, cir_in) <- case ext of
    ".qc" -> parseQC str
    "tfc" -> {-# SCC "parseTfc-" #-} parseTfc str
    _ -> parseQuipper str
  print ivq

  starts <- getCPUTime

  let homoids = case lookup "-identity" options of
        Nothing -> []
        Just ids -> case ids of
          "4" -> ZX.id4s
          "6" -> ZX.id6s
          "45" -> ZX.id4s ++ ZX.id45s
          "56" -> ZX.id56s
          "96" -> ZX.wid96
          "4-56" -> ZX.id4s ++ ZX.id56s
          "456" -> ZX.id4s ++ ZX.id45s ++ ZX.id6s
          "4567" -> ZX.id4s ++ ZX.id45s ++ ZX.id6s ++ ZX.id67s

  let wireids =
        ( case lookup "-identity" options of
            Nothing -> const return
            Just ids -> case ids of
              "4" -> (\f x -> f ZX.id4s x)
              "6" -> (\f x -> f ZX.id6s x)
              "45" -> (\f x -> f ZX.id4s x >>= f ZX.id45s)
              "56" -> (\f x -> f ZX.id56s x)
              "96" -> (\f x -> f ZX.wid96 x)
              "4-56" -> (\f x -> f ZX.id4s x >>= f ZX.id56s)
              "456" -> (\f x -> f ZX.id4s x >>= f ZX.id45s >>= f ZX.id6s)
              "4567" -> (\f x -> f ZX.id4s x >>= f ZX.id45s >>= f ZX.id6s >>= f ZX.id67s)
        ) ::
          ([ZX.Identity] -> ZX.TGCG -> ZX.LMMR) -> ZX.TGCG -> ZX.LMMR
  let order = Data.Maybe.fromMaybe "" (lookup "-order" options)

  {-  let cir_s' = case lookup "-identity" options of
          -- only fusion
          Nothing -> {-# SCC "cir2lmmr-" #-} ZX.cir2lmmr' cir_in
          Just ids -> ({-# SCC "cir2lmmr-" #-} ZX.cir2lmmr' cir_in) >>= case order of
              "homo-eager" -> (ZX.runIds_r stdgen homoids)
              "homo" ->  (ZX.runIds_r stdgen homoids)
              "homo-linear" ->  (ZX.runIds_r stdgen homoids)
              "wire-eager" -> (wireids $ ZX.runIds_r stdgen)
              "wire" -> (wireids $ {-# SCC "runIds_r-" #-} ZX.runIds_r stdgen )

  -}
  let cir_s = (ZX.initLMR' ivq (desugar_cir cir_in) >>= ZX.cir2lmmr') >>= ZX.runIds_rw stdgen 20000 (ZX.id4s ++ ZX.zxid45s) -- >>= (ZX.runIds_r stdgen 5000  (take 127 ZX.id56s ++ ZX.wid96))
  let (af, ((ll, rr), wcw@(vq, fq), tct@(int, fut, idt))) = runState (cir_s >>= ZX.tct) (([], []), (0, 0), (0, 0, 0))
  let cin = ToF.cir2string vq vq cir_in
  let cio = ToF.cir2string vq fq (ll ++ (ZX.gads2cir2 (fst af) ++ (ZX.gads2cir2 (snd af) ++ rr)))
  path <- getExecutablePath
  let fn = takeWhileB (/= '/') file_name
  let file_name1 = "Stomp/" ++ fn ++ "_i.qc"
  putStrLn "Hello"
  writeFile file_name1 cin
  let file_name2 = "Stomp/" ++ fn ++ "_o.qc"
  writeFile file_name2 cio
  case lookup "-tcount" options of
    Nothing -> putStrLn "no -tcount option"
    Just tc -> do
      let t_before = int -- ZX.tcount_zx $ ZX.tolxr cir_in
      --   let t_fusion = ZX.tcount $ ZX.cir2lmmr' cir_in
      let t_fusion = fut
      --      let (w_after, t_after) = seq cir_s (length $ filter isInitg $ ZX.tocir cir_s, ZX.tcount $ cir_s)
      let t_after = idt
      let w_after = fq
      let fname' = if length file_name > 150 then take 19 (drop 15 file_name ++ "              ") else take 19 $ file_name ++ "              "
      appendFile tc ("\n" ++ fname' ++ "       " ++ show (tct, idt - fut) ++ "        " ++ show (vq, fq, fq - vq))
      print (tct, idt - fut, (vq, fq, fq - vq))

  --  let result        = stomp options cir_in
  ends <- getCPUTime
  let time = fromIntegral (ends - starts) / 10 ^ 12
  let tc = unJust $ lookup "-tcount" options
  appendFile tc ("       " ++ show time)
  putStrLn $ "Success (took " ++ take 6 (show time) ++ "s)"
--  putStrLn result

run stdgen options (x : y : xs)
  | x == "-tcount" = run stdgen (("-tcount", y) : options) xs
  | x == "-ancilla-used" = run stdgen (("-ancilla-used", y) : options) xs
  | x == "-order" = run stdgen (("-order", y) : options) xs
  | x == "-identity" = run stdgen (("-identity", y) : options) xs
  | x == "-pdf" = run stdgen (("-pdf", y) : options) xs
run stdgen _ _ = do
  putStrLn "Invalid argument(s)"

main :: IO ()
main = do
  stdgen <- getStdGen
  getArgs >>= run stdgen []
