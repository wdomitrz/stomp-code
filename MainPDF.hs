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

mainPDF :: IO ()
mainPDF = do
  args <- getArgs
  let file_name = head args
  str <- readFile $ file_name
  let ext = reverse $ take 3 (reverse file_name)
  (_, cir_in) <- case ext of
    ".qc" -> parseQC str
    "tfc" -> parseTfc str
    _ -> parseQuipper str
  let term = c2term cir_in
  let t_before = tCount term
  let term_reduced = heuri5 term
  let t_after = tCount term_reduced
  let file_name' = drop 16 file_name
  --  putStrLn file_name'
  --  putStrLn $ show cir_in
  topdf_file $ z2x cir_in

--  appendFile (args !! 1) ("\n" ++  take 15 (file_name' ++ "              ") ++ "           " ++ show t_before  ++ "            " ++ show t_after )

main :: IO ()
main = mainPDF
