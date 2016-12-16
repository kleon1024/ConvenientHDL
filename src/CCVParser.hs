module VerilogParser where

import Language.Verilog
import System.IO

-- readVerilogFile :: String -> String -> String
-- readVerilogFile filename filepath = do
--     verilogFile <- readFile (filepath ++ filename)

-- parseVerilogFile :: String -> String -> [Module]
-- parseVerilogFile filename filepath = do
--     verilogFile <- readFile (filepath ++ filename)
--     parseFile [] (filepath ++ filename) verilogFile