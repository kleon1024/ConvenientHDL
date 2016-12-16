module VerilogHandle where

import System.Directory             (getDirectoryContents)
import System.FilePath              ((</>), replaceExtension, takeExtension)
import Text.PrettyPrint             (render)
import Text.Parsec                  (parse)
import Text.Parsec.ByteString       (parseFromFile)

import Language.Verilog.Parser
import Language.Verilog.PrettyPrint (ppExpr)
import Language.Verilog.Syntax     

import Data.Maybe                   (catMaybes, fromJust)

import ProjectConfig

-- main = do
--     result <- parseFromFile verilogFile "test.v"
--     case result of
--         Left err1  -> print "Error"
--         Right ast1 -> writeFile "test2.py" $ show ast1

parseFromString :: String -> Verilog
parseFromString x
  = case parse verilogFile "" x of
      Left err -> error (show err)
      Right y  -> y

parseFromFileList :: [FilePath] -> [IO Verilog]
parseFromFileList [] = []
parseFromFileList x = map parseVerilogFile x
    where
        parseVerilogFile fp = do
            result <- parseFromFile verilogFile fp
            case result of
                Left err -> do putStrLn $ "Fail1: " ++ fp ++ "\n" ++ show err ++ "\n"
                               return (Verilog [])
                Right ast -> return ast 

convertToModuleUnitList :: [FilePath] -> [Verilog] -> [ModuleUnit]
convertToModuleUnitList pathL vL = concat $ map convertFromVerilog (zip pathL vL)

convertFromVerilog :: (FilePath, Verilog) -> [ModuleUnit]
convertFromVerilog (path, v) = catMaybes $ map convertFromDescription (map (makeTuple path) (getDescription v))
                                where getDescription (Verilog x) = x
                                      makeTuple x y = (x, y)

convertFromDescription :: (FilePath, Description) -> Maybe ModuleUnit
convertFromDescription (path, UDPDescription _)      = Nothing
convertFromDescription (path, ModuleDescription (Module (Ident ident) mb_para port item))
    = Just (ModuleUnit ident path ""  (maybe [] getParaList mb_para) (getPortList port))
    where
        getParaList []    = []
        getParaList paras = (map ParaBlk) (map getParaAssigns paras)
        getParaAssigns (ParamDecl x) = map getParaAssign x
        getParaAssign (ParamAssign (Ident id) ex) = ParamUnit id (render $ ppExpr ex)
        getPortList = map getPortDecl
        getPortDecl (PortDecl pdir mb_ptype mb_prange (Ident name))
            = Port (name)
                   (getPortDirection pdir) 
                   (maybe EWire getPortType mb_ptype) 
                   (maybe "1" getPortLength mb_prange)  
                   (maybe "1" getPortWidth mb_prange) 
                   ""
        getPortDirection (PortDir Input) = DInput
        getPortDirection (PortDir Output) = DOutput
        getPortDirection (PortDir InOut) = DInOut
        getPortType (PortType Reg) = EReg
        getPortType (PortType Wire) = EWire
        getPortLength (Range ex1 ex2) = case ex2 of 
            ExprNum (IntNum _ _ _ "0") -> case ex1 of 
                ExprBinary Minus (ExprBinary Times n1 n2) (ExprNum (IntNum _ _ _ "1"))
                    -> (render $ ppExpr n1)
                _ -> (render $ ppExpr ex1)
            _ -> (render $ ppExpr ex1) ++ "-" ++ (render $ ppExpr ex2)
        getPortWidth (Range ex1 ex2) = case ex2 of
            ExprNum (IntNum _ _ _ "0") -> case ex1 of
                ExprBinary Minus (ExprBinary Times n1 n2) (ExprNum (IntNum _ _ _ "1"))
                    -> (render $ ppExpr n2)
                _ -> "1"
            _ -> "1"

convertToModuleList :: [Verilog] -> [Module]
convertToModuleList x = concat $ map getDescriptions x
    where
        getDescriptions (Verilog x) = catMaybes $ map getModule x
        getModule (UDPDescription _) = Nothing
        getModule (ModuleDescription mod) = Just mod

convertToProjectTreeList :: [Module] -> [ProjectTree]
convertToProjectTreeList xs =  catMaybes $ map (getProjectRoot xs) xs
    where getProjectRoot vlist x = case isRoot vlist x of
                                    True -> Just $ fromJust $ getProjectTree vlist (x, "top")
                                    False -> Nothing
          getProjectTree _ (_, "") = Nothing
          getProjectTree modList (modNode, nodeName) = 
                Just $ ProjectTree nodeName (getModuleName modNode) 
                (catMaybes $ map (getProjectTree modList) (zip (getInstanceNodes modList modNode) (getInstanceNames modNode)))
          getInstanceNodes modList hNode = catMaybes $ map (getInstanceNode modList) (getInstanceNames hNode)
          getInstanceNode _ [] = Nothing
          getInstanceNode hModList hNodeName = Just $ head $ catMaybes $ map (getSameNameNode hNodeName) hModList
          getSameNameNode hhNodeName hhMod = case (hhNodeName == (getModuleName hhMod)) of
                                                True -> Just hhMod
                                                False -> Nothing
          isRoot mods unit = not $ or $ (map (isInstanceOf unit) mods)
          isInstanceOf inst modu = (getModuleName inst) `elem` (getInstanceNames modu)
          getModuleName (Module (Ident n1) _ _ _) = n1
          getInstanceNames (Module _ _ _ item) = catMaybes $ map getInstanceName item
          getInstanceName (InstanceItem (Instance (Ident iname) _ _)) = Just iname
          getInstanceName _ = Nothing

