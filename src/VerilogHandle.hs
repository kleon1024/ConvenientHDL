module VerilogHandle where

import System.Directory             (getDirectoryContents)
import System.FilePath              ((</>), replaceExtension, takeExtension)
import Text.PrettyPrint             (render)
import Text.Parsec                  (parse)
import Text.Parsec.ByteString       (parseFromFile)
import Data.List                    (intercalate)
-- import Text.Parsec.Prim             (Parsec,Stream)

-- import Control.Monad.Identity       (Identity)

import Language.Verilog.Parser
import Language.Verilog.PrettyPrint 
import Language.Verilog.Syntax     

import Language.Cynide.Parser       (cynideFile)
import qualified Language.Cynide.PrettyPrint as CP
import qualified Language.Cynide.Syntax as CS

import Data.Maybe                   (catMaybes, fromJust, isJust)

import ProjectConfig

parseExpr :: String -> ConstExpr
parseExpr x
    = case parse const_expr "" x of
        Left err -> error (show err)
        Right y  -> y

parsePortDir :: String -> PortDir
parsePortDir x
    = case parse portDir "" x of
        Left err -> error (show err)
        Right y  -> y

parsePortType :: String -> PortType
parsePortType x
    = case parse portType "" x of
        Left err -> error (show err)
        Right y  -> y

parseFromExpr :: CS.ConstExpr -> ConstExpr
parseFromExpr x 
    = case parse const_expr "" $ render (CP.ppExpr x) of
        Left err -> error (show err)
        Right y  -> y

parseFromPortDir :: CS.PortDir -> PortDir
parseFromPortDir x
    = case render $ CP.ppPortDir x of 
        "i" -> parsePortDir "input"
        "o" -> parsePortDir "output"
        "io"-> parsePortDir "inout"

parseFromPortType :: CS.PortType -> PortType
parseFromPortType x
    = case render $ CP.ppPortType x of
        "r" -> parsePortType "reg"
        "w" -> parsePortType "wire"

parseFromFileList :: [FilePath] -> [IO Verilog]
parseFromFileList [] = []
parseFromFileList x = map parseVerilogFile x

parseVerilogFile :: FilePath -> IO Verilog
parseVerilogFile fp = do
    result <- parseFromFile verilogFile fp
    case result of
        Left err -> do error $ "Fail1: " ++ fp ++ "\n" ++ show err ++ "\n"
        Right ast -> return ast 

printVerilog :: Verilog -> FilePath -> IO()
printVerilog fdata filePath = writeFile filePath $ render (ppVerilog fdata)

printVerilogWithHeader :: Verilog -> String -> FilePath -> FilePath -> ConfigData -> String -> IO()
printVerilogWithHeader fdata modName filePath fileName cdata date =
    writeFile filePath $ printLine ++ renderConfig ++ printLine ++ "\n" ++ 
    -- "`timescale 1ps/1ps\n" 
    (render (ppVerilog fdata)) 
    where 
        renderConfig =  
                (printInfo "File" fileName) ++ 
                (printInfo "Module" modName) ++ 
                (printInfo "Author" (author cdata)) ++ 
                (printInfo "Contact" (contact cdata)) ++ 
                (printInfo "Func" "") ++ 
                (printInfo "Create" date) ++
                (printInfo "Version" $ renderVersion (version cdata))
        printLine = (replicate 50 '/') ++ "\n"
        printInfo s1 s2 = "//" ++ s1 ++ (replicate (8 - length s1) ' ') ++": " ++ s2 ++ "\n"
        renderVersion (Version v1 v2 v3 v4) = intercalate "." $ map show [v1,v2,v3,v4]

printCynide :: CS.Cynide -> FilePath -> IO()
printCynide fdata filepath = writeFile filepath $ render (CP.ppCynide fdata)

convertToModuleUnitList :: [FilePath] -> [Verilog] -> [ModuleUnit]
convertToModuleUnitList pathL vL = concat $ map convertFromVerilog (zip pathL vL)

convertFromVerilog :: (FilePath, Verilog) -> [ModuleUnit]
convertFromVerilog (path, v) = catMaybes $ map convertFromDescription (map (makeTuple path) (getDescription v))
                                where makeTuple x y = (x, y)

getDescription (Verilog x) = x

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
        getPortDecl (PortDecl pdir mb_ptype mb_prange (Ident name) _)
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

parseCynideFile :: FilePath -> IO (Maybe CS.Cynide)
parseCynideFile x = do result <- parseFromFile cynideFile x
                       case result of
                            Left err -> do putStrLn $ "Fail1: " ++ x ++ "\n" ++ show err ++ "\n"
                                           return Nothing
                            Right ast -> return $ Just ast

convertCynideToVerilog :: [ModuleUnit] -> CS.Cynide  -> Verilog
convertCynideToVerilog ms (CS.Cynide cs)  = Verilog $ map (getCynideModule ms) cs
    where   
        getCynideModule ms (CS.Module (CS.Ident s) items) = 
            ModuleDescription (Module (Ident s) 
            (maybeEmpty $ concat $ (getFromItems (getParaMaybe) items)) 
            ((getFromItems (getInputMaybe) items) ++
            (getFromItems (getOutputMaybe) items) ++
            (getFromItems (getInOutMaybe) items))
            ((concat (getFromItems (getConMaybe) items)) ++ 
            (getFromItems (getMaybeWithMods) items)))
        getFromItems f x = catMaybes $ (map f) x
        getParaMaybe (CS.ParamDeclItem x) = Just $ convertCParamToVParam x
        getParaMaybe _  = Nothing
        getInputMaybe (CS.InputDeclItem x) = Just $ convertCInToVPort x
        getInputMaybe _  = Nothing
        getOutputMaybe (CS.OutputDeclItem x) = Just $ convertCOutToVPort x
        getOutputMaybe _  = Nothing
        getInOutMaybe (CS.InOutDeclItem x) = Just $ convertCInOutToVPort x
        getInOutMaybe _  = Nothing
        getConMaybe (CS.ConnectDeclItem x) = Just $ convertCConToVCon x
        getConMaybe _  = Nothing        
        getMaybeWithMods (CS.InstDeclItem x) = Just $ convertCInstToVInst x ms
        getMaybeWithMods _ = Nothing
        maybeEmpty [] = Just []
        maybeEmpty x = Just x

convertCParamToVParam :: CS.ParamDecl -> [ParamDecl]
convertCParamToVParam (CS.ParamDecl []) = []
convertCParamToVParam (CS.ParamDecl x ) = [ParamDecl $ map convertCParamAToVParamA x]
    where convertCParamAToVParamA (CS.ParamAssign (CS.Ident x) (cp)) =  ParamAssign (Ident x) (parseFromExpr cp)


convertCInToVPort (CS.InputDecl pt id l w) = genPortDecl Input pt id l w

convertCOutToVPort (CS.OutputDecl pt id l w) = genPortDecl Output pt id l w

convertCInOutToVPort (CS.InOutDecl pt id l w) = genPortDecl InOut pt id l w

genPortDecl x pt (CS.Ident ident) ll ww = PortDecl (PortDir x) (maybe Nothing (Just . parseFromPortType) pt) (maybeGetRange ll ww) (Ident ident) (CommentItem "")
    where
        maybeGetRange l w = case or [(isJust l),(isJust w)] of
            False -> Nothing
            True  -> maybeGetRange1 l w
        maybeGetRange1 l w = case and $ map equalTo1 [l,w] of
            True  -> Nothing
            False -> Just $ Range (parseExpr $ showMult l w) $ intExpr 0
        equalTo1 (Just exp) = (showExpr exp) == "1"
        equalto1 Nothing = True
        showExpr l = render (CP.ppExpr l)
        showMult (Just l) Nothing = (showExpr l) ++ "-1"
        showMult Nothing (Just w) = (showExpr w) ++ "-1"
        showMult (Just l) (Just w) = "(" ++ (showExpr l) ++ ")*(" ++ (showExpr w) ++ ")-1"

convertCConToVCon :: CS.ConnectDecl -> [Item]
convertCConToVCon (CS.ConnectDecl s []) = [AssignItem Nothing Nothing []]
convertCConToVCon (CS.ConnectDecl s ds) = map (getAssignMent s) ds
    where
        getAssignMent n1 n2 = AssignItem Nothing Nothing $ [Assignment (fromNodeToExpr n1) (fromNodeToExpr n2)]

fromNodeToExpr n = parseExpr $ render $ CP.ppConnectNode n

convertCInstToVInst :: CS.InstDecl -> [ModuleUnit] -> Item
convertCInstToVInst (CS.InstDecl (CS.Ident mid) (CS.Ident iid) mb_ic) mods = 
    InstanceItem $ Instance (Ident mid) (Right []) 
    ([Inst (Ident iid) Nothing (NamedConnections $ 
    (maybe [] (map getConFromIC) mb_ic) ++ getConFromModList)])
    where
        getConFromIC (CS.InterConDel n1 n2) = 
            let n1name = getNodeName n1 in
            case n1name `elem` getModUnitPorts of
                True -> (NamedConnection (Ident n1name) (fromNodeToExpr n2) (CommentItem ""))
                False -> error (n1name ++ " is not in " ++ (mod_name $ getModUnit mods mid) ++ "::" ++ (mod_file $ getModUnit mods mid))
        getModUnitPorts = (map port (ports $ getModUnit mods mid))
        getNodeName n = render (CP.ppConnectNode n)
        getConFromModList = map genConFromList $ filter (not . hasSamePortName (maybe [] (map getNodeNameFromIC) mb_ic)) getModUnitPorts
        getNodeNameFromIC (CS.InterConDel n1 _) = getNodeName n1
        hasSamePortName ps p = or $ map (==p) ps
        genConFromList s = (NamedConnection (Ident s) (parseExpr "_")  (CommentItem ""))

getModUnit :: [ModuleUnit] -> String -> ModuleUnit
getModUnit mods mid = case getMaybeModList of
    [] -> error ("Failed to find module " ++ mid)
    x  -> head x
    where
        getMaybeModList = filter (hasEqualName mid) mods
        hasEqualName s m = ((mod_name m) == s)

newVerilogData :: String -> Verilog
newVerilogData s = 
    Verilog ([ModuleDescription (Module 
    (Ident s) 
    (Just [ParamDecl [ParamAssign (Ident "P") (intExpr 1)]]) 
    [(PortDecl (PortDir Input) Nothing Nothing (Ident "clk") (CommentItem "")),
    (PortDecl (PortDir Input) Nothing Nothing (Ident "rst") (CommentItem ""))] []
    )])

addPortComment :: Verilog -> [ModuleUnit] -> Verilog
addPortComment (Verilog v) mods = Verilog (map addPort1 v)
    where 
        addPort1 (ModuleDescription (Module id pa po it)) = 
            ModuleDescription (Module id pa po (map addPort2 it))
        addPort1 x = x
        addPort2 (InstanceItem (Instance id pa it)) = 
            InstanceItem (Instance id pa (map (addPort3 id) it))
        addPort2 x = x
        addPort3 (Ident mid) (Inst id r (NamedConnections ncs)) = 
            Inst id r (NamedConnections (map (addPort4 mid) ncs))
        addPort3 _ x = x
        addPort4 id (NamedConnection (Ident pt) exp _) = 
            NamedConnection (Ident pt) exp (CommentItem (getPortDirFrom pt (getModUnit mods id)))
        -- addPort4 _ x = x
        getPortDirFrom pt mod = case getPortList pt mod of
            [] -> error ("Port not found " ++ (mod_name mod) ++ ":" ++ "pt")
            x ->  case direction $ head x of
                DInput -> "i"
                DOutput -> "o"
                DInOut -> "io"
        getPortList pt mod = filter (hasEqualName pt) $ ports mod
        hasEqualName s m = ((port m) == s)
            
getVFilePath :: [ModuleUnit] -> FilePath -> FilePath
getVFilePath mods v = mod_file $ getModUnit mods v

mergeVerilog :: Verilog -> [Verilog] -> Verilog
mergeVerilog (Verilog []) _ = error "No Verilog file specified."
mergeVerilog (Verilog ds1) vs = Verilog (map mergeModule ds1)
    where
        mergeModule (ModuleDescription (Module id pa pt it))  =
            ModuleDescription (Module id (mergeParaList pa $ (concat $ catMaybes $ map getParaL vs)) 
            (pt ++ (concat $ catMaybes $ map getPortL vs)) (it ++ (concat $ catMaybes $ map getItemL vs)))
        getParaL (Verilog []) = Nothing
        getParaL (Verilog ds) = Just $ concat $ catMaybes $ map getParaL1 ds
        getParaL1 (ModuleDescription a) = getPara a
        getParaL1 _ = Nothing
        getPortL (Verilog []) = Nothing
        getPortL (Verilog ds) = Just $ concat $ catMaybes $ map getPortL1 ds
        getPortL1 (ModuleDescription a) = getPort a
        getPortL1 _ = Nothing
        getItemL (Verilog []) = Nothing
        getItemL (Verilog ds) = Just $ concat $ catMaybes $ map getItemL1 ds
        getItemL1 (ModuleDescription a) = getItem a
        getItemL1 _ = Nothing
        getPara (Module _ a _ _) = a
        getPort (Module _ _ a _) = Just a
        getItem (Module _ _ _ a) = Just a
        mergeParaList Nothing [] = Nothing
        mergeParaList Nothing a = Just a
        mergeParaList (Just b) a = Just $ b ++ a

mergeVerilogFile :: [Verilog] -> Verilog
mergeVerilogFile vs = Verilog (concat $ map getDescription vs)

--Single Description
splitVerilogFile :: Verilog -> [(Verilog,FilePath)]
splitVerilogFile (Verilog []) = error ("Original file empty.")
splitVerilogFile (Verilog ds) = zip (map Verilog $ splitList ds []) (catMaybes $ map getModuleName ds)
    where 
        getModuleName (ModuleDescription (Module (Ident n) _ _ _)) = Just n
        getModuleName _ = Nothing

splitList [] y = y
splitList (x:xs) y = splitList xs ([x]:y)

getCynideFile :: FilePath -> IO CS.Cynide
getCynideFile cyFile = do
     cyData <- fromJust <$> parseCynideFile cyFile
     return cyData