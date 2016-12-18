module ArgParseCCV where
import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

data Flag
    = Email                 -- --email
    | Author                -- --author
    | Initial               -- --init
    | Update                -- --update
    | New                   -- -n
    | Top                   -- -t
    | Description           -- -d
    | Automatic             -- -a
    | Increment             -- -i
    | Port                  -- -p
    | Compliments           -- -c
    | Version               -- -v
    | Hierarchy             -- -h
    | Formatize             -- -f
    | Merge                 -- -m
    | Split                 -- -s
    | Help                  -- --help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags = 
    [Option []["email"] (NoArg Email)
        "Set in email in file header."
    ,Option []["author"] (NoArg Author)
        "Set in contact in file header."
    ,Option []["init"] (NoArg Initial)
        "Set project path, which is used to generate the file hierachy."
    ,Option []["update"] (NoArg Update)
        "Update project hierachy."
    ,Option ['n'][] (NoArg New)
        "Create a new verilog file with file header and simple module frame."
    ,Option ['t'][] (NoArg Top)
        "Generate the top module from module description file(.ccv)."
    ,Option ['d'][] (NoArg Description)
        "Preserve annotations and port descriptions."
    ,Option ['a'][] (NoArg Automatic)
        "Automatically connect ports according to specified rules."
    ,Option ['i'][] (NoArg Increment)
        "Automatically connect ports according to specified rules."
    ,Option ['p'][] (NoArg Port)
        "Add ports directions as annotations."
    ,Option ['c'][] (NoArg Compliments)
        "Compliment incomplete module description file."
    ,Option ['v'][] (NoArg Version)
        "Add version number and set version increamental."
    ,Option ['H'][] (NoArg Hierarchy)
        "Print project hierachy."
    ,Option ['f'][] (NoArg Formatize)
        "Convert port descriptions between various HDL standards."
    ,Option ['m'][] (NoArg Merge)
        "Merge multiple verilog files."
    ,Option ['s'][] (NoArg Split)
        "Split a single verilog file into serveral module files."
    ,Option ['h'][] (NoArg Help)
        "Get more specific usage." 
    ]

argparse argv = case getOpt Permute flags argv of
    (args,fs,[]) -> do
        let files = if null fs then ["-"] else fs
--Define usage here
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo helpme [])
                    exitWith ExitSuccess
            else return (nub (concatMap set args), files)

    (_,_,errs)      -> do
        hPutStrLn stderr (concat errs ++ usageInfo header (init flags))
        exitWith (ExitFailure 1)

    where header = "Use 'ccv -h' to get more specific usage"
          set f  = [f]
          helpme = "Check the readme file first! I'll write later."