{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

import HFlags
import System.Environment (getEnv)
import System.Process
import System.Directory
import System.Exit
import System.FilePath
import Data.String.Utils
import Data.Version
import Data.List
import Data.Maybe
import Control.Monad (when)

import PkgDB
import Helpers
import Defaults
import Update

-- | The status of a package in the cblrepo database.
data PkgStatus  = RepPkg    -- ^ A package available in this repository
                | DistrPkg  -- ^ A package available taken from other repositories
                | MainPkg   -- ^ A package available in [habs] but not in this repo
                | NewPkg    -- ^ A package not present in the DB nor in [habs]
	deriving (Show, Eq, Ord)

-- | A simplyfied version of CblPkg
data SimplePkg
    = DisPkg { pName ::String
             , pVersion :: Version
             , pRelease :: String
             }
    | RePkg  { pName :: String
             , pVersion ::Version
             }
    deriving (Show, Eq, Ord)

-- | A convenient type
type CabalPkg = (String, Version)

-- Options used in command line
defineFlag "n:dryrun" False "Don't do anything, just try"
defineFlag "u:upgradedistro" False "Upgrade DistroPkgs to version in main repository"
defineFlag "b:build" False "Create pkgbuilds and compile"
defineFlag "y:update" False "Update main repo and Hackage file list"
defineFlag "l:listupdates" False
    "list available updates for packages in repository and exit"

main = do
    args <- $(initHFlags helpMessage)
    when flags_update $ update

    home <- getEnv "HOME"
    setCurrentDirectory $ home </> thisRepoDir

    when flags_listupdates $ do
        listUpdates
        exitSuccess

    thisR <- readDb (home </> thisRepoDir </> "cblrepo.db")
    mainR <- readDb (home </> mainRepoDir </> "cblrepo.db")

    let upgradeList =  if flags_upgradedistro
                            then distroUpgrades mainR thisR
                            else []

    let list = filter (newCblPkg thisR mainR) args 
    cabalList <- getCabalList list
    let newList = catMaybes $ map (makeSimplePkg thisR mainR) cabalList
    let installList = upgradeList ++ newList
    let names =  getNames installList

    -- Do the job!
    addCblPkgs installList
    putStrLn $ unwords names
    bump names
    when flags_build $ build names


-- | For each DistroPkg search if there's a newer version in the main
-- repository.
distroUpgrades :: CblDB -> CblDB -> [SimplePkg]
distroUpgrades mainR =
    map toSimpleDisPkg . catMaybes . map (\p -> lookupPkg mainR (pkgName p))
        . filter (needsUpdateD (mainR)) . filter (isDistroPkg)
  where
    needsUpdateD mainR pkg = 
        let Just mainPkg = lookupPkg mainR (pkgName pkg)
            prel s = read (pkgRelease s) :: Int
        in (pkgVersion mainPkg) > (pkgVersion pkg) ||
            ((pkgVersion mainPkg) == (pkgVersion pkg) && (prel mainPkg) > (prel pkg))

-- | Check if a package is already in [habs]
-- TODO: it should be in any other repository, not just habs.
newCblPkg thisR mainR name = 
    if pkgType == MainPkg
        then False
        else True
  where
    (pkgType, _) = checkPkgStatus name thisR mainR

-- | Take a list of packages names and return all needed packages as CabalPkg.
-- As cabal command will exclude already installed packages, we run the 
-- command in the clean "root" chroot.
getCabalList :: [String] -> IO [CabalPkg]
getCabalList [] = return []
getCabalList pkgNames = do
    str <- runCabal (unwords $ map (\x -> "'" ++ x ++ "'") pkgNames)
    let list = drop 2 $ lines str
    let plist = map getPkg list
    -- mapM_ (\(pn,pv) -> putStrLn $ pn ++ ": " ++ show pv) plist
    return plist
  where
    getPkg s =
      let l = split "-" s
          name = join "-" $ init l
	  version = last l
      in (name, (getVersion version))
    runCabal pkgs = do
	chroot chrootRootDir "cabal" ["install", "--dry-run", pkgs]
    getVersion s = Version (ver s) []
    ver s = map (\x -> read x :: Int) (split "." s)

-- | Checks wheter a given package is new, a RepoPkg, a DistroPkg or from the
-- main repository.
checkPkgStatus :: String -> CblDB -> CblDB -> (PkgStatus, Maybe CblPkg)
checkPkgStatus name thisR mainR =
    case lookupPkg thisR name of 
	Just pkg -> if isRepoPkg pkg
		    then (RepPkg, Just pkg)
		    else (DistrPkg, Just pkg)
	Nothing -> do
	    case lookupPkg mainR name of
		Just pkg -> (MainPkg, Just pkg)
		Nothing -> (NewPkg, Nothing)

-- | takes the name and version of a package to be installed from Cabal
-- and returns the appropriate SimplePkg
makeSimplePkg :: CblDB -> CblDB -> CabalPkg -> Maybe SimplePkg
makeSimplePkg thisR mainR (pn, pv) =
    case pkgType of
	MainPkg  -> addDisPkg pkg
	NewPkg   -> Just $ RePkg pn pv
	RepPkg   -> updateRepPkg pkg pv
	DistrPkg -> Nothing
  where
    (pkgType, pkg) = checkPkgStatus pn thisR mainR
    updateRepPkg (Just pkg) nv =
        if (nv > pkgVersion pkg)
            then Just $ toSimplePkg pkg
            else Nothing
    addDisPkg pkg =
        case pkg of
            Just p -> Just $ toSimpleDisPkg p
            Nothing -> Nothing

-- | Add all packages with a call to cblrepo.
addCblPkgs :: [SimplePkg] -> IO ()
addCblPkgs list = do
    putStrLn "==> The following DistroPkgs will be added:"
    putStrLn $ unlines (map distroPkgString' list)
    putStrLn "\n"
    putStrLn "==> The following RepoPkgs will be added:"
    putStrLn $ unlines (map repoPkgString' list)

    let (disList, reList) = partition isDisPkg list
    let args = (map distroPkgString $ disList) ++ (map repoPkgString $ reList)
    res <- if flags_dryrun
	then do
	    putStr "(dry-run) "
	    cblrepoN "add" args
	else cblrepo "add" args
    putStr res
    when (res /= "") exitFailure
    putStrLn "Done."
  where
    distroPkgString pkg =
        "--distro-pkg=" ++ repoPkgString pkg ++ "," ++ pRelease pkg
    repoPkgString pkg =
        pName pkg ++ "," ++ (showVersion $ pVersion pkg)
    distroPkgString' pkg =
        repoPkgString' pkg ++ "-" ++ pRelease pkg
    repoPkgString' pkg =
        pName pkg ++ "-" ++ (showVersion $ pVersion pkg)
    
-- | Call cblrepo to /bump/ packages depending on updated packages.
bump :: [String] -> IO ()
bump []    = return ()
bump names = do
    putStrLn "Bumping dependencies..."
    res <- if flags_dryrun
	then cblrepoN "bump" names
	else cblrepo "bump" names
    putStrLn res
    putStrLn "Done."
    
-- | Build needed package. If there was a sync with main repository, it will
-- build all database. It needs root privilegs to run in chroot. Sudo should
-- be present in your system and configured as needed.
build :: [String] -> IO ()
build []    = return ()
build names = do
    list <- if flags_upgradedistro
	then do
	    l <- cblrepo "build" ["base"]
	    return $ tail (words l)
	else do
	    l <- cblrepo "build" names
	    return $ words l
    putStrLn "Preparing pkgbuild for the following packages:"
    putStrLn $ unwords list
    cblrepo "pkgbuild" list
    code <- system $ "sudo ./makeahpkg -x -- " ++ (unwords list)
    if code == ExitSuccess
	then return()
	else exitFailure
    code' <- system $ "sudo ./makeahpkg -a i686 -x -- " ++ (unwords list)
    if code' == ExitSuccess
	then return()
	else exitFailure

getNames :: [SimplePkg] -> [String]
getNames [] = []
getNames pkgs = map pName pkgs

-- | Convert CblPkg to SimplePkg
toSimplePkg :: CblPkg -> SimplePkg
toSimplePkg (n, DistroPkg v r) = DisPkg n v r
toSimplePkg (n, p) = RePkg n (version p)

-- | Convert RepoPkg in main to DisPkg in current repository
toSimpleDisPkg :: CblPkg -> SimplePkg
toSimpleDisPkg (n, RepoPkg v _ r) = DisPkg n v r

isDisPkg DisPkg {} = True
isDisPkg _ = False
