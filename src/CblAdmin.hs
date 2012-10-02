{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

import HFlags
import System.Environment (getEnv)
import System.Process
import System.Directory
import System.Exit
import Data.String.Utils
import Data.Version
import Data.List
import Control.Monad (when)

import PkgDB
import Helpers
import Defaults

data PkgStatus = RepPkg | DistrPkg | MainPkg | NewPkg
	deriving (Show, Eq, Ord)

type CabalPkg = (String, Version)

defineFlag "n:dryrun" False "Don't do anything, just try"
defineFlag "u:upgradedistro" False "Upgrade DistroPkgs to version in main repository"
defineFlag "b:build" False "Create pkgbuilds and compile"

getVersion s = Version ver []
    where
	ver = map (\x -> read x :: Int) (split "." s)

-- Take a list of packages names and return all needed packages as CabalPkg.
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

onlyRepoPkg thisR pkgName =
    case lookupPkg thisR pkgName of
	Nothing -> False
	Just pkg -> isRepoPkg pkg

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

newCblPkg thisR mainR name = 
    if pkgType == MainPkg
    then False
    else True
    where (pkgType, _) = checkPkgStatus name thisR mainR

preparePkg :: CblDB -> CblDB -> CabalPkg -> [String]
preparePkg thisR mainR (pn, pv) =
    case pkgType of
	MainPkg  -> ["-d", distrPkgString pkg]
	NewPkg   -> repoPkgString pn pv
	RepPkg   -> repoPkgString' pkg pv
	DistrPkg -> []
    where
	(pkgType, pkg) = checkPkgStatus pn thisR mainR
	distrPkgString (Just pkg) = pkgName pkg ++ "," ++ (showVersion $ pkgVersion pkg) ++ "," ++ pkgRelease pkg
	repoPkgString pn pv = [pn ++ "," ++ (showVersion pv)]
	repoPkgString' (Just pkg) pv =
	    if pv > pkgVersion pkg
		then repoPkgString (pkgName pkg) pv
		else []
	

addCblPkgs :: CblDB -> [String] -> IO ()
addCblPkgs thisR list = do
    let args = commonArgs ++ list
    putStrLn $ "Adding repoPkgs " ++ (intercalate ", " list) ++ "... "
    res <- if flags_dryrun
	then do
	    putStr "(dry-run) "
	    cblrepoN "add" args
	else cblrepo "add" args
    putStr res
    when (res /= "") exitFailure
    putStrLn "Done."
    return $ ()
    
addCblPkg :: CblDB -> CblDB -> CabalPkg -> IO (Maybe CabalPkg)
addCblPkg thisR mainR (pn, pv) =
    case pkgType of
	MainPkg  -> addDistrPkg pkg
	NewPkg   -> addRepPkg (pn, pv)
	RepPkg   -> updateRepPkg pkg pv
	DistrPkg -> return Nothing
    where (pkgType, pkg) = checkPkgStatus pn thisR mainR

commonArgs = ["--db", thisRepoF]
addDistrPkg (Just pkg) = do
    let args = commonArgs ++ ["-d", pkgString]
    putStr $ "Adding distroPkg " ++ pkgString' ++ "... "
    res <- if flags_dryrun
	then do
	    putStr "(dry-run) "
	    cblrepoN "add" args
	else cblrepo "add" args
    putStr res
    when (res /= "") exitFailure
    putStrLn "Done."
    return (Just (pkgName pkg, pkgVersion pkg))
    where
	pkgString = pkgName pkg ++ "," ++ (showVersion $ pkgVersion pkg) ++ "," ++ pkgRelease pkg
	pkgString' = pkgName pkg ++ "-" ++ (showVersion $ pkgVersion pkg) ++ "-" ++ pkgRelease pkg

addRepPkg (pn, pv) = do
    let args = commonArgs ++ [pkgString]
    putStrLn $ "Adding repoPkg " ++ pkgString' ++ "... "
    res <- if flags_dryrun
	then do
	    putStr "(dry-run) "
	    cblrepoN "add" args
	else cblrepo "add" args
    putStr res
    when (res /= "") exitFailure
    putStrLn "Done."
    return $ Just (pn, pv)
    where
	pkgString = pn ++ "," ++ (showVersion pv)
	pkgString' = pn ++ "-" ++ (showVersion pv)

updateRepPkg (Just pkg) newVersion = do
    let needsUpdate = newVersion > pkgVersion pkg
    if needsUpdate
	then do
	    putStrLn $ "Updating " ++ pkgName pkg ++ " [ " ++ showVersion (pkgVersion pkg) ++ " -> " ++ showVersion newVersion ++ " ]"
	    addRepPkg (pkgName pkg, newVersion)
	else do
	    putStrLn $ (pkgName pkg) ++ " is already up-to-date."
	    return Nothing

-- Seach for upgraded packages in main (i.e. [habs]) repository
distroUpgrades mainR =
    map (\p -> lookupPkg mainR (pkgName p)) . filter (needsUpdateD (mainR)) . filter (isDistroPkg)
   
updateDistrPkg Nothing = return Nothing
updateDistrPkg (Just pkg) = do
	    putStrLn $ "Updating " ++ pkgName pkg ++ " from main repository [ " ++ showVersion (pkgVersion pkg) ++ " ]"
	    addDistrPkg $ Just pkg
    
needsUpdateD mainR pkg = 
    let Just mainPkg = lookupPkg mainR (pkgName pkg)
	prel s = read (pkgRelease s) :: Int
    in (pkgVersion mainPkg) > (pkgVersion pkg) ||
	((pkgVersion mainPkg) == (pkgVersion pkg) && (prel mainPkg) > (prel pkg))

bump []    = return ()
bump names = do
    putStrLn "Bumping dependencies..."
    res <- if flags_dryrun
	then cblrepoN "bump" names
	else cblrepo "bump" names
    putStrLn res
    putStrLn "Done."
    
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

getNames :: [Maybe CabalPkg] -> [String]
getNames [] = []
getNames (Nothing:xs) = getNames xs
getNames (Just (pn, _):xs) = pn:getNames xs

main = do
    args <- $(initHFlags helpMessage)
    home <- getEnv "HOME"
    let workDir = home ++ "/archhaskell/haskell-extra/"
    setCurrentDirectory workDir

    thisR <- readDb thisRepoF
    mainR <- readDb mainRepoF

    upgradedList <- if flags_upgradedistro
		    then mapM updateDistrPkg $ distroUpgrades mainR thisR
		    else return []

    let list = filter (newCblPkg thisR mainR) args 
    installList <- getCabalList list
    updatedList <- mapM (addCblPkg thisR mainR) installList
    -- newR <- readDb thisRepoF
    -- let names =  filter (onlyRepoPkg newR) $ getNames (upgradedList ++ updatedList)
    let names =  getNames (upgradedList ++ updatedList)
    putStrLn $ unwords names
    bump names
    when flags_build $ build names
