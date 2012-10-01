{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Shelly
import HFlags
import System.Environment
import Data.Text.Lazy as LT
default (LT.Text)

defineFlag "n:noupdate" False "Don't actually update, just copy files"

setPaths = do
    home <- getenv_def "" "HOME"
    setenv "cabalPath" ".cabal/packages/hackage.haskell.org"
    setenv "workdir" $ home `append` "/archhaskell"
    workdir <- getenv_def "" "workdir"
    echo workdir
    setenv "mainRepo" $ workdir `append` "/habs"
    setenv "thisRepo" $ workdir `append` "/haskell-extra"

chrootRootDir = "x86_64-chroot/root/"
chrootBuildDir = "x86_64-chroot/build/"
source_files = ["00-index.tar.gz", "00-index.tar", "00-index.cache"]

run_sudo :: Text -> [Text] -> ShIO Text
run_sudo cmd args = run "/usr/bin/sudo" (cmd:args)

sudo_cp source target = run_sudo "cp" ("-t":target:source)

copy_cblrepo = do
    home <- getenv_def "" "HOME"
    cabalPath <- getenv_def "" "cabalPath"
    let source = home </> cabalPath </> (Prelude.head source_files)
    cp source (home </> ".cblrepo/")

tT = toTextIgnore
main = do
    args <- $(initHFlags "Update Cabal v0.1")
    shelly $ do
	home <- getenv_def "" "HOME"
	setPaths
	thisRepo <- getenv_def "" "thisRepo"
	mainRepo <- getenv_def "" "mainRepo"
	echo thisRepo
	cd $ fromText thisRepo
	chdir (fromText mainRepo) $ do
	    run "git" ["pull", "upstream", "master"]
	if flags_noupdate
	    then echo "Keeping old cache"
	    else run_ "cabal" ["update"]
	cabalPath <- getenv_def "" "cabalPath"
	copy_cblrepo
	let src = Prelude.map (\f -> toTextIgnore $ home </> cabalPath </> f) source_files
	sudo_cp src $ tT $ "/root" </> cabalPath
	sudo_cp src $ tT $ chrootRootDir </> "root" </> cabalPath
	sudo_cp src $ tT $ chrootBuildDir </> "root" </> cabalPath
	echo "Available updates:"
	run "cblrepo" ["updates"]
	exit 0
