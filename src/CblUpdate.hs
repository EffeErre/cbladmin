{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import HFlags
import System.Environment
import System.Exit
import System.Process
import System.FilePath
import System.Directory

import Defaults
import Helpers

defineFlag "n:noupdate" False "Don't actually update, just copy files"

sudoCp source target = sudo "cp" ("-t":target:source)

-- Run "cabal update"
cabalUpdate = do
    code <- rawSystem "cabal" ["update"]
    if code == ExitSuccess
	then putStrLn "Hackage file list was updated.\n"
	else exitFailure

-- cblrepo requires the same hackage index file from cabal, but it has to be
-- in his directory.
copy_cblrepo = do
    home <- getEnv "HOME"
    let source = home </> cabalPath </> (Prelude.head sourceFiles)
    createDirectoryIfMissing False (home </> ".cblrepo")
    rawSystem "cp" [source,(home </> ".cblrepo/")]

-- Copy hackage files from ~/.cabal to root and chroot directories. This is
-- needed to run cabal install in a clean chroot so dependencies are always
-- recorder whether packages are present or not in the host system.
copyDbChroot :: IO ()
copyDbChroot = do
    home <- getEnv "HOME"
    let src = Prelude.map (\f -> home </> cabalPath </> f) sourceFiles
        tgt = Prelude.map (\d -> d </> "root" </> cabalPath) ["/", chrootRootDir, chrootBuildDir]
    sudo "mkdir" ("-p":tgt)
    mapM_ (sudoCp src) tgt

main = do
    args <- $(initHFlags "Update Cabal v0.0.1")
    home <- getEnv "HOME"

    -- Update [habs] repository to latest version
    setCurrentDirectory $ home </> mainRepoDir
    rawSystem "git" ["pull", "upstream", "master"]

    if flags_noupdate
	then putStrLn "Keeping old hackage cache"
	else cabalUpdate
    copy_cblrepo

    setCurrentDirectory $ home </> thisRepoDir
    copyDbChroot
    putStrLn "==> Available updates:"
    rawSystem "cblrepo" ["updates"]
