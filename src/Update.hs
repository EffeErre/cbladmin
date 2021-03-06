{-
 - Copyright 2012 Fabio Riga
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Update
    ( update
    , listUpdates
    ) where
import System.Environment
import System.Exit
import System.Process
import System.FilePath
import System.Directory

import Defaults
import Helpers

update = do
    home <- getEnv "HOME"

    setCurrentDirectory $ home </> mainRepoDir
    pullUpdates

    cabalUpdate
    copy_cblrepo

    setCurrentDirectory $ home </> thisRepoDir
    copyDbChroot

-- | Update [habs] repository to latest version
pullUpdates = do
    rawSystem "git" ["pull", "upstream", "master"]

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
    rawSystem "ln" ["-s",(home </> ".cblrepo" </> (Prelude.head sourceFiles))
                   , (home </> ".cblrepo" </> cblrepoIdx)]

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

listUpdates = do
    putStrLn "==> Available updates:"
    rawSystem "cblrepo" ["updates"]
    return ()
