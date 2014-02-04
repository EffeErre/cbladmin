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

{-# LANGUAGE OverloadedStrings #-}
module Helpers where

import System.Process

sudo :: String -> [String] ->  IO String
sudo cmd args = readProcess "sudo" (cmd:args) []

chroot :: String -> String -> [String] -> IO String
chroot chrootDir cmd args = sudo "chroot" (chrootDir:"/bin/bash":"-c":[unwords (cmd:args)])

cblrepo cmd args = readProcess "cblrepo" (cmd:args) []
cblrepoN cmd args = readProcess "cblrepo" ("-n":cmd:args) []

helpMessage = unlines [
    "Keep your ArchHaskell repository synced and up-to-date.",
    "Version: 0.2.4", "",
    "USAGE: cbladmin [-n] [-u] [-b] [pkg1] .. [pkgN]", "",
    "'pkg' can be anything that cabal-install understand",
    "  cbladmin foo                 Package from the hackage server",
    "  cbladmin foo-1.0             Specific version of a package",
    "  cbladmin 'foo < 2'           Constrained package version"
    ]
