{-# LANGUAGE OverloadedStrings #-}
module Helpers where

import System.Process

sudo :: String -> [String] ->  IO String
sudo cmd args = readProcess "sudo" (cmd:args) []

chroot :: String -> String -> [String] -> IO String
chroot chrootDir cmd args = sudo "chroot" (chrootDir:"/bin/bash":"-c":[unwords (cmd:args)])

cblrepo cmd args = readProcess "cblrepo" (cmd:args) []
cblrepoN cmd args = readProcess "cblrepo" (cmd:"-n":args) []

helpMessage = unlines [
    "Keep your ArchHaskell repository synced and up-to-date.",
    "Version: 0.0.1", "",
    "USAGE: cbladmin [-n] [-u] [-b] [pkg1] .. [pkgN]", "",
    "'pkg' can be anything that cabal-install understand",
    "  cbladmin foo                 Package from the hackage server",
    "  cbladmin foo-1.0             Specific version of a package",
    "  cbladmin 'foo < 2'           Constrained package version"
    ]
