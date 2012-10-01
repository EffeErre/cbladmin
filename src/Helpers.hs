{-# LANGUAGE OverloadedStrings #-}
module Helpers where

import System.Process

sudo :: String -> [String] ->  IO String
sudo cmd args = readProcess "sudo" (cmd:args) []

chroot :: String -> String -> [String] -> IO String
chroot chrootDir cmd args = sudo "chroot" (chrootDir:"/bin/bash":"-c":[unwords (cmd:args)])

cblrepo cmd args = readProcess "cblrepo" (cmd:args) []
cblrepoN cmd args = readProcess "cblrepo" (cmd:"-n":args) []
