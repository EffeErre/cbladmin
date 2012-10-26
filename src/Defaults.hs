module Defaults where

import System.FilePath

workdir = "archhaskell"
mainRepoDir = workdir </> "habs"
thisRepoDir = workdir </> "habs-web"

cabalPath = ".cabal/packages/hackage.haskell.org/"
chrootRootDir = "x86_64-chroot/root/"
chrootBuildDir = "x86_64-chroot/build/"
sourceFiles = ["00-index.tar.gz", "00-index.tar", "00-index.cache"]

