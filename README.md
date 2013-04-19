`cbladmin` is a simple Haskell script that try to take advantage from both
[cabal install][cabal-install] and [cblrepo][cblrepo].

[cabal install][cabal-install] is the default way to install Haskell packages.
It finds all dependencies and install them, but it can't remove packages, nor
easily updates packages.

[cblrepo][cblrepo] is a tool to keep a [Arch Linux][arch] repository of Haskell
packages: it uses cabal's packages description files to create pkgbuild and
track dependencies, although it doesn't automatically use them. So the
developer needs to do a lot of boilerplate to install or update a group of
interdependent package.

So `cbladmin` will use [cabal][cabal-install] to find latest dependencies, then
use [cblrepo][cblrepo] for adding or updating (and optionally build) packages.
Moreover `cbladmin` will check if the packages in install list are already
available in the [main Haskell repository][main-repo]: in this case it will add
those packages as DistroPkg.

## Usage

```
USAGE: cbladmin [-n] [-u] [-b] [pkg1] .. [pkgN]

'pkg' can be anything that cabal-install understand
  cbladmin foo                 Package from the hackage server
  cbladmin foo-1.0             Specific version of a package
  cbladmin 'foo < 2'           Constrained package version


  -h        --help, --usage, --version  Display help and version information.                                 
            --undefok                   Whether to fail on unrecognized command line options.
  -l[BOOL]  --listupdates[=BOOL]        list available updates for packages in repository and exit (default: false, from module: Main)
  -y[BOOL]  --update[=BOOL]             Update main repo and Hackage file list (default: false, from module: Main)
  -b[BOOL]  --build[=BOOL]              Create pkgbuilds and compile (default: false, from module: Main)      
  -u[BOOL]  --upgradedistro[=BOOL]      Upgrade DistroPkgs to version in main repository (default: false, from module: Main)
  -n[BOOL]  --dryrun[=BOOL]             Don't do anything, just try (default: false, from module: Main)  


````

## To do

This script is far from complete. I tried to simplify some usage of
[cblrepo][cblrepo], but in many cases it's just better to use the latter. You
can think of `cbladmin` as an expansion of [cblrepo][cblrepo].

`cbladmin` should also be configurable and allow checking in multiple repositories.

[cabal-install]: http://hackage.haskell.org/package/cabal-install
[cblrepo]: https://github.com/magthe/cblrepo
[arch]: http://www.archlinux.org
[main-repo]: https://github.com/archhaskell/habs
