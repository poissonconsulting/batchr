## Test environments

release 4.0.3

* OSX (local) - release
* OSX (actions) - release
* Ubuntu (actions) - 3.6, oldrel, release and devel
* Windows (actions) - release
* Windows (winbuilder) - devel

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Joe Thorley <joe@poissonconsulting.ca>'

New submission

### Resubmission

> Please do not modify the .GlobalEnv. This is not allowed by the CRAN policies.
e.g.: .Random.seed

Done

### Resubmission 

Fixed LazyData note for all flavors.

ERROR on CRAN Flavor r-release-macos-x86_64 and r-oldrel-macos-x86_64. 
Unable to replicate error package was built and checked on:

  - https://mac.r-project.org/macbuilder/submit.html
    - r-release-macosx-arm64|4.1.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1| 
  - [GitHub Actions](https://github.com/poissonconsulting/batchr/actions/runs/1288929072)
    - macOS-latest (release)
    - macOS-latest (oldrel)
    - macOS-11 (release)
    - macOS-11 (oldrel)
  - [rhub](https://r-hub.github.io/rhub/articles/rhub.html)
    - macos-highsierra-release
    - macos-highsierra-release-cran
    - macos-m1-bigsur-release 
    - windows-x86_64-devel 
    - fedora-clang-devel
    - ubuntu-gcc-release

0 errors | 0 warnings | 0 note

Unable to reproduce errors identified in CRAN Package Check Results. 
The rhub documentation says macos-highsierra-release-cran is the CRAN 
r-release-macos-x86_64 and no errors were produced. Unable to find a system 
identical to test r-oldrel-macos-x86_64 directly. 
