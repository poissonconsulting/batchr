## CRAN Fixes

### Fixed All Notes

Fixed LazyData note for all flavors.

### Unable to Replicate Error

ERROR on CRAN Flavor r-release-macos-x86_64 and r-oldrel-macos-x86_64. 

Unable to reproduce errors identified in CRAN Package Check Results. 

The rhub documentation says macos-highsierra-release-cran is the CRAN 
r-release-macos-x86_64 and no errors were produced. 
Unable to find a system identical to test r-oldrel-macos-x86_64 directly. 

Package was built and checked on:

  - local
    - macOS 11.5.2 | R 4.1.1 | MacBook Pro
  - https://mac.r-project.org/macbuilder/submit.html
    - r-release-macosx-arm64|4.1.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1| 
  - [GitHub Actions](https://github.com/poissonconsulting/batchr/actions/runs/1288929072)
    - macOS-latest (release)
    - macOS-latest (oldrel)
    - macOS-11 (release)
    - macOS-11 (oldrel)
    - windows-latest (release)
    - ubuntu-20.04 (devel)
    - ubuntu-20.04 (release)
    - ubuntu-20.04 (oldrel)
    - ubuntu-20.04 (3.6)
  - `devtools` check_win_*()
    - devel
    - release
    - oldrelease
  - [rhub](https://r-hub.github.io/rhub/articles/rhub.html)
    - macos-highsierra-release
    - macos-highsierra-release-cran
    - macos-m1-bigsur-release 
    - windows-x86_64-devel 
    - fedora-clang-devel
    - ubuntu-gcc-release

0 errors | 0 warnings | 0 note
