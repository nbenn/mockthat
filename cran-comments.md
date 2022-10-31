## Test environments

* local R installation, macOS 12.6, R 4.2.1
* GitHub Actions (ubuntu-latest): oldrel-1, release, devel
* GitHub Actions (windows-latest): release
* Github Actions (macOS-latest): release
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is re-submission of a minor bugfix update.
* The original issue was due to a unit test that relied on jsonlite internals
  which were changed and required removal of the failing test.
* I apologize for not being able to fix the problem in time.
