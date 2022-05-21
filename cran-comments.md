## Comments from Maintainer

* There is one outstanding note - we updated this package recently in an attempt to resolve the issues with noSuggests
however unfortunately it uncovered a different package dependency problem that wasn't caught on the 3 standard RHub platform
configurations.  We have now combed over the entire package and extensively checked all RHub and local platforms to try to 
ensure the suggested packages are just that, suggested!  We also made sure we now have a very small example object to use
that was not made using any of the suggested packages as well.

* We appreciate your consideration of this package update, here is what we have done for this release:
  * Resolved CRAN check errors, including the noSuggests issues
  * Added a new example object so that as many examples can be run as possible without suggested packages
  * Bioconductor packages in suggests are required for testing, now we will not run tests if any are missing
  * Also updated our process to check ALL available/usable RHub platforms prior to submission

---  

## Test environments

RStudio Server Pro (ubuntu 18.04.2)  

* R 3.6.3
* R 4.0.5
* R 4.1.3
* R 4.2.0

CircleCI

* R 4.0.5
* R 4.1.3
* rocker/verse:latest

WinBuilder

* devtools::check_win_devel()  
* devtools::check_win_release()  

RHub

* devtools::check_rhub(interactive = F,
                       platforms   = c(rhub::platforms()$name),
                       env_vars    = c("_R_CHECK_DEPENDS_ONLY_"   = "true",
                                       "_R_CHECK_FORCE_SUGGESTS_" = "false"))

---  

## R CMD check results


```
devtools::check()  

0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

---  

## Reverse dependencies

* DGEobj.utils

```
revdepcheck::cran_revdeps('DGEobj', bioc = T)

[1] "DGEobj"       "DGEobj.utils"
```

```
revdepcheck::revdep_report_cran()

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

```
