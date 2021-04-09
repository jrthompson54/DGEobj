## Comments from Maintainer

* significant documentation review/updates
* underlying structure - added a few new fields and cleaned up unneeded methods
* updated the example object with additional field information
* precompiled the vignette to avoid hitting internet resources and performing intensive calculations with each CMD check

---  

## Test environments

RStudio Server Pro (ubuntu 18.04.2)  

* R 3.6.3
* R 4.0.4

Travis-CI (ubuntu 16.04.6)

* R 3.6.3
* R 4.0.2
* R devel (2021-04-08 r80148)

WinBuilder

* devtools::check_win_devel()  
* devtools::check_win_release()  

RHub

* devtools::check_rhub(interactive = F)

---  

## R CMD check results


```
devtools::check()  

0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

---  

## Reverse dependencies


**NONE**

```
revdepcheck::cran_revdeps('DGEobj', bioc = T)

[1] "DGEobj"
```
