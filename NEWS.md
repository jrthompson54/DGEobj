# NEWS

## DGEobj R Package 

---

### v1.1.2
* Made all Bioconductor packages optional (suggested)
* Added a new, minimal "mini" example object that has no internal Bioconductor-based features (ie normalization, analysis) to allow 
for clean, simple examples that do not depend on suggested packages
* Testing updated so that when Bioconductor suggested packages are unavailable the tests will not be run

### v1.1.1
* Updated tests and examples so that when suggested packages are unavailable the examples and/or tests dependent on them are not run

### v1.1
* made GenomicRanges package optional - used only for exon/gene levels
* added "protein" level data, reviewed/updated isoform data for proper handling
* added imputationMatrix as a type of metadata
* reworked reset to resolve issues and remove unneeded code
* updated tests and documentation

### v1.0.3
* significant documentation review/updates
* underlying structure - added a few new fields and cleaned up unneeded methods
* updated the example object with additional information
* vignette now setup to use the precompiled paradigm

### v1.0.1
* Initial public release to CRAN
