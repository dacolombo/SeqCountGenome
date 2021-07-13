# SeqCountGenome
Bioconductor compliant package for counting sequences occurrences in arbitrary subsets of the genome, realized for the Scientific Programming course of the Bioinformatics for Computational Genomics MSc.

## Installation
For installing this package in R, the devtools::install_github() command is required:
```r
if (!require("devtools")) {install.packages("devtools")}
devtools::install_github("dacolombo/SeqCountGenome")
```
Once installed, the package can be loaded with:
```r
library(SeqCountGenome)
```

## Usage
For information about how to use the function contained in this package, refer to the manual in this github repository: `SeqCountGenome-manual.pdf`.
Otherwhise, after installing the package consult the vignette using this command in an R session:
```r
utils::browseVignettes("SeqCountGenome")
```

## Dependencies
This package main dependencies are the following Bioconductor packages: `BSgenome`, `Biostrings`, `GenomeInfoDb` and `GenomicRanges`
