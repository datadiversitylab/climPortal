# climPortal

Downloading, formatting, and processing CRU climate data in R.

## What is climPortal?

The climPortal R package simplifies accessing climate data
from the Climatic Research Unit (CRU) Time-Series (TS) v4.09 dataset.
climPortal downloads monthly climate variables, converts them to terra
SpatRaster objects, and calculates the 19 bioclimatic variables commonly
used in species distribution modeling.

## Why use climPortal?

climPortal simplifies the CRU data pipeline, increases reproducibility,
and reduces the amount of code needed to go from download to
bioclimatic variables.

## Installing climPortal

climPortal can be installed from GitHub using the following code:

```r
library(devtools)
install_github("datadiversitylab/climPortal")
```

## Quick start

```r
library(climPortal)

# Download CRU data as SpatRasters
r <- get_cru(temp = TRUE, tmin = TRUE, tmax = TRUE, pre = TRUE,
             start_year = 1990, end_year = 2020)

# Calculate bioclimatic variables for a single year
bio <- cru_bioclim(r, year = 2020)

# Or average across a time range
bio_90s <- cru_bioclim(r, start_year = 1990, end_year = 1999)
```

## Dependencies

Please make sure that the R packages `terra`, `curl`, `R.utils`, `ncdf4`, and
`predicts` are correctly installed.

## Contact

Please see the package DESCRIPTION for package authors.
