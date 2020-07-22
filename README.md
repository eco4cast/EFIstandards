  <!-- badges: start -->
  [![R build status](https://github.com/cboettig/forecast-standards/workflows/R-CMD-check/badge.svg)](https://github.com/cboettig/forecast-standards/actions) 
  [![CRAN status](https://www.r-pkg.org/badges/version/EFIstandards)](https://CRAN.R-project.org/package=EFIstandards)
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
  <!-- badges: end -->


This package summarizes the proposed community standards for the common formatting and archiving of ecological forecasts developed by the Ecological Forecasting Initiative (EFI). Such open standards are intended to promote interoperability and facilitate forecast adoption, distribution, validation, and synthesis. 

### Output Files:

EFI has proposed a three-tiered approach reflecting trade-offs in forecast data volume and technical expertise. The prefered output file format is in netCDF following standard CF conventions for dimensions and variable naming conventions, with ensemble member as a dimension where appropriate. The second-tier option is a semi-long CSV format, with state variables as columns and each row representing a unique issue datetime, prediction datetime, location, ensemble member, etc. The third-tier option is similar to option 2, but each row represents a specific summary statistic (mean, upper/lower CI) rather than individual ensemble members.

### Output Metadata:

EFI’s proposed metadata represents an expansion upon the Ecological Metadata Language (EML), with two key differences. First, is the specification of additonalMetadata tags to store forecast specific information (e.g. uncertainty propagation and data assimilation) as well as some summary information about model complexity, included uncertainties, etc. designed to facilitate cross-forecast synthesis. Second, a number of EML tags (e.g. temporal resolution, output variables) are considered a required part of forecast metadata that are otherwise optional in base EML.

This package includes an R tool for validating these EML files.

### Archiving:

EFI envisions a three-tiered approach to forecast archiving. At the most basic level, forecasts should be archived before new observations become available (not possible for hindcasts), preferably in a FAIR public archive that permits forecasts to be uploaded automatically, allows metadata to be searchable, and assigns a DOI. Second, in addition to this the codes used to generate forecasts should also be archived, preferably in an open archive or code repository (e.g. Github) that can be assigned a DOI. Finally, in addition to output and code archiving, we encourage running forecast workflows to be archived using virtualization approaches, such as Docker or Singularity containers.

### Vignettes:

This package includes a number of vignettes illustrating the application of the EFI standards to different forecasts

### Documentation:

A more detailed description of the EFI standard can be found at [https://bit.ly/2yDbzbP](https://bit.ly/2yDbzbP). 

Pkgdown rendered documentation of functions and vignettes can be found at https://eco4cast.github.io/EFIstandards/
