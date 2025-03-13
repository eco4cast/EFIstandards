  <!-- badges: start -->
  [![R build status](https://github.com/cboettig/forecast-standards/workflows/R-CMD-check/badge.svg)](https://github.com/cboettig/forecast-standards/actions) 
  [![CRAN status](https://www.r-pkg.org/badges/version/EFIstandards)](https://CRAN.R-project.org/package=EFIstandards)
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
  <!-- badges: end -->


This package summarizes the EFI community standards Version 1.0 for the common formatting and archiving of ecological forecasts developed by the Ecological Forecasting Initiative (EFI). These open standards are intended to promote interoperability and facilitate forecast adoption, distribution, validation, and synthesis.

### Output Files:

EFI Standards v1.0 is a three-tiered approach reflecting trade-offs in forecast data volume and technical expertise. Tier 1: The prefered output file format is in netCDF following standard Climate and Forecast (CF) conventions for dimensions and variable naming conventions, with ensemble member as a dimension where appropriate. Tier 2 is a semi-long CSV format, with state variables as columns and each row representing a unique combination of issue datetime, prediction datetime, location, ensemble member, etc. Tier 3 is similar to Tier 2, but each row represents a specific summary statistic (mean, upper/lower CI) rather than individual ensemble members.

### Output Metadata:

EFI’s metadata represents an expansion upon the Ecological Metadata Language (EML), with two key differences. First, is the specification of additonal metadata tags to store forecast specific information (e.g. uncertainty propagation and data assimilation) as well as some summary information about model complexity, included uncertainties, etc. designed to facilitate cross-forecast synthesis. Second, a number of EML tags (e.g. temporal resolution, output variables) are considered a required part of forecast metadata that are otherwise optional in base EML.

This package includes an R tool for validating these EML forecast and prediction metadata files.

### Archiving:

EFI envisions a three-tiered approach to forecast archiving aligned with FAIR principles. At the most basic level, forecasts should be archived before new observations become available (not possible for hindcasts), preferably in a FAIR public archive that permits forecasts to be uploaded automatically, allows metadata to be searchable, and assigns a DOI. Second, in addition to this the codes used to generate forecasts should also be archived, preferably in an open archive or code repository (e.g. GitHub) that can be assigned a DOI. Finally, in addition to output and code archiving, we encourage running forecast workflows to be archived using virtualization approaches, such as Docker or Singularity containers.

### Vignettes:

This package includes a number of vignettes illustrating the application of the EFI standards to different forecasts

### Documentation:

Version 1.0 of the EFI standard v1 is described by [Dietze et al 2023]([https://doi.org/](https://doi.org/10.1002/ecs2.4686))[^1^]. Note that the Standard is a work in progress. If you find issues as you are applying them, let us know at eco4cast.initiative@gmail.com.

Pkgdown rendered documentation of functions and vignettes can be found at https://eco4cast.github.io/EFIstandards/.

[^1^]: Dietze, Michael C., R. Quinn Thomas, Jody Peters, Carl Boettiger, Gerbrand Koren, Alexey N. Shiklomanov, and Jaime Ashander. 2023. “A Community Convention for Ecological Forecasting: Output Files and Metadata Version 1.0.” Ecosphere 14 (11): e4686. https://doi.org/10.1002/ecs2.4686.
