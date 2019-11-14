Forecasting Metadata
================

Consider the following trivial forecast model from
<https://github.com/EcoForecast/EF_BookCode/blob/master/Ch02_Logitic.Rmd>

``` r
library(EML)
library(tidyverse)
library(uuid)
```

# Population Growth

## Deterministic Growth

Consider the simple discrete logistic growth model

``` r
r = 1
K = 10
n0 = .1
NT = 30
time = 1:NT
```

can then simulate as

``` r
n = rep(n0,NT)
ne = rep(n0,NT)
for(t in 2:NT){
  n[t] = n[t-1] + r*n[t-1]*(1-n[t-1]/K)
  ne[t] = ne[t-1] + r*ne[t-1]
}
time <- as.Date(as.character(2000 + 1:NT), format = "%Y")
df <- data.frame(t = time, n = n, ne = ne)
write.csv(df, "logistic-forecast.csv")
```

Is `logistic-forecast.csv` a suitable format for presenting an
ecological forecast? Our draft document outlines:

  - Project\_identifier. This will be our EML `packageId`
  - Subproject\_identifiers It’s not clear waht these are.
  - Model\_identifier. This could correspond to identifiers defined in
    the `methods` section. Currently this would suggest our data should
    be in ‘long’ form: `t`, `model`, `n`.
  - Forecast\_issued\_date
  - Response\_variable
  - Observed\_value  
  - Predicted\_value these are currently the values `n` or `ne`
  - Observation\_date  
  - Distribution\_of\_response  
  - Conf\_interv\_2.5 In our deterministic model, this is the same as
    predicted value
  - Conf\_intev\_97.5

I think it’s possible to use this format for our simple example with a
univaraite response variable, but the format may be too restrictive for
multi-dimensional responses in time and space, which are more naturally
captured in spatio-temporal arrays like NetCDF. Also the long format
proposed here in which there is a “variable” column rather than allowing
each variable to be it’s own column proses difficulties for metadata –
documenting what variables, what units, bounds groups, and so forth\!

``` r
project_id <- uuid::UUIDgenerate()


df %>% pivot_longer(c(n, ne), 
                    names_to = "model_identifier", 
                    values_to = "predicted_value") %>%
  rename(observation_date = t) %>%
  mutate(conf_interv_2.5 = predicted_value,
         conf_interv_97.5 = predicted_value,
         distribution_of_response = "delta",
         observed_value = NA
         ) %>%
  mutate(forecast_issued_date = Sys.Date(),
         response_variable = "population size",
         project_identifier = project_id,
         subproject_id = "really?")
```

    ## # A tibble: 60 x 11
    ##    observation_date model_identifier predicted_value conf_interv_2.5
    ##    <date>           <chr>                      <dbl>           <dbl>
    ##  1 2001-11-14       n                          0.1             0.1  
    ##  2 2001-11-14       ne                         0.1             0.1  
    ##  3 2002-11-14       n                          0.199           0.199
    ##  4 2002-11-14       ne                         0.2             0.2  
    ##  5 2003-11-14       n                          0.394           0.394
    ##  6 2003-11-14       ne                         0.4             0.4  
    ##  7 2004-11-14       n                          0.773           0.773
    ##  8 2004-11-14       ne                         0.8             0.8  
    ##  9 2005-11-14       n                          1.49            1.49 
    ## 10 2005-11-14       ne                         1.6             1.6  
    ## # … with 50 more rows, and 7 more variables: conf_interv_97.5 <dbl>,
    ## #   distribution_of_response <chr>, observed_value <lgl>,
    ## #   forecast_issued_date <date>, response_variable <chr>,
    ## #   project_identifier <chr>, subproject_id <chr>

-----

Let’s document the metadata of the data table itself. Here I just
document the original columns, `t`, `n`, and `ne`. It may well be that
we decide an Ecological Forecast has to have specific columns like the
ones described above, which would thus correspond to a partially
pre-defined attributes table (e.g. the units would probably still be
allowed to vary, but format would be the same.)

Note one weakness of this format is that it assumes all data in a column
have the same units. This common assumption might be violoated by
transformations to “long” form data, where you have columns like
“variable”, “value”, and “units”. (The long form may be useful, but it
exposes much less information in the metadata layer – e.g. we no longer
know what’s actually being measured without looking at the data file
itself).

``` r
attributes <- tibble::tribble(
  ~attributeName, ~attributeDefinition, ~unit, ~formatString, ~numberType,
  "t",          "time",                        "year",                   "YYYY-MM-DD", "numberType",
  "n",          "population size (logistic)",  "numberPerMeterSquared",   NA,          "real",
  "ne",         "population size (exp)",       "numberPerMeterSquared",    NA,         "real"
  )

attrList <- set_attributes(attributes, 
                           col_classes = c("Date", "numeric", "numeric"))
physical <- set_physical("logistic-forecast.csv")
```

    ## Automatically calculated file size using file.size("logistic-forecast.csv")

    ## Automatically calculated authentication size using digest::digest("logistic-forecast.csv", algo = "md5", file = TRUE)

``` r
dataTable <- eml$dataTable(
                 entityName = "logistic-forecast.csv",
                 entityDescription = "Forecast of population size under a few models",
                 physical = physical,
                 attributeList = attrList)
```

There’s a lot more optional terminology that could be exploited here –
for instance, the specification lets us define different missing value
codes (and explanations) for each column, and allows us to indicate
`precision`, `minimum` and `maximum`.

Note that `physical` type can document almost any formats as well,
including NetCDF etc. A NetCDF file would still document the variables
measured in much the same way regardless of the underlying
representation. Note that

Now that we’ve documented the actual data.frame itself, we can add
additional metadata to the record describing our forecast, which is
essential for citing, discovering, and interpreting the result. We start
with some authorship information.

``` r
me <- list(individualName = list(givenName = "Carl", 
                                 surName = "Boettiger"),
           electronicMailAddress = "cboettig@gmail.com",
           id = "http://orcid.org/0000-0002-1642-628X")
```

Set Taxonomic, Temporal, and Geographic Coverage. (Look, apparently
we’re modeling population densities of *Sarracenia purpurea* in
Harvard Forest starting in about 2012\!)

``` r
coverage <- 
  set_coverage(begin = '2012-06-01', 
               end = '2013-12-31',
               sci_names = "Sarracenia purpurea",
               geographicDescription = "Harvard Forest Greenhouse, Tom Swamp Tract (Harvard Forest)",
               west = -122.44, east = -117.15, 
               north = 37.38, south = 30.00,
               altitudeMin = 160, altitudeMaximum = 330,
               altitudeUnits = "meter")
```

``` r
keywordSet <- list(
    list(
        keywordThesaurus = "EFI controlled vocabulary",
        keyword = list("forecast",
                    "population",
                    "timeseries")
    ))
```

Our dataset needs an abstract describing what this is all about. Also, a
methods section is not required but it’s probably a good idea. Here we
import a methods section that was written in Markdown.

``` r
abstract <- set_TextType("abstract.md")
methods <- set_methods("methods.md")
```

``` r
dataset = eml$dataset(
               title = "A vey silly logistic forecast",
               creator = me,
               contact = list(references="http://orcid.org/0000-0002-1642-628X"),
               pubDate = Sys.Date(),
               intellectualRights = "http://www.lternet.edu/data/netpolicy.html.",
               abstract =  "An illustration of how we might use EML metadata to describe an ecological forecast",
               dataTable = dataTable,
               keywordSet = keywordSet,
               coverage = coverage,
               methods = methods
               )
```

All we need now is to add a unique identifier for the project and we are
good to go\! This could be a DOI or merely an identifier we create,
e.g. a UUID.

``` r
my_eml <- eml$eml(dataset = dataset,
           packageId = project_id,  
           system = "uuid"
           )
```

Once we have finished building our EML metadata, we can confirm it is
valid. This will catch any missing elements. (Recall that what is
‘required’ depends on what you include – for example, you don’t have
to document a `dataTable` at all, but if you do, you have to document
the “physical” file format it is in (e.g. `csv`) and the attributes and
units it uses\!)

``` r
eml_validate(my_eml)
```

    ## [1] TRUE
    ## attr(,"errors")
    ## character(0)

We are now ready to write out a valid EML document:

``` r
write_eml(my_eml, "forecast-eml.xml")
```

At this point, we could easily upload this metadata along with the data
itself to DataONE via the API (or `dataone` R package.)

We can also generate a JSON-LD version of EML:

``` r
emld::as_json(as_emld("forecast-eml.xml"), file = "forecast-eml.json")
```
