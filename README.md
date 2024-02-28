
<!-- badges: start -->
  [![R-CMD-check](https://github.com/VEuPathDB/microbiomeComputations/workflows/R-CMD-check/badge.svg)](https://github.com/VEuPathDB/microbiomeComputations/actions)
  <!-- badges: end -->

# microbiomeComputations

microbiomeComputations is an R package which provides helper functions for performing common analyses 
on microbiome data. The results are available as json, or optionally in a `data.table`. This package is intended to be compliant with the API specified in the VEuPathDB EdaComputeService.

## Installation

Use the R package [remotes](https://cran.r-project.org/web/packages/remotes/index.html) to install microbiomeComputations. From the R command prompt:

```R
remotes::install_github('VEuPathDB/microbiomeComputations')
```

## Usage
```
# df is a data frame of abundance values with samples as rows. One column should hold the sample id, all other columns are assumed to be taxa
# the 'recordIdColumn' is the name of the column that contains the sample ids.
# 'method' can be 'simpson', 'shannon', or 'evenness'.
dt <- alphaDiv(df, recordIdColumn="entity.SampleID", method='simpson')
```

## Testing
This package uses the testthat package for testing.

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update unit tests as appropriate. If making a change that affects the eda-verse, please conduct integration testing if possible.

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)
