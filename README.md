
<!-- badges: start -->
  [![R-CMD-check](https://github.com/microbiomeDB/microbiomeComputations/workflows/R-CMD-check/badge.svg)](https://github.com/microbiomeDB/microbiomeComputations/actions)
  [![](https://dcbadge.limes.pink/api/server/DEjNbxgq?style=flat)](https://discord.gg/DEjNbxgq)
  <!-- badges: end -->

# microbiomeComputations

microbiomeComputations is an R package which provides helper functions for performing common analyses
on microbiome data. The results are available as json, or optionally in a `data.table`. This package is intended to be compliant with the API specified in VEuPathDB/service-eda-compute.

If you have questions or comments of any kind, join us on our [Discord Community Server](https://discord.gg/DEjNbxgq)! We'd love to hear from you.


## Installation

Use the R package [remotes](https://cran.r-project.org/web/packages/remotes/index.html) to install microbiomeComputations. From the R command prompt:

```R
remotes::install_github('microbiomeDB/microbiomeComputations')
```

### Note on Maaslin2 Installation

The `Maaslin2` package from Bioconductor may not have pre-compiled binaries available for all platforms (particularly ARM64 macOS). If you encounter installation issues, you can install it from source:

```R
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("Maaslin2", type = "source")
```

Alternatively, you can install the development version directly from GitHub:

```R
remotes::install_github("biobakery/Maaslin2")
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
Pull requests are welcome and should be made to the **dev** branch. 

For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update unit tests as appropriate.

## License
[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt)
