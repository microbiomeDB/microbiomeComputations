
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

### Troubleshooting Installation Issues

#### Maaslin2 Installation

If you encounter difficulties installing `Maaslin2`, try installing from GitHub directly:

```R
remotes::install_github("biobakery/Maaslin2")
```

#### SpiecEasi and gfortran on macOS

If you encounter errors related to `gfortran` when installing `SpiecEasi`, particularly errors like:

```
ld: library 'emutls_w' not found
ld: warning: search path '/opt/gfortran/lib' not found
```

This typically occurs when gfortran is installed via Homebrew but R expects it in a different location. The recommended solution is to install the official gfortran from the R project:

1. Download and install gfortran from: https://github.com/R-macos/gcc-14-branch/releases
2. Restart R and try installing again

Alternatively, if you have gfortran installed via Homebrew (`/opt/homebrew/bin/gfortran`), you may need to create symlinks or set appropriate environment variables.

### Important: Dependency Change (v5.1.7+)

Starting with version 5.1.7, this package depends on `mbioUtils` instead of `veupathUtils`. If you're updating from an older version and encounter issues, you may need to:

```R
# Remove old installation
remove.packages(c("microbiomeComputations", "veupathUtils"))
# Reinstall fresh
remotes::install_github('microbiomeDB/microbiomeComputations')
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
