
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BOLD.NODE

**BOLD_NODE** is an R package that offers functionality to efficiently
explore BOLD dataset releases
(<https://boldsystems.org/data/data-packages/>) in the **Barcode Core
Data Model (BCDM)** format locally. It uses a **DuckDB** backend to
query **parquet** files directly in R, enabling fast searches even on
systems with limited RAM. Data collection is optimized through
customizable chunk sizes and configurable system pause intervals. The
package also allows seamless conversion of search results into standard
R data structures without collecting the data in memory for downstream
analyses:

The package also allows seamless conversion of search results into
standard R data structures without collecting the data in memory for
downstream analyses:

1)  **occurrence matrix** (biodiversity and ecology related analyses)
2)  **sf** (spatial data analyses)
3)  **DNAStringset** (phylogenetic data analyses)
4)  **fasta** (phylogenetic data analyses in third-party tools or in R)

<!-- badges: start -->

<!-- badges: end -->

## Installation

The package can be installed using `devtools::install_github` function
from the `devtools` package in R (which needs to be installed before)

``` r

devtools::install_github("https://github.com/boldsystems-central/BOLDconnectR")
```

## Workflow

A typical workflow for exploring BOLD data (Steps in *italics* are
optional but useful in some instances) 1) `bold.get.vocab` *(Provides
unique terms present in a particular field, making it easier for
exploring `bold.data.search` search parameters)* 2) `bold.data.search`
(Searches the dataset based on the user criteria and prints the records
in the search) 3) `bold.concise.summary` *(Provides a detailed summary
of the dataset retrieved)* 4) `bold.data.collect`(Collects the output of
the `bold.data.search` in memory for downstream exploration/analyses).  
Details on the BCDM fields (field names and definitions) can be obtained
using the `bold.fields.info()` function or on the BCDM GitHub page
(<https://github.com/boldsystems-central/BCDM>).

### Get the vocabulary for specific fields

### Search the dataset

### Data summary

### collect the search data

**Please note** Some queries (e.g., All “Diptera”) may return very large
datasets. Always check the summary before collecting data to ensure you
don’t exceed the available RAM.
