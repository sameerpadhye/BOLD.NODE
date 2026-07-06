
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BOLD.NODE

**BOLD.NODE** is an R package that offers functionality to efficiently
explore BOLD dataset releases
(<https://boldsystems.org/data/data-packages/>) in the **Barcode Core
Data Model (BCDM)** format (for more information on BCDM please visit
its GitHub repo <https://github.com/boldsystems-central/BCDM>) locally.
It uses a **DuckDB** back end to query **parquet** files directly in R,
enabling fast searches even on systems with limited RAM. Data collection
is optimized through customized chunk sizes and configurable system
pause intervals.

The package also allows seamless conversion of search results into
standard R data structures without collecting the data in memory for
downstream analyses:

1.  **occurrence matrix** (biodiversity and ecology related analyses)
2.  **sf** (spatial data analyses)
3.  **DNAStringset** (phylogenetic data analyses)
4.  **fasta** (phylogenetic data analyses in third-party tools or in R)

<!-- badges: start -->

<!-- badges: end -->

## User manual

The user manual for the package can be downloaded from the following
link
(<https://github.com/boldsystems-central/BOLDconnectR_examples/blob/main/BOLD.NODE_v0.0.3.pdf>)

## Installation

The package can be installed using `devtools::install_github` function
from the `devtools` package in R (which needs to be installed before)

``` r
devtools::install_github("https://github.com/sameerpadhye/BOLD.NODE.git")
```

## Downloading Data Packages

Users need to log into BOLD
(<https://bench.boldsystems.org/index.php/Login/page?destination=MAS_Management_UserConsole>)
to download the datasets in the `parquet` format. The users can then
directly use the file as an input for the search and vocabulary
functions

## BOLD.NODE has 11 functions:

1.  bcdm_field_names
2.  bcdm_field_values
3.  bold_parquet_search
4.  bold_search_collect
5.  get_bin_consensus
6.  get_bin_reps
7.  get_concise_summary
8.  *bcdm_to_dnastringset*
9.  bcdm_to_dwc
10. bcdm_to_fasta
11. bcdm_to_occmatrix
12. bcdm_to_sf

**Note** *Function 7*: *bcdm_to_dnastringset* requires the package
`Biostrings` to be installed and imported in the R session beforehand.
It can be installed using using `BiocManager` package.

``` r
# if (!requireNamespace("BiocManager", quietly=TRUE))
#
# install.packages("BiocManager")
#
# BiocManager::install("Biostrings")
```

## Workflow for search and collect

A typical workflow for exploring and BOLD data (Steps in *italics* are
optional but useful in some instances):

1.  `bcdm_field_values` and `bcdm_field_values` *(Provide the names of
    different BCDM fields and unique terms present in a particular
    field, making it easier for exploring* `bold_parquet_search` *search
    parameters)*
2.  `bold_parquet_search` (Searches the dataset based on the user
    criteria and prints the number of records available)
3.  `bold_search_collect`(Collects the output of the
    `bold_parquet_search` in memory for downstream
    exploration/analyses).
4.  *Optional*: Transform the searched data into a `fasta` or `sf` or
    `DNAStringset` or an `occurrence matrix` for downstream analyses

### 1.Get the vocabulary for specific fields

This function can be used for getting unique values of some of the
categorical fields (e.g. institutes) to make searches easier

``` r
# parquet_file<-'path where the parquet file from BOLD is downloaded'

# vocab.data <- bcdm_field_values(parquet_file,specific.cols = c("country.ocean"))
```

### 2.Search the dataset

This function lets users search by more than 10 different search
parameters including taxonomy (species, genus, family etc.), geography
(country.ocean, province, region), ids (processid, sampleid) and more.
The users can just input the search query directly (e.g. put *Canada* as
a query in the `geography` argument or *Coleoptera* in the `taxonomy`
argument) with the function searching the query in the relevant fields
internally. The search result is a `tbl_sql` object that is used for
collecting or transforming the data.

``` r
# parquet_file<-'path where the parquet file from BOLD is downloaded'

# 1 Taxonomy

# bold_search_taxonomy <- bold_parquet_search(input.parquet=parquet_file,
# taxonomy = c("Odonata","Poecilia"))

# 2 Geography

# 2a without specifying a geographic scope
# bold_search_geography <- bold_parquet_search(input.parquet=parquet_file,
# taxonomy= c("Panthera pardus"),
# scope.geography = 'any',
# geography = c("India"))

# 2b specifying 'country.ocean' as the geographic scope (so that the search will only get the 
# records where India is the assigned country)
# bold_search_geography <- bold_parquet_search(input.parquet=parquet_file,
# taxonomy= c("Panthera pardus"),
# scope.geography = 'country.ocean',
# geography = c("India"))


# 3 Combination of many search criteria

# bold_search_combination <- bold_parquet_search(
# input.parquet=parquet_file,
# taxonomy= "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))
```

### 3.Collect the searched data

The searched data can be collected in memory using this function.
**Please note** Some queries (e.g., All “Diptera”) may return very large
datasets. Always check the printed message in the console (shows the
total records in the search) after search before collecting data to
ensure you don’t exceed the available RAM.

``` r
# Collect data (no export)

# bold_search_geography <- bold_parquet_search(input.parquet=parquet_file,
# taxonomy = c("Panthera pardus"),
# geography = c("India"))

# collected_data<-bold_search_collect(
# bold_search_geography,
# chunk.size = 50000,
# export = FALSE)

# Collect data (with parquet export)

# collected_data_w_export<-bold_search_collect(
# bold_search_geography,
# chunk.size = 50000,
# export = TRUE,
# export.type = "parquet",
# output.path = userdefinedpath)
```

### The `get_` functionality

The `get_` functions provide ways of summarizing the data, focusing more
on the **Barcode Index Number (BIN)** centric summaries that include a
consensus taxonomy as well as representative records for BINs based on
different criteria. The `get_concise_summary` gives a succinct summary
of the searched data.

#### `get_bin_reps`

gets a dataset having one or more representative record(s) from each BIN
based on different criteria

``` r
# bold_search <- bold_parquet_search(
# input.parquet = parquet_file,
#  taxonomy = "Araneae",
# geography = "Canada")

# Select one representative per BIN from the searched data based on sequence length of 658 basepairs
# bin_tax_reps <- get_bin_reps(
# bold.search.res = bold_search,
# criteria = list(seq_length = 658)
# )
```

#### `get_concise_summary`

gets a concise summary of the searched data. Search results include
total records, total countries, amplicon length range and many more

``` r
#  Search the data
# bold_search <- bold_parquet_search(
# input.parquet=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))

#  Get concise summary of the data
# bold_summary <- get_concise_summary(bold_search)
```

### The `bcdm_to_` functionality

The `bcdm_to_` functions convert the search results from the
`bold_parquet_search` into objects used in packages such as `vegan`,
`msa`, `DECIPHER`, `terra`, `geodata` etc.

#### `bcdm_to_fasta`

Creates a fasta file with customized headers of the searched data. This
can be exported locally for any downstream analytical pipelines in third
party tools

``` r
#  Search the data

# bold_search <- bold_parquet_search(
# input.parquet=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))

# Get fasta

# bcdm_to_fasta(
# bold_search,
# output.file = "trial.fas",
# fas.header = c("bin_uri", "processid"))
```

#### `bcdm_to_sf`

generates a `sf` object of the searched data for any downstream spatial
data analyses

``` r
#  Search the data

# bold_search <- bold_parquet_search(
# input.parquet=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))

# Get sf

# sf_res <- bcdm_to_sf(bold_search, chunk.size = 100000)
```

#### `bcdm_to_occmatrix`

creates an occurrence matrix from the searched data based on the
`taxon.rank`, `taxon.name` (optional) and the `site.cat`

``` r
#  Search the data
# bold_search <- bold_parquet_search(
# input.parquet=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))

#  Get occurrence data
# occurrence_data <- bcdm_to_occmatrix(
# bold_search,
# taxon.rank = "family",
# site.cat = "region")
```

#### `bcdm_to_dnastringset`

generates a `DNAStringSet` (Biostrings object) object of the searched
data for any downstream sequence alignment and tree generation with
custom headers. The library `Biostrings` has to be installed and
imported before using this function

``` r
#  Search the data
# bold_search <- bold_parquet_search(
# input.parquet=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))

#  Get DNAStringSet
# bold.dnastringset<-bcdm_to_dnastringset(bold_search,
# marker="COI-5P",
# cols_for_seq_names = c("processid","family"))
```

#### It takes roughly 10 minutes to collect ~10M records on a i7 2.8GHZ 16GB RAM machine

<img src="man/figures/README-benchmark_fig-1.jpeg" alt="" width="100%" />

**The package is under active development and the functionality is
subject to change**

#### Funding

This work was funded by the [New Frontiers in Research Fund (NFRF) -
Transformation
2020](https://sshrc-crsh.canada.ca/funding-financement/nfrf-fnfr/transformation/transformation-eng.aspx)
