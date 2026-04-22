
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BOLD.NODE

**BOLD_NODE** is an R package that offers functionality to efficiently
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
(<https://github.com/boldsystems-central/BOLDconnectR_examples/blob/main/BOLD.NODE_1.0.0.pdf>)

## Installation

The package can be installed using `devtools::install_github` function
from the `devtools` package in R (which needs to be installed before)

``` r

devtools::install_github('https://github.com/sameerpadhye/BOLD.NODE.git')
```

## Downloading Data Packages

Users need to log into BOLD
(<https://bench.boldsystems.org/index.php/Login/page?destination=MAS_Management_UserConsole>)
to download the datasets in the `parquet` format. The users can then
directly use the file as an input for the search and vocabulary
functions

## BOLD.NODE has 10 functions:

1.  bold.bcdm.fields
2.  bold.data.search
3.  bold.data.collect
4.  get.concise.summary
5.  *get.DNAStringset*
6.  get.DwC
7.  get.fasta
8.  get.occ.data
9.  get.sf
10. get.vocab

**Note** *Function 5*: *get.DNAStringset* requires the package
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

1.  `bold.get.vocab` *(Provides unique terms present in a particular
    field, making it easier for exploring* `bold.data.search` *search
    parameters)*
2.  `bold.data.search` (Searches the dataset based on the user criteria
    and prints the number of records available)
3.  `bold.data.collect`(Collects the output of the `bold.data.search` in
    memory for downstream exploration/analyses).

### 1.Get the vocabulary for specific fields

This function can be used for getting unique values of some of the
categorical fields (e.g. institutes) to make searches easier

``` r

# parquet_file<-'path where the parquet file from BOLD is downloaded'

# vocab.data <- bold.get.vocab(parquet_file,specific.cols = c("country.ocean"))
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

#1 Taxonomy 

# bold_search_taxonomy <- bold.data.search(parquet_path=parquet_file,
# taxonomy = c("Odonata","Poecilia"))

#2 Geography

# bold_search_geography <- bold.data.search(parquet_path=parquet_file,
# taxonomy = c("Panthera pardus),
# geography = c("India"))

#3 Combination of many search criteria

# bold_search_combination <- bold.data.search(
# parquet_path=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660)
```

### 3.Collect the searched data

The searched data can be collected in memory using this function.
**Please note** Some queries (e.g., All “Diptera”) may return very large
datasets. Always check the printed message in the console (shows the
total records in the search) after search before collecting data to
ensure you don’t exceed the available RAM.

``` r

# Collect data (no export)

# collected_data<-bold.data.collect(
# bold_search_geography,
# chunk.size = 50000,
# export = FALSE)

# Collect data (with parquet export)

# bold.data.collect(
# bold_search_combination,
# chunk.size = 50000,
# export = TRUE,
# export.type = "parquet",
# output.path = userdefinedpath)
```

### The `get.` functionality

The `get.` functions convert the search results from the
`bold.data.search` into objects used in packages such as `vegan`, `msa`,
`DECIPHER`, `terra`, `geodata` etc.

#### `get.concise.summary`

gets a concise summary of the searched data. Search results include
total records, total countries, amplicon length range and many more

``` r

#  Search the data

# bold_search <- bold.data.search(
# input.parquet=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))


# Get concise summary of the data

# bold_summary <- get.concise.summary(bold_search)
```

#### `get.fasta`

generates a custom header fasta file of the searched data. This can be
exported for any downstream analytical pipelines

``` r

#  Search the data

# bold_search <- bold.data.search(
# input.parquet=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))

# Get fasta

# get.fasta(
# bold_search,
# output.file = "trial.fas",
# fas.header = c("bin_uri", "processid"))
```

#### `get.sf`

generates a `sf` object of the searched data for any downstream spatial
data analyses

``` r

#  Search the data

# bold_search <- bold.data.search(
# input.parquet=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))

# Get sf

# sf_res<- get.sf(bold_search, chunk.size = 100000)
```

#### `get.occ.data`

creates an occurrence matrix from the searched data based on the
`taxon.rank`, `taxon.name` (optional) and the `site.cat`

``` r

#  Search the data

# bold_search <- bold.data.search(
# input.parquet=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))

# Get occurrence data

# occurrence_data <- get.occ.data(
#   bold_search,
#   taxon.rank = "family",
#   site.cat = "region")
```

#### `get.DNAStringSet`

generates a `DNAStringSet` (Biostrings object) object of the searched
data for any downstream sequence alignment and tree generation with
custom headers. The library `Biostrings` has to be installed and
imported before using this function

``` r

#  Search the data

# bold_search <- bold.data.search(
# input.parquet=parquet_file,
# taxonomy = "Coleoptera",
# geography = "Canada",
# marker = "COI-5P",
# basecount = c(500, 660))

# Get DNAStringSet

# bold.dnastringset<-get.DNAStringSet(bold_search,
# marker="COI-5P",
# cols_for_seq_names = c("processid","family"))
```

#### It takes roughly 10 minutes to collect ~10M records on a i7 2.8GHZ 16GB RAM machine

<img src="man/figures/README-benchmark_fig-1.jpeg" width="100%" />

**The package is under active development and the functionality is
subject to change**
