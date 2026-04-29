#' Compute consensus BIN taxonomy for BOLD data
#'
#' @description
#' Computes and returns consensus taxonomic identifications for each BIN in search results or a BCDM data frame.
#'
#' @details
#' Consensus is defined as any name that exceeds the specified 'threshold', expressed as a proportion of
#' records with a concordant identification (i.e. same name, same rank). The function steps backwards through
#' the eligible 'ranks' to determine the lowest available concordant identification that meets the criteria
#' specified by 'threshold', 'min.ids', and 'enforce.scientific'. Different thresholds can be supplied
#' for each rank, if desired (either as a vector of equal length to ranks or as a named list). The function
#' can also be applied to any other grouping variable by modifying 'groups'.
#'
#' \strong{N.B.:} As this function performs operations on the input data, it may be quite slow for very
#' large data sets and/or weaker machines. Please check the size of 'bold.search.res' input objects
#' using \code{\link{get.concise.summary}} and proceed with caution.
#'
#' @param bold.search.res A tbl_sql object obtained from 'bold.data.search'. (Optional; one of bold.search.res or bold.df must be provided.)
#' @param bold.df Data frame in BCDM format, or any data.frame or data table minimally containing bin_uri (or other grouping variable) and taxonomic identifications for all available records. (Optional; one of bold.search.res or bold.df must be provided.)
#' @param ranks A character vector of ranks to consider for consensus identifications. Defaults to the standard BOLD ranks.
#' @param threshold Numeric value(s) between 0 and 1 indicating the minimum proportion of records in a BIN that must have a concordant identification in order to establish a consensus. Supply as a single value, a vector of length equal to the number of ranks in consideration, or a named list with names corresponding to ranks. If supplied as a named list, an optional "default" value can be set for any ranks that are not explicitly specified (e.g. \code{threshold = list(species = 0.95, default = 0.75)}). Default value is 1.0 (i.e. strict consensus at all ranks).
#' @param min.ids Numeric value(s) indicating the minimum number of identifications needed to establish a consensus (names with fewer identifications are still included when calculating proportions). Supply as a single value, a vector of length equal to the number of ranks in consideration, or a named list with names corresponding to ranks. If supplied as a named list, an optional "default" value can be set for any ranks that are not explicitly specified (e.g. \code{min.ids = list(family = 1, default = 2)}). Default value is 2 (i.e. min 2 identifications at any rank).
#' @param enforce.scientific A logical value indicating whether non-scientific, provisional names should be ignored when determining consensus. Default value is TRUE, meaning non-scientific names are ignored.
#' @param groups Grouping variable. Default value is "bin_uri".
#' @param discord.format String indicating the desired output format for the 'discordant_ids' column. Can be one of "text", or "list". If "text" (the default), the output is a string column with comma-separated values in the format "Taxon (proportion)". If "list", the output is a list column with names indicating competing identifications and values indicating proportions of discordant identifications for each taxon.
#'
#' @returns A table of consensus identifications for each BIN (or other grouping variable), with the following columns:
#'    bin_uri, member_count, concordant_rank, concordant_id, discordant_rank, discordant_ids.
#'
#' @importFrom data.table as.data.table fcase setnames set copy
#'
#' @export
get.bin.consensus <- function(
    bold.search.res = NULL,
    bold.df = NULL,
    ranks = c("kingdom", "phylum", "class", "order", "family", "subfamily", "tribe", "genus", "species", "subspecies"),
    threshold = 1.0,
    min.ids = 1,
    enforce.scientific = TRUE,
    groups = "bin_uri",
    discord.format = c("text", "list")) {

  # Generate a data frame if one is not provided
  if(is.null(bold.df)) {
    if(is.null(bold.search.res)) {
      stop("One of 'bold.df' or 'bold.search.res' must be provided and non-null.")
    } else {
      bold.df <- bold.search.res %>%
        dplyr::filter((!is.na(bin_uri)) & (bin_uri != "")) %>%
        dplyr::select(all_of(c(groups, ranks))) %>%
        collect()
    }
  }

  stopifnot("One or more provided `ranks` is/are missing from `bold.df`." = all(ranks %in% names(bold.df)),
            "Provided `groups` column is missing from `bold.df`." = (groups %in% names(bold.df)),
            "`threshold` value(s) must be one or more real numbers (i.e. doubles) between 0 and 1." = is.double(unlist(threshold)) & all(unlist(threshold) >= 0) & all(unlist(threshold) <= 1),
            "`threshold` must be either a single number, a vector of unnamed numbers equal in length to `ranks`, or a named list or vector of numbers with names corresponding to ranks." = ((length(threshold) == 1) | (length(threshold) == length(ranks)) | (!is.null(names(threshold)))),
            "`min.ids` value(s) must be one or more whole numbers greater than zero." = is.numeric(unlist(min.ids)) & all(unlist(min.ids) > 0) & all(unlist(min.ids) %% 1 == 0),
            "`min.ids` must be either a single number, a vector of unnamed numbers equal in length to `ranks`, or a named list or vector of numbers with names corresponding to ranks." = ((length(min.ids) == 1) | (length(min.ids) == length(ranks)) | (!is.null(names(min.ids)))),
            '`discord.format` must be one of "list" or "text".' = all(discord.format %in% c("list", "text")))

  # Define regex for non-scientific names
  re_int <- "\\.\\Z|\\S{2,}\\.\\S|[0-9]|\\s[A-Z]|[A-Z]\\Z|[A-Z]{2}|[a-z][A-Z]|_(?!(hn|sl|ss)\\Z)|%|\\?|!|\\[|\\]|\\{|\\}|\\(|\\)|,|\\s(?:aff|agg|cf|complex|group|grp|gr|gp|cmplx|pr|ms|cfr|nr|nsp|near|nomen|hybrid|voucher|form|from|ss|ssl|see|spp?|sample)\\.?(?:\\s|\\Z)"

  # Parse threshold & min.ids parameters and align them with ranks
  parse_param_vector <- function(param) {
    if((length(param) != 1) | !is.null(names(param))) {
      if(is.null(names(param))) {
        param <- unlist(unname(param))
      } else {
        named <- as.list(param[(names(param) %in% ranks) & (!duplicated(param))])
        default <- ifelse("default" %in% names(param), param[["default"]], max(unlist(param)))
        if((!length(named) %in% c(0, length(ranks))) & (!"default" %in% names(param))) {
          warning(paste0("Only some ranks found among `",substitute(param),"` values, with no default given; highest value applied to all unspecified ranks."))
        }
        param <- rep(default, length(ranks))
        for(r in names(named)) param[match(r, ranks)] <- named[[r]]
      }
    } else {
      param <- rep(unlist(param), length(ranks))
    }
    return(param)
  }

  threshold <- parse_param_vector(threshold)
  min.ids <- parse_param_vector(min.ids)

  # Create a copy of the data to avoid mutating by reference
  dt <- as.data.table(copy(bold.df))

  # Replace NA in taxonomy columns with empty values (if ignoring non-scientific names, replace those too)
  dt[, (ranks) := lapply(.SD, function(x) fcase(enforce.scientific & grepl(re_int, x, perl = TRUE), "",
                                                is.na(x), "",
                                                grepl("^\\s$", x), "",
                                                default = as.character(x))), .SDcols = ranks]

  # Convert data table to matrix for faster row access
  mat <- as.matrix(dt[, c(groups, ranks), with = FALSE])

  # Replace trailing "" with NA so that they are not counted as alternative names
  for (i in seq_len(nrow(mat))) {
    row_vals <- mat[i, ]
    non_blank_idx <- which(row_vals != "")
    if (length(non_blank_idx) > 0) {
      last <- max(non_blank_idx)
      if (last < ncol(mat)) {
        mat[i, (last + 1):ncol(mat)] <- NA_character_
      }
    } else {
      mat[i, ] <- NA_character_  # Entire row is blank
    }
  }

  # Convert back to data.table and restore column names
  dt <- as.data.table(mat)
  setnames(dt, c(groups, ranks))

  # Core consensus logic
  get_consistent_taxon <- function(sub_dt, ranks, threshold, min.ids) {

    id_hier <- sapply(ranks,function(x) NULL)
    concordant = FALSE
    rank_set <- ranks

    # Ensure min.ids does not exceed group size
    if (any(min.ids > nrow(sub_dt))) {
      for(i in seq_along(min.ids)) min.ids[[i]] <- nrow(sub_dt)
    }

    # Expand threshold and min.ids parameters into full vectors if applicable
    if (length(threshold) == 1) { threshold <- rep(threshold, length(ranks)) }
    if (length(min.ids) == 1) { min.ids <- rep(min.ids, length(ranks)) }

    result <- list(
      member_count = nrow(sub_dt),
      concordant_rank = NA_character_,
      concordant_id = NA_character_,
      concordant_id_count = 0L,
      discordant_rank = NA_character_,
      discordant_ids = list(),
      discordant_id_count = 0L
    )

    for (rank_col in rev(ranks)) {  # Step backwards through ranks

      rank_threshold <- threshold[which(ranks==rank_col)]
      rank_min_ids <- min.ids[which(ranks==rank_col)]
      vals <- sub_dt[[rank_col]]
      filtered <- table(vals[!is.na(vals)])
      name_vals <- proportions(filtered)
      props <- proportions(filtered)[(proportions(filtered) >= rank_threshold) & (filtered >= rank_min_ids)]
      names(name_vals) <- sub("^$","<None>",names(name_vals))

      if ((length(props) != 1) & (length(unique(filtered)) > 0)) {

        concordant <- FALSE

        if(id_hier[rank_col] != "") {
          rank_set <- ranks[0:(which(ranks==rank_col)-1)]
          id_hier <- id_hier[rank_set]
        }

        if(length(name_vals) > 1) {
          result$discordant_rank <- rank_col
          result$discordant_ids <- list(stats::setNames(as.vector(name_vals), names(name_vals)))
          result$discordant_id_count <- sum(filtered)
        }

      } else if ((length(props) == 1) && (names(props)[1] != "")) {

        if(!concordant) {
          result$concordant_rank <- rank_col
          result$concordant_id <- names(props)[1]
          result$concordant_id_count <- unname(filtered[names(props)[1]])
        }

        concordant <- TRUE

        if (is.null(id_hier[[rank_col]]) || is.na(id_hier[[rank_col]]) || is.na(names(props)[1]) || (names(props)[1] != id_hier[[rank_col]])) {
          rank_set <- ranks[0:which(ranks == rank_col)]
          id_hier <- as.list(sub_dt[get(rank_col) == names(props)[1], .SD, .SDcols = rank_set][1])
        }

      }
    }

    for (r in setdiff(ranks,names(id_hier))) {
      id_hier[r] = NA_character_
    }

    result[ranks] <- id_hier

    return(result)
  }

  # Generate summary of consensus by BIN
  consensus <- dt[!is.na(get(groups)), do.call(get_consistent_taxon, list(.SD, ranks, threshold, min.ids)), by = groups, .SDcols = ranks]

  # Convert discordant_ids to text if appropriate
  if(discord.format[1] == "text"){
    data.table::set(consensus,
                    j = "discordant_ids",
                    value = sapply(consensus[["discordant_ids"]], function(x) {
                      if (length(x) == 0) return("")
                      sort(x, decreasing = TRUE)
                      pairs <- paste0(names(x), " (", formatC(as.numeric(x), format = "f", digits = 2), ")")
                      paste(pairs, collapse = ", ")
                    })
    )
  }

  return(consensus)

}
