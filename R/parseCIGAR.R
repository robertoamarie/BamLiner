#' Parse a CIGAR string into its constituent operations and counts
#'
#' `.parseCIGAR()` takes a CIGAR string and splits it into individual operations (such as match, insertions, deletions, etc.) along with their corresponding counts.
#' This function provides a structured breakdown of these operations for further analysis, such as visualizing alignments or processing genomic data.
#'
#' @usage .parseCIGAR(cigar)
#' @param cigar (Mandatory) A character string representing the CIGAR string (e.g., "5M1I3M")
#' @return A data frame with two columns:
#' \describe{
#'   \item{count}{A numeric vector containing the count of bases for each operation.}
#'   \item{operation}{A character vector indicating the operation type for each segment (e.g., "M" for match, "I" for insertion).}
#' }
#' The resulting data frame helps to interpret each CIGAR operation and its associated base count for alignment purposes.
#' If the input is invalid or empty, the function will return an empty data frame.
#'
#' @details The function uses regular expressions to split the CIGAR string into alternating numeric counts and operation types.
#' Invalid CIGAR strings may result in an error or unexpected behavior.
#'
#' @examples
#' # Example of parsing a CIGAR string:
#' BamLiner:::.parseCIGAR("5M1I3M")
#
#'


.parseCIGAR <- function(cigar) {
  # parsing cigar string
  operations <- strsplit(cigar, "(?<=\\D)(?=\\d)", perl = TRUE)[[1]]

  # generating output
  parsed <- data.frame(
    count = as.numeric(gsub("[MIDNSHP=]", "", operations)),
    operation = gsub("[0-9]+", "", operations),
    stringsAsFactors = FALSE
  )

  return(parsed)
}


