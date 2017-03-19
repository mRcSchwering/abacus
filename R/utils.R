#' .Read_html
#'
#' @keywords internal
#'
#' Read html code blocks stored in a fasta like format.
#' 
#' @export
#'
.Read_html <- function(filepath)
{
  lines <- readLines(filepath)
  idx <- which(grepl("^>.*$", lines))
  idx <- c(idx, length(lines) + 1)
  blocks <- lapply(
    seq_along(idx[-1]), 
    function(i) paste(lines[(idx[i] + 1):(idx[i + 1] - 1)], collapse = "")
  )
  names(blocks) <- substring(lines[idx[-length(idx)]], 2)
  return(blocks)
}


