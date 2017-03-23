#' Table
#'
#' Convenience function for rendering DT datatables.
#'
#' @family rendering functions
#'
#' @param  data        \code{dataframe} to be rendered as table
#' @param  colNames    \code{NULL} or \code{chr} arr of same length as rows in 
#'                     data. if not \code{NULL} \code{chr arr} is
#'                     taken as column names instead of column names of data
#' @param  bRownames   \code{bool} whether to show rownames
#' @param  bScroll     \code{bool} whether xScroll is enabled 
#'                     (horizontal scrolling)
#' @param  style       \code{chr} defining style. currently there is only 
#'                     \code{"default"} and \code{"bootstrap"}
#' @param  class       \code{chr} defining class. \code{"display"} is default, 
#'                     \code{"stripe hover"} is also nice
#'                     there are many stlye combinations possible (DT website)
#' @param  dom         \code{chr} for dom arguments. define table elements 
#'                     in order. \code{f} filtering, 
#'                     \code{r} processing, \code{t} table, \code{i} 
#'                     information, \code{p} pagination, \code{"frtip"} 
#'                     is default
#' @param  ordering    \code{NULL} or \code{list} of \code{num arr} and 
#'                     \code{chr 'asc'} or \code{'desc'}. e.g. 
#'                     \code{list(4, 'asc')}
#' @param  alignment   \code{list} of 3 elements \emph{centre, justify}, 
#'                     and \emph{left}. they can each be \code{NULL}(default) or
#'                     a \code{num} arr for which columns to be aligned 
#'                     accordingly. other columnss are right aligned
#' @param  formatCurr  \code{NULL} or \code{list} of 2 elements \emph{cols} 
#'                     (\code{num arr}) and \emph{curr} (\code{chr}) for 
#'                     currency formatting of \emph{cols} columns
#' @param  formatPerc  \code{NULL} or \code{list} of 1 element \emph{cols} 
#'                     (\code{num arr}) for percentage formatting of cols 
#'                     columns
#' @param  formatRoun  \code{NULL} or \code{list} of 2 elements \emph{cols} 
#'                     (\code{num arr}) and \emph{digits} (\code{num}) for 
#'                     rounding numbers of cols columns to number of digits
#' @param  buttons     \code{NULL} or \code{chr} arr defining download buttons 
#'                     used in table ('copy', 'csv', 'excel', 'pdf', 'print')
#' @param  bResponsive \code{bool} whether responsive is activated: if table 
#'                     is to narrow, columns are excluded
#'                     and there is a ugly plus symbol where they can be 
#'                     displayed by clicking
#' @param  pageLen     \code{num} for how many rows are displayed
#' @param  filename    \code{chr} filename for downloaded data
#' @param  esc         \code{bool} whether to escape HTML, if not JS 
#'                     callback ensures input bindings
#' 
#' @return \code{datatables}, \code{htmlwidget} object
#' 
#' @examples 
#' Table_DT(iris, bButtons = TRUE, alignment = list(left = 2), dom = "t")
#' 
#' @export
#'
Table <- function(data, colNames = NULL, bRownames = FALSE, style = "default", 
                  class = "display", dom = "flrtip", ordering = NULL, 
                  alignment = list(centre = NULL, justify = NULL, left = NULL), 
                  formatCurr = NULL, formatPerc = NULL, formatRoun = NULL, 
                  bButtons = FALSE, bResponsive = FALSE,
                  pageLen = 15, bScroll = FALSE, filename = "*", esc = TRUE)
{
  # dom opts
  if (bButtons) dom <- paste0("B", dom)
  
  # colnames
  if (!is.null(colNames)) names(data) <- colNames
  
  # alignments
  colDefs <- list()
  if (!is.null(alignment$centre)) {
    colDefs[[length(colDefs) + 1]] <- 
      list(className = 'dt-center', targets = alignment$centre)
  }
  if (!is.null(alignment$justify)) {
    colDefs[[length(colDefs) + 1]] <- 
      list(className = 'dt-justify', targets = alignment$justify)
  }
  if (!is.null(alignment$left)){
    colDefs[[length(colDefs) + 1]] <- 
      list(className = 'dt-left', targets = alignment$left)
  }
  
  # options
  opts <- list(
    dom = dom, 
    columnDefs = colDefs ,
    lengthMenu = list(c(5, 15, 50, 100, -1), c('5', '15', '50', '100', 'All')), 
    pageLength = pageLen, scrollX = bScroll
  )
  
  # shiny input bindings
  if (!esc) {
    js <- 'function() { Shiny.%s(this.api().table().node()); }'
    opts[["preDrawCallback"]] <- htmlwidgets::JS(sprintf(js, "unbindAll"))
    opts[["drawCallback"]] <- htmlwidgets::JS(sprintf(js, "bindAll"))
  }
  
  # ordering
  if (!is.null(ordering)) opts[["order"]] <- ordering
  
  # extensions
  ext <- character(0)
  if (bResponsive) ext <- append(ext, "Responsive")
  
  # options and extensions for buttons
  if (bButtons) {
    opts[["buttons"]] <- list(
      "copy",
      "print", 
      list("extend" = 'csv', "text"='csv', 
           "filename" = filename, "title" = filename), 
      list("extend" = 'excel', "text"='Excel', 
           "filename" = filename, "title" = filename), 
      list("extend" = 'pdf', "text"='pdf', 
           "filename" = filename, "title" = filename)
    )
    ext <- append(ext, "Buttons")
  }
  
  # create table
  d <- DT::datatable(
    data, 
    rownames = bRownames, 
    style = style, 
    class = class, 
    options = opts, 
    extensions = ext, 
    escape = esc 
  )
  
  # formatting
  if (!is.null(formatCurr)) {
    d <- DT::formatCurrency(d, formatCurr$cols, currency = formatCurr$curr, 
                            interval = 3, mark = " ")
  }
  if (!is.null(formatPerc)) {
    d <- DT::formatPercentage(d, formatPerc$cols, digits = 2)
  }
  if (!is.null(formatRoun)) {
    d <- DT::formatRound(d, formatRoun$cols, digits = formatRoun$digits)
  }
  
  return(d)
} 
