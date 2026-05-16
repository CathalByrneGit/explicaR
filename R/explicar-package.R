#' @importFrom rlang %||% .data
#' @importFrom stats na.omit setNames
#' @importFrom utils URLdecode capture.output getParseData glob2rx head
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  "name", "fn_name", "title", "label", "label.roxy", "description"
))
