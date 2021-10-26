#' Utility function to check reports inputs are consistent
#'
#' @author Craig Marsh
#' @param report_label string report label
#' @param model casal2 model
#' @keywords internal
#'
check_report_label <- function(report_label, model) {
  ## check report label exists
  passed = TRUE;
  error_msg = ""
  if (!report_label %in% names(model)) {
    passed = FALSE
    error_msg = paste0("The report label '", report_label, "' was not found. The report labels available are: ", paste(names(model), collapse = ", "))
  }
  return(list(check = passed, msg = error_msg))
}
