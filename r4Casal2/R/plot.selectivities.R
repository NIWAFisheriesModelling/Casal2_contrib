#' @title plot.selectivities plot selectivities from a Casal2 model.
#'
#' @description
#' A plotting function to plot selectivities for the 'casal2TAB' and 'casal2MPD' objects.
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @param report_labels vector<string>
#' @param plot.it Whether to generate a default plot or return the values as a matrix.
#' @importFrom ggplot2 ggplot geom_line aes
#' @return generate a plot by age or length or if plot.it = F then returns a data.frame
#' @rdname plot.selectivities
#' @export plot.selectivities


"plot.selectivities" <-
function(model, report_labels, plot.it = T) {
  UseMethod("plot.selectivities", model)
}

#' @return \code{NULL}
#'
#' @rdname plot.selectivities
#' @method plot.selectivities casal2MPD
#' @export
"plot.selectivities.casal2MPD" = function(model, report_labels, plot.it = T) {
  multiple_iterations_in_a_report <- FALSE
  N_runs <- 1
  full_DF = NULL
  for(i in 1:length(report_labels)) {
    ## check report exists
    check_report = check_report_label(report_label = report_labels[i], model = model)
    if(!check_report$check)
      stop(check_report$msg)
    this_report = get(report_labels[i], model)
    if (any(names(this_report) == "type")) {
      if (this_report$type != "selectivity")
        stop(paste0("The report label '", report_labels[i], "' is not a selectivity. Please check that the correct report_label was specified."))
    } else {
      stop("This function does not take multiple inputs i.e reports generated from -i.")
    }
    temp_df = data.frame(bins = as.numeric(names(this_report$Values)), values = this_report$Values, selectivity = report_labels[i], type = this_report$sub_type)
    full_DF = rbind(full_DF, temp_df)
  }
  plt = ggplot(full_DF, aes(x = bins, group = selectivity, col = selectivity)) +
    geom_line(aes(y = values), size = 2)

  if(plot.it)
    return(plt)

  if (!plot.it)
    return(full_DF)

  invisible()
}
