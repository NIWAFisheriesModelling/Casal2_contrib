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
      print("multi iteration report found")
      multiple_iterations_in_a_report <- TRUE
      N_runs <- length(this_report)

      if (this_report$'1'$type != "selectivity") {
        stop(paste0("The report label '", report_labels[i], "' is not an selectivity Please check that the correct report label was specified."))
      }
    }
    if (!multiple_iterations_in_a_report) {
      temp_df = data.frame(values = as.numeric(this_report$Values), bins = as.numeric(names(this_report$Values)));
      temp_df$label = report_labels[i]
      full_DF = rbind(full_DF, temp_df)
    } else {
      n_runs = length(this_report)
      for(dash_i in 1:n_runs) {
        temp_df = data.frame(values = as.numeric(this_report[[dash_i]]$Values), bins = as.numeric(names(this_report[[dash_i]]$Values)), par_set = dash_i);
        temp_df$label = report_labels[i]
        full_DF = rbind(full_DF, temp_df)
      }
      full_DF$par_set = factor(full_DF$par_set, ordered = T)
    }
  }

  plt = ggplot(full_DF, aes(x = bins, group = label, col = label)) +
    geom_line(aes(y = values), size = 2)
  if(multiple_iterations_in_a_report) {
    plt = plt + facet_grid(par_set~label)
  }

  if(plot.it)
    return(plt)

  if (!plot.it)
    return(full_DF)

  invisible()
}
