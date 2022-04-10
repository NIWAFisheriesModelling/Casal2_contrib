#' @title get_dqs
#'
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of selectivities
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame with all derived quantitiy reports from Casal2 model output
#' @rdname get_dqs
#' @export get_dqs
#' @importFrom reshape2 melt

"get_dqs" <-
  function(model) {
    UseMethod("get_dqs", model)
  }

#'
#' @rdname get_dqs
#' @method get_dqs casal2MPD
#' @export
"get_dqs.casal2MPD" = function(model) {
  # can be -r or -r -i
  multiple_iterations_in_a_report = FALSE
  complete_df = NULL
  reports_labels = names(model)
  for(i in 1:length(model)) {
    this_report = model[[i]]
    if(any(names(this_report) == "type")) {
      if(this_report$type != "derived_quantity") {
        next;
      }
      DQ_types = names(this_report)[!names(this_report) %in% "type"]
      dq_df = NULL
      for(dq_iter in 1:length(DQ_types)) {
        temp_df = data.frame(par_set = 1, years = names(this_report[[DQ_types[dq_iter]]]$values), values = this_report[[DQ_types[dq_iter]]]$values, dq_label = DQ_types[dq_iter]);
        dq_df = rbind(dq_df, temp_df)
      }
      dq_df$par_set = 1
      dq_df$label = reports_labels[i]
      complete_df = rbind(complete_df, dq_df)
    } else {
      n_runs = length(this_report)
      for(dash_i in 1:n_runs) {
        ##
        DQ_types = names(this_report[[dash_i]])[!names(this_report[[dash_i]]) %in% "type"]
        dq_df = NULL
        for(dq_iter in 1:length(DQ_types)) {
          temp_df = data.frame(par_set = dash_i, years = names(this_report[[dash_i]][[DQ_types[dq_iter]]]$values), values = this_report[[dash_i]][[DQ_types[dq_iter]]]$values, dq_label = DQ_types[dq_iter]);
          dq_df = rbind(dq_df, temp_df)
        }
        full_DF = rbind(full_DF, dq_df)
      }
      dq_df$label = reports_labels[i]
      full_DF$par_set = factor(full_DF$par_set, ordered = T)

    }
  }
  dq_df$years = as.numeric(dq_df$years)
  return(complete_df)
  invisible()
}

#'
#' @rdname get_dqs
#' @method get_dqs list
#' @export
"get_dqs.list" = function(model) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {
    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq = get_dqs(model[[i]])
    this_dq$model_label = run_labs[i]
    full_DF = rbind(full_DF, this_dq);
  }
  return(full_DF)
  invisible()
}

#'
#' @rdname get_dqs
#' @method get_dqs casal2TAB
#' @export
"get_dqs.casal2TAB" = function(model) {
  reports_labels = names(model)
  complete_df = NULL
  for(i in 1:length(model)) {
    this_report = model[[i]]
    if(this_report$type != "derived_quantity") {
      next;
    }
    long_format = suppressMessages({melt((this_report$values), variable.name = "colname", value.name = "values", factorsAsStrings = T)})
    long_format$label = reports_labels[i]
    long_format$colname = as.character(long_format$colname)
    first_component = Reduce(c, lapply(strsplit(long_format$colname, split = "[", fixed = T), FUN = function(x){x[1]}))
    second_component = Reduce(c, lapply(strsplit(long_format$colname, split = "[", fixed = T), FUN = function(x){x[2]}))
    third_component = Reduce(c, lapply(strsplit(long_format$colname, split = "[", fixed = T), FUN = function(x){x[3]}))
    ##
    second_component = substring(second_component, first = 1, last = nchar(second_component) - 1)
    third_component = substring(third_component, first = 1, last = nchar(third_component) - 1)
    long_format$years = third_component
    long_format$dq_label = second_component
    long_format$type = first_component
    complete_df = rbind(complete_df, long_format)
  }
  return(complete_df)
  invisible()
}

