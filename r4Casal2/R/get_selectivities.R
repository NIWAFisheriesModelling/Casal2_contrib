#' @title get_selectivities
#'
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of selectivities
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame with all selectivity reports from Casal2 model output
#' @rdname get_selectivities
#' @export get_selectivities
#' @importFrom reshape2 melt


"get_selectivities" <-
  function(model) {
    UseMethod("get_selectivities", model)
  }

#'
#' @rdname get_selectivities
#' @method get_selectivities casal2MPD
#' @export
"get_selectivities.casal2MPD" = function(model) {
  # can be -r or -r -i
  multiple_iterations_in_a_report = FALSE
  complete_df = NULL
  reports_labels = names(model)
  for(i in 1:length(model)) {
    this_report = model[[i]]
    if(any(names(this_report) == "type")) {
      if(this_report$type != "selectivity") {
        next;
      }
      ## add it to full df
      this_selectivity = data.frame(selectivity = as.numeric(this_report$Values), bin = names(this_report$Values))
      this_selectivity$label = reports_labels[i]
      this_selectivity$par_set = 1 ## so compatible with -i runs
      ## check col compatibility some reports will print residuals and some wont
      if(!is.null(complete_df)) {
        if(any(!colnames(complete_df) %in% colnames(this_selectivity))) {
          drop_cols = which(!colnames(complete_df) %in% colnames(this_selectivity))
          complete_df = complete_df[, -drop_cols]
        }
        if(any(!colnames(this_selectivity) %in% colnames(complete_df))) {
          drop_cols = which(!colnames(this_selectivity) %in% colnames(complete_df))
          this_selectivity = this_selectivity[, -drop_cols]
        }
      }
      complete_df = rbind(complete_df, this_selectivity)
      next;
    } else {
      multiple_iterations_in_a_report <- TRUE
      if (this_report$'1'$type != "selectivity") {
        next;
      }
      n_runs = length(this_report)
      for(dash_i in 1:n_runs) {
        ## add it to full df
        this_selectivity = data.frame(selectivity = as.numeric(this_report[[dash_i]]$Values), bin = names(this_report[[dash_i]]$Values))
        this_selectivity$label = reports_labels[i]
        this_selectivity$par_set = dash_i
        ## check col compatibility some reports will print residuals and some wont
        if(!is.null(complete_df)) {
          if(any(!colnames(complete_df) %in% colnames(this_selectivity))) {
            drop_cols = which(!colnames(complete_df) %in% colnames(this_selectivity))
            complete_df = complete_df[, -drop_cols]
          }
          if(any(!colnames(this_selectivity) %in% colnames(complete_df))) {
            drop_cols = which(!colnames(this_selectivity) %in% colnames(complete_df))
            this_selectivity = this_selectivity[, -drop_cols]
          }
        }
        complete_df = rbind(complete_df, this_selectivity)
      }
    }
  }
  complete_df$bin = as.numeric(complete_df$bin)
  return(complete_df)
  invisible()
}



#'
#' @rdname get_selectivities
#' @method get_selectivities list
#' @export
"get_selectivities.list" = function(model) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {
    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD', you supplied = ", class(model[[i]])))
    }
    this_sel = get_selectivities(model[[i]])
    this_sel$model_label = run_labs[i]
    full_DF = rbind(full_DF, this_sel);
  }
  return(full_DF)
  invisible()
}

#'
#' @rdname get_selectivities
#' @method get_selectivities casal2TAB
#' @export
"get_selectivities.casal2TAB" = function(model) {
  reports_labels = names(model)
  complete_df = NULL
  for(i in 1:length(model)) {
    this_report = model[[i]]
    if(this_report$type != "selectivity") {
      next;
    }
    colabs = colnames(this_report$values)
    bin_labs = Reduce(c, lapply(strsplit(colabs, split = ".", fixed = T), FUN = function(x){x[2]}))
    colnames(this_report$values) = bin_labs
    long_format = suppressMessages({melt((this_report$values), variable.name = "bin", value.name = "selectivity")})
    long_format$label = reports_labels[i]
    complete_df = rbind(complete_df, long_format)
  }
  complete_df$bin = as.numeric(complete_df$bin)
  return(complete_df)
  invisible()
}

