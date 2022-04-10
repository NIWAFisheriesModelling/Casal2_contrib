#' @title get_BH_recruitment
#'
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of process type recruitment
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame from Casal2 model output
#' @rdname get_BH_recruitment
#' @export get_BH_recruitment
#' @importFrom reshape2 melt


"get_BH_recruitment" <-
  function(model) {
    UseMethod("get_BH_recruitment", model)
  }

#'
#' @rdname get_BH_recruitment
#' @method get_BH_recruitment casal2MPD
#' @export
"get_BH_recruitment.casal2MPD" = function(model) {
  # can be -r or -r -i
  multiple_iterations_in_a_report = FALSE
  complete_df = NULL
  reports_labels = names(model)
  for(i in 1:length(model)) {
    this_report = model[[i]]
    if(any(names(this_report) == "type")) {
      if(this_report$type != "process")
        next;
      if(this_report$type == "process" & this_report$sub_type != "recruitment_beverton_holt")
        next;
      ## only a single trajectory
      standardised_ycs = this_report$standardised_ycs
      ycs_values = this_report$ycs_values
      ycs_years = this_report$ycs_years
      Recruits = this_report$Recruits
      true_ycs = this_report$true_ycs

      full_df = data.frame(ycs_years = this_report$ycs_years,
                           standardised_ycs = this_report$standardised_ycs,
                           ycs_values = this_report$ycs_values,
                           recruits = this_report$Recruits,
                           true_ycs = this_report$true_ycs,
                           par_set = 1,
                           label = reports_labels[i])
      complete_df = rbind(complete_df, full_df)
    } else {
      ## Multiple parameter inputs
      n_runs = length(this_report)
      for(dash_i in 1:n_runs) {
        ## only a single trajectory
        temp_df = data.frame(
          standardised_ycs = this_report[[dash_i]]$standardised_ycs,
          ycs_values = this_report[[dash_i]]$ycs_values,
          ycs_years = this_report[[dash_i]]$ycs_years,
          Recruits = this_report[[dash_i]]$Recruits,
          true_ycs = this_report[[dash_i]]$true_ycs,
          par_set = dash_i,
          label = reports_labels[i])
        complete_df = rbind(complete_df, temp_df)
      }
    }
  }
  return(complete_df)
  invisible()
}

#'
#' @rdname get_BH_recruitment
#' @method get_BH_recruitment list
#' @export
"get_BH_recruitment.list" = function(model) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {

    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq = get_BH_recruitment(model[[i]])
    this_dq$model_label = run_labs[i]
    full_DF = rbind(full_DF, this_dq);
  }
  return(full_DF)
  invisible()
}

#'
#' @rdname get_BH_recruitment
#' @method get_BH_recruitment casal2TAB
#' @export
"get_BH_recruitment.casal2TAB" = function(model) {
  error("not implemented")
}

