#' @title summarise_model
#' @description
#' utility function that reads in config.csl2 file and summarises a model in table formats and plots
#'
#' @author Craig Marsh
#' @param config_dir path directory
#' @param config_file the starting config that starts a Casal2 model default config.csl2, but can be overridden with casal2 -c my_config. in the later case it should be the my_config name
#' @importFrom Casal2 extract.csl2.file
#' @return
#' @export
#' @rdname summarise_model
#' @export summarise_model
summarise_model <- function(config_dir = "", config_file = "config.csl2", quiet = T, fileEncoding = "") {
  #config_dir = system.file("extdata", "PosteriorPredictiveChecks", package = "r4Casal2", mustWork = TRUE)
  file = scan(file = file.path(config_dir, config_file), what = "", sep = "\n", quiet = T)
  ## deal with comments
  file <- file[substring(file, 1, 1) != "#"]
  index1 <- ifelse(substring(file, 1, 2) == "/*", 1:length(file), 0)
  index2 <- ifelse(substring(file, 1, 2) == "*/", 1:length(file), 0)
  index1 <- index1[index1 != 0]
  index2 <- index2[index2 != 0]
  if (length(index1) != length(index2))
    stop(paste("Error in the file ", filename, ". Cannot find a matching '/*' or '*/'", sep = ""))
  if (length(index1) > 0 || length(index2) > 0) {
    index <- unlist(apply(cbind(index1, index2), 1, function(x) seq(x[1], x[2])))
    file <- file[!1:length(file) %in% index]
  }
  file <- ifelse(regexpr("#", file) > 0, substring(file, 1, regexpr("#", file) - 1), file)
  file <- file[file != ""]
  ## get includes assumes file names have \" \"
  file = substring(file, first = 10) # '!include ' is 10 characters
  ## check for '"'
  ndx = regexpr("\"", file) > 0
  if(any(ndx)) {
    for(i in 1:length(file)) {
      if(ndx[i])
        file[i] = substring(file[i], first = 2, last = nchar(file[i]) - 1)
    }
  }
  if(!quiet)
    cat("found the following files to read in ", file, "\n")
  ## now read in these config files
  model_block = list()
  observation_blocks = list()
  process_blocks = list()
  estimate_blocks = list()
  time_steps_list = list()
  categories_list = list()
  age_length_list = list()
  category_labels = NULL
  observation_labels = NULL
  category_age_lengths = NULL;
  model_years = NULL
  ages = NULL
  time_steps = NULL
  for(i in 1:length(file)) {
    this_file = tryCatch(extract.csl2.file(path = config_dir, file = file[i], quiet = quiet), error=function(e) {e}, warning=function(w) {w})
    if(inherits(this_file, "error") | inherits(this_file, "warning")) {
      cat("failed to readin the following file ", file[i], " so skipping it.\n\nthe error\n",this_file$message,"\n")
      next
    }
    blocks = (sapply(strsplit(names(this_file), split = "\\["), "[", 1))
    labels = (sapply(strsplit(names(this_file), split = "\\["), "[", 2))
    labels = (sapply(strsplit(labels, split = "\\]"), "[", 1))
    for(j in 1:length(this_file)) {
      if(tolower(blocks[j]) == "model") {
        model_block[[labels[j]]] = this_file[[j]]
        model_years = as.numeric(this_file[[j]]$start_year$value):as.numeric(this_file[[j]]$final_year$value)
        ages = as.numeric(this_file[[j]]$min_age$value):as.numeric(this_file[[j]]$max_age$value)
        time_steps = this_file[[j]]$time_steps$value
      } else if(tolower(blocks[j]) == "time_step") {
        time_steps_list[[labels[j]]] = this_file[[j]]$processes$value
      } else if(tolower(blocks[j]) == "categories") {
        categories_list[[labels[j]]] = this_file[[j]]
        category_labels = expand_category_block(this_file[[j]]$names$value)
        ## TODO catch short hand syntax here.
        category_age_lengths = this_file[[j]]$age_lengths$value
      } else if(tolower(blocks[j]) == "age_length") {
        age_length_list[[labels[j]]] = this_file[[j]]
      } else if(tolower(blocks[j]) == "process") {
        process_blocks[[labels[j]]] = this_file[[j]]
      } else if(tolower(blocks[j]) == "estimate") {
        estimate_blocks[[labels[j]]] = this_file[[j]]
      } else if(tolower(blocks[j]) == "observation") {
        observation_blocks[[labels[j]]] = this_file[[j]]
        observation_labels = c(observation_labels, labels[j])
      }
    }
  }
  ## now summarise...
  ## these are converted to dfs for easy conversion to tables.
  time_step_df = NULL
  time_step_df_just_lab = NULL
  for(i in 1:length(time_steps)) {
    ## get process type
    proceses = time_steps_list[[time_steps[i]]]
    process_type = vector();
    df_entry = vector()
    for(j in 1:length(proceses)) {
      process_type[j] = process_blocks[[proceses[j]]]$type$value
      df_entry[j] = paste0(proceses[j], " (", process_type[j], ")")
    }
    this_step = data.frame(time_step = time_steps[i], processes = paste(df_entry, collapse = ", "))
    time_step_df = rbind(time_step_df, this_step)
    time_step_df_just_lab = rbind(time_step_df_just_lab, data.frame(time_step = time_steps[i], processes = paste(proceses, collapse = ", ")))
  }
  ## categories
  category_df = NULL
  for(i in 1:length(category_labels)) {
    ## get process type
    this_category = categories_list[[category_labels[i]]]
    ## get age-length label type
    ## get length-weight label type
  }
  ## Observations
  obs_year_matrix = matrix(NA, nrow = length(observation_labels), ncol = length(model_years), dimnames = list(observation_labels, model_years))
  for(i in 1:length(observation_labels)) {
    this_obs = observation_blocks[[observation_labels[i]]]
    years = as.numeric(this_obs$years$value)
    obs_year_matrix[,model_years %in% years] = 1
  }
  ## Catch and M
  for(i in 1:length(process_blocks)) {
    this_process = process_blocks[[i]]
    if(this_process$type$value == "mortality_instantaneous") {

    } else if(this_process$type$value == "mortality_instantaneous_retained") {

    }
  }



  return(list(category_df = category_df, time_step_df = time_step_df, obs_year_matrix = obs_year_matrix, model_years = model_years))
}
