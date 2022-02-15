#' @title read.simulated.data
#'
#' @description
#' A utility function for reading in Casal2 simualted data sets. For help on simulated data from Casal2 see
#'
#' @author Craig Marsh
#' @param dir path directory to a folder containing simulated observation from a casal2 -s run
#' @param verbose true print out statements throught function to help debug
#' @return a list of all the simulated data sets
#' @export
#' @rdname read.simulated.data
#' @export read.simulated.data

read.simulated.data <- function(dir, verbose = FALSE) {
  ## add to this as the function grows
  currently_implemented_obs = c("biomass", "abundance", "process_removals_by_length", "proportions_at_length", "process_removals_by_age", "proportions_at_age")

  if(verbose)
    cat("enter: read.simulated.data\n")

  sim_file_names = unique(sapply(strsplit(list.files(dir), split = "\\."), "[", 1))
  extensions = unique(sapply(strsplit(list.files(dir), split = "\\."), "[", 2))
  # check obs are formatted as expected
  if(!all(grepl(extensions, pattern = "_"))) {
    stop(paste0("found a filename with an extension '", extensions[!grepl(extensions, pattern = "_")], "', this is unexpected. Possibly have file_name with multiple '.' in it."))
  }
  n_obs = length(sim_file_names)
  if(verbose) {
    cat("found ", n_obs, " simulated observation sets\n")
    cat("with ", length(extensions), " simulations\n")
  }

  labs = sim_file_names
  sim_obs = list() ##
  ## initialise
  for(n in 1:n_obs)
    sim_obs[[n]] = NULL
  ## start the main loop
  failed_files = vector()
  for(n in 1:n_obs) {
    for (i in 1:length(extensions)) {
      cas_sim_obs = tryCatch(expr = extract.csl2.file(file = paste0(sim_file_names[n],".",extensions[i]), path = dir, quiet = T), error=function(e) {e}, warning=function(w) {w})
      if(inherits(cas_sim_obs, "error") | inherits(cas_sim_obs, "warning")) {
        failed_files = c(failed_files, paste0(paste0(sim_file_names[n],".",extensions[i])))
        next
      }
      this_ob = cas_sim_obs[[1]]
      if(!this_ob$type$value %in% currently_implemented_obs) {
        stop(paste0("file ", sim_file_names[n],".",extensions[i], " is of type = ", this_ob$type$value, ". Currently only ", paste(currently_implemented_obs, collapse = ", "), " are implemented"))
      }
      if(this_ob$type$value %in% c("biomass", "abundance")) {
        ## relative index obs
        obs_table = Reduce(rbind, this_ob$Table$obs[this_ob$years$value])
        class(obs_table) = "numeric"
        rownames(obs_table) = NULL
        sim_obs[n][[1]] = cbind(sim_obs[n][[1]], obs_table[,1])
      } else if (this_ob$type$value %in% c("process_removals_by_length", "proportions_at_length")) {
        props = Reduce(rbind, this_ob$Table$obs[this_ob$years$value])
        class(props) = "numeric"
        rownames(props) = NULL
        bins = this_ob$length_bins$value
        if(!is.null(this_ob$length_plus$value) | this_ob$length_plus$value %in% c("False", "FALSE", "F", "no", "0"))
          bins = bins[-length(bins)]
        mean_bin =  props  %*% as.numeric(bins)
        sim_obs[n][[1]] = cbind(sim_obs[n][[1]], mean_bin)
      } else if(this_ob$type$value %in% c("process_removals_by_age", "proportions_at_age")) {
        props = Reduce(rbind, this_ob$Table$obs[this_ob$years$value])
        class(props) = "numeric"
        rownames(props) = NULL
        bins = as.numeric(this_ob$min_age):as.numeric(this_ob$max_age$value)
        mean_bin =  props  %*% as.numeric(bins)
        sim_obs[n][[1]] = cbind(sim_obs[n][[1]], mean_bin)
      }
    }
  }
  names(sim_obs) = sim_file_names

  if(verbose)
    cat("exit: read.simulated.data\n")
  return(list(sim_obs = sim_obs, failed_files = failed_files))
}
