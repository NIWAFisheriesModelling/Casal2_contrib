#' helper function to see if a matrix is invertable
#' @param m an n x n matrix
#' @export
#' @return bool
#'  \itemize{
#'   \item false: not invertable
#'   \item true: is invertable
#' }
#' @examples
#'\dontrun{
#' x <- matrix(rep(1,25),nc=5)          # singular
#' y <- matrix(1+1e-10*rnorm(25),nc=5)  # very nearly singular matrix
#' z <- 0.001*diag(1,5)                 # non-singular, but very smalll determinant
#' is_matrix_invertable(x)
#' # [1] FALSE
#' is_matrix_invertable(y)
#' # [1] TRUE
#' is_matrix_invertable(z)
#' # [1] TRUE
#' }
is_matrix_invertable <- function(m) {
  any("matrix" %in% class(try(solve(m),silent=TRUE)))
}

#' @title expand_category_block
#'
#' @description
#' A utility function for expanding short hand syntax in @category blocks in casal2 config files
#'
#' @author Craig Marsh
#' @param categories string of categories
#' @return a vector of strings
#' @export
#' @rdname expand_category_block
#' @export expand_category_block
#' \donttest{
#' expand_category_block("stock")
#' expand_category_block("stock.male,female")
#' expand_category_block("stock.male,female.untagged,1990")
#' }
expand_category_block <- function(categories) {
  expanded_labels = vector();
  groups = strsplit(categories, split = ".", fixed = TRUE)[[1]]
  category_list = list()
  for(i in 1:length(groups)) {
    if(grepl(groups[i], pattern = ",")) {
      sub_groups = strsplit(groups[i], split = ",", fixed = TRUE)[[1]]
      category_list[[i]] = sub_groups
    } else {
      category_list[[i]] = groups[i]
    }
  }
  ## now permutate them
  all_perms <- expand.grid(category_list, stringsAsFactors = FALSE)
  for(i in 1:nrow(all_perms)) {
    expanded_labels = c(expanded_labels, paste0(all_perms[i,], collapse = "."))
  }
  return(expanded_labels)
}
