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
