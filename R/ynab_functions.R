# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Set your YNAB API token
#'
#' This function sets a global option ynab_token for use in
#' many other function calls.  It should be the first function
#' called as other functions will not work without a proper
#' token.
#'
#' @param token
#'
#' @return No return value
#' @export
#' @examples
#' set_ynab_token("test************************************************************")
set_ynab_token <- function(token){
  # Check the class of the token argument
  if(class(token) != "character")
    stop("The token argument must have a class of character.")
  # Check the length of the token argument
  if(nchar(token) != 64)
    stop("The token argument must have a length of 64.")
  options(ynab_token = token)
}
