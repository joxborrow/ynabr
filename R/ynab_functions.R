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
# 6062d48a5310689d35cfe589ed73ffe2e8b51da79026f847e0ecce9fe5097eed

#' Set your YNAB API options
#'
#' This function sets a global option ynab_token for use in
#' many other function calls.  It should be the first function
#' called as other functions will not work without a proper
#' token. It also sets the base_url option.
#'
#' @param token
#'
#' @return No return value
#' @export
#' @examples
#' set_ynab_token("test************************************************************")
set_ynab_options <- function(token){
  # Check the class of the token argument
  if(class(token) != "character")
    stop("The token argument must have a class of character.")

  # Check the length of the token argument
  if(nchar(token) != 64)
    stop("The token argument must have a length of 64.")

  # Set the ynab_token option
  options(ynab_token = token)

  # Set the base url
  options(base_url = "https://api.youneedabudget.com/v1/")
}
