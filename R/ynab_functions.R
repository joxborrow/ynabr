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
#

#' Set your YNAB API options
#'
#' This function sets a global option ynab_token for use in
#' many other function calls.  It should be the first function
#' called as other functions will not work without a proper
#' token. It also sets the base_url option, including the YNAB api version.
#'
#' The token is obtained through a registration process on the YNAB website. The
#' token is a 64 character long alphanumeric string.
#'
#' @param token
#'
#' @return No return value
#' @export
#' @examples
#' ## set_ynab_token("test**************************************************")
ynab_set_token <- function(token, api_version = "v1"){
  # Add getting the token from an enviroment variable -----

  # Check the class of the token argument
  if(class(token) != "character")
    stop("The token argument must have a class of character.")

  # Check the length of the token argument
  if(nchar(token) != 64)
    stop("The token argument must have a length of 64.")

  # Set the ynab_token option
  options(ynab_token = token)

  # Set the base url
  options(base_url = paste0("https://api.youneedabudget.com/", api_version, "/"))
}

#' Execute a YNAB GET request
#'
#' @param entry_point
#'
#' @return
#' @export
#'
#' @examples
execute_get_req <- function(endpoint){
  ret_val <- httr::GET(url = paste(getOption("base_url"), endpoint, sep = ""),
                  httr::add_headers(Authorization = paste("Bearer", getOption("ynab_token"))))
  return(ret_val)
}

#' List the available budgets
#'
#' @return
#' @export
#'
#' @examples
ynab_list_budgets <- function(){
  budget_list <- httr::content(execute_get_req("budgets"))
  budget_list <- budget_list[["data"]][["budgets"]]

  final_list <- data.frame(name = NULL,
                           id = NULL,
                           last_modified_on = NULL,
                           first_month = NULL,
                           last_month = NULL)
  for (i in 1:length(budget_list)){
    temp <- data.frame(name = budget_list[[i]][["name"]],
                       id = budget_list[[i]][["id"]],
                       last_modified_on = budget_list[[i]][["last_modified_on"]],
                       first_month = budget_list[[i]][["first_month"]],
                       last_month = budget_list[[i]][["last_month"]])
    final_list <- rbind(final_list, temp)
  }

  return(final_list)
}

#' Get budget data
#'
#' @param budget
#'
#' @return
#' @export
#'
#' @examples
ynab_get_budget <- function(budget){
  budget_list <- ynab_list_budgets()


  # Validate budget name or id
  if (!(budget %in% budget_list$id | budget %in% budget_list$name))
    stop("You must enter a valid budget name or id.")

  # Fetch budget if ID is provided
  if(budget %in% budget_list$id)
    budget_data <- httr::content(execute_get_req(paste0("budgets/", budget)))

  # Fetch budget if Budget name is provided
  if(budget %in% budget_list$name){
    budget <- budget_list$id[budget_list$name == budget]
    budget_data <- httr::content(execute_get_req(paste0("budgets/", budget)))
  }

  # Add S3 class
  class(x) <- c("budget_data", "list")

  # Return the data
  return(x)
}

#' Print Budget data
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.budget_data <- function(x){
  cat("print budget data summary")
}
