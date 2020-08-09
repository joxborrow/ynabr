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

# TODO: Read from an environment variable if available --------------------


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
    bd <- httr::content(execute_get_req(paste0("budgets/", budget)))

  # Fetch budget if Budget name is provided
  if(budget %in% budget_list$name){
    budget <- budget_list$id[budget_list$name == budget]
    bd <- httr::content(execute_get_req(paste0("budgets/", budget)))
  }

  # Add S3 class
  class(bd) <- c("budget_data", "list")

  # Change text dates/times to R date and time classes on budget data
  bd[["data"]][["budget"]][["last_modified_on"]] <-
    as.POSIXct(bd[["data"]][["budget"]][["last_modified_on"]],
               format = "%Y-%m-%dT%H:%M:%S+00:00")
  bd[["data"]][["budget"]][["first_month"]] <-
    as.Date(bd[["data"]][["budget"]][["first_month"]])
  bd[["data"]][["budget"]][["last_month"]] <-
    as.Date(bd[["data"]][["budget"]][["last_month"]])

  # Change text dates/times to R dates on times on months
  bd[["data"]][["budget"]][["months"]] <-
    purrr::map(bd[["data"]][["budget"]][["months"]], ~{
      .x[["month"]] <- as.Date(.x[["month"]])
      return(.x)
    })

  # Change text dates/times to R dates and times on transaction data
  bd[["data"]][["budget"]][["transactions"]] <-
    purrr::map(bd[["data"]][["budget"]][["transactions"]], ~{
      .x[["date"]] <- as.Date(.x[["date"]])
      return(.x)
    })
  bd[["data"]][["budget"]][["scheduled_transactions"]] <-
    purrr::map(bd[["data"]][["budget"]][["scheduled_transactions"]], ~{
      .x[["date_first"]] <- as.Date(.x[["date_first"]])
      .x[["date_next"]] <- as.Date(.x[["date_next"]])
      return(.x)
    })


# TODO: Convert milliunits to actual units --------------------------------



  # Return the data
  return(bd)
}

#' Print Budget data
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.budget_data <- function(bd){
  # Print budget information
  cat("Budget Information\n=============================================")
  cat(paste0("\nName: ", bd[["data"]][["budget"]][["name"]]))
  cat(paste0("\nID: ", bd[["data"]][["budget"]][["id"]]))
  cat(paste0("\nLast Modified: ", bd[["data"]][["budget"]][["last_modified_on"]]))
  cat(paste0("\nTimeframe: ", format(bd[["data"]][["budget"]][["first_month"]]), " to ",
             bd[["data"]][["budget"]][["last_month"]]))
  cat(paste0("\nCurrency: ", bd[["data"]][["budget"]][["currency_format"]][["iso_code"]]))

  # Print account information
  cat("\n\nAccount Information\n=============================================")
  cat(paste0("\n# of Accounts: ", length(bd[["data"]][["budget"]][["accounts"]])))
  active_accounts <- purrr::keep(bd[["data"]][["budget"]][["accounts"]], function(.x) {return (!.x[["closed"]])})
  cat(paste0("\n# of Active Accounts: ", length(active_accounts)))
  on_budget_accounts <- purrr::keep(bd[["data"]][["budget"]][["accounts"]], function(.x) {return (.x[["on_budget"]] & !.x[["closed"]])})
  cat(paste0("\n# of Active On Budget Accounts: ", length(on_budget_accounts)))

  # Print Payee information
  cat("\n\nPayee Summary\n=============================================")
  cat(paste0("\n# of Payees: ", length(bd[["data"]][["budget"]][["payees"]])))
  active_payees <- purrr::keep(bd[["data"]][["budget"]][["payees"]], function(.x) {return (!.x[["deleted"]])})
  cat(paste0("\n# of Active Payees: ", length(active_payees)))

  # Print Category information
  cat("\n\nCategory Summary\n=============================================")
  cat(paste0("\n# of Category Groups: ", length(bd[["data"]][["budget"]][["category_groups"]])))
  cat(paste0("\n# of Categories: ", length(bd[["data"]][["budget"]][["categories"]])))

  # Print Transaction information
  cat("\n\nTransaction Summary\n=============================================")
  cat(paste0("\n# of Transactions: ", length(bd[["data"]][["budget"]][["transactions"]])))
  cat(paste0("\n# of Scheduled Transactions: ", length(bd[["data"]][["budget"]][["scheduled_transactions"]])))

}

#' Budget Data Summary
#'
#' @param bd
#'
#' @return
#' @export
#'
#' @examples
summary.budget_data <- function(bd){
  print(bd)
}
