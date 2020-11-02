#' Set your YNAB API options
#'
#' This function sets a global option ynab_token for use in
#' many other function calls.  It should be the first function
#' called as other functions will not work without a proper
#' token. It also sets the base_url option, including the YNAB api version
#' (currently v1).
#'
#' The token is obtained through a registration process on the YNAB website. The
#' token is a 64 character long alphanumeric string.
#'
#' @param token This can be the 64 alphanumeric personal access token from the
#' YNAB registration process. If left blank, the ynab_token option will be set
#' from the environment variable YNAB_TOKEN.
#'
#' @return No return value
#' @export
ynab_set_token <- function(token = NULL){
  # Get the token from an enviroment variable
  if(is.null(token))
    token <- Sys.getenv("YNAB_TOKEN")

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

#' Execute a YNAB GET request
#'
#' This function executes a GET request against the YNAB api.
#' @param entry_point
#'
#' @return what ever obect has been requested
#'
execute_get_req <- function(endpoint, timeout = 20){

  ret_val <- httr::GET(url = paste(getOption("base_url"), endpoint, sep = ""),
                  httr::add_headers(Authorization =
                                      paste("Bearer", getOption("ynab_token"))),
                  httr::timeout(timeout))
  return(ret_val)
}
#' List the available budgets
#'
#' This function is to be used after setting the ynab_token object. It will fetch
#' and display the available budgets associated with the personal access token
#' used.
#'
#' @return a data frame of the names and id's of all available budgets for download.
#' @export
#'
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
#' This command is used to fetch budget data using the YNAB api.
#'
#' @param budget the name or id of an available budget. It can be determined by
#' using the ynab_list_budgets() command.
#'
#' @return a "budget_data" object representing a list of the various data returned
#' by the YNAB API, with only some convenient adjustments.
#' @export
#'
ynab_get_budget <- function(budget, remove_deleted = TRUE, remove_closed = TRUE){
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

  # Deal with deleted data
  if(remove_deleted == TRUE){
    # Remove deleted accounts
    bd[["data"]][["budget"]][["accounts"]] <-
      purrr::discard(bd[["data"]][["budget"]][["accounts"]], ~{.x[["deleted"]]})
    # Remove deleted payees
    bd[["data"]][["budget"]][["payees"]] <-
      purrr::discard(bd[["data"]][["budget"]][["payees"]], ~{.x[["deleted"]]})
    # Remove deleted payee_locations
    bd[["data"]][["budget"]][["payee_locations"]] <-
      purrr::discard(bd[["data"]][["budget"]][["payee_locations"]], ~{.x[["deleted"]]})
    # Remove deleted category_groups
    bd[["data"]][["budget"]][["category_groups"]] <-
      purrr::discard(bd[["data"]][["budget"]][["category_groups"]], ~{.x[["deleted"]]})
    # Remove deleted categories
    bd[["data"]][["budget"]][["categories"]] <-
      purrr::discard(bd[["data"]][["budget"]][["categories"]], ~{.x[["deleted"]]})
    # Remove deleted months
    bd[["data"]][["budget"]][["months"]] <-
      purrr::discard(bd[["data"]][["budget"]][["months"]], ~{.x[["deleted"]]})
    # Remove deleted transactions
    bd[["data"]][["budget"]][["transactions"]] <-
      purrr::discard(bd[["data"]][["budget"]][["transactions"]], ~{.x[["deleted"]]})
    # Remove deleted subtransactions
    bd[["data"]][["budget"]][["subtransactions"]] <-
      purrr::discard(bd[["data"]][["budget"]][["subtransactions"]], ~{.x[["deleted"]]})
    # Remove deleted scheduled_transactions
    bd[["data"]][["budget"]][["scheduled_transactions"]] <-
      purrr::discard(bd[["data"]][["budget"]][["scheduled_transactions"]],
                     ~{.x[["deleted"]]})
    # Remove deleted scheduled_subtransactions
    bd[["data"]][["budget"]][["scheduled_subtransactions"]] <-
      purrr::discard(bd[["data"]][["budget"]][["scheduled_subtransactions"]],
                     ~{.x[["deleted"]]})
  }

# TODO: Deal with closed data ---------------------------------------------
if (remove_closed == TRUE){
  # Get list of accounts to be deleted
  del_acct_list <- purrr::map(bd[["data"]][["budget"]][["accounts"]],
                              ~{ifelse(.x[["closed"]] == TRUE,
                                       .x[["id"]],
                                       "Open")})
  del_acct_list <- as.character(purrr::discard(del_acct_list, ~{.x == "Open"}))

  # Get a list of transactions to be deleted
  del_trans_list <- purrr::map(bd[["data"]][["budget"]][["transactions"]],
                               ~{ifelse(.x[["account_id"]] %in% del_acct_list,
                                        .x[["id"]], "Open")})
  del_trans_list <- as.character(purrr::discard(del_trans_list, ~{.x == "Open"}))

  # Get a list of subtransactions to delete
  del_subtrans_list <- purrr::map(bd[['data']][['budget']][['subtransactions']],
                                  ~{ifelse(.x[["transaction_id"]] %in% del_trans_list,
                                           .x[['id']], 'Open')})
  del_subtrans_list <- as.character(purrr::discard(del_subtrans_list, ~(.x == "Open")))

}

# TODO: Deal with off budget data -----------------------------------------


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

  # Convert category budgeted, activity, and balance to actual units
  bd[["data"]][["budget"]][["categories"]] <-
    purrr::map(bd[["data"]][["budget"]][["categories"]], ~{
      .x[["budgeted"]] <- .x[["budgeted"]]/1000
      .x[["activity"]] <- .x[["activity"]]/1000
      .x[["balance"]] <- .x[["balance"]]/1000
      return(.x)
    })

  # Convert months income, budgeted, activity, and to_be_budgeted to actual units
  bd[["data"]][["budget"]][["months"]] <-
    purrr::map(bd[["data"]][["budget"]][["months"]], ~{
      .x[["income"]] <- .x[["income"]]/1000
      .x[["budgeted"]] <- .x[["budgeted"]]/1000
      .x[["activity"]] <- .x[["activity"]]/1000
      .x[["to_be_budgeted"]] <- .x[["to_be_budgeted"]]/1000
      return(.x)
    })

  # Convert transactions amount to actual units
  bd[["data"]][["budget"]][["transactions"]] <-
    purrr::map(bd[["data"]][["budget"]][["transactions"]], ~{
      .x[["amount"]] <- .x[["amount"]]/1000
      return(.x)
    })

  # Convert subtransactions amount to actual units
  bd[["data"]][["budget"]][["subtransactions"]] <-
    purrr::map(bd[["data"]][["budget"]][["subtransactions"]], ~{
      .x[["amount"]] <- .x[["amount"]]/1000
      return(.x)
    })

  # Convert scheduled transactions amount to actual units
  bd[["data"]][["budget"]][["scheduled_transactions"]] <-
    purrr::map(bd[["data"]][["budget"]][["scheduled_transactions"]], ~{
      .x[["amount"]] <- .x[["amount"]]/1000
      return(.x)
    })

  # Convert scheduled subtransactions amount to actual units
  bd[["data"]][["budget"]][["scheduled_subtransactions"]] <-
    purrr::map(bd[["data"]][["budget"]][["scheduled_subtransactions"]], ~{
      .x[["amount"]] <- .x[["amount"]]/1000
      return(.x)
    })

  # Return the data
  return(bd)
}

#' Print Budget data
#'
#' Prints a summary of the budget_data object including key budget, account,
#' payee, category, and transaction summary information.
#'
#' @param bd a budget_data object
#'
#' @return
#' @export
#'
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
  active_accounts <- purrr::keep(bd[["data"]][["budget"]][["accounts"]],
                                 function(.x) {return (!.x[["closed"]])})
  cat(paste0("\n# of Active Accounts: ", length(active_accounts)))
  on_budget_accounts <- purrr::keep(bd[["data"]][["budget"]][["accounts"]],
                                    function(.x) {return (.x[["on_budget"]] & !.x[["closed"]])})
  cat(paste0("\n# of Active On Budget Accounts: ", length(on_budget_accounts)))

  # Print Payee information
  cat("\n\nPayee Summary\n=============================================")
  cat(paste0("\n# of Payees: ", length(bd[["data"]][["budget"]][["payees"]])))
  active_payees <- purrr::keep(bd[["data"]][["budget"]][["payees"]],
                               function(.x) {return (!.x[["deleted"]])})
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
#' Prints a summary of the budget_data object including key budget, account,
#' payee, category, and transaction summary information.
#'
#' @param bd a budget_data object
#'
#' @return
#' @export
#'
summary.budget_data <- function(bd){
  print(bd)
}


#' ynab_get_account_data
#'
#' This takes a budget data object as an argument and returns all of the the
#' transactional account data for all accounts in that budget. Depending on the
#' amount of data, this can take some time.
#'
#' @param bd a budget_data object
#'
#' @return
#' @export
#'
ynab_get_account_data <- function(bd){
  # Fetch the raw account data from the budget object
suppressWarnings(
  ad <- purrr::map_df(bd[["data"]][["budget"]][["transactions"]], ~{
    df <- data.frame(
      id = .x[["id"]],
      date = .x[["date"]],
      amount = .x[["amount"]],
      memo = ifelse(is.null(.x[["memo"]]), "", .x[["memo"]]),
      cleared = .x[["cleared"]],
      approved = .x[["approved"]],
      flag_color = ifelse(is.null(.x[["flag_color"]]), "", .x[["flag_color"]]),
      account_id = .x[["account_id"]],
      payee_id = .x[["payee_id"]],
      category_id = ifelse(is.null(.x[["category_id"]]), "", .x[["category_id"]]),
      transfer_account_id = ifelse(is.null(.x[["transfer_account_id"]]), "",
                                   .x[["transfer_account_id"]]),
      transfer_transaction_id = ifelse(is.null(.x[["transfer_transaction_id"]]),
                                       "", .x[["transfer_transaction_id"]]),
      matched_transaction_id = ifelse(is.null(.x[["matched_transaction_id"]]),
                                      "", .x[["matched_transaction_id"]]),
      import_id = ifelse(is.null(.x[["import_id"]]), "", .x[["import_id"]]),
      deleted = ifelse(is.null(.x[["deleted"]]), "", .x[["deleted"]])
    )

# TODO: Fetch and Merge Account Meta data ---------------------------------------

# TODO: Deal with sub transactions ----------------------------------------



    return(df)
  }))

  return(ad)
}
