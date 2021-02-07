YNABR - A ‘You Need a Budget’ API client for R
================

This is the beginnings of an API client for the
[YNAB](http://www.youneedabudget.com) online budget software. This is
not even in “alpha” yet, as there are many function to adjust and add.
The first stage of this project is to make it easy to pull all of your
budget and bank account data from the YNAB API (v1) into R so that you
can analyze it. This package tries to be tidy verse friendly, returning
data frames where ever possible.

It is currently functional to pull a dump of your data of your bank
transactional data. Follow the steps below to get set up:

### Installation

1.  Install the devtools package it you do not already have it
    installed.

``` r
install.packages("devtools")
```

1.  Install the ynabr package using devtools

``` r
devtools::install_github("joxborrow/ynabr")
```

### Basic Usage

#### Download Budget Data

1.  Setup developer access to YNAB and obtain an personal access token.
    This can be done at the following address
    <https://api.youneedabudget.com/#personal-access-tokens>. The access
    token is a 64 bit long alpha numeric that is required to download
    data.

2.  Set the personal access token for usage.

-   Preferred Method: The preferred method of setting the personal
    access token is to set the environment variable YNAB\_TOKEN and then
    executing the `ynab_set_token()` function.

``` r
Sys.setenv(YNAB_TOKEN = "<ynab token>")
ynab_set_token()
```

-   Not recommended: The personal token may also be passed directly to
    the `ynab_set_token()` function as shown below. Note that having a
    token directly in your code is a bad security practice.

``` r
ynab_set_token("<ynab token>")
```

1.  List the available budgets available budgets using the
    `ynab_list_budgets()` function. This returns and prints a data frame
    of the available budgets that can be downloaded.

``` r
ynab_list_budgets()
```

1.  Using either the name or budget id printed from the
    `ynab_list_budgets` you can download all of the budgets associated
    data. This returns a object of class `"budget_data"` that is used as
    the input for other functions in the package. It is an extensive
    list containing all budget and account data downloaded from YNAB.

``` r
bd <- ynab_get_budget("<budget name or id>")
```

#### Fetch Account Data

Once you have downloaded a budget and assigned it to a variable, you can
then extract account information for all account using the
`ynab_get_account_data()` function.

``` r
ac <- ynab_get_account_data(bd)
```

Note that this downloads account data in your YNAB account. This
includes reconciled, cleared, and uncleared transactions. This will not
necessarily match data against your true bank account as uncleared
transactions, and differing dates may be different.
