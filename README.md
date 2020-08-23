README
================

## YNABR - A ‘You Need a Budget’ API client for R

This is the beginnings of an API client for the
[YNAB](http://www.youneedabudget.com) online budget software. This is
not even in “alpha” yet, as there are many function to adjust and add.
The first stage of this project is to make it easy to pull all of your
budget and bank account data from the YNAB API (v1) into R so that you
can analyze it.

It is currently functional to pull a dump of your data, though you must
have set up API access and obtained a security token directly from YNAB.
The basic workflow is

1)  Set your token with ynab\_set\_token() -The token can be held in an
    environment variable YNAB\_TOKEN and this function can be called
    with no argument -Or, the token can be used as the arguement to this
    function -This function must always be called before and other
    functions are called.
2)  List your available budgets with ynab\_list\_budgets()
3)  Pull your budget data of interest with ynab\_get\_budget(). Either
    the budget name or the budget id can be used as the argument to the
    function.
4)  Pull your account transactional data with ynab\_get\_account\_data.
