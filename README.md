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

1)  Set your token with ynab\_set\_token()
2)  List your available budgets with ynab\_list\_budgets()
3)  Pull your budget data of interest with ynab\_get\_budget()
