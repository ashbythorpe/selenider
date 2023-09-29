## Resubmission
This is a resubmission responding to the review comments.

The response to the original submission stated:
"Please add a web reference for the API in the form <https:.....> to the description 
of the DESCRIPTION file with no space after 'https:' and angle brackets for auto-linking."

The package does not directly use a web API, instead wrapping RSelenium and chromote,
which themselves use web APIs. Many other packages do this without links (e.g. parsel
and shadowr).

The response also stated:
"Please do not modify the global environment (e.g. by using <<-) in your functions.
This is not allowed by the CRAN policies. e.g.: R/lazy_list.R"

The three places where `<<-` is used (R/lazy_list.R, in the counter() and state()
functions) do not allow `<<-` to modify the global environment (since `<<-` is
contained within a sub-function): the global environment is not modified.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
