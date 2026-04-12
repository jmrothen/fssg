## R CMD check results

0 errors | 0 warnings | 1 note

* There is one note due to the use of dplyr function calls where a column is specified in plain text instead of as a string
* Ex:  data %>% select(column) 

If needed, these can be rewritten. 
