library("rstudioapi")                                 # Load rstudioapi package
setwd(dirname(getActiveDocumentContext()$path))       # Set working directory to source file location

install.packages("tidyverse")
library("readr")
library("tidyverse")

# ------ import app ------
app<-read_csv("app.csv",col_types = cols (col_integer(), col_integer(), col_date(), col_double(), col_integer(), col_double()
                                          ,col_date(), col_character(), col_character(), col_integer(),col_logical(), col_character(), col_character(), col_double()))

glimpse(app)

#get duplicated id, if any
dupl_id<-app$id[duplicated(app$id)]
dupl_id
# -> 436 id which are duplicated, but we don't know how many times, nor if other columns are duplicated as well
dupl_values<-filter(app, id  %in%  dupl_id)
glimpse(dupl_values)
# -> it seems that only column income is different for each id

#get unique combination of id and income
unique(dupl_values[,c('id','income')])
# -> for each duplicated id there are 2 different values for income - 1 or other value
# -> we can assume that value which is not 1 is correct, therefor we will do filter accordingly

app<-filter(app, (id  %in%  dupl_id & income !=1) |  !(id  %in%  dupl_id))
(dupl_id<-app$id[duplicated(app$id)])
# ->no more duplicates
