library(vroom)
library(data.table)
library(tidyverse)

# col_types <- list(
#   id = col_character(),
#   type = col_character(),
#   number = col_character(),
#   country = col_character(),
#   date = col_date("%Y-%m-%d"),
#   abstract = col_character(),
#   title = col_character(),
#   kind = col_character(),
#   num_claims = col_double(),
#   filename = col_character(),
#   withdrawn = col_double()
# )
# 
# patent_tbl <- vroom(
#   file       = "data-science/00_data/patent.tsv", 
#   delim      = "\t", 
#   col_types  = col_types,
#   na         = c("", "NA", "NULL")
# )

col_types <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "data-science/00_data/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "data-science/00_data/patent_assignee.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
# 
# col_types <- list(
#   uuid = col_character(),
#   patent_id = col_character(),
#   mainclass_id = col_character(),
#   sublass_id = col_character(),
#   sequence = col_integer()
#)
# 
# uspc_tbl <- vroom(
#   file       = "data-science/00_data/uspc.tsv", 
#   delim      = "\t", 
#   col_types  = col_types,
#   na         = c("", "NA", "NULL")
# )

