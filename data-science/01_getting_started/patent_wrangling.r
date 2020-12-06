library(vroom)
library(data.table)
library(tidyverse)

col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "data-science/00_data/patent.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_tbl <- select(patent_tbl, id, date)

tic()

patent_tbl <- patent_tbl %>% separate(col = date,
                                      into = c("year", "month", "day"),
                                      sep = "-",
                                      remove = TRUE) %>%

                            mutate(year = as.numeric(year)) %>%

                            filter(year == 2019)


toc()

col_types <- list(
  id = col_character(),
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
  assignee_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "data-science/00_data/patent_assignee.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

setDT(assignee_tbl)
setDT(patent_assignee_tbl)

setnames(assignee_tbl, "id", "assignee_id")

combined_data_tbl <- merge(x=assignee_tbl, y= patent_assignee_tbl, by = "assignee_id")


combined_data_tbl <- combined_data_tbl %>% filter(patent_id %in% patent_tbl$id)

summed_patents_tbl <- combined_data_tbl[is.na(name_first), .N, by=organization]

summed_patents_tbl <- arrange(summed_patents_tbl, desc(summed_patents_tbl$N))

