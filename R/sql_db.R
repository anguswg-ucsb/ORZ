
con <- dbConnect(RMariaDB::MariaDB(), host = "localhost", port = 3306, username = "root", password = "Whiskey21!", dbname = "crms_hydrographic_db")
con
crms <- readr::read_csv("data/CRMS.csv")
chunks <- nrow(crms) / 10000
n_chunks <- length(chunks)
lapply(
  X = 1L:(n_chunks - 1),
  FUN = function(i) {
    dbAppendTable(con, value = crms[i:(i+1), ], table = "tablename")
  }
)
dbWriteTable(con, value = readr::read_csv("data/CRMS_Continuous_Hydrographic.csv",
                                                  progress = TRUE) %>%
               janitor::clean_names() %>% # snake_case the column headers
               dplyr::select(1:20, 43:46)  %>% # select relevant columns, removed wind variables
               rename(date = date_mm_dd_yyyy),
                  row.names = FALSE,
                  overwrite = TRUE,
                  name = "crms_hydrographic_table",
                  append = TRUE)
dbRemoveTable(con, name = "crms_hydrographic_table")
