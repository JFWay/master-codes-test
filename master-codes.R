#packages
library(dplyr)
library(RSQLite)
library(odbc)
library(DBI)
library(dbplyr)
library(openxlsx)
library(tidyr)

#load master codes sheets to export to SQL
tables <- read.xlsx("//universityofexeteruk.sharepoint.com@SSL/DavWWWRoot/sites/ResPI/analysis/Team Information/Source Data/Mappings/master-codes.xlsx",
                    sheet = "tables")

codes <- read.xlsx("//universityofexeteruk.sharepoint.com@SSL/DavWWWRoot/sites/ResPI/analysis/Team Information/Source Data/Mappings/master-codes.xlsx",
                    sheet = "codes",
                   cols = 1:5)

maps <- read.xlsx("//universityofexeteruk.sharepoint.com@SSL/DavWWWRoot/sites/ResPI/analysis/Team Information/Source Data/Mappings/master-codes.xlsx",
                    sheet = "maps")

#check key constraints

#tables - NOT NULL
isTRUE(is.na(tables))

#tables - tblcode primary key
isTRUE(length(tables$tbl_code) == length(unique(tables[,1])))

#codes - ref_code & table code primary key
comb_test <- length(distinct(codes[,c("ref_code", "tbl_code")]))

isTRUE(length(codes$ref_code) == length(comb_test$ref_code))

#codes - tbl_code foreign key
codes_fk <- codes %>%
  filter(tbl_code %in% tables$tbl_code)

isTRUE(length(codes_fk$tbl_code) == length(codes$tbl_code))

#codes - parent foreign keys
codes_parent_fk_1 <- distinct(codes[,c("parent_code", "parent_tbl")]) %>%
  filter(!is.na(parent_code),
         parent_code %in% codes$ref_code)
codes_parent_fk_2 <- distinct(codes[,c("parent_code", "parent_tbl")]) %>%
  filter(!is.na(parent_code))

isTRUE(length(codes_parent_fk_1) == length(codes_parent_fk_2))

#maps - foreign key ref/tbl code 1
maps_code_1_fk_1 <- distinct(maps[,c("ref_code_1", "tbl_code_1")]) %>%
  filter(!is.na(ref_code_1),
         ref_code_1 %in% codes$ref_code & tbl_code_1 %in% codes$tbl_code)
maps_code_1_fk_2 <- distinct(maps[,c("ref_code_1", "tbl_code_1")]) %>%
  filter(!is.na(ref_code_1))

isTRUE(length(maps_code_1_fk_1) == length(maps_code_1_fk_2))

#maps - foreign key ref/tbl code 2
maps_code_2_fk_1 <- distinct(maps[,c("ref_code_2", "tbl_code_2")]) %>%
  filter(!is.na(ref_code_2),
         ref_code_2 %in% codes$ref_code & tbl_code_2 %in% codes$tbl_code)
maps_code_2_fk_2 <- distinct(maps[,c("ref_code_2", "tbl_code_2")]) %>%
  filter(!is.na(ref_code_2))

isTRUE(length(maps_code_2_fk_1) == length(maps_code_2_fk_2))


#connect to database
db <-
  file.path(
    "C:", "Users", "jfw213", "OneDrive - University of Exeter",
    "SQL Test", "SQL Test - R.db"
  )

con <- dbConnect(RSQLite::SQLite(), db)

#write the new tables to the database
dbWriteTable(con, "tables", tables, overwrite = TRUE)
dbWriteTable(con, "codes", codes, overwrite = TRUE)
dbWriteTable(con, "maps", maps, overwrite = TRUE)

dbDisconnect(con)
