## RESET
rm(list= ls())
graphics.off() 
cat("\014")

## DESCRIPCIO:    Script de prova per poder escriure un dataset a SQL Server.
#
## VERSIÓ:        10-10-2023      
#
## FONT:          https://solutions.rstudio.com/db/r-packages/odbc/
#                 https://statisticsglobe.com/create-connect-database-r 
#                 https://rdbsql.rsquaredacademy.com/index.html


library(data.table)
library(odbc)
library(DBI)
#sort(unique(odbcListDrivers()[[1]]))


# -------------------------------------------------------------------------- #
# SET CONNECTION                                                          ####
# -------------------------------------------------------------------------- #

# Conexió al servidor ICO
driver    <- "SQL Server"
server    <- "icosrvceeiscat01.ico.scs.local"  
database  <- "COBATEST"        
uid       <- "jaceiton"      ## User ID
# cobatest    "GvqmXGhQqC*+"

con <- dbConnect(odbc(),
                 Driver = driver,
                 Server = server, 
                 Database = database,
                 UID = uid,
                 PWD = rstudioapi::askForPassword("Database Password"))

# help(DBI)
# dbGetInfo(con)
# dbGetQuery(con, "SELECT * FROM table;")                                     # QUERY table
# odbcListObjects(con)                                                        # Top level objects
# odbcListObjectTypes(con)                                                    # Database structure
# odbcListDataSources()                                                       # All data sources
# odbcListDrivers()                                                           # All drivers
# odbcListObjects(con, catalog=database, schema="dbo")                        # Tables in a schema
# odbcListColumns(con, catalog=database, schema="dbo", table= cobatest )      # List columns in a table


# _______________________________________________________________________ ####
# -------------------------------------------------------------------------- #
# READ TABLES                                                             ####
# -------------------------------------------------------------------------- #

## cobatest       ####
# ------------------ #
cobatest <- "cobatest"
cobatest <- dbReadTable(con, cobatest)
cobatest <- setDT(cobatest)


## cobatest_export   ####
# --------------------- #
cobatest_export <- "export_cobatest"
cobatest_export <- dbReadTable(con, cobatest_export)
cobatest_export <- setDT(cobatest_export)











colnames(cobatest)[grepl(x = colnames(cobatest), pattern = 'date')]
colnames(cobatest)[grepl(x = colnames(cobatest), pattern = 'data')]

summary(tbl[, .(date_last_time)])


tbl2 <- tbl[, .(user_id, cbvct_name, date_last_test, date_last_time)]

tbl[date_last_test == "", .N]
tbl2[date_last_time == "", .N]

tbl[, .N, hivtest_lastTime]
tbl[, .N, date_last_time]
tbl[, .N, date_last_test]



tbl[, .(hivtest_data1, hivtest_data4, hivtest_data4_d, hivtest_data4_m, hivtest_data4_y)]

tbl[, .N, hivtest_data2_y]


# Desconectem DDBB. 
dbDisconnect(con)
