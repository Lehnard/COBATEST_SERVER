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
library(openxlsx)
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
# fwrite(cobatest, "out/cobatest_20231031.csv")
# write.xlsx(cobatest, "out/cobatest_20231102.xlsx")

## cobatest_export   ####
# --------------------- #
cobatest_export <- "export_cobatest"
cobatest_export <- dbReadTable(con, cobatest_export)
cobatest_export <- setDT(cobatest_export)
# fwrite(cobatest_export, "out/cobatest_export_20231031.csv")
# write.xlsx(cobatest_export, "out/cobatest_export_20231102.xlsx")


## hg_centros        ####
# --------------------- #
hg_centro <- "hg_centro"
hg_centro <- dbReadTable(con, hg_centro)
hg_centro <- setDT(hg_centro)
# fwrite(hg_centro, "out/hg_centro.csv")
# write.xlsx(hg_centro, "out/cobatest_export_20231102.xlsx")


## ext_log_entries        ####
# -------------------------- #
ext_log_entries <- "ext_log_entries"
ext_log_entries <- dbReadTable(con, ext_log_entries)
ext_log_entries <- setDT(ext_log_entries)


# _______________________________________________________________________ ####
# -------------------------------------------------------------------------- #
# COBATEST                                                                ####
# -------------------------------------------------------------------------- #

## Variables. 
cat(paste(colnames(cobatest), collapse= '\n'))

numeric_vars <- c("hivtest_data2_d", "hivtest_data2_m", "hivtest_data2_y")
categoric_vars <- c("anulado_sn","usuario_id","centro_id","hivtest_namecbvct",
                    "hivtest_testingsite", "hivtest_gender", "hivtest_nob",
                    "hivtest_nos", "hivtest_initiallt", "hivtest_gender2",
                    "hivtest_foreignnational","hivtest_country", 
                    "hivtest_residenttourist", "hivtest_foreignStudent",
                    "hivtest_asylumSeekingMigrant","hivtest_undocumentedMigrant",
                    "hivtest_foreignWorker","hivtest_refugee",
                    "hivtest_nationalityHolders","hivtest_resident","hivtest_accessHealthCare", 
                    "hivtest_reasons01", "hivtest_reason0101","hivtest_reason0102",
                    "hivtest_reason0103","hivtest_reason0104","hivtest_reason0105",
                    "hivtest_reason0106","hivtest_reason0107","hivtest_reason0108", 
                    "hivtest_reasons02", "hivtest_reason0201","hivtest_reason0202",
                    "hivtest_reason0203","hivtest_reason0204","hivtest_reason0205",
                    "hivtest_reason0206","hivtest_reason0207","hivtest_reasons03",
                    "hivtest_reasons04","hivtest_reasons05", "hivtest_reasonsb01",
                    "hivtest_reasonsb02","hivtest_reasonsb03","hivtest_reasonsb04",
                    "hivtest_reasonsb05", "hivtest_yndk01")

id_vars <- c("cobatest_id" )
text_vars <- c("hivtest_citycbvct","hivtest_whichOtherForeign","hivtest_municipality", 
               "hivtest_whichOtherHCare","hivtest_reason01text","hivtest_reason02text", 
               "hivtest_textothers","hivtest_reasonsbtext")
date_vars <- c()
date_vars <- c("dtcrea", "hivtest_data1", "hivtest_data3")



source("src/utils/describe.R")

descr <- describe(data_table = cobatest, 
                  numeric_vars = numeric_vars, 
                  categoric_vars = categoric_vars, 
                  text_vars = text_vars, 
                  date_vars = date_vars,
                  id_vars = id_vars, 
                  n_top_cat = 20,
                  n_top_id = 10)




cobatest[, .N, .(hivtest_gender2, hivtest_gender)][order(as.numeric(hivtest_gender2))]

## .N de cada variable
cobatest[, .N, cobatest_id][order(as.numeric(cobatest_id))]
cobatest[, .N, dtcrea][order(-dtcrea)]
cobatest[, .N, usuario_id][order(as.numeric(usuario_id))]
cobatest[, .N, anulado_sn][order(-N)]
cobatest[, .N, centro_id][order(as.numeric(centro_id))]     # cat(paste(sort(unique(as.numeric(cobatest$centro_id))), collapse = '\n'))
cobatest[, .N, hivtest_namecbvct][order(-N)]                # cobatest[, .N, .(centro_id, hivtest_namecbvct)][order(as.numeric(centro_id))]
cobatest[, .N, hivtest_citycbvct][order(-N)]
cobatest[, .N, hivtest_testingsite][order(as.numeric(hivtest_testingsite))]
cobatest[, .N, hivtest_data1][order(as.Date(hivtest_data1))]
cobatest[, .N, hivtest_centre][order(-N)]
cobatest[, .N, hivtest_gender][order(as.numeric(hivtest_gender))]
cobatest[, .N, hivtest_data2_d][order(as.numeric(hivtest_data2_d))]
cobatest[, .N, hivtest_data2_m][order(as.numeric(hivtest_data2_m))]
cobatest[, .N, hivtest_data2_y][order(as.numeric(hivtest_data2_y))]
cobatest[, .N, hivtest_nob][order(-N)]
cobatest[, .N, hivtest_nos][order(-N)]
cobatest[, .N, hivtest_initiallt][order(-N)]
cobatest[, .N, hivtest_gender2][order(as.numeric(hivtest_gender2))]
cobatest[, .N, hivtest_data3][order(-N)]
cobatest[, .N, hivtest_foreignnational][order(as.numeric(hivtest_foreignnational))]
cobatest[, .N, hivtest_country][order(as.numeric(hivtest_country))]
cobatest[, .N, hivtest_enter4][order(-N)]
cobatest[, .N, hivtest_residenttourist][order(-N)]
cobatest[, .N, hivtest_foreignStudent][order(-N)]
cobatest[, .N, hivtest_asylumSeekingMigrant][order(-N)]
cobatest[, .N, hivtest_undocumentedMigrant][order(-N)]
cobatest[, .N, hivtest_foreignWorker][order(-N)]
cobatest[, .N, hivtest_refugee][order(-N)]
cobatest[, .N, hivtest_nationalityHolders][order(-N)]
cobatest[, .N, hivtest_resident][order(-N)]
cobatest[, .N, hivtest_whichOtherForeign][order(-N)]
cobatest[, .N, hivtest_municipality][order(-N)]
cobatest[, .N, hivtest_accessHealthCare][order(-N)]
cobatest[, .N, hivtest_whichOtherHCare][order(-N)]
cobatest[, .N, hivtest_reasons01][order(-N)]
cobatest[, .N, hivtest_reason0101][order(-N)]
cobatest[, .N, hivtest_reason0102][order(-N)]
cobatest[, .N, hivtest_reason0103][order(-N)]
cobatest[, .N, hivtest_reason0104][order(-N)]
cobatest[, .N, hivtest_reason0105][order(-N)]
cobatest[, .N, hivtest_reason0106][order(-N)]
cobatest[, .N, hivtest_reason0107][order(-N)]
cobatest[, .N, hivtest_reason0108][order(-N)]
cobatest[, .N, hivtest_reason01text][order(-N)]
cobatest[, .N, hivtest_reasons02][order(-N)]
cobatest[, .N, hivtest_reason0201][order(-N)]
cobatest[, .N, hivtest_reason0202][order(-N)]
cobatest[, .N, hivtest_reason0203][order(-N)]
cobatest[, .N, hivtest_reason0204][order(-N)]
cobatest[, .N, hivtest_reason0205][order(-N)]
cobatest[, .N, hivtest_reason0206][order(-N)]
cobatest[, .N, hivtest_reason0207][order(-N)]
cobatest[, .N, hivtest_reason02text][order(-N)]
cobatest[, .N, hivtest_reasons03][order(-N)]
cobatest[, .N, hivtest_reasons04][order(-N)]
cobatest[, .N, hivtest_reasons05][order(-N)]
cobatest[, .N, hivtest_textothers][order(-N)]
cobatest[, .N, hivtest_reasonsb01][order(-N)]
cobatest[, .N, hivtest_reasonsb02][order(-N)]
cobatest[, .N, hivtest_reasonsb03][order(-N)]
cobatest[, .N, hivtest_reasonsb04][order(-N)]
cobatest[, .N, hivtest_reasonsb05][order(-N)]
cobatest[, .N, hivtest_reasonsbtext][order(-N)]
cobatest[, .N, hivtest_yndk01][order(-N)]
cobatest[, .N, hivtest_hivTestP][order(-N)]
cobatest[, .N, hivtest_data4][order(-N)]
cobatest[, .N, hivtest_data4_d][order(-N)]
cobatest[, .N, hivtest_data4_m][order(-N)]
cobatest[, .N, hivtest_data4_y][order(-N)]
cobatest[, .N, hivtest_yndk03][order(-N)]
cobatest[, .N, hivtest_yndk02][order(-N)]
cobatest[, .N, hivtest_sxbh][order(-N)]
cobatest[, .N, hivtest_yndk04][order(-N)]
cobatest[, .N, hivtest_yndk05][order(-N)]
cobatest[, .N, hivtest_yndk06][order(-N)]
cobatest[, .N, hivtest_yndk07][order(-N)]
cobatest[, .N, hivtest_yndk08][order(-N)]
cobatest[, .N, hivtest_yndk09][order(-N)]
cobatest[, .N, hivtest_yndk10][order(-N)]
cobatest[, .N, hivtest_yndk11][order(-N)]
cobatest[, .N, hivtest_yndk12][order(-N)]
cobatest[, .N, hivtest_lastTime][order(-N)]
cobatest[, .N, hivtest_data5][order(-N)]
cobatest[, .N, hivtest_data5_d][order(-N)]
cobatest[, .N, hivtest_data5_m][order(-N)]
cobatest[, .N, hivtest_data5_y][order(-N)]
cobatest[, .N, hivtest_yndk13][order(-N)]
cobatest[, .N, hivtest_yndk14][order(-N)]
cobatest[, .N, hivtest_yndk15][order(-N)]
cobatest[, .N, hivtest_otherAddictiveSubstances][order(-N)]
cobatest[, .N, hivtest_whichOtherSubstances][order(-N)]
cobatest[, .N, hivtest_screeningHIVTest][order(-N)]
cobatest[, .N, hivtest_data6][order(-N)]
cobatest[, .N, hivtest_data6_d][order(-N)]
cobatest[, .N, hivtest_data6_m][order(-N)]
cobatest[, .N, hivtest_data6_y][order(-N)]
cobatest[, .N, hivtest_data6_text][order(-N)]
cobatest[, .N, hivtest_totu][order(-N)]
cobatest[, .N, hivtest_yndk32][order(-N)]
cobatest[, .N, hivtest_HIVExtraTestType][order(-N)]
cobatest[, .N, hivtest_HIVExtraTestResult][order(-N)]
cobatest[, .N, hivtest_str][order(-N)]
cobatest[, .N, hivtest_yndk16][order(-N)]
cobatest[, .N, hivtest_data7][order(-N)]
cobatest[, .N, hivtest_data7_d][order(-N)]
cobatest[, .N, hivtest_data7_m][order(-N)]
cobatest[, .N, hivtest_data7_y][order(-N)]
cobatest[, .N, hivtest_data7_text][order(-N)]
cobatest[, .N, hivtest_yndk17][order(-N)]
cobatest[, .N, hivtest_yndk18][order(-N)]
cobatest[, .N, hivtest_data8][order(-N)]
cobatest[, .N, hivtest_data8_d][order(-N)]
cobatest[, .N, hivtest_data8_m][order(-N)]
cobatest[, .N, hivtest_data8_y][order(-N)]
cobatest[, .N, hivtest_data8_text][order(-N)]
cobatest[, .N, hivtest_ctr][order(-N)]
cobatest[, .N, hivtest_yndk19][order(-N)]
cobatest[, .N, hivtest_data9][order(-N)]
cobatest[, .N, hivtest_data9_d][order(-N)]
cobatest[, .N, hivtest_data9_m][order(-N)]
cobatest[, .N, hivtest_data9_y][order(-N)]
cobatest[, .N, hivtest_data9_text][order(-N)]
cobatest[, .N, hivtest_yndk20][order(-N)]
cobatest[, .N, hivtest_data10][order(-N)]
cobatest[, .N, hivtest_data10_d][order(-N)]
cobatest[, .N, hivtest_data10_m][order(-N)]
cobatest[, .N, hivtest_data10_y][order(-N)]
cobatest[, .N, hivtest_data10_text][order(-N)]
cobatest[, .N, hivtest_cd4][order(-N)]
cobatest[, .N, hivtest_data11][order(-N)]
cobatest[, .N, hivtest_data11_d][order(-N)]
cobatest[, .N, hivtest_data11_m][order(-N)]
cobatest[, .N, hivtest_data11_y][order(-N)]
cobatest[, .N, hivtest_data11_text][order(-N)]
cobatest[, .N, hivtest_yndk29][order(-N)]
cobatest[, .N, hivtest_data18][order(-N)]
cobatest[, .N, hivtest_data18_d][order(-N)]
cobatest[, .N, hivtest_data18_m][order(-N)]
cobatest[, .N, hivtest_data18_y][order(-N)]
cobatest[, .N, hivtest_syphilisTestP][order(-N)]
cobatest[, .N, hivtest_data18_text][order(-N)]
cobatest[, .N, hivtest_yndk21][order(-N)]
cobatest[, .N, hivtest_data12][order(-N)]
cobatest[, .N, hivtest_data12_d][order(-N)]
cobatest[, .N, hivtest_data12_m][order(-N)]
cobatest[, .N, hivtest_data12_y][order(-N)]
cobatest[, .N, hivtest_data12_text][order(-N)]
cobatest[, .N, hivtest_yndk22][order(-N)]
cobatest[, .N, hivtest_data13][order(-N)]
cobatest[, .N, hivtest_data13_d][order(-N)]
cobatest[, .N, hivtest_data13_m][order(-N)]
cobatest[, .N, hivtest_data13_y][order(-N)]
cobatest[, .N, hivtest_data13_text][order(-N)]
cobatest[, .N, hivtest_totus][order(-N)]
cobatest[, .N, hivtest_rtr][order(-N)]
cobatest[, .N, hivtest_yndk23][order(-N)]
cobatest[, .N, hivtest_data14][order(-N)]
cobatest[, .N, hivtest_data14_d][order(-N)]
cobatest[, .N, hivtest_data14_m][order(-N)]
cobatest[, .N, hivtest_data14_y][order(-N)]
cobatest[, .N, hivtest_data14_text][order(-N)]
cobatest[, .N, hivtest_sd][order(-N)]
cobatest[, .N, hivtest_yndk24][order(-N)]
cobatest[, .N, hivtest_data15][order(-N)]
cobatest[, .N, hivtest_data15_d][order(-N)]
cobatest[, .N, hivtest_data15_m][order(-N)]
cobatest[, .N, hivtest_data15_y][order(-N)]
cobatest[, .N, hivtest_data15_text][order(-N)]
cobatest[, .N, hivtest_yndk25][order(-N)]
cobatest[, .N, hivtest_data16][order(-N)]
cobatest[, .N, hivtest_data16_d][order(-N)]
cobatest[, .N, hivtest_data16_m][order(-N)]
cobatest[, .N, hivtest_data16_y][order(-N)]
cobatest[, .N, hivtest_data16_text][order(-N)]
cobatest[, .N, hivtest_tothcv][order(-N)]
cobatest[, .N, hivtest_rtrhcv][order(-N)]
cobatest[, .N, hivtest_yndk26][order(-N)]
cobatest[, .N, hivtest_data17][order(-N)]
cobatest[, .N, hivtest_data17_d][order(-N)]
cobatest[, .N, hivtest_data17_m][order(-N)]
cobatest[, .N, hivtest_data17_y][order(-N)]
cobatest[, .N, hivtest_data17_text][order(-N)]
cobatest[, .N, hivtest_hcvd][order(-N)]
cobatest[, .N, hivtest_yndk27][order(-N)]
cobatest[, .N, hivtest_yndk28][order(-N)]
cobatest[, .N, hivtest_yndk30][order(-N)]
cobatest[, .N, hivtest_hcvTestPerformed][order(-N)]
cobatest[, .N, hivtest_data19][order(-N)]
cobatest[, .N, hivtest_data19_d][order(-N)]
cobatest[, .N, hivtest_data19_m][order(-N)]
cobatest[, .N, hivtest_data19_y][order(-N)]
cobatest[, .N, hivtest_data19_text][order(-N)]
cobatest[, .N, hivtest_yndk31][order(-N)]
cobatest[, .N, hivtest_yndk33][order(-N)]
cobatest[, .N, hivtest_yndk34][order(-N)]
cobatest[, .N, hivtest_comment][order(-N)]
cobatest[, .N, hivtest_prep][order(-N)]
cobatest[, .N, hivtest_prep_taken][order(-N)]
cobatest[, .N, hivtest_prep_interested][order(-N)]
cobatest[, .N, hivtest_prep_why01][order(-N)]
cobatest[, .N, hivtest_prep_why02][order(-N)]
cobatest[, .N, hivtest_prep_why03][order(-N)]
cobatest[, .N, hivtest_prep_why04][order(-N)]
cobatest[, .N, hivtest_prep_why05][order(-N)]
cobatest[, .N, hivtest_prep_why06][order(-N)]
cobatest[, .N, hivtest_prep_why06text][order(-N)]
cobatest[, .N, hivtest_chemsex_drugs][order(-N)]
cobatest[, .N, hivtest_chemsex_which_drugs01][order(-N)]
cobatest[, .N, hivtest_chemsex_which_drugs02][order(-N)]
cobatest[, .N, hivtest_chemsex_which_drugs03][order(-N)]
cobatest[, .N, hivtest_chemsex_which_drugs04][order(-N)]
cobatest[, .N, hivtest_chemsex_which_drugs05][order(-N)]
cobatest[, .N, hivtest_chemsex_inject_drug][order(-N)]
cobatest[, .N, hivtest_belgium_type][order(-N)]
cobatest[, .N, hivtest_belgium_status][order(-N)]
cobatest[, .N, hivtest_belgium_status_text][order(-N)]
cobatest[, .N, hivtest_belgium_have_residence][order(-N)]
cobatest[, .N, hivtest_belgium_describe_ethnicity][order(-N)]
cobatest[, .N, hivtest_belgium_describe_ethnicity_text][order(-N)]
cobatest[, .N, hivtest_tispr_q1][order(-N)]
cobatest[, .N, hivtest_tispr_q1_puntuacio][order(-N)]
cobatest[, .N, hivtest_tispr_q2][order(-N)]
cobatest[, .N, hivtest_tispr_q2_puntuacio][order(-N)]
cobatest[, .N, hivtest_tispr_q3][order(-N)]
cobatest[, .N, hivtest_tispr_q3_puntuacio][order(-N)]
cobatest[, .N, hivtest_tispr_q4][order(-N)]
cobatest[, .N, hivtest_tispr_q4_puntuacio][order(-N)]
cobatest[, .N, hivtest_tispr_q5][order(-N)]
cobatest[, .N, hivtest_tispr_q5_puntuacio][order(-N)]
cobatest[, .N, hivtest_tispr_q6][order(-N)]
cobatest[, .N, hivtest_tispr_q6_puntuacio][order(-N)]
cobatest[, .N, hivtest_tispr_total_puntuacio][order(-N)]
cobatest[, .N, hivtest_tispr_q7][order(-N)]
cobatest[, .N, hivtest_tispr_q8][order(-N)]
cobatest[, .N, hivtest_tispr_q9][order(-N)]
cobatest[, .N, hivtest_tispr_q10][order(-N)]
cobatest[, .N, hivtest_tispr_q10_why_text][order(-N)]
cobatest[, .N, hivtest_tispr_q11][order(-N)]
cobatest[, .N, hivtest_tispr_q11_why_text][order(-N)]
cobatest[, .N, migra_hivtest_data2][order(-N)]
cobatest[, .N, migra_hivtest_kp01][order(-N)]
cobatest[, .N, migra_hivtest_kp02][order(-N)]
cobatest[, .N, migra_hivtest_kp03][order(-N)]
cobatest[, .N, migra_hivtest_kp04][order(-N)]
cobatest[, .N, migra_hivtest_kp05][order(-N)]
cobatest[, .N, migra_hivtest_kp06][order(-N)]
cobatest[, .N, migra_hivtest_collector][order(-N)]
cobatest[, .N, migra_hivtest_testingsite_other][order(-N)]
cobatest[, .N, migra_hivtest_npendents][order(-N)]
cobatest[, .N, migra_hivtest_citycbvct2][order(-N)]
cobatest[, .N, migra_hivtest_searchfield][order(-N)]
cobatest[, .N, migra_hivtest_report][order(-N)]
cobatest[, .N, migra_hivtest_ehetest][order(-N)]
cobatest[, .N, migra_hivtest_fuptest][order(-N)]
cobatest[, .N, migra_hivtest_wcopiat][order(-N)]
cobatest[, .N, migra_hivtest_wesborrat][order(-N)]
cobatest[, .N, hivtest_rtrhcv_backup][order(-N)]
cobatest[, .N, hivtest_prueba_extra_vih_test][order(-N)]
cobatest[, .N, hivtest_prueba_extra_vih_test_tipo][order(-N)]
cobatest[, .N, hivtest_prueba_extra_vih_resultado][order(-N)]
cobatest[, .N, previous_sti_test][order(-N)]
cobatest[, .N, previous_sti_test_date][order(-N)]
cobatest[, .N, previous_sti_diagnosis][order(-N)]
cobatest[, .N, which_previous_sti_tp][order(-N)]
cobatest[, .N, which_previous_sti_gono][order(-N)]
cobatest[, .N, which_previous_sti_clam][order(-N)]
cobatest[, .N, which_previous_sti_herp][order(-N)]
cobatest[, .N, which_previous_sti_papi][order(-N)]
cobatest[, .N, which_previous_sti_slg][order(-N)]
cobatest[, .N, which_previous_sti_tv][order(-N)]
cobatest[, .N, which_previous_sti_other][order(-N)]
cobatest[, .N, which_other_previous_sti][order(-N)]
cobatest[, .N, number_sex_partners][order(-N)]
cobatest[, .N, new_sex_partner][order(-N)]
cobatest[, .N, sex_simultanious][order(-N)]
cobatest[, .N, sw_typology][order(-N)]
cobatest[, .N, sw_places][order(-N)]
cobatest[, .N, sw_years][order(-N)]
cobatest[, .N, sample_collected][order(-N)]
cobatest[, .N, vaginal_self_sample][order(-N)]
cobatest[, .N, rectal_self_sample][order(-N)]
cobatest[, .N, pharyngeal_self_sample][order(-N)]
cobatest[, .N, result_vaginal_self_sample][order(-N)]
cobatest[, .N, positive_clam_vaginal][order(-N)]
cobatest[, .N, positive_gono_vaginal][order(-N)]
cobatest[, .N, positive_myco_vaginal][order(-N)]
cobatest[, .N, positive_urea_vaginal][order(-N)]
cobatest[, .N, positive_trico_vaginal][order(-N)]
cobatest[, .N, positive_herp_vaginal][order(-N)]
cobatest[, .N, result_rectal_self_sample][order(-N)]
cobatest[, .N, positive_clam_rectal][order(-N)]
cobatest[, .N, positive_gono_rectal][order(-N)]
cobatest[, .N, positive_myco_rectal][order(-N)]
cobatest[, .N, positive_ureal_rectal][order(-N)]
cobatest[, .N, positive_urea_rectal][order(-N)]
cobatest[, .N, positive_trico_rectal][order(-N)]
cobatest[, .N, positive_herp_rectal][order(-N)]
cobatest[, .N, result_pharyngeal_self_sample][order(-N)]
cobatest[, .N, positive_clam_pharyngeal][order(-N)]
cobatest[, .N, positive_gono_pharyngeal][order(-N)]
cobatest[, .N, positive_myco_pharyngeal][order(-N)]
cobatest[, .N, positive_ureal_pharyngeal][order(-N)]
cobatest[, .N, positive_urea_pharyngeal][order(-N)]
cobatest[, .N, positive_trico_pharyngeal][order(-N)]
cobatest[, .N, positive_herp_pharyngeal][order(-N)]
cobatest[, .N, treatment][order(-N)]
cobatest[, .N, treatment_date][order(-N)]
cobatest[, .N, sti_comments][order(-N)]
cobatest[, .N, sample_collected_date][order(-N)]
cobatest[, .N, deriv_esp][order(-N)]
cobatest[, .N, its_previous_sti_test][order(-N)]
cobatest[, .N, its_previous_sti_test_date][order(-N)]
cobatest[, .N, its_previous_sti_diagnosis][order(-N)]
cobatest[, .N, its_which_previous_sti_tp][order(-N)]
cobatest[, .N, its_which_previous_sti_gono][order(-N)]
cobatest[, .N, its_which_previous_sti_clam][order(-N)]
cobatest[, .N, its_which_previous_sti_herp][order(-N)]
cobatest[, .N, its_which_previous_sti_papi][order(-N)]
cobatest[, .N, its_which_previous_sti_slg][order(-N)]
cobatest[, .N, its_which_previous_sti_tv][order(-N)]
cobatest[, .N, its_which_previous_sti_other][order(-N)]
cobatest[, .N, its_which_other_previous_sti][order(-N)]
cobatest[, .N, its_number_sex_partners][order(-N)]
cobatest[, .N, its_new_sex_partner][order(-N)]
cobatest[, .N, its_sex_simultanious][order(-N)]
cobatest[, .N, its_ct_test][order(-N)]
cobatest[, .N, its_ct_sample_date][order(-N)]
cobatest[, .N, its_ct_orina_sample][order(-N)]
cobatest[, .N, its_ct_genital_sample][order(-N)]
cobatest[, .N, its_ct_rectal_sample][order(-N)]
cobatest[, .N, its_ct_pharyngeal_sample][order(-N)]
cobatest[, .N, its_ct_test_type][order(-N)]
cobatest[, .N, its_ct_test_result][order(-N)]
cobatest[, .N, its_ct_treatment][order(-N)]
cobatest[, .N, its_ct_treatment_date][order(-N)]
cobatest[, .N, its_gono_test][order(-N)]
cobatest[, .N, its_gono_sample_date][order(-N)]
cobatest[, .N, its_gono_orina_sample][order(-N)]
cobatest[, .N, its_gono_genital_sample][order(-N)]
cobatest[, .N, its_gono_rectal_sample][order(-N)]
cobatest[, .N, its_gono_pharyngeal_sample][order(-N)]
cobatest[, .N, its_gono_test_type][order(-N)]
cobatest[, .N, its_gono_test_result][order(-N)]
cobatest[, .N, its_gono_treatment][order(-N)]
cobatest[, .N, its_gono_treatment_date][order(-N)]
cobatest[, .N, its_sti_comments][order(-N)]
cobatest[, .N, mon_relazione][order(-N)]
cobatest[, .N, num_de_partner12mes][order(-N)]
cobatest[, .N, liv_istruzione][order(-N)]
cobatest[, .N, sesso_gruppo][order(-N)]
cobatest[, .N, cond_sex_toys][order(-N)]
cobatest[, .N, fisting][order(-N)]
cobatest[, .N, ingoiare_sperma][order(-N)]
cobatest[, .N, sesso_penetrativo_con_hivpos][order(-N)]
cobatest[, .N, sesso_penetrativo_con_hivpos_si][order(-N)]
cobatest[, .N, altro_txt][order(-N)]
cobatest[, .N, evitsesso][order(-N)]


# _______________________________________________________________________ ####
# -------------------------------------------------------------------------- #
# COBATEST EXPORT                                                         ####
# -------------------------------------------------------------------------- #

## Variables. 
cat(paste(colnames(cobatest_export), collapse= '\n'))




# old ------------------------------------------------------------------------------

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
