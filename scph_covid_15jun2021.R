
#title: "scph_covid"
#author: "Hayden Hedman"
#date: "10/1/2020"

## LOAD PACKAGES  
pacman::p_load(dplyr, data.table, bit64, curl, tidyr, tidyverse, zoo, gsheet)

##(1) SUMMARY DATA
## SET WORKING DIRECTORY
setwd("C:/Users/hayde/Google Drive/Summit County - COVID-19/Database/Datawrapper")
## LOAD DATA
df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/cedrs_Summit.txt'); dim(df)
df <- as.data.frame(df)
## FILTER OUT DUPLICATE ENTRIES
df$unique_id <- as.integer(factor(with(df, paste(profileid, first_name, last_name, date_of_birth))))
df<- subset(df, !duplicated(unique_id))
###############################################################################################################
## LOAD HOSPITALIZATIONS DF

##SASMC HOSPITAL DATA FROM GOOGLE SHEETS
sasmc <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1cuJdqAzx_kh6v8zkmDlhsV9SDU7wSO-9QHH7grKGuwE/edit?usp=sharing')
sasmc <- as.data.frame(sasmc)
sasmc <- sasmc[,c("date","covid_pac","covid_pac_trans")]
hospitalized_sum <- sum(sasmc$covid_pac)
###############################################################################################################
elr_df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/elr_rollup_Summit.txt'); dim(df)
elr_df <- as.data.frame(elr_df)
##
TT_sum <- elr_df %>%
  filter(test_type == "PCR") %>% 
  select(dateadded, total_tests) %>% 
  distinct() %>% 
  mutate(sum = sum(total_tests))
TT_1 <- tail(TT_sum, 1)
TT = TT_1$sum
###############################################################################################################
## DAILY VALUES ENTERED
ob <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1oC3O0dfyyr1sAoPT6Et1Htdg4wwhIR8WictHTX7Fv_0/edit#gid=0')
ob <- as.data.frame(ob)
ob$Full_Date <- ob$`First Case / Primer Caso`
## CONVERT DATE
ob$Full_Date <- format(as.Date(ob$Full_Date, "%m/%d/%Y"), "%Y-%m-%d")
ob$Date <- ob$Full_Date
obg$Date <- format(as.Date(ob$Date, "%Y-%m-%d"), "%m-%Y")
ob$freq=1
## SUMMARIZE NUMBER OF OUTBREAKS BY M-YYYY
ob_summ <- data.frame(group_by(ob, Date) %>% 
                        summarize(outbreak_sum = sum(freq=="1")))
## OUTBREAK SUM
O = sum(ob_summ$outbreak_sum)
###############################################################################################################
## DEATH SUM
DA = 8
df.death <- subset(df, outcome=="Patient died")
df.death$count=1
DA = length(df.death$count)
###############################################################################################################
## SUMMARIZE DAILY REPORT
total_summ <- data.frame(group_by(df) %>% 
                           summarize(pos_confirm_sum = sum(casestatus=="confirmed"),
                                     prob_sum = sum(casestatus=="probable"),
                                     total_cases = sum(pos_confirm_sum, prob_sum),
                                     death_sum = sum(outcome=="Patient died"))); print(total_summ)

type_eng = c("Confirmed Positive Tests", "Probable Cases", "Total Cases", "Hospitalizations*", "Total Tested", "Outbreaks", "Deaths Among Cases")
type_esp = c("Pruebas positivas confirmadas", "Casos probables", "Total de casos", "Hospitalizaciones", "Total de pruebas", "Brotes", "Muertes entre casos")

value = c(total_summ$pos_confirm_sum, total_summ$prob_sum, total_summ$total_cases, hospitalized_sum, TT, O, DA)
summary_eng <- data.frame(type_eng, value)
summary_esp <- data.frame(type_esp, value)
## SUMMARY TABLE (ENG)
write.csv(summary_eng, row.names = F, "summary_table_eng.csv")
## SUMMARY TABLE (ESP)
write.csv(summary_esp, row.names = F, "summary_table_esp.csv")

##(2) PCR TEST PROPORTION AND NEGATIVE TEST COUNTS
## LOAD DATA
df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/elr_rollup_Summit.txt'); dim(df)
##############################################################################################
## FILTER DF OF ONLY PCR OUTCOMES
df <- as.data.frame(df)
df <- subset(df, test_type=="PCR"); dim(df)
##############################################################################################
## Summarize sum of  tests by dateadded
total_tests <- data.frame(group_by(df, dateadded) %>%
                            summarize(total_tested = sum(n_tests))); dim(total_tests)
##############################################################################################
## CREATE TEMP ID TO FILTER OUT DATES UP TO 2020-03-13
total_tests$sort_ID <- as.integer(factor(with(total_tests, paste(dateadded))))
## FILTER OUT DATES BEFORE 13-MARCH-2020
total_tests <- subset(total_tests, sort_ID >3); dim(total_tests)
total_tests <- arrange(total_tests, -sort_ID); head(total_tests)
##############################################################################################
## PULL OUT POSITIVE DF BUT FILTER BY DUPLICATED DATEADDED 
## THE SUM OF POSITIVE TESTS IS SAME ACCROSS CDPHE AND NON-CDPHE LAB TYPES (2X)
pos_tests <- df[!duplicated(df[3]),]; dim(pos_tests)

## PULL OUT MEANINGFUL VARIABLES
pos_tests <- pos_tests[,c("total_confirmed_cases", "county", "dateadded")]; dim(pos_tests)

## CREATE TEMP ID TO FILTER OUT DATES UP TO 2020-03-13
pos_tests$sort_ID <- as.integer(factor(with(pos_tests, paste(dateadded))))
## SORT OLDEST TO NEWEST M
pos_tests <- arrange(pos_tests, sort_ID); head(pos_tests)
## MAKE SURE 3/10 - 3/12 ARE REMOVED
pos_tests <- subset(pos_tests, sort_ID > 3)
## SORT BACK TO NEWEST TO OLDEST
pos_tests <- arrange(pos_tests, -sort_ID); head(pos_tests)
## REMOVE DATEADDED VARIABLE - THIS IS HERE JUST FOR QUALITY CONTROL
pos_tests$dateadded = NULL

##############################################################################################
## CBIND UP THE POSITIVE AND TOTAL TESTS
df2 <- cbind(total_tests, pos_tests); dim(df2)
## CORRECT FOR CDPHE DATA ENTRY ERRORS (SOMETIMES THEY HAVE 0 TOTAL & POS >0)
## IMPORTANT THAT NONE OF THE % >> 100 OR = INF
setDT(df2)[total_tested <total_confirmed_cases, total_tested := total_confirmed_cases]
## SUBSTRACT TOTAL-POS = SUM NEG TESTS
df2$total_neg_tests = df2$total_tested-df2$total_confirmed_cases
## CALCULATE PROPORTION OF POSITIVE TESTS [Total Number of PCR POSITIVE Tests/Total Number of PCR Tests]
df2$positive_prop = (df2$total_confirmed_cases/df2$total_tested)*100
## CONVERT ALL NAN PROPORTIONS (I.E. 0/0) TO 0
df2$positive_prop[is.na(df2$positive_prop)] <- 0
## REMOVE PLACEHOLDER 'COUNTY' VARIABLE
df2$county = NULL
############################################################################################
## CALCULATE MOVING 3-DAY MEAN OF POSITIVE PROP
#df2$sort_ID <- as.integer(factor(with(df2, paste(dateadded))))
#df2 <- arrange(df2, -sort_ID); head(df2)
## TEMPORARILY UNLOAD PACKAGES

#unloadNamespace("tidyr")
#require(dplyr)
#stats::filter
## CALCULATE MOVING 3-DAY MEAN OF POSITIVE PROP
df2<- subset(df2, sort_ID > 11)

df2$positive_prop <- as.double(df2$positive_prop)

df2$avg_7day <- stats::filter(df2$positive_prop, rep(1/7,7)); head(df2)

## SHIFT FUNCTION
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
## SHIFT UP
df2$avg_7day <- shift(df2$avg_7day, 3)
## REPLACE ALL NA WITH 0 
df2$avg_7day[is.na(df2$avg_7day)] <- 0


## DEBUG THIS LATER!!
#demo <- head(df2)
#require(dplyr)
#demo<- arrange(demo, -sort_ID); head(demo)
#demo$avg_3day <- filter(demo$positive_prop, rep(1/3,3)); head(demo)


############################################################################################
## LAST CONFIRMATION THAT DF MATCHES ORGINAL DIMESNIONS
dim(df2)
##############################################################################################
## PULL OUT VARIABLES FOR DF'S
## 1) POSITIVE PROPORTION
pos_percent_df <- df2[,c("dateadded", "positive_prop", "avg_7day")]
## CLEAN COLUMN NAMES - ENG
colnames(pos_percent_df)[1] <- "Date"
colnames(pos_percent_df)[2] <- "Percentage of Tests Confirmed Positive"
colnames(pos_percent_df)[3] <- "7-day Moving Average Percentage of Confirmed Positive Tests"
##############################################################################################
## SUBSET LAST 30-DAY PERIOD
pos_percent_df$sort_ID <- as.integer(factor(with(pos_percent_df, paste(Date))))
## SORT BY TEMP ID
pos_percent_df <- arrange(pos_percent_df, -sort_ID); head(pos_percent_df)
pos_percent_df_sub <- head(pos_percent_df, 30); dim(pos_percent_df_sub)
pos_percent_df$sort_ID= NULL
pos_percent_df_sub$sort_ID= NULL

##############################################################################################
## 1) TEST COUNT - ENG
test_count_df <- df2[,c("dateadded", "total_tested", "total_confirmed_cases")]
## CLEAN COLUMN NAMES
colnames(test_count_df)[1] <- "Date"
colnames(test_count_df)[2] <- "Total People Tested"
colnames(test_count_df)[3] <- "People Confirmed Positive"

test_count_df <- test_count_df[,c("Date", "Total People Tested")]
## SAVE PCR COUNT DF
total_pcr_pos <- df2[,c("dateadded", "total_tested", "total_confirmed_cases")]

write.csv(total_pcr_pos, "pcr_count_df.csv", row.names=F)
##############################################################################################
## SUBSET LAST 30-DAY PERIOD
test_count_df$sort_ID <- as.integer(factor(with(test_count_df, paste(Date))))
## SORT BY TEMP ID
test_count_df <- arrange(test_count_df, -sort_ID); head(test_count_df)
test_count_df_sub <- head(test_count_df, 30); dim(test_count_df_sub)
test_count_df$sort_ID= NULL
test_count_df_sub$sort_ID= NULL

##############################################################################################
## WRITE TO CSV - ENG
write.csv(pos_percent_df, row.names=FALSE, "pos_percent_eng.csv")
write.csv(test_count_df, row.names=FALSE, "test_count_eng.csv")
## 30-DAY PERIOD - ENG
write.csv(pos_percent_df_sub, row.names=FALSE, "pos_percent_30day_eng.csv")
write.csv(test_count_df_sub, row.names=FALSE, "test_count_30day_eng.csv")
##############################################################################################
## 1) POSITIVE PROPORTION - ESP
pos_percent_ESP_df <- pos_percent_df
colnames(pos_percent_ESP_df)[1] <- "Fecha"
colnames(pos_percent_ESP_df)[2] <- "Porcentaje de pruebas que son positivas"
colnames(pos_percent_ESP_df)[3] <- "Promedio de 7 dias"
## WRITE CSV
write.csv(pos_percent_ESP_df, row.names=FALSE, "pos_percent_esp.csv")
########################################################################################################
## 30 DAY PERIOD - POS PERCENNT (ESP)
## 1.5) POSITIVE PROPORTION - ESP
pos_percent_ESP_sub <- pos_percent_df_sub
colnames(pos_percent_ESP_sub)[1] <- "Fecha"
colnames(pos_percent_ESP_sub)[2] <- "Porcentaje de pruebas que son positivas"
colnames(pos_percent_ESP_sub)[3] <- "Promedio de 3 dias"
## WRITE CSV
write.csv(pos_percent_ESP_sub, row.names=FALSE, "pos_percent_30day_esp.csv")
########################################################################################################
## 2) TEST COUNT - ENG
test_count_ESP_df <- test_count_df
## CLEAN COLUMN NAMES
colnames(test_count_ESP_df)[1] <- "Fecha"
colnames(test_count_ESP_df)[2] <- "Total de Pruebas"


test_count_ESP_df <- test_count_ESP_df[,c("Fecha", "Total de Pruebas")]


########################################################################################################
## WRITE CSV
write.csv(test_count_ESP_df, row.names=FALSE, "test_count_esp.csv")
########################################################################################################
## 30 DAY PERIOD - TESTING (ESP)
test_count_ESP_sub <- test_count_ESP_df
test_count_ESP_sub <- head(test_count_ESP_sub, 30); dim(test_count_ESP_sub)

## WRITE CSV
write.csv(test_count_ESP_sub, row.names=FALSE, "test_count_30day_esp.csv")
########################################################################################################
##(3) DAILY COUNTY CASES, HOSPITALIZATIONS, DEATHS
df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/cedrs_Summit.txt'); dim(df)
## CONVERT DATA CLASS TO DATA.FRAME
df <- as.data.frame(df)
## SUBSET ONLY COUNTY == SUMMIT (CLEANS OUT POTENTIAL ERROR FROM FORMATTING)
df <- subset(df, county=="SUMMIT"); dim(df)
## filter out duplicates
df$unique_id <- as.integer(factor(with(df, paste(profileid, first_name, last_name, date_of_birth))))
df<- subset(df, !duplicated(unique_id))

## LOAD PREVIOUSLY CREATED test_count_df from #2
date_df <- read.csv("test_count_eng.csv", header=T)
########################################################################################################
## 2. DATA CLEANING
df <- df[!is.na(df$casestatus),];dim(df)

## FORMAT DATE: YYYY-MM-DD - In case need to convert date format
#df$reporteddate <- format(as.Date(df$reporteddate, "%Y-%m-%d"), "%Y-%m-%d")
#date_df$Date <- format(as.Date(date_df$Date, "%m/%d/%Y"), "%Y-%m-%d")
########################################################################################################
## CREATE SUBSET FOR ONLY DEATH CASES
##  NOTE: DEATHDATE DOES NOT = REPORTDATE BUT MERGING TO HAVE A COMMON DATE FIELD
df.death <- subset(df, outcome=="Patient died"); dim(df.death)
########################################################################################################
## SUMMARY TABLE FOR CHCECKING DEATH REPORTING
#df_CCT <- df.death[,c("first_name", "last_name", "date_of_birth", "profileid", "eventid", "outcome", "reporteddate", "deathdate")]

df.death <- df.death[,c("outcome", "deathdate")]; dim(df.death)
df.death$reporteddate <- df.death$deathdate
df.death$deathdate <- NULL
df.death$death_count = 1


########################################################################################################
## PULL OUT DATE DF VARIABLES
date_df <- date_df[,c("Date", "Total.People.Tested")]
########################################################################################################
## 3. SUMMARIZE COUNT POSITIVES, NEGATIVES, TOTAL TESTS, AND PROP POSITIVES BY DATE
require(dplyr)
day_summ <- data.frame(group_by(df, reporteddate) %>% 
                         summarize(pos_confirm_sum = sum(casestatus=="confirmed"),
                                   prob_sum = sum(casestatus=="probable"),
                                   total_pos_case_sum = sum(pos_confirm_sum,prob_sum)))
dim(day_summ)

########################################################################################################
## 4. POSITIVE DATA MERGED WITH DEATH DATA
## IMPORTANT MAKE SURE DATES ARE ALL YYYY-MM-DD
summ_death_pos <- merge(x= day_summ, y=df.death, by=c("reporteddate"), all.x=T); dim(summ_death_pos)
## REPLACE NA WITH 0 FOR DEATH COUNT
summ_death_pos$outcome= NULL
summ_death_pos[is.na(summ_death_pos)]<- 0
########################################################################################################
## 5. ADD HOSPITALIZATION DF WITH SUMM_DEATH_ADD
hosp_df <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1cuJdqAzx_kh6v8zkmDlhsV9SDU7wSO-9QHH7grKGuwE/edit?usp=sharing')
hosp_df <- as.data.frame(hosp_df)
colnames(hosp_df)[1]<-"Date"
hosp_df <- hosp_df[,c("Date", "covid_pac", "covid_pac_trans")]
## FORMAT DATE
hosp_df$Date <- format(as.Date(hosp_df$Date, "%m/%d/%Y"), "%Y-%m-%d")

write.csv(hosp_df, "hosp_df_temp.csv", row.names=F)

#hosp_df <- read.csv("hosp_df.csv", header=T)
colnames(hosp_df)[1]<- "reporteddate"
colnames(hosp_df)[2]<- "hosp_count"
colnames(hosp_df)[3]<- "transf_count"

## SUMMARIZE HOSPITAL DATA BY REPORTEDDATE
#hosp_df <- data.frame(group_by(hosp_df, reporteddate) %>% 
#                        summarize(hospitalization_count = sum(hosp_count=="1")))
## FORMAT DATE FORMAT TO YYYY-MM-DD
#hosp_df$reporteddate <- format(as.Date(hosp_df$reporteddate, "%m/%d/%Y"), "%Y-%m-%d")
########################################################################################################
## STORE HISTORICAL HOSPITAL DF
write.csv(hosp_df, "hospital_historical.csv", row.names=F)

## SUBSET LAST 30-DAY PERIOD
hosp_df2<-hosp_df
hosp_df2$sort_ID <- as.integer(factor(with(hosp_df2, paste(reporteddate ))))
## SORT BY TEMP ID
hosp_df2 <- arrange(hosp_df2, -sort_ID); head(hosp_df2)
hosp_df2_sub <- head(hosp_df2, 30); dim(hosp_df2_sub)
hosp_df2_sub$sort_ID=NULL

write.csv(hosp_df2_sub, "hospital_8wks.csv", row.names=F)

########################################################################################################
## HOSP + DATE
colnames(hosp_df)[1]<-"Date"
hosp_date <- merge(x=date_df,y=hosp_df, by=c("Date"), all=T);dim(hosp_date)

## ADD DEATH+POS & HOSP_DF
summ_death_pos$reporteddate <- as.character(summ_death_pos$reporteddate)
colnames(summ_death_pos)[1]<-"Date"
summ_death_pos_hosp <- merge(x=hosp_date, y = summ_death_pos, by = c("Date"), all.x=T)

summ_death_pos_hosp <- merge(x=summ_death_pos_hosp, y = ob_summ, by = c("Date"), all.x=T)

## ADD "0" VALUES FOR ALL NA ENTRIES
summ_death_pos_hosp$death_count[is.na(summ_death_pos_hosp$death_count)] <- 0
summ_death_pos_hosp$hosp_count[is.na(summ_death_pos_hosp$hosp_count)] <- 0
summ_death_pos_hosp$transf_count[is.na(summ_death_pos_hosp$transf_count)] <- 0
summ_death_pos_hosp$outbreak_sum[is.na(summ_death_pos_hosp$outbreak_sum)] <- 0

## ALL MEANINGFUL COLUMNS WITH NA = 0
summ_death_pos_hosp[is.na(summ_death_pos_hosp)] <- 0

## PULL OUT MEANINGFUL VARIABLES
compiled_df <- summ_death_pos_hosp[,c("Date", "total_pos_case_sum", "death_count","hosp_count", "transf_count","outbreak_sum")]
## CHECK NO BLANKS ROWS INCLUDED
compiled_df <- subset(compiled_df, Date !=""); dim(compiled_df)
#####################################################################################
## PULL OUT VARIABLES OF INTEREST
compiled_df2 <- compiled_df[,c("Date","total_pos_case_sum", "death_count", "hosp_count", "transf_count", "outbreak_sum")]

## CLEAN UP VARIABLE NAMES FOR .CSV FILE
colnames(compiled_df2)[1] <- "Date"
colnames(compiled_df2)[3] <- "Deaths"
colnames(compiled_df2)[2] <- "Cases"
colnames(compiled_df2)[4] <- "Hospitalizations"
colnames(compiled_df2)[5] <- "Patient Transfers"
colnames(compiled_df2)[6] <- "Outbreaks"

## SUBSET LAST 30-DAY PERIOD
compiled_df2$sort_ID <- as.integer(factor(with(compiled_df2, paste(Date))))
## SORT BY TEMP ID
compiled_df2 <- arrange(compiled_df2, -sort_ID); head(compiled_df2)
compiled_df_sub <- head(compiled_df2, 30); dim(compiled_df_sub)
compiled_df2$sort_ID= NULL
compiled_df_sub$sort_ID=NULL
## WRITE CSV AND INCLUDE DATE AS A PROOFREADING APPROACH
write.csv(compiled_df2,row.names=FALSE, "daily_count_eng.csv")
## WRITE CSV 30-DAY PERIOD
write.csv(compiled_df_sub,row.names=FALSE, "daily_count_30d_eng.csv")

#####################################################################################
## CREATE SPANISH FILE
compiled_df_esp <- compiled_df2
## CHANGE VARIABLE NAMES TO SPANISH
colnames(compiled_df_esp)[1] <- "Fecha"
colnames(compiled_df_esp)[2] <- "Casos"
colnames(compiled_df_esp)[3] <- "Muertes"
colnames(compiled_df_esp)[4] <- "Hospitalizaciones"
colnames(compiled_df_esp)[5] <- "Traslado de pacientes"
colnames(compiled_df_esp)[6] <- "Brotes"

## WRITE CSV
write.csv(compiled_df_esp,row.names=FALSE, "daily_count_esp.csv")
## SUBSET LAST 30-DAY PERIOD
compiled_df_esp$sort_ID <- as.integer(factor(with(compiled_df_esp, paste(Fecha))))
## SORT BY TEMP ID
compiled_df_esp <- arrange(compiled_df_esp, -sort_ID); head(compiled_df_esp)
compiled_df_esp_sub <- head(compiled_df_esp, 30); dim(compiled_df_esp_sub)
compiled_df_esp_sub$sort_ID= NULL

write.csv(compiled_df_esp_sub,row.names=FALSE, "daily_count_30d_esp.csv")

##(4) DEMOGRAPHICS SUMMARY
df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/cedrs_Summit.txt'); dim(df)
##############################################################################################
## CONVERT DATA CLASS TO DATA.FRAME
df <- as.data.frame(df)
## SUBSET ONLY COUNTY == SUMMIT (CLEANS OUT POTENTIAL ERROR FROM FORMATTING)
df <- subset(df, county=="SUMMIT"); dim(df)
##############################################################################################
## FILTER OUT DUPLICATE ENTRIES
df$unique_id <- as.integer(factor(with(df, paste(profileid, first_name, last_name, date_of_birth))))
df<- subset(df, !duplicated(unique_id))
##############################################################################################
## EXCLUDE UNKNOWN SEX 
df$exclude = 0
df$exclude[which(df$gender=="Unknown")] <- "1"
##############################################################################################
## AVOID RISK OF REPORTING NON-BINARY GENDERS IN DATA PUBLIC TO COMMUNITY  
## SUBSET & SUMMARIZE BY 1) AGE, 2) SEX, 3) RACE/ETHNIC
df <- subset(df, exclude=="0");dim(df)
## MADE A DUMMY VARIABLE FOR INDEX MEASUREMENTS BY DEMOGRAPHICS
df$index_measure=1
##############################################################################################
## AGE CLASS
## ASSIGN AGE CLASS VARIABLE IN BINS OF 10 YEARS
df$age_class = 0
df$age_class[which(df$age_at_reported < 10)] <- "0-9"
df$age_class[which(df$age_at_reported > 9 & df$age_at_reported < 20)] <- "10-19"
df$age_class[which(df$age_at_reported > 19 & df$age_at_reported < 30)] <- "20-29"
df$age_class[which(df$age_at_reported > 29 & df$age_at_reported < 40)] <- "30-39"
df$age_class[which(df$age_at_reported > 39 & df$age_at_reported < 50)] <- "40-49"
df$age_class[which(df$age_at_reported > 49 & df$age_at_reported < 60)] <- "50-59"
df$age_class[which(df$age_at_reported > 59 & df$age_at_reported < 70)] <- "60-69"
df$age_class[which(df$age_at_reported > 69 & df$age_at_reported < 80)] <- "70-79"
df$age_class[which(df$age_at_reported >= 80)] <- "80+"
##############################################################################################
## SUMMARIZE BY AGE CLASS
age_summ <- data.frame(group_by(df, age_class) %>% 
                         summarize(sum_cases = sum(index_measure=="1")))
## PREPARE AGE HEADER FOR .CSV
colnames(age_summ)[1] <- "Age Group"
colnames(age_summ)[2] <- "Number of Cases"
write.csv(age_summ,row.names=FALSE, "age_summ_eng.csv")
##############################################################################################
## SEX
sex_summ <- data.frame(group_by(df, gender) %>% 
                         summarize(sum_cases = sum(index_measure=="1")))

## PREPARE AGE HEADER FOR .CSV
# NOTE: CDPHE ONLY REPORTS SEX
colnames(sex_summ)[1] <- "Sex" 
colnames(sex_summ)[2] <- "Number of Cases"

write.csv(sex_summ,row.names=FALSE, "sex_summ_eng.csv")
##############################################################################################
## TRANSLATE SEX VARIABLES TO SPANISH
sex_summ_esp <- sex_summ

sex_summ_esp$Sexo = 0
sex_summ_esp$Sexo[which(sex_summ_esp$Sex=="Female")] <- "Mujeres"
sex_summ_esp$Sexo[which(sex_summ_esp$Sex=="Male")] <- "Hombres"

## REMOVE VARIABLE 'SEX'
sex_summ_esp$Sex = NULL
## WRITE .CSV
write.csv(sex_summ_esp,row.names=FALSE, "sex_summ_esp.csv")
##############################################################################################
## CREATE NEW VARIABLE (race_ethnic) FOR RACE+ETHNICITY 
df$race_ethnic = 0
df$race_ethnic[which(df$single_race_ethnicity_with_ciis=="White - Non Hispanic")] <- "White"
df$race_ethnic[which(df$single_race_ethnicity_with_ciis=="Hispanic, All Races")] <- "Hispanic/Latinx"
df$race_ethnic[which(df$single_race_ethnicity_with_ciis=="Unknown")] <- "Unknown"
df$race_ethnic[which(df$single_race_ethnicity_with_ciis!="Hispanic, All Races" & df$single_race_ethnicity_with_ciis!="White - Non Hispanic" & df$single_race_ethnicity_with_ciis!="Unknown")] <- "Other"

## SUMMARIZE BY RACE & ETHNICITY
race_summ <- data.frame(group_by(df, race_ethnic) %>% 
                          summarize(sum_cases = sum(index_measure=="1")))

## WRITE .CSV
write.csv(race_summ,row.names=FALSE, "race_summ_eng.csv")
##############################################################################################
## RACE/ETHNICITY SUMMARY TRANSLATED TO SPANISH
race_summ_esp <- race_summ

## SET DUMMY VARIABLE
#race_summ_esp$race_ethnic_esp= 0
## CHANGE RACE/ETHNICITY TO ESP
race_summ_esp$race_ethnic_esp[which(race_summ_esp$race_ethnic=="Hispanic/Latinx")] <- "hispano/latinx"
race_summ_esp$race_ethnic_esp[which(race_summ_esp$race_ethnic=="Other")] <- "otros"
race_summ_esp$race_ethnic_esp[which(race_summ_esp$race_ethnic=="Unknown")] <- "desconocido"
race_summ_esp$race_ethnic_esp[which(race_summ_esp$race_ethnic=="White")] <- "blanco"

## PULL ONLY MEANINGFUL VARIABLES
race_summ_esp <- race_summ_esp[,c("race_ethnic_esp", "sum_cases")]

## WRITE CSV
write.csv(race_summ_esp, row.names=FALSE, "race_summ_esp.csv")

##PON MILESTONE #1 - HOSPITAL OCCUPANCY & HOSPITAL ADMISSIONS
##SASMC HOSPITAL DATA FROM GOOGLE SHEETS
hosp_count <- read.csv("daily_count_eng.csv", header=T)
##############################################################################################
## HOSP ADMISSIONS DF FOR MILESTONES
hosp_count <- hosp_count[,c("Date", "Hospitalizations")]
## CLEAN UP VARIABLES
colnames(hosp_count)[2]<- "Hospital Admissions"
## CREATE TEMP SORT ID
hosp_count$sort_ID <- as.integer(factor(with(hosp_count, paste(Date))))
## SORT BY TEMP ID
hosp_count <- arrange(hosp_count, -sort_ID); head(hosp_count)
##############################################################################################
## HISTORICAL HOSPITAL COUNT DATA
hosp_count <- hosp_count[,c(1:2)]
##WRITE.CSV
write.csv(hosp_count, row.names=FALSE,"pon_hosp_count_hist.csv")
##############################################################################################
## 8-WEEK HOSPITAL COUNT DATA
hosp_count <- head(hosp_count, 56); dim(hosp_count)
hosp_count$sort_ID = NULL
## WRITE TO CSV
write.csv(hosp_count, row.names=FALSE,"pon_hosp_count_8wk.csv")
##############################################################################################
## HOSPITAL OCCUPANCY 
## CHANGE DATE FORMAT
df <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1cuJdqAzx_kh6v8zkmDlhsV9SDU7wSO-9QHH7grKGuwE/edit?usp=sharing')
df <- as.data.frame(df)
df$date <- format(as.Date(df$date, "%m/%d/%Y"), "%Y-%m-%d")
##############################################################################################
## SUMMARIZE DAILY % HOSPITAL BED OCCUPANCY (N, BEDS = 34)
df$max_threshold= 80.0
hosp_occ <- df[,c("date","prop_occ","max_threshold")]
hosp_occ$min = 0
hosp_occ$max = 100
##############################################################################################
## CREATE PROXY TEMP_ID 
hosp_occ$sort_ID <- as.integer(factor(with(hosp_occ, paste(date))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
hosp_occ <- arrange(hosp_occ, -sort_ID); head(hosp_occ)
hosp_occ$sort_ID = NULL
##############################################################################################
## HISTORICAL HOSPITAL OCCUPANCY (ENG)
hosp_occ <- na.omit(hosp_occ)
colnames(hosp_occ)[1] <- "Date"
colnames(hosp_occ)[2] <- "Bed Occupancy (%)"
colnames(hosp_occ)[3] <- "Goal (80% or lower)"
## WRITE CSV - HISTORICAL HOSPITAL OCCUPANCY
write.csv(hosp_occ,row.names=FALSE, "pon_hosp_occ_hist_eng.csv")
##############################################################################################
## HISTORICAL HOSPITAL OCCUPANCY (ESP)
hosp_occ_esp <- hosp_occ
colnames(hosp_occ_esp)[1] <- "Fecha"
colnames(hosp_occ_esp)[2] <- "Cupo de camas (%)"
colnames(hosp_occ_esp)[3] <- "Meta (80% o menos)"
## WRITE CSV - HISTORICAL HOSPITAL OCCUPANCY (ESP)
write.csv(hosp_occ_esp,row.names=FALSE, "pon_hosp_occ_hist_esp.csv")
##############################################################################################
## SUBSET 8-WEEK DATA (ENG)
hosp_occ_8wk_eng <- head(hosp_occ, 56); dim(hosp_occ_8wk_eng)
write.csv(hosp_occ_8wk_eng,row.names=FALSE, "pon_hosp_occ_8wk_eng.csv")
##############################################################################################
## SUBSET 8-WEEK DATA (ESP)
hosp_occ_8wk_esp <- head(hosp_occ_esp, 56); dim(hosp_occ_8wk_esp)
write.csv(hosp_occ_8wk_esp,row.names=FALSE, "pon_hosp_occ_8wk_esp.csv")
##############################################################################################
##PON MILESTONE #2 - CUMULATIVE 7-D INCIDENCE PER 100,000
## COVID-19 7-DAY CUMULATIVE INCIDENCE (POSITIVE TESTS + PCR)
pos_df <- read.csv("daily_count_eng.csv", header=T) ## COMPILED PCR DF 
##############################################################################################
## CHANGE FORMAT OF DATEADDED
##pos_df$Date <- format(as.Date(pos_df$Date, "%m/%d/%Y"), "%Y-%m-%d")

##############################################################################################
##PON MILESTONE #2 - CUMULATIVE 7-D INCIDENCE PER 100,000
## COVID-19 7-DAY CUMULATIVE INCIDENCE (POSITIVE TESTS + PCR)
pos_df <- read.csv("daily_count_eng.csv", header=T) ## COMPILED PCR DF 
##############################################################################################
## CHANGE FORMAT OF DATEADDED
##pos_df$Date <- format(as.Date(pos_df$Date, "%m/%d/%Y"), "%Y-%m-%d")

##############################################################################################
## PULL OUT MEANINGFUL VARIABLES
pos_df <- pos_df[,c("Date","Cases")]; dim(pos_df)
## CREATE PROXY TEMP_ID 
pos_df$sort_ID <- as.integer(factor(with(pos_df, paste(Date))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
pos_df <- arrange(pos_df, -sort_ID); head(pos_df)
# ASSIGN ID FOR 7-DAY INTERVALS 
pos_df$ID <-rep(1:100, each=7, length.out=nrow(pos_df)); head(pos_df)
## FILTER MOST RECENT DATE TO FOLLOW CDPHE'S DIAL REPORTING
pos_df<- tail(pos_df, -1)
##############################################################################################
## 7-DAY MOVING SUM
pos_df <- pos_df %>%
  arrange(sort_ID,Date) %>%
  mutate(rollapply_sum =zoo::rollapplyr(Cases, 7, sum, partial = TRUE)) # requires package zoom
##############################################################################################
##############################################################################################
## CUMULATIVE 7-DAY INCIDENCE (TOTAL NUMBER OF POSITIVE CASES PER 7D) 
## MUST MULTIPLE BY SUM OF POSITIVE CASES AND POPULATION FACTOR
## POPULATION VALUE IS TAKEN FROM CO DEMOGRAPHY 2018 POPULATION SIZE
pos_df$pos_case_7d <- pos_df$rollapply_sum*(100000/30974)
##############################################################################################
## HISTORICAL INCIDENCE (ENG)
pos_hist <- pos_df
## PREPARE FILE FOR CSV
pos_hist$min = 0
pos_hist$target1 = 100
pos_hist$target2 = 250
pos_hist$target3 = 500
pos_hist$target4 = 501


pos_hist$max = 800
pos_hist <- pos_hist[,c("Date","pos_case_7d","min","target1","target2","target3", "max")]
colnames(pos_hist)[2] <- "Rolling 7-Day Positive Cases Per 100,000"
colnames(pos_hist)[4] <- "Level Green: Little to No Risk"
colnames(pos_hist)[5] <- "Level Blue: Caution"
colnames(pos_hist)[6] <- "Level Yellow: Concern"
colnames(pos_hist)[7] <- "Level Orange: High Risk"

pos_hist2 <- subset(pos_hist, Date!=0)

##WRITE CSV
write.csv(pos_hist2, row.names=FALSE,"pon_incidence_hist_eng.csv")
##############################################################################################
## HISTORICAL INCIDENCE (ESP)
pos_hist_esp <- pos_hist
colnames(pos_hist_esp)[1] <- "Fecha"
colnames(pos_hist_esp)[2] <- "positivos nuevos por 100,000 personas en los 7 dias anteriores"
colnames(pos_hist_esp)[4] <- "Nivel Verde: Sin Riesgo"
colnames(pos_hist_esp)[5] <- "Nivel Azul: Precacion"
colnames(pos_hist_esp)[6] <- "Nivel Amarillo: Preocupaction"
colnames(pos_hist_esp)[7] <- "Nivel Anaranjado: Alto Riesgo"

pos_hist_esp2 <- subset(pos_hist_esp, Fecha!=0)

## WRITE CSV
write.csv(pos_hist_esp2, row.names=FALSE,"pon_incidence_hist_esp.csv")
##############################################################################################
## SUBSET 8-WEEK DATA
pos_df <- arrange(pos_df, -sort_ID); head(pos_df)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
pos_df <- head(pos_df, 56); dim(pos_df)
## PREPARE FILE FOR CSV
pos_df$target1 = 100
pos_df$target2 = 250
pos_df$target3 = 500
pos_df$target4 = 501
pos_df$min=0
pos_df$max = 800

pos_df2 <- pos_df[,c("Date","pos_case_7d","min", "target1","target2","target3", "max")]
##############################################################################################
## 8-WEEK INCIDENCE (ENG)
colnames(pos_df2)[2] <- "Rolling 7-Day Positive Cases Per 100,000"
colnames(pos_df2)[4] <- "Level Green: Little to No Threat"
colnames(pos_df2)[5] <- "Level Blue: Caution"
colnames(pos_df2)[6] <- "Level Yellow: Concern"
colnames(pos_df2)[7] <- "Level Orange: High Risk"

## WRITE CSV
write.csv(pos_df2, row.names=FALSE,"pon_incidence_8wk_eng.csv")
##############################################################################################
## 8-WEEK INCIDENCE (ESP)
pos_df2_esp <- pos_df2
colnames(pos_df2_esp)[1] <- "Fecha"
colnames(pos_df2_esp)[2] <- "positivos nuevos por 100,000 personas en los 7 dias anteriores"
colnames(pos_df2_esp)[4] <- "Nivel Verde: Sin Riesgo"
colnames(pos_df2_esp)[5] <- "Nivel Azul: Precacion"
colnames(pos_df2_esp)[6] <- "Nivel Amarillo: Preocupaction"
colnames(pos_df2_esp)[7] <- "Nivel Anaranjado: Alto Riesgo"

## WRITE CSV
write.csv(pos_df2_esp, row.names=FALSE,"pon_incidence_8wk_esp.csv")

##PON MILESTONE #3 - CUMULATIVE 7-D PERCENT PCR POSITIVE
## 7-DAY PCR POSITIVITY RATE
## LOAD DATA
df <- read.csv("pcr_count_df.csv", header=T)
##############################################################################################
## CHANGE FORMAT OF DATEADDED (*If Needed)
#df$Date <- format(as.Date(df$Date, "%m/%d/%Y"), "%Y-%m-%d")
##############################################################################################
## CLEAN UP VARIABLE NAMES FOR SIMPLICITY
colnames(df)[1]<-"Date" # Date
colnames(df)[2]<-"total_pcr" # TOTAL NUMBER OF PCR TESTS
colnames(df)[3]<-"pos_pcr"   # NUMBER OF POSITIVE CONFIRMED TESTS
##############################################################################################
## REMOVE GARBAGE DATES
start_date = "2020-03-10"
start_date = as.Date(start_date)
df$Date <- as.Date(df$Date)
df <- subset(df, Date > start_date)
##############################################################################################
## CREATE PROXY ID FOR SORTING - DF SHOULD ALREADY BE ORDERED NEWEST TO OLDEST BUT JUST AS A CHECK
df$sort_ID <- as.integer(factor(with(df, paste(Date))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
df <- arrange(df, -sort_ID); head(df)
##############################################################################################
## 7-DAY MOVING SUM
df <- df %>%
  arrange(sort_ID,Date) %>%
  mutate(rollapply_pos_sum =zoo::rollapplyr(pos_pcr, 7, sum, partial = TRUE)) # need package zoom

df <- df %>%
  arrange(sort_ID,Date) %>%
  mutate(rollapply_total_sum =zoo::rollapplyr(total_pcr, 7, sum, partial = TRUE)) # need package zoom
##############################################################################################
##############################################################################################
## 1)7-DAY POSITIVITY RATE 
df$pos_pcr_rate <- (df$rollapply_pos_sum/df$rollapply_total_sum)*100
df$pcr_pos_target1 = 5
df$pcr_pos_target2 = 7.5
df$pcr_pos_target3 = 10
df$min = 0
## ARRANGE NEWEST TO OLDEST DATE
df <- arrange(df, -sort_ID); head(df)


##############################################################################################
## HISTORICAL DATA (ENG)
pcr_pos_perc_hist_eng <- df[,c("Date","pos_pcr_rate", "min", "pcr_pos_target1", "pcr_pos_target2", "pcr_pos_target3")]
pcr_pos_perc_hist_eng$max = max(pcr_pos_perc_hist_eng$pos_pcr_rate)+2
colnames(pcr_pos_perc_hist_eng)[2] <- "Rolling 7-Day Percent of Tests Positive"
colnames(pcr_pos_perc_hist_eng)[4] <- "Level Blue: Caution"
colnames(pcr_pos_perc_hist_eng)[5] <- "Level Yellow: Concern"
colnames(pcr_pos_perc_hist_eng)[6] <- "Level Orange: High Risk"
colnames(pcr_pos_perc_hist_eng)[7] <- "Level Red: Severe Risk"

## WRITE CSV
write.csv(pcr_pos_perc_hist_eng, row.names=FALSE,"pon_pos_rate_hist_eng.csv")
##############################################################################################
## HISTORICAL DATA (ESP)
pcr_pos_perc_hist_esp <- df[,c("Date","pos_pcr_rate", "min", "pcr_pos_target1", "pcr_pos_target2", "pcr_pos_target3")]
pcr_pos_perc_hist_esp$max = max(pcr_pos_perc_hist_esp$pos_pcr_rate)+2

colnames(pcr_pos_perc_hist_esp)[1] <- "Fecha"
colnames(pcr_pos_perc_hist_esp)[2] <- "Porcentaje de pruebas positivas durante los 7 dias anteriores"
colnames(pcr_pos_perc_hist_esp)[4] <- "Nivel Azul: Precacion"
colnames(pcr_pos_perc_hist_esp)[5] <- "Nivel Amarillo: Preocupaction"
colnames(pcr_pos_perc_hist_esp)[6] <- "Nivel Anaranjado: Alto Riesgo"
colnames(pcr_pos_perc_hist_esp)[7] <- "Nivel Rojo: Reisgo severo"

## WRITE CSV
write.csv(pcr_pos_perc_hist_esp, row.names=FALSE,"pon_pos_rate_hist_esp.csv")
##############################################################################################
## SUBSET OUT LAST 8 WEEKS
df <- head(df, 56); dim(df)
##############################################################################################
## 8-WEEK POSITIVITY (ENG)
pcr_pos_perc_eng <- df[,c("Date", "pos_pcr_rate", "min", "pcr_pos_target1", "pcr_pos_target2", "pcr_pos_target3")]
#pcr_pos_perc_eng$max = max(pcr_pos_perc_eng$pos_pcr_rate)

pcr_pos_perc_eng$max = 20


colnames(pcr_pos_perc_eng)[2] <- "Rolling 7-Day Percent of Tests Positive"
colnames(pcr_pos_perc_eng)[4] <- "Level Blue: Caution"
colnames(pcr_pos_perc_eng)[5] <- "Level Yellow: Concern"
colnames(pcr_pos_perc_eng)[6] <- "Level Orange: High Risk"
colnames(pcr_pos_perc_eng)[7] <- "Level Red: Severe Risk"

##  WRITE CSV (8-WEEK DATA - ENG)
write.csv(pcr_pos_perc_eng,row.names=FALSE, "pon_pos_rate_8wk_eng.csv")
##############################################################################################
## 8-WEEK POSITIVITY (ESP)
pcr_pos_perc_esp <- pcr_pos_perc_eng
colnames(pcr_pos_perc_esp)[1] <- "Fecha"
colnames(pcr_pos_perc_esp)[2] <- "Porcentaje de pruebas positivas durante los 7 dias anteriores"
colnames(pcr_pos_perc_esp)[4] <- "Nivel Azul: Precacion"
colnames(pcr_pos_perc_esp)[5] <- "Nivel Amarillo: Preocupaction"
colnames(pcr_pos_perc_esp)[6] <- "Nivel Anaranjado: Alto Riesgo"
colnames(pcr_pos_perc_esp)[7] <- "Nivel Rojo: Reisgo severo"

##  WRITE CSV [8 WEEKS OF DATA - ESP]
write.csv(pcr_pos_perc_esp, row.names=FALSE,"pon_pos_rate_8wk_esp.csv")






##############################################################################################

##PON MILESTONE #3 - TESTING RATE PER 1,000
## PROTECT OUR NEIGHBORS PCR TESTING MILESTONE
df <- read.csv("test_count_eng.csv", header=T)
##############################################################################################
## CHANGE FORMAT OF DATEADDED (*If Needed)
#df$Date <- format(as.Date(df$Date, "%m/%d/%Y"), "%Y-%m-%d")
##############################################################################################
## CLEAN UP VARIABLE NAMES FOR SIMPLICITY
colnames(df)[2]<-"total_pcr" # TOTAL NUMBER OF PCR TESTS
## SELECT OUT VARIABLES OF INTEREST
df <- df[,c("Date", "total_pcr")]; dim(df)
##############################################################################################
## CREATE PROXY ID FOR SORTING
df$sort_ID <- as.integer(factor(with(df, paste(Date))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
df <- arrange(df, -sort_ID); head(df)
##############################################################################################
## 7-day MOVING TESTING AVERAGE
df <- df %>%
  arrange(sort_ID,Date) %>%
  mutate(rollapply_7d_avg =zoo::rollapplyr(total_pcr, 7, mean, partial = TRUE)) # need package zoom

#TESTING POPULATION FACTOR PROPORTION
PF_1K = (1000/30974) 
## 7 DAY AVG TESTING RATE 
df$test_rate7d <- (df$rollapply_7d_avg)*PF_1K
## DAILY TESTING RATE
df$test_rate_daily<- (df$total_pcr)*PF_1K

## TESTING POPULATION FACTOR PER 0.75, 1,000 PEOPLE
df$test_target = 0.75
##############################################################################################
## SORT DATE NEWEST TO OLDEST
df <- arrange(df, -sort_ID); head(df)
df$min = 0 
df$max = max(df$test_rate_daily)+2
##############################################################################################
## HISTORICAL TESTING (ENG)
pcr_test_hist_eng <- df[,c("Date","test_rate_daily", "test_rate7d", "test_target", "min", "max")]
colnames(pcr_test_hist_eng)[2] <- "Daily Testing Conducted per 1,000 population"
colnames(pcr_test_hist_eng)[3] <- "7 Day Moving Average"
colnames(pcr_test_hist_eng)[4] <- "Goal (0.75 or higher)"
## WRITE CSV - HISTORICAL TESTING (ENG)
write.csv(pcr_test_hist_eng, row.names=FALSE,"pon_test_rate_hist_eng.csv")
##############################################################################################
## HISTORICAL TESTING (ESP)
pcr_test_hist_esp <- pcr_test_hist_eng
colnames(pcr_test_hist_esp)[1] <- "Fecha"
colnames(pcr_test_hist_esp)[2] <- "Pruebas por 1,000 Personas"
colnames(pcr_test_hist_esp)[3] <- "Promedio de 7 dias"
colnames(pcr_test_hist_esp)[4] <- "Meta (0.75 o mayor)"
## WRITE CSV - HISTORICAL TESTING (ENG)
write.csv(pcr_test_hist_esp, row.names=FALSE,"pon_test_rate_hist_esp.csv")
##############################################################################################
## SUBSET RECENT 8-WEEK DATA
df <- head(df, 56); dim(df)
df$max = max(df$test_rate_daily)+2
##############################################################################################
## 8-WEEK TESTING (ENG)
pcr_test_eng <- df[,c("Date","test_rate_daily", "test_rate7d", "test_target", "min", "max")]
colnames(pcr_test_eng)[2] <- "Daily Testing Conducted per 1,000 population"
colnames(pcr_test_eng)[3] <- "7 Day Moving Average"
colnames(pcr_test_eng)[4] <- "Goal (0.75 or higher)"
## WRITE CSV
write.csv(pcr_test_eng, row.names=FALSE,"pon_test_rate_8wk_eng.csv")
##############################################################################################
## 8-WEEK TESTING (ESP)
pcr_test_esp <- pcr_test_eng
colnames(pcr_test_esp)[1] <- "Fecha"
colnames(pcr_test_esp)[2] <- "Pruebas por 1,000 Personas"
colnames(pcr_test_esp)[3] <- "Promedio de 7 dias"
colnames(pcr_test_esp)[4] <- "Meta (0.75 o mayor)"
## WRITE CSV
write.csv(pcr_test_esp, row.names=FALSE,"pon_test_rate_8wk_esp.csv")

##AGE GROUP PER 100,000
## LOAD DATA
df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/cedrs_Summit.txt'); dim(df)
##############################################################################################
## CONVERT DATA CLASS TO DATA.FRAME
df <- as.data.frame(df)
## SUBSET ONLY COUNTY == SUMMIT (CLEANS OUT POTENTIAL ERROR FROM FORMATTING)
df <- subset(df, county=="SUMMIT"); dim(df)
## FILTER OUT DUPLICATE ENTRIES
df$unique_id <- as.integer(factor(with(df, paste(profileid, first_name, last_name, date_of_birth))))
df<- subset(df, !duplicated(unique_id))
## COUNT VARIABLE
df$index_measure=1
##############################################################################################
## ADD IN MISSING DUMMY DATES 
total_dates <- data.frame(reporteddate=seq(min(df$reporteddate),max(df$reporteddate),1))
## MERGE DUMMY DATE DF with total case DF
df <- merge(x= total_dates, y=df,by="reporteddate",all.x=TRUE)
df$index_measure[is.na(df$index_measure)] <- 0
##############################################################################################
## CREATE AGE GROUPS
## AGE CLASS
## ASSIGN AGE CLASS VARIABLE IN BINS OF 10 YEARS
df$age_class = 0
df$age_class[which(df$age_at_reported < 10)] <- "0-9"
df$age_class[which(df$age_at_reported > 9 & df$age_at_reported < 20)] <- "10-19"
df$age_class[which(df$age_at_reported > 19 & df$age_at_reported < 30)] <- "20-29"
df$age_class[which(df$age_at_reported > 29 & df$age_at_reported < 40)] <- "30-39"
df$age_class[which(df$age_at_reported > 39 & df$age_at_reported < 50)] <- "40-49"
df$age_class[which(df$age_at_reported > 49 & df$age_at_reported < 60)] <- "50-59"
df$age_class[which(df$age_at_reported > 59 & df$age_at_reported < 70)] <- "60-69"
df$age_class[which(df$age_at_reported > 69 & df$age_at_reported < 80)] <- "70-79"
df$age_class[which(df$age_at_reported >= 80)] <- "80+"
##############################################################################################
## SUMMARIZE BY AGE CLASS & DATE
age_daily <- data.frame(group_by(df, age_class, reporteddate) %>% 
                          summarize(sum_cases = sum(index_measure=="1")))
## ----------------------------------------------------------------------
## DO SOME PRE-MAGIC FOR THE AGE DF
age_daily$sort_ID <- as.integer(factor(with(age_daily, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
age_daily <- arrange(age_daily, -sort_ID); head(age_daily)
## PULL OUT SELECT NEEDED VARIABLES 
date_ID <- age_daily[,c("reporteddate", "sort_ID")]
date_ID <- date_ID[!duplicated(date_ID[1]),]
colnames(date_ID)[2]<-"ID"
## ----------------------------------------------------------------------
## AGE GROUP (0-9)
age_09 <- subset(age_daily, age_class=="0-9"); dim(age_09)
age_09 <- merge(x= total_dates, y=age_09,by="reporteddate",all.x=TRUE); dim(age_09)
age_09$age_class = "0-9"
age_09$sum_cases[is.na(age_09$sum_cases)] <- 0

age_09$sort_ID <- as.integer(factor(with(age_09, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
age_09 <- arrange(age_09, -sort_ID); head(age_09)
## 7-DAY MOVING SUM
age_09 <- age_09 %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) # requires package zoom
## POPULATION FACTOR
age_09$pos_case_7d <- age_09$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
age_09_8wk <- arrange(age_09, -sort_ID); head(age_09_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
age_09_8wk <- head(age_09_8wk, 56); dim(age_09_8wk)
## ----------------------------------------------------------------------
## AGE GROUP (10-19)
age_19 <- subset(age_daily, age_class=="10-19"); dim(age_19)
age_19 <- merge(x= total_dates, y=age_19,by="reporteddate",all.x=TRUE); dim(age_19)
age_19$age_class = "10-19"
age_19$sum_cases[is.na(age_19$sum_cases)] <- 0

age_19$sort_ID <- as.integer(factor(with(age_19, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
age_19 <- arrange(age_19, -sort_ID); head(age_19)
## 7-DAY MOVING SUM
age_19 <- age_19 %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) 
## POPULATION FACTOR
age_19$pos_case_7d <- age_19$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
age_19_8wk <- arrange(age_19, -sort_ID); head(age_19_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
age_19_8wk <- head(age_19_8wk, 56); dim(age_19_8wk)
## ----------------------------------------------------------------------
## AGE GROUP (20-29)
age_29 <- subset(age_daily, age_class=="20-29"); dim(age_29)
age_29 <- merge(x= total_dates, y=age_29,by="reporteddate",all.x=TRUE); dim(age_29)
age_29$age_class = "20-29"
age_29$sum_cases[is.na(age_29$sum_cases)] <- 0

age_29$sort_ID <- as.integer(factor(with(age_29, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
age_29 <- arrange(age_29, -sort_ID); head(age_29)
## 7-DAY MOVING SUM
age_29 <- age_29 %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) 
## POPULATION FACTOR
age_29$pos_case_7d <- age_29$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
age_29_8wk <- arrange(age_29, -sort_ID); head(age_29_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
age_29_8wk <- head(age_29_8wk, 56); dim(age_29_8wk)
## ----------------------------------------------------------------------
## AGE GROUP (30-39)
age_39 <- subset(age_daily, age_class=="30-39"); dim(age_39)
age_39 <- merge(x= total_dates, y=age_39,by="reporteddate",all.x=TRUE); dim(age_39)
age_39$age_class = "30-39"
age_39$sum_cases[is.na(age_39$sum_cases)] <- 0

age_39$sort_ID <- as.integer(factor(with(age_39, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
age_39 <- arrange(age_39, -sort_ID); head(age_39)
## 7-DAY MOVING SUM
age_39 <- age_39 %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) 
## POPULATION FACTOR
age_39$pos_case_7d <- age_39$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
age_39_8wk <- arrange(age_39, -sort_ID); head(age_39_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
age_39_8wk <- head(age_39_8wk, 56); dim(age_39_8wk)
## ----------------------------------------------------------------------
## AGE GROUP (40-49)
age_49 <- subset(age_daily, age_class=="40-49"); dim(age_49)
age_49 <- merge(x= total_dates, y=age_49,by="reporteddate",all.x=TRUE); dim(age_49)
age_49$age_class = "40-49"
age_49$sum_cases[is.na(age_49$sum_cases)] <- 0

age_49$sort_ID <- as.integer(factor(with(age_49, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
age_49 <- arrange(age_49, -sort_ID); head(age_49)
## 7-DAY MOVING SUM
age_49 <- age_49 %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) 
## POPULATION FACTOR
age_49$pos_case_7d <- age_49$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
age_49_8wk <- arrange(age_49, -sort_ID); head(age_49_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
age_49_8wk <- head(age_49_8wk, 56); dim(age_49_8wk)
## ----------------------------------------------------------------------
## AGE GROUP (50-59)
age_59 <- subset(age_daily, age_class=="50-59"); dim(age_59)
age_59 <- merge(x= total_dates, y=age_59,by="reporteddate",all.x=TRUE); dim(age_59)
age_59$age_class = "50-59"
age_59$sum_cases[is.na(age_59$sum_cases)] <- 0

age_59$sort_ID <- as.integer(factor(with(age_59, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
age_59 <- arrange(age_59, -sort_ID); head(age_59)
## 7-DAY PERIOD SUM
age_59 <- age_59 %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) 
## POPULATION FACTOR
age_59$pos_case_7d <- age_59$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
age_59_8wk <- arrange(age_59, -sort_ID); head(age_59_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
age_59_8wk <- head(age_59_8wk, 56); dim(age_59_8wk)
## ----------------------------------------------------------------------
## AGE GROUP (60-69)
age_69 <- subset(age_daily, age_class=="60-69"); dim(age_69)
age_69 <- merge(x= total_dates, y=age_69,by="reporteddate",all.x=TRUE); dim(age_69)
age_69$age_class = "60-69"
age_69$sum_cases[is.na(age_69$sum_cases)] <- 0

age_69$sort_ID <- as.integer(factor(with(age_69, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
age_69 <- arrange(age_69, -sort_ID); head(age_69)
## 7-DAY MOVING SUM
age_69 <- age_69 %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) 
## POPULATION FACTOR
age_69$pos_case_7d <- age_69$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
age_69_8wk <- arrange(age_69, -sort_ID); head(age_69_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
age_69_8wk <- head(age_69_8wk, 56); dim(age_69_8wk)
## ----------------------------------------------------------------------
## AGE GROUP (70-79)
age_79 <- subset(age_daily, age_class=="70-79"); dim(age_79)
age_79 <- merge(x= total_dates, y=age_79,by="reporteddate",all.x=TRUE); dim(age_79)
age_79$age_class = "70-79"
age_79$sum_cases[is.na(age_79$sum_cases)] <- 0

age_79$sort_ID <- as.integer(factor(with(age_79, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
age_79 <- arrange(age_79, -sort_ID); head(age_79)
## 7-DAY MOVING SUM
age_79 <- age_79 %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) 
## POPULATION FACTOR
age_79$pos_case_7d <- age_79$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
age_79_8wk <- arrange(age_79, -sort_ID); head(age_79_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
age_79_8wk <- head(age_79_8wk, 56); dim(age_79_8wk)
## ----------------------------------------------------------------------
## AGE GROUP (80)
age_80 <- subset(age_daily, age_class=="80+")
age_80 <- merge(x= total_dates, y=age_80,by="reporteddate",all.x=TRUE)
age_80$age_class = "80+"
age_80$sum_cases[is.na(age_80$sum_cases)] <- 0

age_80$sort_ID <- as.integer(factor(with(age_80, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
age_80 <- arrange(age_80, -sort_ID); head(age_80)
## 7-DAY MOVING SUM
age_80 <- age_80 %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) 
## POPULATION FACTOR
age_80$pos_case_7d <- age_80$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
age_80_8wk <- arrange(age_80, -sort_ID); head(age_80_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
age_80_8wk <- head(age_80_8wk, 56); dim(age_80_8wk)
## ----------------------------------------------------------------------
##############################################################################################
## COMPILE AGE CLASSES
age_compile <- rbind(age_09, age_19, age_29, age_39, age_49, age_59, age_69, age_79, age_80); dim(age_compile)
## SORT BY ID
age_compile <- arrange(age_compile, -sort_ID); head(age_compile)
##############################################################################################
## PREPARE DF FOR PUBLISHING
age_compile_2 <- age_compile[,c("reporteddate", "age_class", "pos_case_7d")]
## LONG TO WIDE
age_compile_2 <- spread(age_compile_2, age_class, pos_case_7d, fill = 0)
## CHANGE VARIABLE NAMES
colnames(age_compile_2)[1] <- "Date"
colnames(age_compile_2)[2] <- "0 - 9"
colnames(age_compile_2)[3] <- "10 - 19"
colnames(age_compile_2)[4] <- "20 - 29"
colnames(age_compile_2)[5] <- "30 - 39"
colnames(age_compile_2)[6] <- "40 - 49"
colnames(age_compile_2)[7] <- "50 - 59"
colnames(age_compile_2)[8] <- "60 - 69"
colnames(age_compile_2)[9] <- "70 - 79"
colnames(age_compile_2)[10] <- "80+"
## WRITE CSV HISTORICAL (ENG)
write.csv(age_compile_2, row.names=F, "age_7d_period_historical_eng.csv")


##############################################################################################
## SUBSET LAST 30-DAY PERIOD
age_compile_2$sort_ID <- as.integer(factor(with(age_compile_2, paste(Date))))
## SORT BY TEMP ID
age_compile_2 <- arrange(age_compile_2, -sort_ID); head(age_compile_2)
age_compile_2_sub <- head(age_compile_2, 44); dim(age_compile_2_sub)
age_compile_2$sort_ID= NULL
age_compile_2_sub$sort_ID = NULL
age_compile_2_sub<- tail(age_compile_2_sub, -7)
write.csv(age_compile_2_sub, row.names=F, "age_7d_period_30day_eng.csv")
######################################################################################################
## COMPILE AGE CLASSES - 8 WEEK
age_compile_8wk <- rbind(age_09_8wk, age_19_8wk, age_29_8wk, age_39_8wk, age_49_8wk, age_59_8wk, age_69_8wk, age_79_8wk, age_80_8wk); dim(age_compile_8wk)
## SORT BY ID
age_compile_8wk <- arrange(age_compile_8wk, -sort_ID); head(age_compile_8wk)
## PREPARE DF FOR PUBLISHING
age_compile_8wk_2 <- age_compile_8wk[,c("reporteddate", "age_class", "pos_case_7d")]
## LONG TO WIDE
age_compile_8wk_2 <- spread(age_compile_8wk_2, age_class, pos_case_7d, fill = 0)
## CHANGE VARIABLE NAMES
colnames(age_compile_8wk_2)[1] <- "Date"
colnames(age_compile_8wk_2)[2] <- "0 - 9 years"
colnames(age_compile_8wk_2)[3] <- "10 - 19 years"
colnames(age_compile_8wk_2)[4] <- "20 - 29 years"
colnames(age_compile_8wk_2)[5] <- "30 - 39 years"
colnames(age_compile_8wk_2)[6] <- "40 - 49 years"
colnames(age_compile_8wk_2)[7] <- "50 - 59 years"
colnames(age_compile_8wk_2)[8] <- "60 - 69 years"
colnames(age_compile_8wk_2)[9] <- "70 - 79 years"
colnames(age_compile_8wk_2)[10] <- "80+ years"
## WRITE CSV HISTORICAL (ENG)
write.csv(age_compile_8wk_2, row.names=F, "age_7d_period_8wk_eng.csv")

##RACE/ETHNICITY PER 100,000
## RACE/ETHNICITY
## LOAD DATA
df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/cedrs_Summit.txt'); dim(df)
##############################################################################################
## CONVERT DATA CLASS TO DATA.FRAME
df <- as.data.frame(df)
## SUBSET ONLY COUNTY == SUMMIT (CLEANS OUT POTENTIAL ERROR FROM FORMATTING)
df <- subset(df, county=="SUMMIT"); dim(df)
## FILTER OUT DUPLICATE ENTRIES
df$unique_id <- as.integer(factor(with(df, paste(profileid, first_name, last_name, date_of_birth))))
df<- subset(df, !duplicated(unique_id))
## COUNT VARIABLE
df$index_measure=1
## ADD IN MISSING DUMMY DATES 
total_dates <- data.frame(reporteddate=seq(min(df$reporteddate),max(df$reporteddate),1))
## CREATE RACE/ETHNICITY CATEGORIES
df$race_ethnic = 0
df$race_ethnic[which(df$single_race_ethnicity_with_ciis=="White - Non Hispanic")] <- "White"
df$race_ethnic[which(df$single_race_ethnicity_with_ciis=="Hispanic, All Races")] <- "Hispanic/Latinx"
df$race_ethnic[which(df$single_race_ethnicity_with_ciis=="Unknown")] <- "Unknown"
df$race_ethnic[which(df$single_race_ethnicity_with_ciis!="Hispanic, All Races" & df$single_race_ethnicity_with_ciis!="White - Non Hispanic"& df$single_race_ethnicity_with_ciis!="Unknown")] <- "Other"
## MERGE DUMMY DATE DF with total case DF
df <- merge(x= total_dates, y=df,by="reporteddate",all.x=TRUE)
df$index_measure[is.na(df$index_measure)] <- 0
## SUMMARIZE BY RACE/ETHNICITY
race_daily <- data.frame(group_by(df, race_ethnic, reporteddate) %>% 
                           summarize(sum_cases = sum(index_measure=="1")))
##############################################################################################
## ----------------------------------------------------------------------
## DO SOME PRE-MAGIC 
race_daily$sort_ID <- as.integer(factor(with(race_daily, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
race_daily <- arrange(race_daily, -sort_ID); head(race_daily)
## PULL OUT SELECT NEEDED VARIABLES 
date_ID <- race_daily[,c("reporteddate", "sort_ID")]
date_ID <- date_ID[!duplicated(date_ID[1]),]
colnames(date_ID)[2]<-"ID"
## ----------------------------------------------------------------------
## WHITE 
race_white <- subset(race_daily, race_ethnic=="White"); dim(race_white)
race_white <- merge(x= total_dates, y=race_white,by="reporteddate",all.x=TRUE); dim(race_white)
race_white$race_ethnic = "White"
race_white$sum_cases[is.na(race_white$sum_cases)] <- 0

race_white$sort_ID <- as.integer(factor(with(race_white, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
race_white <- arrange(race_white, -sort_ID); head(race_white)
## 7-DAY MOVING SUM
race_white <- race_white %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) # requires package zoom
## POPULATION FACTOR
race_white$pos_case_7d <- race_white$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
race_white_8wk <- arrange(race_white, -sort_ID); head(race_white_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
race_white_8wk <- head(race_white_8wk, 56); dim(race_white_8wk)
## ----------------------------------------------------------------------
## LATINX/HISPANIC 
race_hispanic <- subset(race_daily, race_ethnic=="Hispanic/Latinx"); dim(race_hispanic)
race_hispanic <- merge(x= total_dates, y=race_hispanic,by="reporteddate",all.x=TRUE); dim(race_hispanic)
race_hispanic$race_ethnic = "Hispanic/Latinx"
race_hispanic$sum_cases[is.na(race_hispanic$sum_cases)] <- 0

race_hispanic$sort_ID <- as.integer(factor(with(race_hispanic, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
race_hispanic <- arrange(race_hispanic, -sort_ID); head(race_hispanic)
## 7-DAY MOVING SUM
race_hispanic <- race_hispanic %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) # requires package zoom
## POPULATION FACTOR
race_hispanic$pos_case_7d <- race_hispanic$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
race_hispanic_8wk <- arrange(race_hispanic, -sort_ID); head(race_hispanic_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
race_hispanic_8wk <- head(race_hispanic_8wk, 56); dim(race_hispanic_8wk)
## ----------------------------------------------------------------------
## OTHER 
race_other <- subset(race_daily, race_ethnic=="Other"); dim(race_other)
race_other <- merge(x= total_dates, y=race_other,by="reporteddate",all.x=TRUE); dim(race_other)
race_other$race_ethnic = "Other"
race_other$sum_cases[is.na(race_other$sum_cases)] <- 0

race_other$sort_ID <- as.integer(factor(with(race_other, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
race_other <- arrange(race_other, -sort_ID); head(race_other)
## 7-DAY MOVING SUM
race_other <- race_other %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) # requires package zoom
## POPULATION FACTOR
race_other$pos_case_7d <- race_other$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
race_other_8wk <- arrange(race_other, -sort_ID); head(race_other_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
race_other_8wk <- head(race_other_8wk, 56); dim(race_other_8wk)
##################################################################################################
## UNKNOWN RACE/ETHNICITY 
race_unknown <- subset(race_daily, race_ethnic=="Unknown"); dim(race_unknown)
race_unknown <- merge(x= total_dates, y=race_unknown,by="reporteddate",all.x=TRUE); dim(race_unknown)
race_unknown$race_ethnic = "Unknown"
race_unknown$sum_cases[is.na(race_unknown$sum_cases)] <- 0

race_unknown$sort_ID <- as.integer(factor(with(race_unknown, paste(reporteddate))))
## SORT BY PROXY TEMP_ID: NEWEST DATE FIRST
race_unknown <- arrange(race_unknown, -sort_ID); head(race_unknown)
## 7-DAY MOVING SUM
race_unknown <- race_unknown %>%
  arrange(-sort_ID,reporteddate ) %>%
  mutate(rollapply_sum =zoo::rollapplyr(sum_cases , 7, sum, partial = TRUE)) # requires package zoom
## POPULATION FACTOR
race_unknown$pos_case_7d <- race_unknown$rollapply_sum*(100000/30974)
## SUBSET 8-WEEK DATA
race_unknown_8wk <- arrange(race_unknown, -sort_ID); head(race_unknown_8wk)
## SELECT FIRST 56 ROWS (8WKS OF DATA)
race_unknown_8wk <- head(race_unknown_8wk, 56); dim(race_unknown_8wk)


##################################################################################################
## COMPILE RACE/ETHNICITY - HISTROICAL DATA
compile_race <- rbind(race_other, race_unknown, race_hispanic, race_white); dim(compile_race)
## SELECT MEANINGFUL VARIABLES
compile_race <- compile_race[,c("reporteddate","race_ethnic","pos_case_7d")]
compile_race2 <- spread(compile_race, race_ethnic, pos_case_7d, fill = 0)
## CHANGE VARIABLE NAMES
colnames(compile_race2)[1] <- "Date"

## WRITE CSV (ENG)
write.csv(compile_race2, row.names=F, "race_7d_historical_eng.csv")
##
compile_race2$sort_ID <- as.integer(factor(with(compile_race2, paste(Date))))
## SORT BY TEMP ID
compile_race2 <- arrange(compile_race2, -sort_ID); head(compile_race2)
compile_race2_sub <- head(compile_race2, 44); dim(compile_race2_sub)
compile_race2_sub$sort_ID= NULL
compile_race2_sub$sort_ID = NULL
compile_race2_sub<- tail(compile_race2_sub, -7)

write.csv(compile_race2_sub, row.names=F, "race_7d_period_30day_eng.csv")
###########################################################################


##################################################################################################
## COMPILE RACE/ETHNICITY - 8 WEEK DATA
compile_race_8wk <- rbind(race_other_8wk, race_hispanic_8wk, race_white_8wk, race_unknown_8wk); dim(compile_race_8wk)
## SELECT MEANINGFUL VARIABLES
compile_race_8wk <- compile_race_8wk[,c("reporteddate","race_ethnic","pos_case_7d")]
compile_race_8wk_2 <- spread(compile_race_8wk, race_ethnic, pos_case_7d, fill = 0)
## CHANGE VARIABLE NAMES
colnames(compile_race_8wk_2)[1] <- "Date"

## WRITE CSV (ENG)
write.csv(compile_race_8wk_2, row.names=F, "race_7d_period_8wk_eng.csv")


## WRITE CSV (ESP)
compile_race_historical_esp <- compile_race2
colnames(compile_race_historical_esp)[1]<- "Fecha"
colnames(compile_race_historical_esp)[2]<- "hispano/latinx"
colnames(compile_race_historical_esp)[3]<- "otro"
colnames(compile_race_historical_esp)[4]<- "desconocido"
colnames(compile_race_historical_esp)[5]<- "blanco"

compile_race_historical_esp$sort_ID = NULL
write.csv(compile_race_historical_esp, row.names=F, "race_7d_historical_esp.csv")
#####################################
compile_race_historical_esp$sort_ID <- as.integer(factor(with(compile_race_historical_esp, paste(Fecha))))
## SORT BY TEMP ID
compile_race_historical_esp <- arrange(compile_race_historical_esp, -sort_ID); head(compile_race_historical_esp)
compile_race_sub3 <- head(compile_race_historical_esp, 44)
compile_race_sub3$sort_ID= NULL
compile_race_historical_esp$sort_ID = NULL
compile_race_sub3<- tail(compile_race_sub3, -7)
write.csv(compile_race_sub3, row.names=F, "race_7d_period_30day_esp.csv")

##CREATE TABLE OF MOST RECENT ONE-DAY SUMMARY

## LOAD DATA
df_elr <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/elr_rollup_Summit.txt'); dim(df)

## SORT BY NEWEST DATES
## CREATE PROXY DATE INTEGER
df_elr$sort_ID <- as.integer(factor(with(df_elr, paste(dateadded))))

df_elr <- arrange(df_elr, -sort_ID); head(df_elr)
max_date <- max(df_elr$dateadded)
df_elr2 <- subset(df_elr, dateadded==max_date); dim(df_elr2)
df_elr3 <- subset(df_elr2, test_type=="PCR"); dim(df_elr3)
df_elr4 <- subset(df_elr3, !duplicated(total_tests));dim(df_elr4)
df_elr4 <- df_elr4[,c("total_tests")]
total_tests_recent_day <- df_elr4$total_tests
##########################################################################
## CASE DATA SUMMARY
case_df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/cedrs_Summit.txt'); dim(df)

## FILTER OUT DUPLICATE ENTRIES
#case_df$unique_id <- as.integer(factor(with(df, paste(profileid, first_name, last_name, date_of_birth))))
#case_df<- subset(case_df, !duplicated(unique_id))

## PULL OUT DEATH DF FOR CTT 
##total_deaths <- subset(case_df, outcome=="Patient died"); dim(total_deaths)
##total_deaths <- total_deaths[,c("profileid", "eventid", "first_name", "last_name", "outcome", "deathdate","reporteddate", "collectiondate")]; dim(df_death)
##write.csv(total_deaths, "SCPH_deaths_reported12march.csv", row.names=F)

case_df %>% 
  mutate(ID = group_indices_(case_df, .dots=c("profileid", "first_name", "last_name", "date_of_birth"))) 


df_death <- subset(case_df, outcome=="Patient died"); dim(df_death)
df_death <- df_death[,c("outcome", "deathdate")]; dim(df_death)
df_death$reporteddate <- df_death$deathdate
df_death$deathdate <- NULL
df_death$death_count = 1

## SUBSET OUT TOTAL AND DEATH DF'S == max_date
df_death <- subset(df_death, reporteddate==max_date)
death_sum = sum(df_death$death_count)
case_df2 <- subset(case_df, reporteddate==max_date);dim(case_df2)

hosp_day <- read.csv("hosp_df_temp.csv", header=T)
hosp_day$Date <- as.IDate(hosp_day$Date)
hosp_recent <- tail(hosp_day, 1)
colnames(hosp_recent)[2]<-"hosp_count"
hosp_day_total = hosp_recent$hosp_count


## MEGA DF
day_summary <- data.frame(group_by(case_df2, reporteddate) %>% 
                            summarize(pos_confirm_sum = sum(casestatus=="confirmed"),
                                      prob_sum = sum(casestatus=="probable"),
                                      total_pos_case_sum = sum(pos_confirm_sum,prob_sum)))

day_summary <- subset(day_summary, reporteddate==max_date)

day_summary2 <- cbind(day_summary,death_sum,hosp_day_total,death_sum, total_tests_recent_day)
day_summary2 <- day_summary2[,c("reporteddate","hosp_day_total", "death_sum", "pos_confirm_sum", "prob_sum","total_pos_case_sum", "total_tests_recent_day")]



## CLEAN UP TABLE NAMES
colnames(day_summary2)[1]<-"Reported Date"
colnames(day_summary2)[2]<-"Hospital Admissions"
colnames(day_summary2)[3]<-"Deaths"
colnames(day_summary2)[4]<-"Confirmed positive tests"
colnames(day_summary2)[5]<-"Probables"
colnames(day_summary2)[6]<-"Total Cases"
colnames(day_summary2)[7]<-"Total Tests"
## WRITE CSV
write.csv(day_summary2, "SCPH_Daily_Summary_Report.csv", row.names = F)

##TESTING BY SOURCE SUMMARY
df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/elr_tests_Summit.txt'); dim(df)
df <- as.data.frame(df)
## REMOVE ALL NAs
df$resultdate <- as.character(df$resultdate)
df <- subset(df, !resultdate=="")
df <- df[!is.na(df$resultdate),]

df<- subset(df, county=="SUMMIT")
## FILTER ONLY PCR
df<- subset(df, test_type=="PCR")


## FILTER OUT DUPLICATE ENTRIES
df$unique_id <- as.integer(factor(with(df, paste(first_name, last_name, date_of_birth, dateadded))))
df<- subset(df, !duplicated(unique_id))
## FILTER OUT ONLY SUMMIT
#df$dateadded <- as.character(df$dateadded)
#test_check <- subset(df, dateadded=="2020-11-22");dim(test_check) ## all checks out!!


########################################################
## SOURCE = 0 | garbage
## SOURCE  = 1 | CENTURA
## SOURCE = 2 | VAIL

df$source = 0
## CENTURA
df$source[which(df$submitter=="ST ANTHONY SUMMIT MEDICAL CENTER")] <- "1"
df$source[which(df$submitter=="CENTURA HEALTHCARE")] <- "1"
df$source[which(df$submitter=="CENTURA HEALTH DEFAULT")] <- "1"
df$source[which(df$submitter=="CHPG HIGH COUNTRY HEALTHCARE")] <- "1"
df$source[which(df$submitter=="CHPG COVID MOBILE TESTING")] <- "1"
df$source[which(df$submitter=="CMM MAIN")] <- "1"
df$source[which(df$submitter=="ST ANTHONY HOSPITAL")] <- "1"
df$source[which(df$submitter=="ST ANTHONY SUMMIT MEDICAL CTR")] <- "1"
df$source[which(df$submitter=="WORKSAFE CENTURA")] <- "1"
df$source[which(df$submitter=="CENTURA LABORATORY SERVICES")] <- "1"
df$source[which(df$submitter=="LABCORP.COM SEROLOGY TESTING")] <- "1"
df$source[which(df$submitter=="HIGH COUNTRY SUMMIT OB")] <- "1"
df$source[which(df$submitter=="CENTURA OCC MED RED PACKET")] <- "1"
df$source[which(df$submitter=="PORTER ADVENTIST HOSPITAL")] <- "1"
df$source[which(df$submitter=="AVON EMERGENCY AND URGENT CARE CENTER")] <- "1"
df$source[which(df$submitter=="SUMMIT CARDIOLOGY")] <- "1"
df$source[which(df$submitter=="CHPG HIGH COUNTRY PRIMARY CARE FRISCO")] <- "1"
df$source[which(df$submitter=="CHPG HIGH COUNTRY PRIMARY CARE SILVERTHORNE")] <- "1"
df$source[which(df$submitter=="PARKER ADVENTIST HOSPITAL")] <- "1"
df$source[which(df$submitter=="CHPG HIGH COUNTRY OBGYN")] <- "1"
df$source[which(df$submitter=="CHPG HIGH COUNTRY PRIMARY CARE BRECKENRIDGE")] <- "1"
df$source[which(df$submitter=="CHPG PRIM CARE SOUTHMOOR")] <- "1"
df$source[which(df$submitter=="CHPG PRIMARY CARE SOUTHMOOR")] <- "1"
df$source[which(df$submitter=="OCCMED-#0620-CO-LAKEWOOD")] <- "1"
df$source[which(df$submitter=="AVISTA ADVENTIST HOSPITAL")] <- "1"
df$source[which(df$submitter=="BRECKENRIDGE MEDICAL CENTER")] <- "1"
df$source[which(df$submitter=="CENTRA HEALTH")] <- "1"
df$source[which(df$submitter=="CENTURA NEURO CONCUSSION")] <- "1"
df$source[which(df$submitter=="CHPG HIGH COUNTRY WOMEN'S HEALTH FRISCO")] <- "1"
df$source[which(df$submitter=="CHPG PRIMARY CARE HIGHLANDS")] <- "1"
df$source[which(df$submitter=="CHPG PRIMARY CARE MERCY")] <- "1"
df$source[which(df$submitter=="MIDDLE PK MED KREMM CPU")] <- "1"
df$source[which(df$submitter=="SAINT ANTHONY CENTRAL (CENTURA)")] <- "1"
##########################################################################################################
### VAIL HEALTH
df$source[which(df$submitter=="VAIL HEALTH HOSPITAL")] <- "2"
df$source[which(df$submitter=="VAIL VALLEY MED CTR LAB I/F")] <- "2"
df$source[which(df$submitter=="COLORADO MOUNTAIN MEDICAL-AVON/EDWA")] <- "2"
df$source[which(df$submitter=="COLORADO MOUNTAIN MEDICAL-VAIL")] <- "2"
df$source[which(df$submitter=="VAIL VALLEY MEDICAL CENTER EMERGENCY")] <- "2"
df$source[which(df$submitter=="CMM-AVON")] <- "2"
df$source[which(df$submitter=="CMM-EAGLE")] <- "2"
df$source[which(df$submitter=="COLORADO MOUNTAIN MEDICAL")] <- "2"
df$source[which(df$submitter=="CDH-SILVERTHORNE REC CENTER")] <- "2"
df$source[which(df$submitter=="VAIL INTEGRATIVE MEDICAL")] <- "2"

##########################################################################################################
## SUBSET ONLY CENTURA & VAIL
df <- subset(df, source == "1" | source == "2")

## SUMMARIZE TESTING BY SOURCE

daily_test_source <- data.frame(group_by(df, collectiondate) %>% 
                                  summarize(centura_sum = sum(source=="1"),
                                            vail_sum = sum(source=="2")))

## CREATE TEMP ID TO FILTER OUT DATES UP TO 2020-03-13
daily_test_source$sort_ID <- as.integer(factor(with(daily_test_source, paste(collectiondate))))
## FILTER OUT DATES BEFORE 13-MARCH-2020
daily_test_source <- arrange(daily_test_source, -sort_ID); head(daily_test_source)
daily_test_source$sort_ID = NULL

## SUBSET MAKO LAB DATA
mako <- subset(df, sender=="MAKO MEDICAL LABORATORIES" & submitter=="CDH-SILVERTHORNE REC CENTER");dim(mako)
mako$freq = 1
mako$collectiondate<- as.Date(mako$collectiondate)

daily_test_mako <- data.frame(group_by(mako, collectiondate) %>% 
                                summarize(Sum_Mako_Silverthorne_Tests = sum(freq)))


total_tests <- merge(x=daily_test_source,y=daily_test_mako, by=c("collectiondate"), all.x=T); dim(total_tests)

is.na(total_tests$Sum_Mako_Silverthorne_Tests) <- 0

total_tests$Sum_Mako_Silverthorne_Tests[is.na(total_tests$Sum_Mako_Silverthorne_Tests)] <- 0

total_tests$sort_ID <- as.integer(factor(with(total_tests, paste(collectiondate))))
total_tests<- arrange(total_tests, -sort_ID); head(total_tests)
total_tests$sort_ID= NULL
## RENAME VARIABLES
colnames(total_tests)[1]<-"Collection_Date"
colnames(total_tests)[2]<-"Centura_Sum"
colnames(total_tests)[3]<-"Vail_Health_Sum"
colnames(total_tests)[4]<-"Mako_Silverthorne_Rec_Sum"

## WRITE CSV
write.csv(total_tests, "Summit_Testing_Source_Daily_Summary.csv", row.names=F)


##SASMC HOSPITAL DATA FROM GOOGLE SHEETS
sasmc <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1cuJdqAzx_kh6v8zkmDlhsV9SDU7wSO-9QHH7grKGuwE/edit?usp=sharing')
sasmc <- as.data.frame(sasmc)
sasmc <- sasmc[,c("date","covid_pac","covid_pac_trans")]

## FORMAT DATE: YYYY-MM-DD - In case need to convert date format
sasmc$date <- format(as.Date(sasmc$date, "%m/%d/%Y"), "%Y-%m-%d")

colnames(sasmc)[1]<-"Date"
colnames(sasmc)[2]<-"COVID-19 Patients"
colnames(sasmc)[3]<-"COVID-19 Patient Transfers"

write.csv(sasmc, "sasmc_historical_eng.csv", row.names=F)

## SASMC DATA (ESP)
sasmc_esp <- sasmc
colnames(sasmc_esp)[1]<-"Fecha"
colnames(sasmc_esp)[2]<-"Paciente COVID-19"
colnames(sasmc_esp)[3]<-"Traslados de pacientes COVID-19"
write.csv(sasmc_esp, "sasmc_historical_esp.csv", row.names=F)
## 30-DAY PERIOD (ENG)
sasmc30d <- sasmc
## CREATE TEMP ID TO FILTER OUT DATES UP TO 2020-03-13
sasmc30d$Date <- as.character(sasmc30d$Date)
sasmc30d$sort_ID <- as.integer(factor(with(sasmc30d, paste(Date))))
## FILTER OUT DATES BEFORE 13-MARCH-2020
sasmc30d <- arrange(sasmc30d, -sort_ID); head(sasmc30d)
sasmc30d <- head(sasmc30d, 30); dim(sasmc30d)
sasmc30d$sort_ID = NULL
## WRITE CSV (ENG)
write.csv(sasmc30d, "sasmc_30d_eng.csv", row.names=F)
## WRITE CSV (ESP)
sasmc30d_esp <- sasmc30d
colnames(sasmc30d_esp)[1]<-"Fecha"
colnames(sasmc30d_esp)[2]<-"Paciente COVID-19"
colnames(sasmc30d_esp)[3]<-"Traslados de pacientes COVID-19"
## WRITE CSV (ESP)
write.csv(sasmc30d_esp, "sasmc_30d_esp.csv", row.names=F)
#########################################################################################################################
## VACCINE DATA DASHBOARD
df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/PatientImmunizations_Summit.txt', encoding="unknown"); dim(df)

df <- as.data.frame(df)
#############################################################################################################################
## FILTER ROW DUPLICATES
df$DUP_ROW_FILTER_STRING <- as.integer(factor(with(df, paste(patient_first_name, patient_last_name, patient_dob,vaccination_date, clinic_id, clinic_desc, dosage_num, vaccination_code, clinic_county, gender_code, age_at_1stvaccination, race_ethnicity, patient_street_number, patient_street_prefix,patient_street_name,patient_street_name,patient_street_suffix,patient_address,patient_city))))
df <- subset(df, !duplicated(DUP_ROW_FILTER_STRING))
#############################################################################################################################
## FILTER BY SUMMIT COUNTY
df <- subset(df, patient_county=="SUMMIT" | patient_county=="Summit")
#############################################################################################################################
## REVIEW >2 DOSES
dosage_suprluss <- subset(df, dosage_num=="3" | dosage_num=="4")
ds <- subset(dosage_suprluss, !duplicated(patient_id))
ds$extra_doses_given = 1
ds_id <- ds[,c("patient_id", "extra_doses_given")]

dfc <- merge(x=df, y=ds_id, by=c("patient_id"));dim(dfc)
#write.csv(dfc, "total_vaccine_surplus.csv", row.names=F)
#write.csv(dosage_suprluss, "dosage_suprluss.csv", row.names=F)
#############################################################################################################################
df$age_65plus = 0
df$age_65plus[which(df$age_at_1stvaccination>= 65)] <- "1"
##############################################################################################################################
## AGE CLASS
## ASSIGN AGE CLASS VARIABLE IN BINS OF 10 YEARS
df$age_class = 0
#df$age_class[which(df$age_at_1stvaccination < 10)] <- "0-9"
#df$age_class[which(df$age_at_1stvaccination > 9 & df$age_at_1stvaccination < 20)] <- "10-19"
#df$age_class[which(df$age_at_1stvaccination < 16)] <- "0-15"
df$age_class[which(df$age_at_1stvaccination > 12)] <- "0-11"
df$age_class[which(df$age_at_1stvaccination > 11 & df$age_at_1stvaccination < 16)] <- "12-15"
df$age_class[which(df$age_at_1stvaccination > 15 & df$age_at_1stvaccination < 20)] <- "16-19"
df$age_class[which(df$age_at_1stvaccination > 19 & df$age_at_1stvaccination < 30)] <- "20-29"
df$age_class[which(df$age_at_1stvaccination > 29 & df$age_at_1stvaccination < 40)] <- "30-39"
df$age_class[which(df$age_at_1stvaccination > 39 & df$age_at_1stvaccination < 50)] <- "40-49"
df$age_class[which(df$age_at_1stvaccination > 49 & df$age_at_1stvaccination < 60)] <- "50-59"
df$age_class[which(df$age_at_1stvaccination > 59 & df$age_at_1stvaccination < 70)] <- "60-69"
df$age_class[which(df$age_at_1stvaccination > 69 & df$age_at_1stvaccination < 80)] <- "70-79"
#df$age_class[which(df$age_at_1stvaccination >= 70)] <- "70+"
df$age_class[which(df$age_at_1stvaccination >= 80)] <- "80+"
#############################################################################################################################
## MAKE SPECIAL AGE CATEGORY FOR SHS (16-18)
df$high_school=0
df$high_school[which(df$age_at_1stvaccination > 11 & df$age_at_1stvaccination < 16)] <- "1"


#############################################################################################################################
## VACCINE ETHICS ANALYSIS
#df$youth = 0
#df$youth[which(df$age_at_1stvaccination < 20)] <- "1"
#youth_df <-subset(df, youth=="1")
#youth_df2 <- subset(youth_df, sd_p=="0" & dd_p=="0")

#############################################################################################################################
## FREQ VARIABLE
df$dose_count =1
## SUMMIT COUNTY 2018 ADULT POPULATION (>18)
#AP = 26350
## SUMMIT COUNTY 2018 ADULT POPULATION (>16)
#AP = 26837
## SUMMIT COUNTY 2019 ADULT POPULATION (12+)
AP = 28059

#############################################################################################################################
## VACCINE COVERAGE
df$vaccine_level = 0
df$vaccine_level[which(df$vaccination_code =="COVID-19 Vector-NR (JSN)" | df$dosage_num=="2")] <- "1"
## 1 DOSE PH
df$sd_p=0
df$sd_p[which(df$vaccination_code=="COVID-19 mRNA (PFR)" & df$dosage_num == "1")] <- "1"
## 2 DOSE PH
df$dd_p=0
df$dd_p[which(df$vaccination_code=="COVID-19 mRNA (PFR)" & df$dosage_num == "2")] <- "1"
## 1 DOSE MO
df$sd_m=0
df$sd_m[which(df$vaccination_code=="COVID-19 mRNA (MOD)" & df$dosage_num == "1")] <- "1"
## 2 DOSE MO
df$dd_m=0
df$dd_m[which(df$vaccination_code=="COVID-19 mRNA (MOD)" & df$dosage_num == "2")] <- "1"
## 1 DOSE JJ
df$sd_j=0
df$sd_j[which(df$vaccination_code=="COVID-19 Vector-NR (JSN)" & df$dosage_num == "1")] <- "1"
#############################################################################################################################
rep_fv <- subset(df, sd_j=="1" | dd_m=="1" | dd_p=="1");dim(rep_fv)
rep_fv$id_code <- with(rep_fv, paste0(patient_first_name, patient_last_name, patient_dob))
rep_fv2 <- subset(rep_fv, !duplicated(id_code));dim(rep_fv2)



#############################################################################################################################
## SUMMARIZE TOTAL DOSES
dose_sum <- as.data.frame(group_by(df,) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/AP*100,
                                      vaccinated_perc = (sum_full/AP)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))
#############################################################################################################################
## SUMMARIZE BY HIGH SCHOOL (N=716)
shs_df <- subset(df, high_school=="1")
## 2018 CENSUS POPULATION
#AP_1618= 716
## 2019 CENSUS POPULATION
AP_1618= 728
#age18 <- df$high_school[which(df$age_at_1stvaccination > 15 & df$age_at_1stvaccination < 19)] <- "1"
#age18 <- subset(df, age_at_1stvaccination < 18 & age_at_1stvaccination > 15)
shs_summary <- as.data.frame(group_by(shs_df,) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/AP_1618*100,
                                      vaccinated_perc = (sum_full/AP_1618)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))
#SIG FIGS
shs_summary$partial_perc =  signif(shs_summary$partial_perc, digits=3)
shs_summary$vaccinated_perc =  signif(shs_summary$vaccinated_perc, digits=3)
shs_summary$atl1 =  signif(shs_summary$atl1, digits=3)

## SHS WEBSITE FIGURE
wb_shs <- shs_summary[,c(12,11,13)]
## RENAME VARIABLES
colnames(wb_shs)[1]<-"Fully Vaccinated"
colnames(wb_shs)[2]<-"Partially Vaccinated (awaiting second dose)"
colnames(wb_shs)[3]<-"At least 1 Vaccine Dose"

write.csv(wb_shs, "vaccine_prop_16_18_eng.csv", row.names=F)


## RENAME NAMES
shs_summary2 <- shs_summary[,c(1:7,10:13)]
colnames(shs_summary2)[1]<-"SUM_FIRST_DOSE_ONLY_PFIZER"
colnames(shs_summary2)[2]<-"SUM_SECOND_DOSE_ONLY_PFIZER"
colnames(shs_summary2)[3]<-"SUM_FRIST_DOSE_ONLY_MODERNA"
colnames(shs_summary2)[4]<-"SUM_SECOND_DOSE_ONLY_MODERNA"
colnames(shs_summary2)[5]<-"SUM_J&J"
colnames(shs_summary2)[6]<-"TOTAL_DOSES_ADMINISTERED"
colnames(shs_summary2)[7]<-"TOTAL_FULLY_VACCINATED_RESIDENTS"
colnames(shs_summary2)[8]<-"TOTAL_PARTIALLY_VACCINATED_RESIDENTS"
colnames(shs_summary2)[9]<-"PERCENTAGE_PARTIALLY_VACCINATED_RESIDENTS"
colnames(shs_summary2)[10]<-"PERCENTAGE_FULLY_VACCINATED_RESIDENTS"
colnames(shs_summary2)[11]<-"PERCENTAGE_AT_LEAST_1_DOSE"

write.csv(shs_summary2, "SHS_Vaccine_Summary.csv", row.names=F)

#############################################################################################################################
## SUMMARY BY VACCINE CLINIC
clinic_daily_sum <- as.data.frame(group_by(df,clinic_desc,vaccination_date) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_1st = sum(sum_sd_p, sum_sd_m),
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/AP*100,
                                      vaccinated_perc = (sum_full/AP)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))

clinic_daily_sum2 <- subset(clinic_daily_sum, clinic_desc=="SUMMIT COUNTY PUBLIC HEALTH")



clinic_daily_sum2$date<-clinic_daily_sum2$vaccination_date

clinic_daily_sum2$date <- format(as.Date(clinic_daily_sum2$date, "%m/%d/%Y"), "%Y-%m-%d")
clinic_daily_sum2$date <- as.Date(clinic_daily_sum2$date)

#clinic_daily_sum2$date<-as.Date(clinic_daily_sum2$date)

#clinic_daily_sum2$sort_ID <- as.integer(factor(with(clinic_daily_sum2, paste(vaccination_date))))
clinic_daily_sum3 <- arrange(clinic_daily_sum2, date); head(clinic_daily_sum3)

clinic_daily_sum4 <- tail(clinic_daily_sum3, 7)

## CHANGE NAMES
clinic_daily_sum5 <- clinic_daily_sum4[,c(1:9,12)]
colnames(clinic_daily_sum5)[1]<-"CLINIC_NAME"
colnames(clinic_daily_sum5)[2]<-"VACCINATION_DATE"

colnames(clinic_daily_sum5)[3]<-"FIRST_DOSES_PFIZER"
colnames(clinic_daily_sum5)[4]<-"SECOND_DOSES_PFIZER"
colnames(clinic_daily_sum5)[5]<-"FIRST_DOSE_MODERNA"
colnames(clinic_daily_sum5)[6]<-"SECOND_DOSE_MODERNA"
colnames(clinic_daily_sum5)[7]<-"SINGLE_DOSE_J&J"
colnames(clinic_daily_sum5)[8]<-"TOTAL_DOSES_ADMINISTERED"
colnames(clinic_daily_sum5)[9]<-"TOTAL_FULL_VACCINES_ADMINISTERED"
colnames(clinic_daily_sum5)[10]<-"TOTAL_FIRST_DOESES(EXCLUDES_J&J)"

##write.csv(clinic_daily_sum5, "SCPH_DAILY_DOSES_26apr21.csv", row.names=F)



#############################################################################################################################
## PULL OUT SINGLE DOSE DF FOR ADMIN USE
df$sd_total = NULL
sd_total <- df
sd_total1<- subset(sd_total, sd_j=="0")
sd_total2<-subset(sd_total1, dd_m=="0" & dd_p=="0")
## SINGLE DOSE ID
sd_total2$id_code <- with(sd_total2, paste0(patient_first_name, patient_last_name, patient_dob))


dd_total <- subset(df, sd_j=="1" | dd_m=="1" |dd_p=="1")

dd_total$id_code <- with(dd_total, paste0(patient_first_name, patient_last_name, patient_dob))


only_single_dose <- subset(sd_total2, !(id_code %in% dd_total$id_code))
only_single_dose2 <- subset(only_single_dose, clinic_desc=="SUMMIT COUNTY PUBLIC HEALTH")
#write.csv(only_single_dose2, "only_single_dose_21April2021.csv", row.names=F)


#############################################################################################################################
## DAILY FULL VACCINE SUM 
daily_fv <- as.data.frame(group_by(df,vaccination_date) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/AP*100,
                                      vaccinated_perc = (sum_full/AP)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))
#############################################################################################################################
## VACCINE COVERAGE TREND GRAPH 
daily_fv2 <- daily_fv[,c(1,13,14)]

daily_fv2$date = daily_fv2$vaccination_date
daily_fv2$date <- format(as.Date(daily_fv2$date, "%m/%d/%Y"), "%Y-%m-%d")  
  
daily_fv2$sort_ID <- as.integer(factor(with(daily_fv2, paste(date))))
daily_fv2 <- arrange(daily_fv2, sort_ID); head(daily_fv2)

## CUMULATIVE SUM ACROSS ALL ROWS
daily_fv2$cum_vacc_perc <- 0
daily_fv2$cum_vacc_perc <- as.numeric(cumsum(daily_fv2[,2]))
daily_fv2$cum_atl1_perc <- 0
daily_fv2$cum_atl1_perc <- as.numeric(cumsum(daily_fv2[,3]))
## PULL OUT MEANINGFUL VARIABLES
daily_fv3 <- daily_fv2[,c(1,6,7)]
## RENAME VARIABLES
colnames(daily_fv3)[1]<-"Date"
colnames(daily_fv3)[2]<-"Fully Vaccinated (%)"
colnames(daily_fv3)[3]<-"At least 1 dose (%)"
daily_fv3$min =0
daily_fv3$Goal=75
colnames(daily_fv3)[5]<-"Goal (75%)"

## WRITE CSV (ENG)
write.csv(daily_fv3, "vaccine_trend_eng.csv", row.names=F)
## RENAME VARIABLES - ESP
daily_fv3_esp <- daily_fv3
colnames(daily_fv3_esp)[1]<-"Fecha"
colnames(daily_fv3_esp)[2]<-"Totalmente Vacunado (%)"
colnames(daily_fv3_esp)[3]<-"Al menos 1 Dosis de Vacuna (%)"
colnames(daily_fv3_esp)[5]<-"Meta (75%)"

## WRITE CSV (ESP)
write.csv(daily_fv3_esp, "vaccine_trend_esp.csv", row.names=F)
#############################################################################################################################
## SUMMARY REVIEW ON VACCINE RATES
daily_fv <- daily_fv[,c(1,8,7)]

n=7

daily_fv$date = daily_fv$vaccination_date
daily_fv$date <- format(as.Date(daily_fv$date, "%m/%d/%Y"), "%Y-%m-%d")


daily_fv$month =daily_fv$date
daily_fv$month <- format(as.Date(daily_fv$month, "%Y-%m-%d"), "%Y-%m")

## MONTHLY SUMMARY
monthly_fv <- as.data.frame(group_by(daily_fv,month) %>%
                            summarize(sum_fv = sum(sum_full),
                                      med= median(sum_full),
                                      max= max(sum_full), 
                                      min = min(sum_full),
                                      neab = mean(sum_full)))


daily_fv$sort_ID <- as.integer(factor(with(daily_fv, paste(date))))
daily_fv <- arrange(daily_fv, sort_ID); head(daily_fv)
daily_fv2 <- tail(daily_fv, -35)

d7_fv_sum <- rollapply(daily_fv2$sum_full, n, sum, by = n)
d7_total_dose_sum <- rollapply(daily_fv2$total_doses, n, sum, by = n)

pv <- read.csv("prop_vacc.csv", header=T)

## EXCLUDE FIRST 2 WEEKS
#daily_fv2 <- daily_fv2[-1]
#daily_fv2 <- daily_fv2[-1]

mean(daily_fv3)
median(daily_fv3)



#############################################################################################################################
## SUMMARIZE DOSES BY RACE/ETHNICITY
race_dose_sum <- as.data.frame(group_by(df,race_ethnicity) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/AP*100,
                                      vaccinated_perc = (sum_full/AP)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))

################################################################################################################
## 2018 CENSUS METRICS (16+) 
#W = 22805
#O = 770
#H = 3262
#U = 0
#A = 344
#AI = 79
#B = 347
## 2019 CENSUS METRICS (12+) 
W = 23748
O = 777
H = 3534
U = 0
A = 348
AI = 78
B = 351

################################################################################################################
SUM_FULL_TOTAL = sum(race_dose_sum$sum_full)
SUM_PARTIAL_TOTAL = sum(race_dose_sum$sum_partial)


## PULL OUT RACE/ETHNIC
## AMERICAN INDIAN
AI_sum <- subset(race_dose_sum, race_ethnicity=="American Indian or Alaskan Native - Non Hispanic")
AI_sum$partial_perc = AI_sum$sum_partial/AI*100
AI_sum$vaccinated_perc = AI_sum$sum_full/AI*100
AI_sum$atl1 = AI_sum$partial_perc+AI_sum$vaccinated_perc
AI_sum$prop_full = AI_sum$sum_full/SUM_FULL_TOTAL*100
AI_sum$prop_part = AI_sum$sum_partial/SUM_PARTIAL_TOTAL*100

## WHITE
W_sum <- subset(race_dose_sum, race_ethnicity=="White - Non Hispanic")
W_sum$partial_perc = W_sum$sum_partial/W*100
W_sum$vaccinated_perc = W_sum$sum_full/W*100
W_sum$atl1 = W_sum$partial_perc+W_sum$vaccinated_perc
W_sum$prop_full = W_sum$sum_full/SUM_FULL_TOTAL*100
W_sum$prop_part = W_sum$sum_partial/SUM_PARTIAL_TOTAL*100
## HISPANIC
H_sum <- subset(race_dose_sum, race_ethnicity=="Hispanic, All Races")
H_sum$partial_perc = H_sum$sum_partial/H*100
H_sum$vaccinated_perc = H_sum$sum_full/H*100
H_sum$atl1 = H_sum$partial_perc+H_sum$vaccinated_perc
H_sum$prop_full = H_sum$sum_full/SUM_FULL_TOTAL*100
H_sum$prop_part = H_sum$sum_partial/SUM_PARTIAL_TOTAL*100
## ASIAN
A_sum <- subset(race_dose_sum, race_ethnicity=="Asian - Non Hispanic")
A_sum$partial_perc = A_sum$sum_partial/A*100
A_sum$vaccinated_perc = A_sum$sum_full/A*100
A_sum$atl1 = A_sum$partial_perc+A_sum$vaccinated_perc
A_sum$prop_full = A_sum$sum_full/SUM_FULL_TOTAL*100
A_sum$prop_part = A_sum$sum_partial/SUM_PARTIAL_TOTAL*100
## BLACK
B_sum <- subset(race_dose_sum, race_ethnicity=="Black or African American - Non Hispanic")
B_sum$partial_perc = B_sum$sum_partial/B*100
B_sum$vaccinated_perc = B_sum$sum_full/B*100
B_sum$atl1 = B_sum$partial_perc+B_sum$vaccinated_perc
B_sum$prop_full = B_sum$sum_full/SUM_FULL_TOTAL*100
B_sum$prop_part = B_sum$sum_partial/SUM_PARTIAL_TOTAL*100

compile_race <- rbind(AI_sum, A_sum, B_sum, H_sum, W_sum)
census18race <- c(0.29, 1.28, 1.29, 12.15, 84.98)
compile_race <- cbind(compile_race, census18race)
#############################################################################################################################
## SELECT MEANINGFUL VARIABLES
compile_race2 <- compile_race[,c(1:6,12:17)]

## CLEAN UP SIG FIGS
compile_race2$partial_perc =  signif(compile_race2$partial_perc, digits=3)
compile_race2$vaccinated_perc =  signif(compile_race2$vaccinated_perc, digits=3)
compile_race2$atl1 =  signif(compile_race2$atl1, digits=3)
compile_race2$prop_full =  signif(compile_race2$prop_full, digits=3)
compile_race2$prop_part  =  signif(compile_race2$prop_part , digits=3)

## CLEAN UP VARIABLE NAMES
colnames(compile_race2)[1]<- "Race/Ethnicity"
colnames(compile_race2)[2]<- "Total 1-dose Pfizer"
colnames(compile_race2)[3]<- "Total 2-dose Pfizer"
colnames(compile_race2)[4]<- "Total 1-dose Moderna"
colnames(compile_race2)[5]<- "Total 2-dose Moderna"
colnames(compile_race2)[6]<- "Total 1-dose Janssen"
colnames(compile_race2)[7]<- "Partially Vaccinated (%)"
colnames(compile_race2)[8]<- "Fully Vaccinated (%)"
colnames(compile_race2)[9]<- "At least 1-dose Vaccinated (%)"
colnames(compile_race2)[10]<- "Proportion Among Fully Vaccinated (%)"
colnames(compile_race2)[11]<- "Proportion Among Partially Vaccinated (%)"
colnames(compile_race2)[12]<- "Proportion Race/Ethnicity by 2019 Census (%)"

## WRITE.CSV 
write.csv(compile_race2, "SCPH_Race_Ethnicity_Dose_Percentage.csv", row.names=F)

#############################################################################################################################
VP = dose_sum$vaccinated_perc
PP = dose_sum$partial_perc 

## ADD NEEDED CHARACTERS
dose_sum$lp <- "("
dose_sum$rp <- ")"
dose_sum$sp <- " "
dose_sum$perc <- "%"

## SET VACCINE PROPORTIONS TO CORRECT SF'S
dose_sum$vaccinated_perc <- signif(dose_sum$vaccinated_perc, digits=3)
dose_sum$partial_perc <- signif(dose_sum$partial_perc, digits=3)
## VACCINATED %
dose_sum$vaccine_perc <- with(dose_sum, paste0(sum_full, sp, lp, vaccinated_perc, perc, rp))
## PARTIAL %
dose_sum$partial_prop <- with(dose_sum, paste0(sum_partial, sp, lp, partial_perc, perc, rp))
## PULL OUT MEANINGFUL VARIABLES
dose_table <- dose_sum[,c("vaccine_perc", "partial_prop", "total_doses")]
dose_table$Type=1
#############################################################################################################################
## (1) TOTAL % TABLE
tp <- dose_sum[,c("vaccinated_perc", "partial_perc")]
tp$at_least <- tp$vaccinated_perc + tp$partial_perc
tp$Type=1
tp_esp <- tp
## RENAME VARIABLES
colnames(tp)[1]<-"Fully Vaccinated"
colnames(tp)[2]<-"Partially Vaccinated (awaiting second dose)"
colnames(tp)[3]<-"At least 1 Vaccine Dose"
## TRANSFORM DATA
tp <-gather(tp, "Type")
## RENAME VARIABLES
colnames(tp)[1]<-"Level of Vaccine Coverage"
colnames(tp)[2]<-"Percentage"
## WRITE CSV (ENG)
write.csv(tp, "vaccine_table_coverage_eng.csv", row.names=F)
#############################################################################################################################
## (1) TOTAL % TABLE (ESP)
## RENAME VARIABLES
colnames(tp_esp)[1]<-"Totalmente Vacunado"
colnames(tp_esp)[2]<-"Parcialmente Vacunado (esperando la segunda dosis)"
colnames(tp_esp)[3]<-"Al menos 1 Dosis de Vacuna"
## TRANSFORM DATA
tp_esp <-gather(tp_esp, "Type")
## RENAME VARIABLES
colnames(tp_esp)[1]<-"Nivel de Cobertura de la Vacuna"
colnames(tp_esp)[2]<-"Porcentaje de Cobertura de la Vacuna"
## WRITE CSV (ENG)
write.csv(tp_esp, "vaccine_table_coverage_esp.csv", row.names=F)
#############################################################################################################################
## CHANGE COLUMN NAMES
colnames(dose_table)[1]<-"Fully Vaccinated (%)"
colnames(dose_table)[2]<-"Partially Vaccinated (%) (Awaiting second dose)"
colnames(dose_table)[3]<-"Total Doses Delivered"

summary_vaccine_table <-gather(dose_table, "Type")
## WRITE.CSV (ENG)
write.csv(summary_vaccine_table, "vaccine_summary_table_eng.csv", row.names=F)
#############################################################################################################################
## TABLE 2 (ESP)
dose_table_esp <- dose_table
colnames(dose_table_esp)[1]<-"Totalmente Vacunado (%)"
colnames(dose_table_esp)[2]<-"Parcialmente Vacunado (%) (esperando la segunda dosis)"
colnames(dose_table_esp)[3]<-"Numero de Dosis Administradas"
summary_vaccine_table_esp <-gather(dose_table_esp, "Type")
## RENAME VARIABLES
colnames(summary_vaccine_table_esp)[1] <- "Numero de Dosis de Vacunas Administradas"
colnames(summary_vaccine_table_esp)[2] <- "Conteo"
## WRITE.CSV (ENG)
write.csv(summary_vaccine_table_esp, "vaccine_summary_table_esp.csv", row.names=F)
#############################################################################################################################
## (2) TABLE OF VACCINES 
dd <- subset(df, vaccine_level=="1")
dd$age_group= 0
dd$age_group[which(dd$age_at_1stvaccination >69)] <- "plus_70"
summ_70 <- as.data.frame(group_by(dd,) %>%
                           summarize(sum_70plus = sum(age_group=="plus_70")))
## 2018 CENSUS: 70+ 
#P70 = 2516
## 2019 CENSUS: 70+ 
P70 = 2719

summ_70$prop70= summ_70$sum_70plus/P70*100
dose_sum$sum_70count = summ_70$sum_70plus 
dose_sum$prop70 = summ_70$prop70

dose_sum$prop70 <- signif(dose_sum$prop70, digits=3)

## PULL OUT COUNTS
dose_sum$prop70_count <- with(dose_sum, paste0(sum_70count, sp, lp, prop70, perc, rp))

total_counts<- dose_sum[,c("sum_full", "prop70_count", "sum_partial",  "total_doses")]
total_counts$type=1
total_counts_esp <- total_counts
## RENAME VARIABLES
colnames(total_counts)[1]<- "Fully Vaccinated Residents"
colnames(total_counts)[2]<- "Fully Vaccinated Residents 70+ (%)"
colnames(total_counts)[3]<- "Partially Vaccinated Residents (awaiting second dose)"
colnames(total_counts)[4]<- "Total Doses Delivered"
## TRANSFORM DF
total_counts <- gather(total_counts, "type")
## CHANGE HEADER NAMES
colnames(total_counts)[1]<- "Number of Doses Delivered"
colnames(total_counts)[2]<- "Count"

## WRITE.CSV (ENG)
write.csv(total_counts, "vaccine_counts_table_eng.csv", row.names=F)
#############################################################################################################################
## VACCINE COUNTS (ESP)
## RENAME VARIABLES
colnames(total_counts_esp)[1]<- "Totalmente Vacunado"
colnames(total_counts_esp)[2]<- "Totalmente Vacunado 70+ (%)"
colnames(total_counts_esp)[3]<- "Parcialmente Vacunado (esperando la segunda dosis)"
colnames(total_counts_esp)[4]<- "Suministro Total de Dosis"
## TRANSFORM DF
total_counts_esp <- gather(total_counts_esp, "type")
## CHANGE HEADER NAMES
colnames(total_counts_esp)[1]<- "Numero de Dosis de Vacunas Administradas"
colnames(total_counts_esp)[2]<- "Conteo"

## WRITE.CSV (ENG)
write.csv(total_counts_esp, "vaccine_counts_table_esp.csv", row.names=F)
#############################################################################################################################
#vacc_df <- subset(df, dosage_num=="2")
dd <- subset(df, vaccine_level=="1")
#############################################################################################################################
## DOUBLE DOSE SUMMARY
dd_dos_sum <- as.data.frame(group_by(dd,) %>%
                              summarize(white_sum = sum(race_ethnicity=="White - Non Hispanic"),
                                        native_sum = sum(race_ethnicity=="American Indian or Alaskan Nat"),
                                        asian_sum = sum(race_ethnicity=="Asian - Non Hispanic"),
                                        hispanic_sum = sum(race_ethnicity=="Hispanic, All Races"),
                                        multi_race_sum = sum(race_ethnicity=="Multi Race - Non Hispanic"),
                                        pi_sum = sum(race_ethnicity=="Native Hawaiian or Other Pacif"),
                                        black_sum = sum(race_ethnicity=="Black or African American - No"),
                                        Unknown_sum = sum(race_ethnicity=="Unknown"),
                                        other_sum = sum(race_ethnicity=="Other"),
                                        total_other=sum(native_sum,asian_sum,multi_race_sum,pi_sum,black_sum,other_sum),
                                        sum015=sum(age_class=="0-15"),
                                        sum1619=sum(age_class=="16-19"),
                                        sum2029=sum(age_class=="20-29"),
                                        sum3039=sum(age_class=="30-39"),
                                        sum4049=sum(age_class=="40-49"),
                                        sum5059=sum(age_class=="50-59"),
                                        sum6069=sum(age_class=="60-69"),
                                        sum7079=sum(age_class=="70-79"),
                                        sum80=sum(age_class=="80+")))
#############################################################################################################################
## ZIP CODE VACCINE SUMMARY
df$sd_total = as.numeric(df$sd_m) + as.numeric(df$sd_p)
df$fv_total = as.numeric(df$sd_j) + as.numeric(df$dd_m) + as.numeric(df$dd_p)
df$p_vac = df$sd_total - df$fv_total
zip_summ <- as.data.frame(group_by(df,zip_code) %>%
                           summarize(full_vac = sum(fv_total),
                                     sd_p = sum(sd_p=="1"),
                                     dd_p = sum(dd_p=="1"),
                                     sd_m = sum(sd_m=="1"),
                                     dd_m = sum(dd_m=="1"),
                                     sd_j = sum(sd_j=="1")))

## SUBSET ONLY SUMMIT ZIP CODES
zip_summ2 <- subset(zip_summ, zip_code=="80424" |zip_code== "80435" | zip_code== 80443 | zip_code =="80497" | zip_code=="80498")
US_CENSUS <- c(10169,7278,4483,333,7999)
Town <- c("BRECKENRIDGE", "DILLON", "FRISCO", "SILVERTHORNE-1", "SILVERTHORNE-2")
## COMPILE COLUMNS
zip_summ3 <- cbind(Town, zip_summ2, US_CENSUS)
zip_summ3$Perc=signif((zip_summ3$full_vac/zip_summ3$US_CENSUS)*100, digits=3)
zip_summ3 <- zip_summ3[c("Town", "zip_code","Perc", "full_vac", "US_CENSUS", "sd_p", "dd_p", "sd_m", "dd_m", "sd_j")]

## CLEAN UP VARIABLE NAMES
colnames(zip_summ3)[2]<- "Zip Code"
colnames(zip_summ3)[3]<- "Percentage Fully Vaccinated"
colnames(zip_summ3)[4]<- "Number of Persons Fully Vaccinated"
colnames(zip_summ3)[5]<- "US Census Population (18+)"
colnames(zip_summ3)[6]<- "Number of 1-dose Pfizer Administered"
colnames(zip_summ3)[7]<- "Number of 2-dose Pfizer Administered"
colnames(zip_summ3)[8]<- "Number of 1-dose Modnerna Administered"
colnames(zip_summ3)[9]<- "Number of 2-dose Modnerna Administered"
colnames(zip_summ3)[10]<- "Number of 1-dose Janssen Administered"
## WRITE CSV (ENG)
write.csv(zip_summ3, "SCPH_Vaccine_Administration_Zip_Code.csv", row.names=F)
#############################################################################################################################
## DAILY VACCINE (%) 
dd$count_freq=1
daily_vacc_per <- as.data.frame(group_by(dd, vaccination_date) %>%
                                 summarize(sum_vaccines=sum(count_freq=="1"),
                                           prop_vacc = sum_vaccines/AP*100))

daily_vacc_per$sum_vaccines = NULL

## FORMAT DATE
daily_vacc_per$vaccination_date <- format(as.Date(daily_vacc_per$vaccination_date, "%m/%d/%Y"), "%Y-%m-%d")
## ARRANGE BY DATE
daily_vacc_per <- arrange(daily_vacc_per, vaccination_date); head(daily_vacc_per)
## cumulative sum
daily_vacc_per <- within(daily_vacc_per, acc_sum <- cumsum(prop_vacc))
daily_vacc_per$prop_vacc = NULL
## RENAME VARIABLES
colnames(daily_vacc_per)[1]<-"Date"
colnames(daily_vacc_per)[2]<-"Fully Vaccinated (%)"
daily_vacc_per$Goal = 70
colnames(daily_vacc_per)[3]<-"Level Green: Little to No Threat (70%)"
daily_vacc_per$Blue = 60
colnames(daily_vacc_per)[4]<-"Level Blue: Caution (60%)"
daily_vacc_per$caution = 50
colnames(daily_vacc_per)[5]<-"Level Yellow: Concern"
daily_vacc_per$min = 0
daily_vacc_per$max = 100
## WRITE CSV (ESP)
write.csv(daily_vacc_per, "vaccine_percentage_daily_eng.csv", row.names=F)
## RENAME VARIABLES (ESP)
daily_vacc_per_esp <- daily_vacc_per
colnames(daily_vacc_per_esp)[1]<-"Fecha"
colnames(daily_vacc_per_esp)[2]<-"Totalmente Vacunado (%)"
colnames(daily_vacc_per_esp)[3]<-"Nivel Verde: Sin Reisgo (70%)"
colnames(daily_vacc_per_esp)[4]<-"Nivel Azul: Precacion (60%)"
colnames(daily_vacc_per_esp)[5] <- "Nivel Amarillo: Preocupaction"

## WRITE CSV (ESP)
write.csv(daily_vacc_per_esp, "vaccine_percentage_daily_esp.csv", row.names=F)
#############################################################################################################################
## MONTHLY FULLY VACCINE COVERAGE (%)
dd$month_year<-dd$vaccination_date
dd$month_year <- format(as.Date(dd$month_year, "%m/%d/%Y"), "%Y-%m")
dd$count_freq=1
month_yr_summ <- as.data.frame(group_by(dd, month_year) %>%
                                 summarize(sum_vaccines=sum(count_freq=="1"),
                                           prop_vacc = sum_vaccines/AP*100))
month_yr_summ$garbage_character_day = "-01"


month_yr_summ$date_year <- with(month_yr_summ, paste0(month_year, garbage_character_day))
month_yr_summ$date_year <- as.Date(month_yr_summ$date_year)

## ARRANGE DATA BY MONTH-YEAR
month_yr_summ <- arrange(month_yr_summ, date_year); head(month_yr_summ)
## CUMULATIVE VACCINATED %
month_yr_summ <- within(month_yr_summ, acc_sum <- cumsum(prop_vacc))
## PULL OUT MEANINGFUL VARIABLES
month_yr_summ2 <- month_yr_summ[,c("month_year", "acc_sum")]
## RENAME VARIABLES
colnames(month_yr_summ2)[1]<-"Date"
colnames(month_yr_summ2)[2]<-"Percentage of Fully Vaccinated"
## WRITE CSV (ENG)
write.csv(month_yr_summ2, "vaccine_percentage_monthly_eng.csv", row.names=F)
## RENAME VARIABLES (ESP)
month_yr_summ2_esp <- month_yr_summ2
colnames(month_yr_summ2_esp)[1]<-"Fecha"
colnames(month_yr_summ2_esp)[2]<-"Totalmente Vacunado"
## WRITE CSV (ESP)
write.csv(month_yr_summ2_esp, "vaccine_percentage_monthly_esp.csv", row.names=F)
#############################################################################################################################
## PULL OUT DD AGE GROUPS


## CREATE AGE GROUP FACTORS

## SUMMARIZE BY AGE GROU
age_summ_vacc <- as.data.frame(group_by(df,age_class) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      atl1_sum = sum(sum_partial,sum_full)))

## PULL OUT AGE VARIABLES
age_summ_vacc2 <- age_summ_vacc[,c(1,8,12)]
########################################################################
## 2018 CENSUS POPULATION SUMS
#c09 = 2469
#c19 = 2458
#c15 = 4004
#c1619 = 923
#c29 = 6742
#c39 = 6005
#c49 = 3821
#c59 = 3571
#c69 = 3259
#c79 = 2021
#c80 = 495
########################################################################
## 2019 CENSUS POPULATION SUMS
#c09 = 2469
#c19 = 2458
c11 = 2938
c1215 = 1001
c1619 = 950
c29 = 6464
c39 = 6296
c49 = 3770
c59 = 3538
c69 = 3321
c79 = 2149
c80 = 570

########################################################################
## TEMP DATA FOR AGES: 0-11
dummy_sub11 <- data.frame(age_class= "0-11",
                          sum_full=0,
                          atl1_sum=0,
                          sum_full_perc = 0,
                         at1_perc = 0) 

#dummy_1215 <- data.frame(age_class= "12-15",
#                          sum_full=0,
#                          atl1_sum=0,
#                          sum_full_perc = 0,
#                          at1_perc = 0) 

## AGES: 0-11
sub_011 <- subset(age_summ_vacc2, age_class=="0-11")
sub_011$sum_full_perc = sub_011$sum_full/c11
sub_011$at1_perc = sub_011$atl1_sum/c11
## AGES: 12-15
sub_1215 <- subset(age_summ_vacc2, age_class=="12-15")
sub_1215$sum_full_perc = sub_1215$sum_full/c1215
sub_1215$at1_perc = sub_1215$atl1_sum/c1215
## AGES: 16-19
sub_1619 <- subset(age_summ_vacc2, age_class=="16-19")
sub_1619$sum_full_perc = sub_1619$sum_full/c1619
sub_1619$at1_perc = sub_1619$atl1_sum/c1619
## AGES: 20-29
sub_2029 <- subset(age_summ_vacc2, age_class=="20-29")
sub_2029$sum_full_perc = sub_2029$sum_full/c29
sub_2029$at1_perc = sub_2029$atl1_sum/c29
## AGES: 30-39
sub_3039 <- subset(age_summ_vacc2, age_class=="30-39")
sub_3039$sum_full_perc = sub_3039$sum_full/c39
sub_3039$at1_perc = sub_3039$atl1_sum/c39
## AGES: 40-49
sub_4049 <- subset(age_summ_vacc2, age_class=="40-49")
sub_4049$sum_full_perc = sub_4049$sum_full/c49
sub_4049$at1_perc = sub_4049$atl1_sum/c49
## AGES: 50-59
sub_5059 <- subset(age_summ_vacc2, age_class=="50-59")
sub_5059$sum_full_perc = sub_5059$sum_full/c59
sub_5059$at1_perc = sub_5059$atl1_sum/c59
## AGES: 60-69
sub_6069 <- subset(age_summ_vacc2, age_class=="60-69")
sub_6069$sum_full_perc = sub_6069$sum_full/c69
sub_6069$at1_perc = sub_6069$atl1_sum/c69
## AGES: 70-79
sub_7079 <- subset(age_summ_vacc2, age_class=="70-79")
sub_7079$sum_full_perc = sub_7079$sum_full/c79
sub_7079$at1_perc = sub_7079$atl1_sum/c79
## AGES: 80+
sub_80 <- subset(age_summ_vacc2, age_class=="80+")
sub_80$sum_full_perc = sub_80$sum_full/c80
sub_80$at1_perc = sub_80$atl1_sum/c80
## COMPILE ALL AGE CLASSES
compile_vac_age <- rbind(dummy_sub11, sub_1215, sub_1619, sub_2029, sub_3039, sub_4049, sub_5059, sub_6069, sub_7079, sub_80)
## TAKE SIG FIGS
compile_vac_age$sum_full_perc <- signif((compile_vac_age$sum_full_perc)*100, digits=3)
compile_vac_age$at1_perc <- signif((compile_vac_age$at1_perc)*100, digits=3)
## PULL OUT MEANINGFUL VARIABLES
compile_vac_age2 <- compile_vac_age[,c(1, 4,5)]
colnames(compile_vac_age2)[1]<- "Age Group"
colnames(compile_vac_age2)[2]<- "Fully Vaccinated (%)"
colnames(compile_vac_age2)[3]<- "At least 1 dose (%)"
## WRITE.CSV (ENG)
write.csv(compile_vac_age2, "vaccine_age_distribution_eng.csv", row.names=F)
## WRITE.CSV (ESP)
compile_vac_age2_esp <- compile_vac_age2
colnames(compile_vac_age2_esp)[1]<- "Groupo de edad"
colnames(compile_vac_age2_esp)[2]<- "Totalmente Vacunado (%)"
colnames(compile_vac_age2_esp)[3]<- "Al menos 1 Dosis de Vacuna (%)"
write.csv(compile_vac_age2_esp, "vaccine_age_distribution_esp.csv", row.names=F)
#############################################################################################################################
## TIME SERIES: AGE

## SUMMARIZE BY AGE GROUP DAILY
age_vacc_daily <- as.data.frame(group_by(df,age_class,vaccination_date) %>%
                                 summarize(sum_sd_p = sum(sd_p=="1"),
                                           sum_dd_p = sum(dd_p=="1"),
                                           sum_sd_m = sum(sd_m=="1"),
                                           sum_dd_m = sum(dd_m=="1"),
                                           sum_sd_j = sum(sd_j=="1"),
                                           total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                           sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                           diff_p = sum_sd_p-sum_dd_p,
                                           diff_m = sum_sd_m-sum_dd_m,
                                           sum_partial = sum(diff_p,diff_m),
                                           atl1_sum = sum(sum_partial,sum_full)))

######################################################################################################################################
## AGES: 12-15
sub1215 <- subset(df, age_class=="12-15") 

## SUMMARIZE BY AGE GROUP DAILY
vd_1215 <- as.data.frame(group_by(sub1215,vaccination_date) %>%
                           summarize(sum_sd_p = sum(sd_p=="1"),
                                     sum_dd_p = sum(dd_p=="1"),
                                     sum_sd_m = sum(sd_m=="1"),
                                     sum_dd_m = sum(dd_m=="1"),
                                     sum_sd_j = sum(sd_j=="1"),
                                     total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                     sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                     diff_p = sum_sd_p-sum_dd_p,
                                     diff_m = sum_sd_m-sum_dd_m,
                                     sum_partial = sum(diff_p,diff_m),
                                     partial_perc = sum_partial/c1619*100,
                                     vaccinated_perc = (sum_full/c1619)*100,
                                     atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
vd_1215_2 <- vd_1215[,c(1,13,14)]


vd_1215_2$date = vd_1215_2$vaccination_date
vd_1215_2$date <- format(as.Date(vd_1215_2$date, "%m/%d/%Y"), "%Y-%m-%d")  

vd_1215_2$sort_ID <- as.integer(factor(with(vd_1215_2, paste(date))))
vd_1215_2 <- arrange(vd_1215_2, sort_ID); head(vd_1215_2)

## CUMULATIVE SUM ACROSS ALL ROWS
vd_1215_2$cum_vacc_perc <- 0
vd_1215_2$cum_vacc_perc <- as.numeric(cumsum(vd_1215_2[,2]))
vd_1215_2$cum_atl1_perc <- 0
vd_1215_2$cum_atl1_perc <- as.numeric(cumsum(vd_1215_2[,3]))

##check
library(zoo) 
vd_1215_3 <- vd_1215_2 %>% split(vd_1215_2$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)


######################################################################################################################################
sub1619 <- subset(df, age_class=="16-19") 

## SUMMARIZE BY AGE GROUP DAILY
vd_1619 <- as.data.frame(group_by(sub1619,vaccination_date) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/c1619*100,
                                      vaccinated_perc = (sum_full/c1619)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
vd_1619_2 <- vd_1619[,c(1,13,14)]


vd_1619_2$date = vd_1619_2$vaccination_date
vd_1619_2$date <- format(as.Date(vd_1619_2$date, "%m/%d/%Y"), "%Y-%m-%d")  

vd_1619_2$sort_ID <- as.integer(factor(with(vd_1619_2, paste(date))))
vd_1619_2 <- arrange(vd_1619_2, sort_ID); head(vd_1619_2)

## CUMULATIVE SUM ACROSS ALL ROWS
vd_1619_2$cum_vacc_perc <- 0
vd_1619_2$cum_vacc_perc <- as.numeric(cumsum(vd_1619_2[,2]))
vd_1619_2$cum_atl1_perc <- 0
vd_1619_2$cum_atl1_perc <- as.numeric(cumsum(vd_1619_2[,3]))

##check
library(zoo) 
vd_1619_3 <- vd_1619_2 %>% split(vd_1619_2$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)
##########################################################################################
## DUMMY: O-11
dummy_sub11 <- data.frame(vaccination_date= "12/15/2020",
                          vaccinated_perc=0,
                          atl1=0,
                          date  = 0,
                          sort_ID = 1,
                          cum_vacc_perc = 0,
                          cum_atl1_perc = 0,
                          age_class = "0-11")
## DUMMY: 12-15
dummy_sub11 <- data.frame(vaccination_date= "12/15/2020",
                          vaccinated_perc=0,
                          atl1=0,
                          date  = 0,
                          sort_ID = 1,
                          cum_vacc_perc = 0,
                          cum_atl1_perc = 0,
                          age_class = "12-15")
##########################################################################################
## AGE GROUP: 20-29
sub29 <- subset(df, age_class=="20-29") 

## SUMMARIZE BY AGE GROUP DAILY
vd_29 <- as.data.frame(group_by(sub29,vaccination_date) %>%
                           summarize(sum_sd_p = sum(sd_p=="1"),
                                     sum_dd_p = sum(dd_p=="1"),
                                     sum_sd_m = sum(sd_m=="1"),
                                     sum_dd_m = sum(dd_m=="1"),
                                     sum_sd_j = sum(sd_j=="1"),
                                     total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                     sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                     diff_p = sum_sd_p-sum_dd_p,
                                     diff_m = sum_sd_m-sum_dd_m,
                                     sum_partial = sum(diff_p,diff_m),
                                     partial_perc = sum_partial/c29*100,
                                     vaccinated_perc = (sum_full/c29)*100,
                                     atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
vd_29 <- vd_29[,c(1,13,14)]

vd_29$date = vd_29$vaccination_date
vd_29$date <- format(as.Date(vd_29$date, "%m/%d/%Y"), "%Y-%m-%d")  

vd_29$sort_ID <- as.integer(factor(with(vd_29, paste(date))))
vd_29_2 <- arrange(vd_29, sort_ID); head(vd_29)

## CUMULATIVE SUM ACROSS ALL ROWS
vd_29_2$cum_vacc_perc <- 0
vd_29_2$cum_vacc_perc <- as.numeric(cumsum(vd_29_2[,2]))
vd_29_2$cum_atl1_perc <- 0
vd_29_2$cum_atl1_perc <- as.numeric(cumsum(vd_29_2[,3]))

##check
library(zoo) 
vd_29_3 <- vd_29_2 %>% split(vd_29_2$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)
##########################################################################################
## AGE GROUP: 30-39
sub39 <- subset(df, age_class=="30-39") 

## SUMMARIZE BY AGE GROUP DAILY
vd_39 <- as.data.frame(group_by(sub39,vaccination_date) %>%
                         summarize(sum_sd_p = sum(sd_p=="1"),
                                   sum_dd_p = sum(dd_p=="1"),
                                   sum_sd_m = sum(sd_m=="1"),
                                   sum_dd_m = sum(dd_m=="1"),
                                   sum_sd_j = sum(sd_j=="1"),
                                   total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                   sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                   diff_p = sum_sd_p-sum_dd_p,
                                   diff_m = sum_sd_m-sum_dd_m,
                                   sum_partial = sum(diff_p,diff_m),
                                   partial_perc = sum_partial/c39*100,
                                   vaccinated_perc = (sum_full/c39)*100,
                                   atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
vd_39 <- vd_39[,c(1,13,14)]

vd_39$date = vd_39$vaccination_date
vd_39$date <- format(as.Date(vd_39$date, "%m/%d/%Y"), "%Y-%m-%d")  

vd_39$sort_ID <- as.integer(factor(with(vd_39, paste(date))))
vd_39_2 <- arrange(vd_39, sort_ID); head(vd_39)

## CUMULATIVE SUM ACROSS ALL ROWS
vd_39_2$cum_vacc_perc <- 0
vd_39_2$cum_vacc_perc <- as.numeric(cumsum(vd_39_2[,2]))
vd_39_2$cum_atl1_perc <- 0
vd_39_2$cum_atl1_perc <- as.numeric(cumsum(vd_39_2[,3]))

library(zoo) 
vd_39_3 <- vd_39_2 %>% split(vd_39_2$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)
##########################################################################################
## AGE GROUP: 40-49
sub49 <- subset(df, age_class=="40-49") 

## SUMMARIZE BY AGE GROUP DAILY
vd_49 <- as.data.frame(group_by(sub49,vaccination_date) %>%
                         summarize(sum_sd_p = sum(sd_p=="1"),
                                   sum_dd_p = sum(dd_p=="1"),
                                   sum_sd_m = sum(sd_m=="1"),
                                   sum_dd_m = sum(dd_m=="1"),
                                   sum_sd_j = sum(sd_j=="1"),
                                   total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                   sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                   diff_p = sum_sd_p-sum_dd_p,
                                   diff_m = sum_sd_m-sum_dd_m,
                                   sum_partial = sum(diff_p,diff_m),
                                   partial_perc = sum_partial/c49*100,
                                   vaccinated_perc = (sum_full/c49)*100,
                                   atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
vd_49 <- vd_49[,c(1,13,14)]

vd_49$date = vd_49$vaccination_date
vd_49$date <- format(as.Date(vd_49$date, "%m/%d/%Y"), "%Y-%m-%d")  

vd_49$sort_ID <- as.integer(factor(with(vd_49, paste(date))))
vd_49_2 <- arrange(vd_49, sort_ID); head(vd_49_2)

## CUMULATIVE SUM ACROSS ALL ROWS
vd_49_2$cum_vacc_perc <- 0
vd_49_2$cum_vacc_perc <- as.numeric(cumsum(vd_49_2[,2]))
vd_49_2$cum_atl1_perc <- 0
vd_49_2$cum_atl1_perc <- as.numeric(cumsum(vd_49_2[,3]))

library(zoo) 
vd_49_3 <- vd_49_2 %>% split(vd_49_2$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)
##########################################################################################
## AGE GROUP: 50-59
sub59 <- subset(df, age_class=="50-59") 

## SUMMARIZE BY AGE GROUP DAILY
vd_59 <- as.data.frame(group_by(sub59,vaccination_date) %>%
                         summarize(sum_sd_p = sum(sd_p=="1"),
                                   sum_dd_p = sum(dd_p=="1"),
                                   sum_sd_m = sum(sd_m=="1"),
                                   sum_dd_m = sum(dd_m=="1"),
                                   sum_sd_j = sum(sd_j=="1"),
                                   total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                   sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                   diff_p = sum_sd_p-sum_dd_p,
                                   diff_m = sum_sd_m-sum_dd_m,
                                   sum_partial = sum(diff_p,diff_m),
                                   partial_perc = sum_partial/c59*100,
                                   vaccinated_perc = (sum_full/c59)*100,
                                   atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
vd_59 <- vd_59[,c(1,13,14)]

vd_59$date = vd_59$vaccination_date
vd_59$date <- format(as.Date(vd_59$date, "%m/%d/%Y"), "%Y-%m-%d")  

vd_59$sort_ID <- as.integer(factor(with(vd_59, paste(date))))
vd_59_2 <- arrange(vd_59, sort_ID); head(vd_59_2)

## CUMULATIVE SUM ACROSS ALL ROWS
vd_59_2$cum_vacc_perc <- 0
vd_59_2$cum_vacc_perc <- as.numeric(cumsum(vd_59_2[,2]))
vd_59_2$cum_atl1_perc <- 0
vd_59_2$cum_atl1_perc <- as.numeric(cumsum(vd_59_2[,3]))

library(zoo) 
vd_59_3 <- vd_59_2 %>% split(vd_59_2$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)
##########################################################################################
## AGE GROUP: 60-69
sub69 <- subset(df, age_class=="60-69") 

## SUMMARIZE BY AGE GROUP DAILY
vd_69 <- as.data.frame(group_by(sub69,vaccination_date) %>%
                         summarize(sum_sd_p = sum(sd_p=="1"),
                                   sum_dd_p = sum(dd_p=="1"),
                                   sum_sd_m = sum(sd_m=="1"),
                                   sum_dd_m = sum(dd_m=="1"),
                                   sum_sd_j = sum(sd_j=="1"),
                                   total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                   sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                   diff_p = sum_sd_p-sum_dd_p,
                                   diff_m = sum_sd_m-sum_dd_m,
                                   sum_partial = sum(diff_p,diff_m),
                                   partial_perc = sum_partial/c69*100,
                                   vaccinated_perc = (sum_full/c69)*100,
                                   atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
vd_69 <- vd_69[,c(1,13,14)]

vd_69$date = vd_69$vaccination_date
vd_69$date <- format(as.Date(vd_69$date, "%m/%d/%Y"), "%Y-%m-%d")  

vd_69$sort_ID <- as.integer(factor(with(vd_69, paste(date))))
vd_69_2 <- arrange(vd_69, sort_ID); head(vd_69_2)

## CUMULATIVE SUM ACROSS ALL ROWS
vd_69_2$cum_vacc_perc <- 0
vd_69_2$cum_vacc_perc <- as.numeric(cumsum(vd_69_2[,2]))
vd_69_2$cum_atl1_perc <- 0
vd_69_2$cum_atl1_perc <- as.numeric(cumsum(vd_69_2[,3]))

library(zoo) 
vd_69_3 <- vd_69_2 %>% split(vd_69_2$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)
##########################################################################################
## AGE GROUP: 70-79
sub79 <- subset(df, age_class=="70-79") 

## SUMMARIZE BY AGE GROUP DAILY
vd_79 <- as.data.frame(group_by(sub79,vaccination_date) %>%
                         summarize(sum_sd_p = sum(sd_p=="1"),
                                   sum_dd_p = sum(dd_p=="1"),
                                   sum_sd_m = sum(sd_m=="1"),
                                   sum_dd_m = sum(dd_m=="1"),
                                   sum_sd_j = sum(sd_j=="1"),
                                   total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                   sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                   diff_p = sum_sd_p-sum_dd_p,
                                   diff_m = sum_sd_m-sum_dd_m,
                                   sum_partial = sum(diff_p,diff_m),
                                   partial_perc = sum_partial/c79*100,
                                   vaccinated_perc = (sum_full/c79)*100,
                                   atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
vd_79 <- vd_79[,c(1,13,14)]

vd_79$date = vd_79$vaccination_date
vd_79$date <- format(as.Date(vd_79$date, "%m/%d/%Y"), "%Y-%m-%d")  

vd_79$sort_ID <- as.integer(factor(with(vd_79, paste(date))))
vd_79_2 <- arrange(vd_79, sort_ID); head(vd_79_2)

## CUMULATIVE SUM ACROSS ALL ROWS
vd_79_2$cum_vacc_perc <- 0
vd_79_2$cum_vacc_perc <- as.numeric(cumsum(vd_79_2[,2]))
vd_79_2$cum_atl1_perc <- 0
vd_79_2$cum_atl1_perc <- as.numeric(cumsum(vd_79_2[,3]))

library(zoo) 
vd_79_3 <- vd_79_2 %>% split(vd_79_2$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)
##########################################################################################
## AGE GROUP: 80+
sub80 <- subset(df, age_class=="80+") 

## SUMMARIZE BY AGE GROUP DAILY
sub80 <- as.data.frame(group_by(sub80,vaccination_date) %>%
                         summarize(sum_sd_p = sum(sd_p=="1"),
                                   sum_dd_p = sum(dd_p=="1"),
                                   sum_sd_m = sum(sd_m=="1"),
                                   sum_dd_m = sum(dd_m=="1"),
                                   sum_sd_j = sum(sd_j=="1"),
                                   total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                   sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                   diff_p = sum_sd_p-sum_dd_p,
                                   diff_m = sum_sd_m-sum_dd_m,
                                   sum_partial = sum(diff_p,diff_m),
                                   partial_perc = sum_partial/c80*100,
                                   vaccinated_perc = (sum_full/c80)*100,
                                   atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
sub80 <- sub80[,c(1,13,14)]

sub80$date = sub80$vaccination_date
sub80$date <- format(as.Date(sub80$date, "%m/%d/%Y"), "%Y-%m-%d")  

sub80$sort_ID <- as.integer(factor(with(sub80, paste(date))))
sub80_2 <- arrange(sub80, sort_ID); head(sub80_2)

## CUMULATIVE SUM ACROSS ALL ROWS
sub80_2$cum_vacc_perc <- 0
sub80_2$cum_vacc_perc <- as.numeric(cumsum(sub80_2[,2]))
sub80_2$cum_atl1_perc <- 0
sub80_2$cum_atl1_perc <- as.numeric(cumsum(sub80_2[,3]))

library(zoo) 
sub80_3 <- sub80_2 %>% split(sub80_2$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)
##########################################################################################
## COMPILE AGE TIME SERIES
## ADD AGE GROUP VARIABLE
sub80_3$age_group = "80+"
vd_79_3$age_group = "70-79"
vd_69_3$age_group = "60-69"
vd_59_3$age_group = "50-59"
vd_49_3$age_group = "40-49"
vd_39_3$age_group = "30-39"
vd_29_3$age_group = "20-29"
vd_1619_3$age_group = "16-19"
vd_1215_3$age_group = "12-15"
## COMPILE AGE TIME SERIES
comp_age_ts <- rbind(sub80_3, vd_79_3, vd_69_3, vd_59_3, vd_49_3, vd_39_3, vd_29_3, vd_1619_3, vd_1215_3)


comp_age_ts2 <- comp_age_ts[,c(1,6,8)]
## RESHAPE DATA
comp_age_ts3 <- spread(comp_age_ts2, key = age_group, value = cum_vacc_perc)

comp_age_ts3$date = comp_age_ts3$vaccination_date
comp_age_ts3$date <- format(as.Date(comp_age_ts3$date, "%m/%d/%Y"), "%Y-%m-%d") 

comp_age_ts3$sort_ID <- as.integer(factor(with(comp_age_ts3, paste(date))))
comp_age_ts4 <- arrange(comp_age_ts3, sort_ID); head(comp_age_ts4)


comp_age_ts5 <- comp_age_ts4[,c(1:10)]
colnames(comp_age_ts5)[1]<-"Date"


sub_15 <- comp_age_ts4[,c("vaccination_date", "12-15", "sort_ID")]
sub_15 <- na.locf(sub_15)
sub_15 <-subset(sub_15, sort_ID >168)
##sub_15 <- subset(sub_15, sort_ID > 10);dim(sub_15)
#sub_15 <- subset(sub_15, !duplicated(vaccination_date))

sub_16 <- comp_age_ts4[,c("vaccination_date", "16-19", "sort_ID")]
sub_16 <- na.locf(sub_16)
sub_16 <- subset(sub_16, sort_ID > 10);dim(sub_16)

proxy_vd <- subset(sub_16, sort_ID < 169)
proxy_vd$`16-19`=0
colnames(proxy_vd)[2]<-"12-15"

sub_15_2 <- rbind(proxy_vd, sub_15)


sub_29 <- comp_age_ts4[,c("vaccination_date", "20-29", "sort_ID")]
sub_29 <- na.locf(sub_29)
sub_29 <- subset(sub_29, sort_ID > 10);dim(sub_29)

sub_39 <- comp_age_ts4[,c("vaccination_date", "30-39", "sort_ID")]
sub_39 <- na.locf(sub_39)
sub_39 <- subset(sub_39, sort_ID > 10);dim(sub_39)

sub_49 <- comp_age_ts4[,c("vaccination_date", "40-49", "sort_ID")]
sub_49 <- na.locf(sub_49)
sub_49 <- subset(sub_49, sort_ID > 10);dim(sub_49)

sub_59 <- comp_age_ts4[,c("vaccination_date", "50-59", "sort_ID")]
sub_59 <- na.locf(sub_59)
sub_59 <- subset(sub_59, sort_ID > 10);dim(sub_59)

sub_69 <- comp_age_ts4[,c("vaccination_date", "60-69", "sort_ID")]
sub_69 <- na.locf(sub_69)
sub_69 <- subset(sub_69, sort_ID > 10);dim(sub_69)

sub_79 <- comp_age_ts4[,c("vaccination_date", "70-79", "sort_ID")]
sub_79 <- na.locf(sub_79)
sub_79 <- subset(sub_79, sort_ID > 10);dim(sub_79)

sub_80 <- comp_age_ts4[,c("vaccination_date", "80+", "sort_ID")]
sub_80 <- na.locf(sub_80)
sub_80 <- subset(sub_80, sort_ID > 10);dim(sub_80)

compile_age_vac <- cbind(sub_15_2, sub_16, sub_29, sub_39, sub_49, sub_59, sub_69, sub_79, sub_80)
compile_age_vac2 <- compile_age_vac[,c("vaccination_date", "12-15", "16-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")]

colnames(compile_age_vac2)[1]<-"Date"

## write.csv (ENG)
write.csv(compile_age_vac2, "vaccine_time_series_age_eng.csv", row.names=F)

## REPLACE NAs
library(zoo) 
fixed_16 <- sub_16 %>% split(sub_16$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)

library(zoo) 
comp_age_ts5 <- comp_age_ts4 %>% split(comp_age_ts4$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)


comp_age_ts6 <- arrange(comp_age_ts5, sort_ID); head(comp_age_ts6)



## SIG FIGS
vd_1619_3$Hispanic =  signif(vd_1619_3$Hispanic, digits=3)
vd_1619_3$Other =  signif(vd_1619_3$Other, digits=3)
vd_1619_3$White =  signif(vd_1619_3$White, digits=3)
## CHANGE UP COLUMN NAMES
vd_1619_4 <- vd_1619_3[,c(1:4)]
colnames(comp_race_ts6)[1]<-"Date"
colnames(comp_race_ts6)[2]<-"Hispanic/Latinx"
## WRITE.CSV (ENG)
write.csv(comp_race_ts6, "vaccine_race_time_series_eng.csv", row.names=F)

## CONVERT TO ESP
comp_race_ts6_esp <- comp_race_ts6
colnames(comp_race_ts6_esp)[1]<- "Fecha"
colnames(comp_race_ts6_esp)[2]<- "hispano/latinx"
colnames(comp_race_ts6_esp)[3]<- "otros"
colnames(comp_race_ts6_esp)[4]<- "blanco"
## WRITE CSV (ESP)
write.csv(comp_race_ts6_esp, "vaccine_race_time_series_esp.csv", row.names=F)





######################################################################################################################################
## (4) SUMMARIZE BY RACE/ETHNICITY

df$race_ethnic=0
df$race_ethnic[which(df$race_ethnicity=="American Indian or Alaskan Native - Non Hispanic")]<-"Other"
df$race_ethnic[which(df$race_ethnicity=="Black or African American - Non Hispanic")]<-"Other"
df$race_ethnic[which(df$race_ethnicity=="Multi Race - Non Hispanic Native Hawaiian or Other Pacific Islander - Non Hispanic")]<-"Other"
#df$race_ethnic[which(df$race_ethnicity=="Multi Race - Non Hispanic")]<-"Other"
df$race_ethnic[which(df$race_ethnicity=="Other")]<-"Other"
df$race_ethnic[which(df$race_ethnicity=="White - Non Hispanic")]<-"White"
df$race_ethnic[which(df$race_ethnicity=="Asian - Non Hispanic")]<-"Other"
df$race_ethnic[which(df$race_ethnicity=="Hispanic, All Races")]<-"Hispanic/Latinx"
df$race_ethnic[which(df$race_ethnicity=="Native Hawaiian or Other Pacific Islander - Non Hispanic")]<-"Other"

## SUMMARIZE RACE/ETHNIC
race_summ_vacc <- as.data.frame(group_by(df,race_ethnic) %>%
                                 summarize(sum_sd_p = sum(sd_p=="1"),
                                           sum_dd_p = sum(dd_p=="1"),
                                           sum_sd_m = sum(sd_m=="1"),
                                           sum_dd_m = sum(dd_m=="1"),
                                           sum_sd_j = sum(sd_j=="1"),
                                           total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                           sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                           diff_p = sum_sd_p-sum_dd_p,
                                           diff_m = sum_sd_m-sum_dd_m,
                                           sum_partial = sum(diff_p,diff_m),
                                           atl1_sum = sum(sum_partial,sum_full)))
#############################################################################################################
## 2018 CENSUS METRICS
#W = 25395
#O = 927
#H = 4519
#U = 0
#############################################################################################################
## 2019 CENSUS METRICS
W = 23748
O = 777
H = 3534
U = 0
#############################################################################################################
## WHITE
sub_w <- subset(race_summ_vacc, race_ethnic =="White")
sub_w$sum_full_perc = sub_w$sum_full/W
sub_w$at1_perc = sub_w$atl1_sum/W
################################################
## OTHER
sub_O <- subset(race_summ_vacc, race_ethnic =="Other")
sub_O$sum_full_perc = sub_O$sum_full/O
sub_O$at1_perc = sub_O$atl1_sum/O
################################################
## HISPANIC/LATINX
sub_H <- subset(race_summ_vacc, race_ethnic =="Hispanic/Latinx")
sub_H$sum_full_perc = sub_H$sum_full/H
sub_H$at1_perc = sub_H$atl1_sum/H
################################################
## PULL OUT MEANINGFUL VARIABLES
compile_race_vacc <- rbind(sub_H, sub_O, sub_w)
compile_race_vacc$at1_perc <- signif((compile_race_vacc$at1_perc)*100, digits=3)
compile_race_vacc$sum_full_perc <- signif((compile_race_vacc$sum_full_perc)*100, digits=3)
compile_race_vacc2 <- compile_race_vacc[,c(1, 13, 14)]
## CHANGE COLUMN NAMES
colnames(compile_race_vacc2)[1]<-"Race/Ethnicity"
colnames(compile_race_vacc2)[2]<-"Fully Vaccinated (%)"
colnames(compile_race_vacc2)[3]<-"At least 1 dose (%)"

## WRITE.CSV (ENG)
write.csv(compile_race_vacc2, "vaccine_race_ethnicity_distribution_eng.csv", row.names=F)
#############################################################################################################################
## RACE/ETHNICITY VACCINATED (ESP)
compile_race_vacc2_esp <- compile_race_vacc2

colnames(compile_race_vacc2_esp)[1]<-"Raza/Grupo Etnico"
colnames(compile_race_vacc2_esp)[2]<-"Totalmente vacunados (%)"
colnames(compile_race_vacc2_esp)[3]<-"Al menos 1 Dosis de Vacuna (%)"
## WRITE CSV (ESP)
write.csv(compile_race_vacc2_esp, "vaccine_race_ethnicity_distribution_esp.csv", row.names=F)
#############################################################################################################################
## TIME SERIES: RACE/ETHNICITY VACCINE
## PULL OUT KNOWN RACE/ETHNICITY 
h_subset <- subset(df, race_ethnic=="Hispanic/Latinx")
o_subset <- subset(df, race_ethnic=="Other")
w_subset <- subset(df, race_ethnic=="White")

## DAILY FULL VACCINE SUM 
daily_hv <- as.data.frame(group_by(h_subset,vaccination_date) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/H*100,
                                      vaccinated_perc = (sum_full/H)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))


## VACCINE COVERAGE TREND GRAPH 
daily_hv2 <- daily_hv[,c(1,13,14)]

daily_hv2$date = daily_hv2$vaccination_date
daily_hv2$date <- format(as.Date(daily_hv2$date, "%m/%d/%Y"), "%Y-%m-%d")  

daily_hv2$sort_ID <- as.integer(factor(with(daily_hv2, paste(date))))
daily_hv2 <- arrange(daily_hv2, sort_ID); head(daily_hv2)

## CUMULATIVE SUM ACROSS ALL ROWS
daily_hv2$cum_vacc_perc <- 0
daily_hv2$cum_vacc_perc <- as.numeric(cumsum(daily_hv2[,2]))
daily_hv2$cum_atl1_perc <- 0
daily_hv2$cum_atl1_perc <- as.numeric(cumsum(daily_hv2[,3]))
#############################################################################################################################
## TIME SERIES: WHITE
## PULL OUT KNOWN RACE/ETHNICITY 
## DAILY FULL VACCINE SUM 
daily_wv <- as.data.frame(group_by(w_subset,vaccination_date) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/W*100,
                                      vaccinated_perc = (sum_full/W)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
daily_wv2 <- daily_wv[,c(1,13,14)]

daily_wv2$date = daily_wv2$vaccination_date
daily_wv2$date <- format(as.Date(daily_wv2$date, "%m/%d/%Y"), "%Y-%m-%d")  

daily_wv2$sort_ID <- as.integer(factor(with(daily_wv2, paste(date))))
daily_wv2 <- arrange(daily_wv2, sort_ID); head(daily_wv2)

## CUMULATIVE SUM ACROSS ALL ROWS
daily_wv2$cum_vacc_perc <- 0
daily_wv2$cum_vacc_perc <- as.numeric(cumsum(daily_wv2[,2]))
daily_wv2$cum_atl1_perc <- 0
daily_wv2$cum_atl1_perc <- as.numeric(cumsum(daily_wv2[,3]))
#############################################################################################################################
## TIME SERIES: OTHER
## PULL OUT KNOWN RACE/ETHNICITY 
## DAILY FULL VACCINE SUM 
daily_ov <- as.data.frame(group_by(o_subset,vaccination_date) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/O*100,
                                      vaccinated_perc = (sum_full/O)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
daily_ov2 <- daily_ov[,c(1,13,14)]

daily_ov2$date = daily_ov2$vaccination_date
daily_ov2$date <- format(as.Date(daily_ov2$date, "%m/%d/%Y"), "%Y-%m-%d")  

daily_ov2$sort_ID <- as.integer(factor(with(daily_ov2, paste(date))))
daily_ov2 <- arrange(daily_ov2, sort_ID); head(daily_ov2)

## CUMULATIVE SUM ACROSS ALL ROWS
daily_ov2$cum_vacc_perc <- 0
daily_ov2$cum_vacc_perc <- as.numeric(cumsum(daily_ov2[,2]))
daily_ov2$cum_atl1_perc <- 0
daily_ov2$cum_atl1_perc <- as.numeric(cumsum(daily_ov2[,3]))
#############################################################################################################################
daily_ov2$race="Other"
daily_wv2$race="White"
daily_hv2$race="Hispanic"
## COMPILE RACE VACCINE TIME SERIES
comp_race_ts <- rbind(daily_ov2, daily_wv2, daily_hv2)
## PULL OUT MEANINGFUL VARIABLES
comp_race_ts2 <- comp_race_ts[,c(1,6,8)]
## RESHAPE DATA
comp_race_ts3 <- spread(comp_race_ts2, key = race, value = cum_vacc_perc)

comp_race_ts3$date = comp_race_ts3$vaccination_date
comp_race_ts3$date <- format(as.Date(comp_race_ts3$date, "%m/%d/%Y"), "%Y-%m-%d") 

comp_race_ts3$sort_ID <- as.integer(factor(with(comp_race_ts3, paste(date))))
comp_race_ts4 <- arrange(comp_race_ts3, sort_ID); head(comp_race_ts4)

library(zoo) 
comp_race_ts5 <- comp_race_ts4 %>% split(comp_race_ts4$sort_ID) %>% 
  lapply(function(x) {na.locf(na.locf(x), fromLast=T)}) %>% 
  do.call(rbind, .)

## SIG FIGS
comp_race_ts5$Hispanic =  signif(comp_race_ts5$Hispanic, digits=3)
comp_race_ts5$Other =  signif(comp_race_ts5$Other, digits=3)
comp_race_ts5$White =  signif(comp_race_ts5$White, digits=3)
## CHANGE UP COLUMN NAMES
comp_race_ts6 <- comp_race_ts5[,c(1:4)]
colnames(comp_race_ts6)[1]<-"Date"
colnames(comp_race_ts6)[2]<-"Hispanic/Latinx"
## WRITE.CSV (ENG)
write.csv(comp_race_ts6, "vaccine_race_time_series_eng.csv", row.names=F)
#############################################################################################################################
## TIME SERIES: ZIP CODE
## CREATE NEW ZIP_CODE VAR
df$zip <- df$zip_code


df$zip <- gsub(".*-", "", df$zip)

df$sc_zip=0
df$sc_zip[which(df$zip=="80435" | df$zip=="80424" | df$zip=="80443" | df$zip=="80497" | df$zip =="80498")] <- "1"


US_CENSUS <- c(8952,4870,3787,273,6790)
## US CENSUS ZIP CODE POPULATIONS: https://data.census.gov/cedsci/table?q=population%20of%2080498&tid=ACSDP5Y2019.DP05&hidePreview=false
c80424 = 10169
c80435 = 7278
c80443 = 4483
c80497 = 333
c80498 = 7999
#############################################################################################################################
## SUBSET ZIP CODES
sub_80424 <- subset(df, zip =="80424")
sub_80435 <- subset(df, zip =="80435")
sub_80443 <- subset(df, zip =="80443")
sub_80497 <- subset(df, zip =="80497")
sub_80498 <- subset(df, zip =="80498")
#############################################################################################################################
## 80424 BRECKENRIDGE SUMMARY
daily_b <- as.data.frame(group_by(sub_80424,vaccination_date) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/c80424*100,
                                      vaccinated_perc = (sum_full/c80424)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))


## VACCINE COVERAGE TREND GRAPH 
daily_b2 <- daily_b[,c(1,13,14)]

daily_b2$date = daily_b2$vaccination_date
daily_b2$date <- format(as.Date(daily_b2$date, "%m/%d/%Y"), "%Y-%m-%d")  

daily_b2$sort_ID <- as.integer(factor(with(daily_b2, paste(date))))
daily_b2 <- arrange(daily_b2, sort_ID); head(daily_b2)

## CUMULATIVE SUM ACROSS ALL ROWS
daily_b2$cum_vacc_perc <- 0
daily_b2$cum_vacc_perc <- as.numeric(cumsum(daily_b2[,2]))
daily_b2$cum_atl1_perc <- 0
daily_b2$cum_atl1_perc <- as.numeric(cumsum(daily_b2[,3]))
#############################################################################################################################
## 80435 DILLON SUMMARY
daily_d <- as.data.frame(group_by(sub_80435,vaccination_date) %>%
                           summarize(sum_sd_p = sum(sd_p=="1"),
                                     sum_dd_p = sum(dd_p=="1"),
                                     sum_sd_m = sum(sd_m=="1"),
                                     sum_dd_m = sum(dd_m=="1"),
                                     sum_sd_j = sum(sd_j=="1"),
                                     total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                     sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                     diff_p = sum_sd_p-sum_dd_p,
                                     diff_m = sum_sd_m-sum_dd_m,
                                     sum_partial = sum(diff_p,diff_m),
                                     partial_perc = sum_partial/c80435*100,
                                     vaccinated_perc = (sum_full/c80435)*100,
                                     atl1 = sum(partial_perc,vaccinated_perc)))


## VACCINE COVERAGE TREND GRAPH 
daily_d2 <- daily_d[,c(1,13,14)]

daily_d2$date = daily_d2$vaccination_date
daily_d2$date <- format(as.Date(daily_d2$date, "%m/%d/%Y"), "%Y-%m-%d")  

daily_d2$sort_ID <- as.integer(factor(with(daily_d2, paste(date))))
daily_d2 <- arrange(daily_d2, sort_ID); head(daily_d2)

## CUMULATIVE SUM ACROSS ALL ROWS
daily_d2$cum_vacc_perc <- 0
daily_d2$cum_vacc_perc <- as.numeric(cumsum(daily_d2[,2]))
daily_d2$cum_atl1_perc <- 0
daily_d2$cum_atl1_perc <- as.numeric(cumsum(daily_d2[,3]))
#############################################################################################################################
## 80443 FRISCO SUMMARY
daily_f <- as.data.frame(group_by(sub_80443,vaccination_date) %>%
                           summarize(sum_sd_p = sum(sd_p=="1"),
                                     sum_dd_p = sum(dd_p=="1"),
                                     sum_sd_m = sum(sd_m=="1"),
                                     sum_dd_m = sum(dd_m=="1"),
                                     sum_sd_j = sum(sd_j=="1"),
                                     total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                     sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                     diff_p = sum_sd_p-sum_dd_p,
                                     diff_m = sum_sd_m-sum_dd_m,
                                     sum_partial = sum(diff_p,diff_m),
                                     partial_perc = sum_partial/c80443*100,
                                     vaccinated_perc = (sum_full/c80443)*100,
                                     atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
daily_f2 <- daily_f[,c(1,13,14)]

daily_f2$date = daily_f2$vaccination_date
daily_f2$date <- format(as.Date(daily_f2$date, "%m/%d/%Y"), "%Y-%m-%d")  

daily_f2$sort_ID <- as.integer(factor(with(daily_f2, paste(date))))
daily_f2 <- arrange(daily_f2, sort_ID); head(daily_f2)

## CUMULATIVE SUM ACROSS ALL ROWS
daily_f2$cum_vacc_perc <- 0
daily_f2$cum_vacc_perc <- as.numeric(cumsum(daily_f2[,2]))
daily_f2$cum_atl1_perc <- 0
daily_f2$cum_atl1_perc <- as.numeric(cumsum(daily_f2[,3]))
###################################################################
## 80497 SILVERTHORNE-1 SUMMARY
daily_s1 <- as.data.frame(group_by(sub_80497,vaccination_date) %>%
                           summarize(sum_sd_p = sum(sd_p=="1"),
                                     sum_dd_p = sum(dd_p=="1"),
                                     sum_sd_m = sum(sd_m=="1"),
                                     sum_dd_m = sum(dd_m=="1"),
                                     sum_sd_j = sum(sd_j=="1"),
                                     total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                     sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                     diff_p = sum_sd_p-sum_dd_p,
                                     diff_m = sum_sd_m-sum_dd_m,
                                     sum_partial = sum(diff_p,diff_m),
                                     partial_perc = sum_partial/c80497*100,
                                     vaccinated_perc = (sum_full/c80497)*100,
                                     atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
daily_s1 <- daily_s1[,c(1,13,14)]

daily_s1$date = daily_s1$vaccination_date
daily_s1$date <- format(as.Date(daily_s1$date, "%m/%d/%Y"), "%Y-%m-%d")  

daily_s1$sort_ID <- as.integer(factor(with(daily_s1, paste(date))))
daily_s1_2 <- arrange(daily_s1, sort_ID); head(daily_s1_2)

## CUMULATIVE SUM ACROSS ALL ROWS
daily_s1_2$cum_vacc_perc <- 0
daily_s1_2$cum_vacc_perc <- as.numeric(cumsum(daily_s1_2[,2]))
daily_s1_2$cum_atl1_perc <- 0
daily_s1_2$cum_atl1_perc <- as.numeric(cumsum(daily_s1_2[,3]))
###################################################################
## 80498 SILVERTHORNE-2 SUMMARY
daily_s2 <- as.data.frame(group_by(sub_80498,vaccination_date) %>%
                            summarize(sum_sd_p = sum(sd_p=="1"),
                                      sum_dd_p = sum(dd_p=="1"),
                                      sum_sd_m = sum(sd_m=="1"),
                                      sum_dd_m = sum(dd_m=="1"),
                                      sum_sd_j = sum(sd_j=="1"),
                                      total_doses = sum(sum_sd_p,sum_dd_p, sum_sd_m, sum_dd_m, sum_sd_j),
                                      sum_full = sum(sum_dd_p, sum_dd_m, sum_sd_j),
                                      diff_p = sum_sd_p-sum_dd_p,
                                      diff_m = sum_sd_m-sum_dd_m,
                                      sum_partial = sum(diff_p,diff_m),
                                      partial_perc = sum_partial/c80498*100,
                                      vaccinated_perc = (sum_full/c80498)*100,
                                      atl1 = sum(partial_perc,vaccinated_perc)))

## VACCINE COVERAGE TREND GRAPH 
daily_s2 <- daily_s2[,c(1,13,14)]

daily_s2$date = daily_s2$vaccination_date
daily_s2$date <- format(as.Date(daily_s2$date, "%m/%d/%Y"), "%Y-%m-%d")  

daily_s2$sort_ID <- as.integer(factor(with(daily_s2, paste(date))))
daily_s2_2 <- arrange(daily_s2, sort_ID); head(daily_s2)

## CUMULATIVE SUM ACROSS ALL ROWS
daily_s2_2$cum_vacc_perc <- 0
daily_s2_2$cum_vacc_perc <- as.numeric(cumsum(daily_s2_2[,2]))
daily_s2_2$cum_atl1_perc <- 0
daily_s2_2$cum_atl1_perc <- as.numeric(cumsum(daily_s2_2[,3]))
#############################################################################################################################
## COMPILE ZIP CODE VACCINE
daily_s2_2$zip_code = 80435
daily_f2$zip_code = 80443

daily_b2$zip_code = 80424
daily_d2$zip_code = 80435

comp_zip_vacc <- rbind(daily_s2_2,daily_f2,daily_b2,daily_d2)

D2 <- daily_d2[,c(1,8,6)]
D2 <- tail(D2, -1)

D2 <- na.locf(D2)


#D2 <- subset(D2, vaccination_date > '12/14/2020')
S2 <- daily_s2_2[,c(1,8,6)]
F2 <- daily_f2[,c(1,8,6)]
B2 <- daily_b2[,c(1,8,6)]

#garbage
date_proxy <- comp_race_ts4[,c(1:2)]
comp1 <- merge(x=date_proxy, y=D2, by=c("vaccination_date"), all.x=T)
colnames(comp1)[4]<-"80435"
comp1$Hispanic= NULL
comp1$zip_code= NULL
comp2 <- merge(x=comp1, y=S2, by=c("vaccination_date"), all.x=T)
colnames(comp2)[4]<-"80498"
comp2$zip_code= NULL
comp3 <- merge(x=comp2, y=F2, by=c("vaccination_date"), all.x=T)
comp3$zip_code=NULL
colnames(comp3)[4]<-"80443"
comp4 <- merge(x=comp3, y=B2, by=c("vaccination_date"), all.x=T)
comp4$zip_code=NULL
colnames(comp4)[5]<-"80424"

comp4$date = comp4$vaccination_date
comp4$date <- format(as.Date(comp4$date, "%m/%d/%Y"), "%Y-%m-%d") 

comp4$sort_ID <- as.integer(factor(with(comp4, paste(date))))
comp5 <- arrange(comp4, sort_ID); head(comp5)

## ADJUST FOR MISSING VALUES WITH PREVIOUS ROW ENTRY
df_35 <- comp5[,c(1:2)]
df_35_fixed <- na.locf(df_35)
df_98 <- comp5[,c(1,3)]
df_98_fixed <- na.locf(df_98)
df_43 <- comp5[,c(1,4)]
df_43_fixed <- na.locf(df_43)
df_24 <- comp5[,c(1,5)]
df_24_fixed <- na.locf(df_24)

comp_zips_fixed <- cbind(df_35_fixed, df_98_fixed, df_43_fixed, df_24_fixed)
comp_zips_fixed2 <- comp_zips_fixed[,c(1,2,4,6,8)]

## CLEAN UP VARIABLES
colnames(comp_zips_fixed2)[1]<-"Date"
## WRITE CSV (ENG)
write.csv(comp_zips_fixed2, "vaccine_zip_time_series_eng.csv", row.names=F)
#############################################################################################################################
## SUMMARIZE VACCINE DOSES BY DATE
d2 <- subset(df, vaccination_code=="COVID-19 mRNA (MOD)" | vaccination_code=="COVID-19 mRNA (PFR)")
d1 <- subset(df, vaccination_code=="COVID-19 Vector-NR (JSN)")

vaccine_day_d2_summ <- data.frame(group_by(d2, vaccination_date) %>% 
                                 summarize(single_sum = sum(dosage_num=="1"),
                                           double_sum = sum(dosage_num=="2")))

vaccine_day_d1_summ <- data.frame(group_by(d1, vaccination_date) %>% 
                                    summarize(single_sum = sum(dosage_num=="1")))

## TRANSFORM DAILY
#vd1 <- vaccine_day_summ[,c("vaccination_date", "single_sum")]
#vd1$Dose = "1"
#colnames(vd1)[2]<-"dose_sum"
#vd2 <- vaccine_day_summ[,c("vaccination_date", "double_sum")]
#vd2$Dose = "2"
#colnames(vd2)[2]<-"dose_sum"

#daily_compile<- rbind(vd1, vd2)
#write.csv(daily_compile, "vaccine_daily_compile.csv", row.names=F)
## CLEAN UP COLUMN NAMES
colnames(vaccine_day_d2_summ)[1]<-"Vaccination Date"
colnames(vaccine_day_d2_summ)[2]<-"Dose 1"
colnames(vaccine_day_d2_summ)[3]<-"Dose 2"
## WRITE.CSV (ENG)
write.csv(vaccine_day_d2_summ, "vaccine_2dosage_day_summ_eng.csv", row.names=F)
####################################################################################################
## J&J VACCINE
## CLEAN UP COLUMN NAMES
colnames(vaccine_day_d1_summ)[1]<-"Vaccination Date"
colnames(vaccine_day_d1_summ)[2]<-"Dose 1"
## WRITE.CSV (ENG)
write.csv(vaccine_day_d1_summ, "vaccine_1dosage_day_summ_eng.csv", row.names=F)
####################################################################################################
## MILESTONE SUMMARY TABLE
## INCIDENCE
incid <- head(pos_df2, 1)
incid$Status = 0
incid$Status[which(incid$`Rolling 7-Day Positive Cases Per 100,000` > 500)] <- "Level Orange: High Risk"
incid$Status[which(incid$`Rolling 7-Day Positive Cases Per 100,000` <= 500)] <- "Level Yellow: Concern"
incid$Status[which(incid$`Rolling 7-Day Positive Cases Per 100,000` <= 250)] <- "Level Blue: Caution"
incid$Status[which(incid$`Rolling 7-Day Positive Cases Per 100,000` <= 100)] <- "Level Green: Little to No Threat"

incid$Milestone="7-day Cumulative Incidence Rate"
incid <- incid[,c(9,2, 8)]
colnames(incid)[2]<-"Metric"
incid$Metric <- format(round(incid$Metric, 1), nsmall = 1)
## POSITIVITY
pos_rate <- head(pcr_pos_perc_eng, 1)
pos_rate$Status = 0
pos_rate$Status[which(pos_rate$`Rolling 7-Day Percent of Tests Positive` >= 10)] <- "Level Red: Severe Risk"
pos_rate$Status[which(pos_rate$`Rolling 7-Day Percent of Tests Positive`< 10)] <- "Level Orange: High Risk"
pos_rate$Status[which(pos_rate$`Rolling 7-Day Percent of Tests Positive` <= 7.5)] <- "Level Yellow: Concern"
pos_rate$Status[which(pos_rate$`Rolling 7-Day Percent of Tests Positive` <= 5)] <- "Level Blue: Caution"
pos_rate$Milestone="7-day Average Positivity (%)"
pos_rate <- pos_rate[,c(9,2,8)]
colnames(pos_rate)[2]<-"Metric"
pos_rate$Metric <- format(round(pos_rate$Metric, 1), nsmall = 1)

## HOSPITAL OCCUPANCY
hosp_occ_perc<- head(hosp_occ, 1)
hosp_occ_perc$Status = 0
hosp_occ_perc$Status[which(hosp_occ_perc$`Bed Occupancy (%)` > 80 | hosp_occ_perc$`Bed Occupancy (%)` == 80)] <- "Level Red: Severe Risk"
hosp_occ_perc$Status[which(hosp_occ_perc$`Bed Occupancy (%)` < 80)] <- "Level Blue: Caution"
hosp_occ_perc$Milestone = "Hospital Occupancy (%)"
hosp_occ_perc <- hosp_occ_perc[,c(7, 2,6)]
colnames(hosp_occ_perc)[2]<-"Metric"
hosp_occ_perc$Metric <- format(round(hosp_occ_perc$Metric, 1), nsmall = 1)

## TESTING RATE
test_rate<- head(pcr_test_eng, 1)
test_rate$Status = 0
test_rate$Status[which(test_rate$`Daily Testing Conducted per 1,000 population` == 0.75 | test_rate$`Daily Testing Conducted per 1,000 population` < 0.75)] <- "Concern: Below Minimum Threshold"
test_rate$Status[which(test_rate$`Daily Testing Conducted per 1,000 population` > 0.75)] <- "Excellent: Above Minimum Threshold"
test_rate$Milestone = "Testing Rate"
test_rate <- test_rate[,c(8, 2,7)]
colnames(test_rate)[2]<-"Metric"
test_rate$Metric <- format(round(test_rate$Metric, 1), nsmall = 1)


tp_fv <- subset(tp, tp$`Level of Vaccine Coverage`=="Fully Vaccinated")
tp_fv$Milestone = "Vaccine Coverage"
tp_fv$Metric = ""

tp_fv$Status=0
tp_fv$Status[which(tp_fv$Percentage < 60)] <- "Level Yellow: Concern"
tp_fv$Status[which(tp_fv$Percentage >= 60 & tp_fv$Percentage < 70)] <- "Level Blue: Caution"
tp_fv$Status[which(tp_fv$Percentage >= 70)] <- "Level Green: Little to No Threat"

top_vacc = tp_fv[,c(3,4,5)]
bot_vacc = tp[c(1,3),]

bot_vacc$Milestone_fix=0
bot_vacc$Milestone_fix[which(bot_vacc$`Level of Vaccine Coverage`=="Fully Vaccinated")] <- "Fully Vaccinated (%)"
bot_vacc$Milestone_fix[which(bot_vacc$`Level of Vaccine Coverage`=="At least 1 Vaccine Dose")] <-"At least 1 Vaccine Dose (%)"

bot_vacc$Milestone = NULL
bot_vacc <- bot_vacc[,c(3,2)]
colnames(bot_vacc)[1]<-"Milestone"
colnames(bot_vacc)[2]<-"Metric"

bot_vacc$Status = ""

bot_vacc$Percentage <- format(round(bot_vacc$Percentage, 1), nsmall = 1)

hosp_occ_perc$Milestone = "Hospital Occupancy (%)"
hosp_occ_perc <- hosp_occ_perc[,c(7, 2,6)]
colnames(hosp_occ_perc)[2]<-"Metric"
hosp_occ_perc$Metric <- format(round(hosp_occ_perc$Metric, 1), nsmall = 1)



## COMBINE FILES (ENG)
m_table <- rbind(incid, hosp_occ_perc, top_vacc,bot_vacc)
## WRITE CSV (ENG)
write.csv(m_table, "pon_table_eng.csv", row.names=F)
####################################################################################################
## SPANISH TRANSLATIONS
## INCIDENCE (ESP)
m_table_esp <- m_table
Meta <- c("Casos Positivos Nuevos por 100,000 Personas en los Ultimos 7 dias", "Ocupacion del Hospital", "Cobertura de la Vacuna", "Totalmente Vacunado (%)", "Al menos 1 Dosis de Vacuna (%)")

m_table_esp$Meta = Meta
colnames(m_table_esp)[2]<-"Datos"
m_table_esp$Estado= ""
m_table_esp$Estado[which(m_table_esp$Status=="Level Red: Severe Risk")] <-"Nivel Rojo: Reisgo severo"
m_table_esp$Estado[which(m_table_esp$Status=="Level Yellow: Concern")] <-"Nivel Amarillo: Preocupaction"
m_table_esp$Estado[which(m_table_esp$Status=="Level Blue: Caution")] <-"Nivel Azul: Precacion"
m_table_esp$Estado[which(m_table_esp$Status=="Level Orange: High Risk")] <-"Nivel Anaranjado: Alto Riesgo"
m_table_esp$Estado[which(m_table_esp$Status=="Level Red: Severe Risk")] <-"Nivel Rojo: Reisgo severo"
m_table_esp$Estado[which(m_table_esp$Status=="Level Red: Severe Risk")] <-"Nivel Rojo: Reisgo severo"
m_table_esp$Estado[which(m_table_esp$Status=="Level Green: Little to No Threat")] <-"Nivel Verde: Sin Reisgo"
m_table_esp$Estado[which(m_table_esp$Status=="Level Green: Little to No Threat")] <-"Nivel Verde: Sin Reisgo"

m_table_esp$Estado[which(m_table_esp$Status=="Excellent: Above Minimum Threshold")] <-"Excelente"
m_table_esp$Estado[which(m_table_esp$Status=="Concern: Below Minimum Threshold")] <-"Precacion: Debajo de la Meta"


m_table_esp <- m_table_esp[,c(4,2,5)]
## WRITE CSV (ESP)
write.csv(m_table_esp, "pon_table_esp.csv", row.names=F)
####################################################################################################
## OUTBREAK SUMMARY 
require(gsheet)
g <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1oC3O0dfyyr1sAoPT6Et1Htdg4wwhIR8WictHTX7Fv_0/edit#gid=0')
g <- as.data.frame(g)
g$Full_Date <- g$`First Case / Primer Caso`
## CONVERT DATE
g$Full_Date <- format(as.Date(g$Full_Date, "%m/%d/%Y"), "%Y-%m-%d")
g$Date <- g$Full_Date
g$Date <- format(as.Date(g$Date, "%Y-%m-%d"), "%m-%Y")
g$freq=1
## SUMMARIZE NUMBER OF OUTBREAKS BY M-YYYY
ob_summ <- data.frame(group_by(g, Date) %>% 
                                    summarize(outbreak_sum = sum(freq=="1")))

## CASE MONTHLY SUM
case_month <- compiled_df2
case_month$Date_M_Y <- case_month$Date
case_month$Date_M_Y <- format(as.Date(case_month$Date_M_Y, "%Y-%m-%d"), "%m-%Y")
## SUMMARIZE NUMBER OF CASES BY M-YYYY
monthly_cases_summ <- data.frame(group_by(case_month, Date_M_Y) %>% 
                        summarize(case_sum = sum(Cases)))
## ADD CASE DATA TO OUTBREAK DF
ob_summ$Cases = monthly_cases_summ$case_sum
## CLEAN UP VARIABELS
colnames(ob_summ)[2]<-"Outbreaks"
## WRITE.CSV (ENG)
write.csv(ob_summ, "outbreak_case_month_eng.csv", row.names=F)
##################################################################################################################
## DIAGNOSTIC TEST SUMMARY
## LOAD DATA
df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/elr_rollup_Summit.txt'); dim(df)
##df <- read.csv("elr_rollup_Summit.csv", header=T); dim(df)
##############################################################################################
## FILTER DF OF ONLY PCR OUTCOMES
df <- as.data.frame(df)
##############################################################################################
## Summarize sum of  tests by dateadded
diag_tests <- data.frame(group_by(df, dateadded, test_type) %>%
                            summarize(total_tested = sum(n_tests))); dim(total_tests)


diag_tests_wide <- diag_tests %>%
  spread(dateadded, test_type, total_tested)

pcr_tests <- subset(diag_tests, test_type =="PCR"); dim(pcr_tests)
pcr_tests$test_type = NULL
colnames(pcr_tests)[2]<-"PCR"

ser_tests <- subset(diag_tests, test_type =="serology"); dim(ser_tests)
ser_tests$test_type = NULL
colnames(ser_tests)[2]<-"Serology"

ant_tests <- subset(diag_tests, test_type =="antigen"); dim(ant_tests)
ant_tests$test_type = NULL
colnames(ant_tests)[2]<-"Antigen"
################################################################################
## 7-DAY AVERAGES
pcr_tests$avg_7d <- stats::filter(pcr_tests$PCR, rep(1/7,7)); head(pcr_tests)
## CREATE SORT_ID TO ARRANGE BY NEWEST DATE
pcr_tests$sort_ID <- as.integer(factor(with(pcr_tests, paste(dateadded))))
pcr_tests <- arrange(pcr_tests, -sort_ID); head(pcr_tests)

## SHIFT FUNCTION
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
## SHIFT UP
pcr_tests$avg_7d <- shift(pcr_tests$avg_7d, 3)
## REPLACE ALL NA WITH 0 
pcr_tests$avg_7d[is.na(pcr_tests$avg_7d)] <- 0
pcr_tests$sort_ID=NULL
## RENAME VARIABLES
colnames(pcr_tests)[2]<-"Molecular"
colnames(pcr_tests)[3]<-"Molecular 7-day Average"
####################################################################################
## SEROLOGY TEST - 7DAY AVG CALCUALTIONS
ser_tests$avg_7d <- stats::filter(ser_tests$Serology, rep(1/7,7)); head(ser_tests)
## CREATE SORT_ID TO ARRANGE BY NEWEST DATE
ser_tests$sort_ID <- as.integer(factor(with(ser_tests, paste(dateadded))))
ser_tests <- arrange(ser_tests, -sort_ID); head(ser_tests)

## SHIFT UP
ser_tests$avg_7d <- shift(ser_tests$avg_7d, 3)
## REPLACE ALL NA WITH 0 
ser_tests$avg_7d[is.na(ser_tests$avg_7d)] <- 0
ser_tests$sort_ID=NULL
colnames(ser_tests)[3]<-"Serology 7-day Average"
####################################################################################
## SEROLOGY TEST - 7DAY AVG CALCUALTIONS
ant_tests$avg_7d <- stats::filter(ant_tests$Antigen, rep(1/7,7)); head(ant_tests)
## CREATE SORT_ID TO ARRANGE BY NEWEST DATE
ant_tests$sort_ID <- as.integer(factor(with(ant_tests, paste(dateadded))))
ant_tests <- arrange(ant_tests, -sort_ID); head(ant_tests)

## SHIFT UP
ant_tests$avg_7d <- shift(ant_tests$avg_7d, 3)
## REPLACE ALL NA WITH 0 
ant_tests$avg_7d[is.na(ant_tests$avg_7d)] <- 0
ant_tests$sort_ID=NULL
colnames(ant_tests)[3]<-"Antigen 7-day Average"
####################################################################################
## COMPILE TESTS
comp_tests <- merge(x=pcr_tests,y=ser_tests,by=c("dateadded"),all.x=T);dim(comp_tests)
comp_tests2 <- merge(x=comp_tests,y=ant_tests,by=c("dateadded"),all.x=T);dim(comp_tests)
df[is.na(df)] <- 0

comp_tests2[is.na(comp_tests2)] <- 0
comp_tests3 <- subset(comp_tests2, dateadded > "2020-03-12")
colnames(comp_tests3)[1] <-"Date"
## WRITE.CSV (ENG)
write.csv(comp_tests3, "diagnostic_testing_hist_eng.csv", row.names=F)
comp_tests3_30d <- tail(comp_tests3, 30)
write.csv(comp_tests3_30d, "diagnostic_testing_30d_eng.csv", row.names=F)
####################################################################################
## TRANSLATE TO ESP
comp_tests3_esp <- comp_tests3
##RENAME VARIABELS
colnames(comp_tests3_esp)[1]<-"Fecha"
colnames(comp_tests3_esp)[3]<-"Promedio molecular de 7 dias"
colnames(comp_tests3_esp)[4]<-"Serologia"
colnames(comp_tests3_esp)[5]<-"Promedio serologia de 7 dias"
colnames(comp_tests3_esp)[6]<-"Antigeno"
colnames(comp_tests3_esp)[7]<-"Promedio serologia de 7 dias"
## WRITE CSV (ESP - HIST)
write.csv(comp_tests3_esp, "diagnostic_testing_hist_esp.csv", row.names=F)
comp_tests3_esp_30d <- tail(comp_tests3_esp, 30)
write.csv(comp_tests3_esp_30d, "diagnostic_testing_30d_esp.csv", row.names=F)
####################################################################################
## TIME SERIES VIDEO ANIMATION
incid1 <-pos_hist
colnames(incid1)[2]<- "rate"
incid2 <- incid1[,c("Date", "rate")]

## CREATE MM-YYYY
incid2$m_yr = incid2$Date
incid2$m_yr <- format(as.Date(incid2$Date, "%Y-%m-%d"), "%m-%Y")

## SUMMARIZE DAILY REPORT
incid_month <- data.frame(group_by(incid2, m_yr) %>% 
                           summarize(mean_rate = mean(rate))); print(incid_month)
incid_month$min = 0
incid_month$t1 = 100
incid_month$t2 = 300
incid_month$t3 = 500
incid_month$t4 = 800
## CLEAN UP VARIABLE NAMES
colnames(incid_month)[1]<-"Date"
colnames(incid_month)[2]<-"Average monthly 7-day Cumulative Incidence Rate"
colnames(incid_month)[4]<-"Level Blue: Caution"
colnames(incid_month)[5]<-"Level Yellow: Concern"
colnames(incid_month)[6]<-"Level Orange: High Risk"
colnames(incid_month)[7]<-"Level Red: Severe Risk"

write.csv(incid_month, "pon_monthly_incid.csv", row.names=F)
##########################################################################################################################################################
## SUMMARY TABLE - CURRENT VS. PREVIOUS
df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/cedrs_Summit.txt'); dim(df)
df <- as.data.frame(df)
## FILTER OUT DUPLICATE ENTRIES
df$unique_id <- as.integer(factor(with(df, paste(profileid, first_name, last_name, date_of_birth))))
df<- subset(df, !duplicated(unique_id))
###############################################################################################################
##SASMC HOSPITAL DATA FROM GOOGLE SHEETS
hdf <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1cuJdqAzx_kh6v8zkmDlhsV9SDU7wSO-9QHH7grKGuwE/edit?usp=sharing')
hdf <- as.data.frame(sasmc)
hospitalized_sum <- sum(hdf$covid_pac)
###############################################################################################################
elr_df <- fread('https://www.dcphrapps.dphe.state.co.us/Authenticated/Reporting/Downloads/elr_rollup_Summit.txt'); dim(df)
elr_df <- as.data.frame(elr_df)
##
TT_sum <- elr_df %>%
  filter(test_type == "PCR") %>% 
  select(dateadded, total_tests) %>% 
  distinct() %>% 
  mutate(sum = sum(total_tests))
TT_1 <- tail(TT_sum, 1)
TT = TT_1$sum
###############################################################################################################
## DAILY VALUES ENTERED
## SUMMARIZE DAILY REPORT
daily_summ1 <- data.frame(group_by(df, reporteddate) %>% 
                           summarize(pos_confirm_sum = sum(casestatus=="confirmed"),
                                     prob_sum = sum(casestatus=="probable"),
                                     total_cases = sum(pos_confirm_sum, prob_sum),
                                     death_sum = sum(outcome=="Patient died"))); print(daily_summ1)

daily_summ1$reporteddate <- as.Date(daily_summ1$reporteddate)

## SUMMARIZE OUTBREAK DATA
daily_ob <- data.frame(group_by(g, Full_Date) %>% 
                            summarize(obreak_count = sum(freq=="1"))); print(daily_summ1)
colnames(daily_ob)[1]<-"reporteddate"
daily_ob$reporteddate <- as.Date(format(as.Date(daily_ob$reporteddate, "%Y-%m-%d"), "%Y-%m-%d"))
###############################################################################################################
## SUMMARIZE DAILY COVID PATIENT DATA 
hdf <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1cuJdqAzx_kh6v8zkmDlhsV9SDU7wSO-9QHH7grKGuwE/edit?usp=sharing')
hdf <- as.data.frame(sasmc)
colnames(hdf)[1]<-"reporteddate"
#hdf$reporteddate <- as.Date(format(as.Date(hdf$reporteddate, "%m/%d/%Y"), "%Y-%m-%d"))
hdf$sort_ID <- as.integer(factor(with(hdf, paste(reporteddate))))
hdf <- arrange(hdf, -sort_ID); head(hdf)

hdf<- hdf[,c("reporteddate", "covid_pac")]
#colnames(hdf)[1]<- "reporteddate"
colnames(hdf)[2]<-"covid_pac"
colnames(TT_sum)[1]<-"reporteddate"
###############################################################################################################
## MERGE DIFFERENT DF'S
daily_summ2 <- merge(x = daily_summ1, y = daily_ob, by = c("reporteddate"), all.x=T)
daily_summ3 <- merge(x = daily_summ2, y = hdf, by = c("reporteddate"), all.x=T)
daily_summ4 <- merge(x = daily_summ3, y = TT_sum, by = c("reporteddate"), all.y=T)
###############################################################################################################
## REPLACE NAs
daily_summ4[is.na(daily_summ4)] <- 0
###############################################################################################################
## ARANGE BY REPORTEDDATE
daily_summ4$sort_ID <- as.integer(factor(with(daily_summ4, paste(reporteddate))))

daily_summ4 <- arrange(daily_summ4, -sort_ID); head(daily_summ4)
###############################################################################################################
## INCIDENCE RATES
daily_summ5 <- daily_summ4 %>%
  arrange(sort_ID,reporteddate) %>%
  mutate(rollapply_sum =zoo::rollapplyr(total_cases, 7, sum, partial = TRUE)) # requires package zoom
##############################################################################################
##############################################################################################
## CUMULATIVE 7-DAY INCIDENCE (TOTAL NUMBER OF POSITIVE CASES PER 7D) 
## MUST MULTIPLE BY SUM OF POSITIVE CASES AND POPULATION FACTOR
## POPULATION VALUE IS TAKEN FROM CO DEMOGRAPHY 2018 POPULATION SIZE
daily_summ5$pos_case_7d <- daily_summ5$rollapply_sum*(100000/30974)
##############################################################################################
## SUB OUT DIFF DF'S
daily_summ5 <- arrange(daily_summ5, -sort_ID); head(daily_summ5)

sum_7d <- head(daily_summ5, 7)
sum_14d <- head(daily_summ5, 14)
prev_7d <- tail(sum_14d, 7)
sum_28d <- head(daily_summ5, 28)
##############################################################################################
## SUMMARIZE MEANS - CURRENT
curr_7d <- data.frame(group_by(sum_7d, ) %>% 
                            summarize(pos_confirm_sum = mean(pos_confirm_sum),
                                      prob_sum = mean(prob_sum),
                                      total_cases = mean(total_cases),
                                      death_sum = mean(death_sum),
                                      obreak_count = mean(obreak_count),
                                      covid_pac = mean(covid_pac),
                                      incid = mean(pos_case_7d)))
##############################################################################################
## SUMMARIZE MEANS - PREV 28-DAY
prev_28d <- data.frame(group_by(sum_28d, ) %>% 
                         summarize(confirm_sum = mean(pos_confirm_sum),
                                   prob_sum = mean(prob_sum),
                                   total_cases = mean(total_cases),
                                   death_sum = mean(death_sum),
                                   obreak_count = mean(obreak_count),
                                   covid_pac = mean(covid_pac),
                                   incid = mean(pos_case_7d)))

##############################################################################################
## DIFFERENCES PREV/CURRENT - 
## 0 = INCREASING
## 1 = DECREASING
## 3 = NO CHANGE
curr_7d$pos_con_status =0 
curr_7d$pos_con_status[which(curr_7d$pos_confirm_sum < prev_28d$confirm_sum)] <- "1"

curr_7d$prob_sum_status =0 
curr_7d$prob_sum_status[which(curr_7d$prob_sum < prev_28d$prob_sum)] <- "1"
curr_7d$prob_sum_status[which(curr_7d$prob_sum == prev_28d$prob_sum)] <- "3"
curr_7d$prob_sum_status[which(curr_7d$prob_sum == "0" & prev_28d$prob_sum == "0")] <- "None"

curr_7d$total_cases_status =0 
curr_7d$total_cases_status[which(curr_7d$total_cases< prev_28d$total_cases)] <- "1"
curr_7d$total_cases_status[which(curr_7d$total_cases == prev_28d$total_cases)] <- "3"
curr_7d$total_cases_status[which(curr_7d$total_cases == "0" & prev_28d$total_cases == "0")] <- "None"

curr_7d$death_sum_status =0 
curr_7d$death_sum_status[which(curr_7d$death_sum< prev_28d$death_sum)] <- "1"
curr_7d$death_sum_status[which(curr_7d$death_sum == prev_28d$death_sum)] <- "3"
curr_7d$death_sum_status[which(curr_7d$death_sum == "0" & prev_28d$death_sum == "0")] <- "None"


curr_7d$covid_pac_status =0 
curr_7d$covid_pac_status[which(curr_7d$covid_pac< prev_28d$covid_pac)] <- "1"
curr_7d$covid_pac_status[which(curr_7d$covid_pac == prev_28d$covid_pac)] <- "3"
curr_7d$covid_pac_status[which(curr_7d$covid_pac == "0" & prev_28d$covid_pac== "0")] <- "None"



curr_7d$obreak_status =0 
curr_7d$obreak_status[which(curr_7d$obreak_count< prev_28d$obreak_count)] <- "1"
curr_7d$obreak_status[which(curr_7d$obreak_count == prev_28d$obreak_count)] <- "3"
curr_7d$obreak_status[which(curr_7d$obreak_count == "0" & prev_28d$obreak_count == "0")] <- "None"

curr_7d$incid_status =0 
curr_7d$incid_status[which(curr_7d$incid< prev_28d$incid)] <- "1"
curr_7d$incid_status[which(curr_7d$incid == prev_28d$incid)] <- "3"
curr_7d$incid_status[which(curr_7d$incid == "0" & prev_28d$incid == "0")] <- "None"


type_eng =        c("7-day Incidence",   "Confirmed Cases", "Probable Cases", "Total Cases", "Hospitalizations", "Deaths Among Cases", "Outbreaks")
daily_7d =        c(curr_7d$incid,        curr_7d$pos_confirm_sum,  curr_7d$prob_sum,  curr_7d$total_cases,  curr_7d$covid_pac,  curr_7d$death_sum,  curr_7d$obreak_count)
daily_28d =       c(prev_28d$incid,       prev_28d$confirm_sum, prev_28d$prob_sum, prev_28d$total_cases, prev_28d$covid_pac, prev_28d$death_sum, prev_28d$obreak_count)
daily_7d_status = c(curr_7d$incid_status, curr_7d$pos_con_status, curr_7d$prob_sum_status, curr_7d$total_cases_status, curr_7d$covid_pac_status, curr_7d$death_sum_status, curr_7d$obreak_status)
summary_eng <- data.frame(type_eng, daily_7d, daily_28d, daily_7d_status)

summary_eng$daily_7d <- format(round(summary_eng$daily_7d, 2), nsmall = 1)
summary_eng$daily_28d <- format(round(summary_eng$daily_28d, 2), nsmall = 1)


summary_eng$daily_7d_status[which(summary_eng$daily_7d_status=="0")]<-"Increasing"
summary_eng$daily_7d_status[which(summary_eng$daily_7d_status=="1")]<-"Decreasing"
summary_eng$daily_7d_status[which(summary_eng$daily_7d_status=="3")]<-"No Change"
## CHANGE COLUMN NAMES
colnames(summary_eng)[1]<- "Metric"
colnames(summary_eng)[2]<- "Daily average (last 7 days)"
colnames(summary_eng)[3]<- "Daily average (last 28 days)"
colnames(summary_eng)[4]<- "Trend"
##############################################################################################
## wRITE CSV (ENG)
write.csv(summary_eng, "comparison_table_eng.csv", row.names=F)
##############################################################################################
## CONVERT TO ESP
compar_esp <- summary_eng

colnames(compar_esp)[1]<-"Metrico"
colnames(compar_esp)[2]<-"Promedio (7 dias anteriores)"
colnames(compar_esp)[3]<-"Promedio (28 dias anteriores)"
colnames(compar_esp)[4]<-"Tendencia"

## change rows
compar_esp$Metrico[which(compar_esp$Metrico=="7-day Incidence")] <- "Incidencia de 7 dias"
compar_esp$Metrico[which(compar_esp$Metrico=="Confirmed Cases")] <- "Casos confirmados"
compar_esp$Metrico[which(compar_esp$Metrico=="Probable Cases")] <- "Casos probables"
compar_esp$Metrico[which(compar_esp$Metrico=="Total Cases")] <- "Total de casos"
compar_esp$Metrico[which(compar_esp$Metrico=="Hospitalizations")] <- "Hospitalizations"
compar_esp$Metrico[which(compar_esp$Metrico=="Outbreaks")] <- "Brotes"

compar_esp$Tendencia[which(compar_esp$Tendencia=="Increasing")] <- "Creciente"
compar_esp$Tendencia[which(compar_esp$Tendencia=="Decreasing")] <- "Decreciente"
compar_esp$Tendencia[which(compar_esp$Tendencia=="None")] <- "Ninguno"
  ## WRITE CSV (ESP)
write.csv(compar_esp, "comparison_table_esp.csv", row.names=F)
##############################################################################################
## PRINT OUTBREAK TABLE FROM GOOGLE SHEETS
ob_table <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1oC3O0dfyyr1sAoPT6Et1Htdg4wwhIR8WictHTX7Fv_0/edit?usp=sharing')

## WRITE CSV 
write.csv(ob_table,"ob_table.csv", row.names = F)

