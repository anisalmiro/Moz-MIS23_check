##---------------------------------------------------------------
##--Pulling down Mozambique MIS household survey data from ODK
##--Matt Worges & Eleanore Sternberg
##--November 10, 2023
##---------------------------------------------------------------

##--load libraries
library(tidyverse)
library(readxl)
library(openxlsx)
library(ruODK)
library(here)
library(lubridate)
library(knitr)
library(readr)
library(dplyr)
library(tidyr)
library(gridExtra)

##---------------------------------------------------------------
##--access ODK Central and read in the survey data
##---------------------------------------------------------------

##--set location of Rproj folder
#i_am("MIS-Moz.Rproj")

##--connect to ODK Central by passing hidden credentials
##--the linked file should be made previous to this step and stored in your project folder
##--ODK Central's OData URL contains base URL, project ID, and form ID
##--ODK Central credentials can live in .Renviron
##--run 'vignette("setup")' for setup and authentication options (will appear in 'Help' pane)

source(here("ODKC_MOZ_MIS_MAIN_CREDENTIALS.R")) #replace with your personal credential file

##--run ruODK setup to load your credentials into ruODK
ruODK::ru_setup(
  svc = Sys.getenv("ODKC_SVC"),
  un = Sys.getenv("ODKC_UN"),
  pw = Sys.getenv("ODKC_PW"),
  tz = "Africa/Maputo",
  verbose = TRUE
)

loc <- fs::path(here("/raw"))

##--manually create a folder in your R project called 'raw' and link to it


##--list the 'repeat groups' available in the submitted data
fq_svc <- ruODK::odata_service_get()
fq_svc %>% knitr::kable(.)

##--read in hh module, by each repeat group
##--main household questionnaire
hhraw <- ruODK::odata_submission_get(
  table = fq_svc$name[1], 
  local_dir = loc,
  wkt=TRUE)

##--remove empty columns from each data set

hh <- hhraw %>% 
  rename_with(.fn = ~ tolower(gsub("g1_id_", "", .x, fixed = TRUE)), .col = starts_with("g1_id_")) %>% 
  rename_with(.fn = ~ tolower(gsub("teaminfo_g_", "", .x, fixed = TRUE)), .col = starts_with("teaminfo_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("visitstatus_g_", "", .x, fixed = TRUE)), .col = starts_with("visitstatus_g_")) %>%
  rename_with(.fn = ~ tolower(gsub("geoinfo_g_", "", .x, fixed = TRUE)), .col = starts_with("geoinfo_g_")) %>% 
  rename_with(.fn = ~ tolower(gsub("hhinfo_g_", "", .x, fixed = TRUE)), .col = starts_with("hhinfo_g_"))

#writexl::write_xlsx(hh, "raw/hh.csv")
write.csv(hh, "raw/hh.csv", row.names = FALSE)

#hh <- read.csv(here("raw/hh.csv"))


##--clear environment
#rm(fq_svc)
#rm(hhraw)
#rm(loc)

##---------------------------------------------------------------
##--restrict data set and recode variables of interest
##---------------------------------------------------------------

##--restrict data frame: earliest date is Nov 1, 2023
hh_copy <- hh %>% 
  filter(end_survey > "2023-11-01",
         start_survey > "2023-11-01")

##--set end_survey as date variable
hh_copy$end_survey <- as.Date(hh_copy$end_survey)

##--recode province & district (numeric to character)
hh_copy$prov_char <- NA
hh_copy$prov_char <- ifelse(hh_copy$province=="9", "Gaza",
                            ifelse(hh_copy$province=="10", "Maputo Provincia",
                                   ifelse(hh_copy$province=="11", "Maputo Cidade", NA)))

hh_copy$dist_char <- plyr::revalue(hh_copy$district, 
                                   c("901"="Cidade de Xai-Xai",
                                     "902"="Bilene",
                                     "903"="Chibuto",
                                     "904"="Chicualacuala",
                                     "905"="Chigubo",
                                     "906"="Chokwe",
                                     "907"="Guija",
                                     "908"="Mabalane",
                                     "909"="Mandlakaze",
                                     "910"="Massangena",
                                     "911"="Massingir",
                                     "912"="Limpopo",
                                     "913"="Chongoene",
                                     "914"="Mapai",
                                     "1001"="Matola",
                                     "1002"="Boane",
                                     "1003"="Magude",
                                     "1004"="Manhiça",
                                     "1005"="Marracuene",
                                     "1006"="Matutuine",
                                     "1007"="Moamba",
                                     "1008"="Namaacha",
                                     "1101"="Kampfumo",
                                     "1102"="Nlhamankulu",
                                     "1103"="Kamaxaquene",
                                     "1104"="Kamavota",
                                     "1105"="Kamubukwana",
                                     "1106"="Katembe",
                                     "1107"="Kanyaka"))

##---------------------------------------------------------------
##--create table of enumerator by number of completed surveys
##---------------------------------------------------------------

##--use a workaround to fix the spelling of each enumerator's name, if entered/spelled differently
length(unique(hh_copy$surveyor_id)) #total number of records that should be in enum_name

#Metodo para remover duplicados na basereferente a repeticao imrs id.
hh_copy$dist_same <- ifelse(hh_copy$dist_calc==hh_copy$dist_char, "SAME", "DIFFERENT")

##--set up dataframe for plots
hh_dist <- hh_copy %>% 
  distinct(hh_id_field, .keep_all = TRUE) %>%
  filter(visit_result=="1") %>%
  filter(dist_same=="SAME") %>%
  group_by(prov_char, dist_char) %>%
  count(area) %>%
  rename(Household = n,
         Province = prov_char,
         District = dist_char,
         EA = area) %>%
  arrange(Province, District, EA)



enum_name <- hh_copy %>%
  select(surveyor_id, name_surveyor) %>%
  distinct(surveyor_id, .keep_all = TRUE) %>%
  rename(enum_name = name_surveyor)

##--left join enum_name to hh_copy
hh_copy <- left_join(hh_copy, enum_name, by="surveyor_id")


##--create table that shows number of completed surveys by enumerator and date
##--no location; just a count of HH modules submitted/completed by enumerator and date
##--create table that shows number of HH modules submitted/completed by enumerator, date, and location
hh_enum_EA <- hh_copy %>% 
  filter(visit_result==1,
         end_survey >= "2023-11-27") %>%
  group_by(prov_char, dist_char, area, surveyor_id, enum_name) %>%
  count(end_survey) %>%
  rename(Household = n,
         Enumerator = enum_name,
         Enum_ID = surveyor_id,
         Province = prov_char,
         District = dist_char,
         EA = area) %>%
  arrange(Province, District, EA, Enum_ID, Enumerator)



##--send to Excel
writexl::write_xlsx(hh_enum_EA, "QOS/Enum_EA_check_29.11.xlsx")
writexl::write_xlsx(hh_enum, "QOS/Enum_check_29.11.xlsx")

##---------------------------------------------------------------
##--plot number of surveys by EA
##---------------------------------------------------------------

##--set up dataframe for plots
hh_dist <- hh_copy %>% 
  filter(visit_result==1) %>%
  group_by(prov_char, dist_char) %>%
  count(area) %>%
  rename(Household = n,
         Province = prov_char,
         District = dist_char,
         EA = area) %>%
  arrange(Province, District, EA)

##--import data from Survey Team showing those EAs reported as complete
#ReportComp <- as.data.frame(read_excel("ReportedComplete.xlsx"))
ReportComp <- as.data.frame(read_excel("ReportedComplete.xlsx")) %>% 
  distinct(EA, .keep_all = TRUE)


##--left join to hh_dist
hh_dist <- left_join(hh_dist, ReportComp, by="EA")

##--finish categorizing survey status by EA
hh_dist$ReportComp <- ifelse(hh_dist$Household<25 & is.na(hh_dist$ReportComp), "Incompleto",
                             ifelse(hh_dist$Household>=25 & is.na(hh_dist$ReportComp), "Limite alcancado",
                                    hh_dist$ReportComp))

##--create graph title as variable inside dataframe
hh_dist$graph_title <- NA
hh_dist$graph_title <- paste(hh_dist$Province, hh_dist$District, sep = " - ")

##--create looping variables for graphs
dist_name <- dput(as.character(unique(hh_dist$District)))

##--create storage place for plots
plot_list = list()

for (i in dist_name) {
  
  temp <- ggplot(subset(hh_dist, District %in% c(i)),
                 aes(x=as.factor(EA), y=Household, fill=ReportComp)) + 
    geom_bar(stat="identity") +
    geom_hline(aes(yintercept=25, 
                   linetype="Goal (25 HHs)"), #set linetype below (see 'values')
               col="blue", 
               size=1.25) +
    scale_fill_manual(name="", 
                      values = c("Incompleto"="red",
                                 "Limite alcancado"="turquoise",
                                 "Completos Reportados"="green"),
                      drop = FALSE) +
    scale_linetype_manual(name="", 
                          values=2, #dotted line 
                          guide=guide_legend(override.aes=list(color=c("blue")))) +
    labs(title=i) + 
    ylab("Number of Completed Household Surveys") + 
    xlab("Enumeration Area Code") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
          legend.position="bottom",
          text=element_text(size=15))
  
  plot_list[[i]] = temp
  
  ggsave(filename = "EA_checks/plots.pdf", 
         plot = marrangeGrob(plot_list, nrow=1, ncol=1), 
         width = 15, height = 9)
  
  #ggsave(paste0("EA_checks/",i,".png"),
  #       plot=temp,
  #       width=16.8, height=9.4, units="in")
  
}

