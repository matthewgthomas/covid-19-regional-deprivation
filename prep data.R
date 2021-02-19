library(tidyverse)
library(lubridate)
library(readxl)
library(httr)

# ---- MSOA data ----
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/articles/deathsinvolvingcovid19interactivemap/2020-06-12
GET("https://www.ons.gov.uk/visualisations/dvc1134/covid-death-map/data/datadownload.xlsx", 
    write_disk(tf <- tempfile(fileext = ".xlsx")))
deaths <- read_excel(tf, skip = 7)

# MSOA-level IMD
imd <- read_csv("https://github.com/matthewgthomas/IMD/raw/master/data/English%20IMD%20-%20MSOA.csv")

pop <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/population%20estimates%20msoa11%20lad17%20lad19%20tacticall%20cell.csv")

regions <- read_csv("https://opendata.arcgis.com/datasets/3ba3daf9278f47daba0f561889c3521a_0.csv")

msoa <- 
  deaths %>% 
  left_join(pop, by = c("MSOA code" = "MSOA11CD")) %>% 
  left_join(imd, by = c("MSOA code" = "MSOA11CD")) %>% 
  left_join(regions, by = "LAD19CD") %>% 
  
  mutate(DeathRate = (`10 month total (March to December)` / pop_msoa11) * 100000) %>% 
  
  filter(!is.na(Extent))

msoa %>% 
  select(MSOA11CD = `MSOA code`, RGN19NM, DeathRate, Deaths = `10 month total (March to December)`, Score, Extent, Proportion) %>% 
  write_csv("data/deaths-msoa.csv")


# ---- Local Authority data ----
rm(list = ls())

deaths <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumDeaths28DaysByDeathDateRate&metric=cumDeaths60DaysByPublishDateRate&metric=cumDeathsByDeathDateRate&metric=cumDeathsByPublishDateRate&format=csv")

deaths <- 
  deaths %>% 
  filter(date == max(date))

GET("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833995/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))

# excel_sheets(tf)

imd <- read_excel(tf, sheet = "IMD")
imd_income <- read_excel(tf, sheet = "Income")
imd_employment <- read_excel(tf, sheet = "Employment")
imd_education <- read_excel(tf, sheet = "Education")
imd_health <- read_excel(tf, sheet = "Health")
imd_crime <- read_excel(tf, sheet = "Crime")
imd_barriers <- read_excel(tf, sheet = "Barriers")
imd_environment <- read_excel(tf, sheet = "Living")

regions <- read_csv("https://opendata.arcgis.com/datasets/3ba3daf9278f47daba0f561889c3521a_0.csv")

la <- deaths %>% 
  left_join(imd, by = c("areaCode" = "Local Authority District code (2019)")) %>% 
  left_join(imd_health, by = c("areaCode" = "Local Authority District code (2019)")) %>% 
  left_join(imd_barriers, by = c("areaCode" = "Local Authority District code (2019)")) %>% 
  left_join(regions, by = c("areaCode" = "LAD19CD")) %>% 
  
  filter(!is.na(`IMD 2019 - Extent`))

la %>% 
  select(LAD19CD = areaCode, RGN19NM, DeathRate = cumDeathsByPublishDateRate, 
         Score = `IMD - Average score`, Extent = `IMD 2019 - Extent`, Proportion = `IMD - Proportion of LSOAs in most deprived 10% nationally`, 
         Health_Score = `Health Deprivation and Disability - Average score`, Health_Proportion = `Health Deprivation and Disability - Proportion of LSOAs in most deprived 10% nationally`,
         Barriers_Score = `Barriers to Housing and Services - Average score`, Barriers_Proportion = `Barriers to Housing and Services - Proportion of LSOAs in most deprived 10% nationally`) %>% 
  write_csv("data/deaths-la.csv")
