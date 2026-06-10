# Download files from the Health Inequality Data Repository (HIDR)

# Citation
# The Health Inequality Data Repository API. Geneva, World Health Organization, 2025. 
# Available from https://www.who.int/data/inequality-monitor/data. [Accessed 15-04-2026]


## Attach library
library(tidyverse)
library(readxl)
library(httr)

## Get dataset ID reference list
id.key <- read.csv("https://srhdpeuwpubsa.blob.core.windows.net/whdh/HIDR/hidr_datasetid.csv")

## API base path
base.path <- "https://datasafe-h5afbhf4gwctabaa.z01.azurefd.net/api/Download/TOP"

## Download a specific dataset from the HIDR for a given dataset ID
url <- paste(base.path, id.key$dataset_id[61], "data", sep = "/")
GET(url, write_disk(tmp <- tempfile(fileext = ".xlsx")))
data <-read_xlsx(tmp)

# checking values
table(data$indicator_name)
table(data$indicator_name, data$subgroup)
table(data$indicator_name, data$dimension)
table(data$setting, data$date)
table(data$whoreg6)
table(data$setting, data$wbincome2025)

# cleaning data
WASH <- data |> 
  mutate(whoreg6 = case_match(whoreg6, "African" ~ "Africa", "European" ~ "Europe", .default = whoreg6), 
                       Update = dmy(update)) |> 
  select(-c(se, ci_lb, ci_ub, flag, ordered_dimension, subgroup_order, reference_subgroup))


# importing investment data
inflow <- read_csv("/Users/simonminasse/Documents/Data/TDS_Application_Project/US.FdiFlowsStock_20260415_184633.csv")
  
# checking and cleaning data

# check
table(inflow$Economy_Label) 

# clean
inflow2 <- inflow |> 
  filter(Year >= 2014, grepl("Africa", Economy_Label)) |> 
  mutate(date = Year) 

# check
table(inflow$Economy_Label)

# clean
fdi_inflow <- inflow2 |>  
  filter(Economy_Label == "Africa") |> 
  mutate(whoreg6 = case_when(Economy_Label == "Africa" ~ "Africa")) |> 
  select(-c(Economy_Label, Year, US_at_current_prices_Footnote, US_at_current_prices_MissingValue))
  

# importing WHO data
occ_env <- read_csv("/Users/simonminasse/Documents/Data/TDS_Application_Project/WHO_occ_env.csv")
table(occ_env$ParentLocation)

# cleaning WHO data and creating new datasets
occ <- occ_env |> 
  filter(ParentLocation == "Africa") |> select(Indicator:IsLatestYear,FactValueNumeric,Value,DateModified) |> 
  mutate(date = Period, setting = Location, whoreg6 = ParentLocation, Workforce = Indicator, DateModified = as.Date(DateModified)) |> 
  select(-c(Indicator, Period))

doc <- read_csv("/Users/simonminasse/Documents/Data/TDS_Application_Project/WHO_doc.csv") |> 
  filter(ParentLocation == "Africa") |> select(Indicator:IsLatestYear,FactValueNumeric,Value,DateModified) |> 
  mutate(date = Period, setting = Location, whoreg6 = ParentLocation, Workforce = Indicator, DateModified = as.Date(DateModified)) |> 
  select(-c(Indicator, Period))

nurse <- read_csv("/Users/simonminasse/Documents/Data/TDS_Application_Project/WHO_nurse.csv") |> 
  filter(ParentLocation == "Africa") |> select(Indicator:IsLatestYear,FactValueNumeric,Value,DateModified) |> 
  mutate(date = Period, setting = Location, whoreg6 = ParentLocation, Workforce = Indicator, DateModified = as.Date(DateModified)) |> 
  select(-c(Indicator, Period))

# previewing / checking datasets
glimpse(WASH)

glimpse(fdi_inflow)

glimpse(doc)
table(doc$Workforce, doc$date)

glimpse(nurse)
table(nurse$Workforce, nurse$date)

glimpse(occ)
table(occ$Workforce, occ$date)

# merging cleaned datasets
Workforce <- rbind(doc, nurse, occ) |> filter(date >= 2014)

WASH_fdi <- left_join(WASH, fdi_inflow, by = join_by(whoreg6, date), relationship = "many-to-many")

WASH_fdi2 <- WASH_fdi |> filter(date >= 2014)

WASH_full <- left_join(WASH_fdi2, Workforce, by = join_by(whoreg6, setting, date), relationship = "many-to-many")

# final cleaning steps
nurse_sum <- nurse |> filter(Workforce == "Nursing and midwifery personnel  (number)", setting == "Ethiopia")
nurse_work <- WASH_full |> filter(Workforce == "Nursing and midwifery personnel  (number)", setting == "Ethiopia", subgroup == "Urban")

# exporting final data for Tableau dashboards
write_csv(WASH_full, "/Users/simonminasse/Documents/Data/TDS_Application_Project/WASH_full.csv")
