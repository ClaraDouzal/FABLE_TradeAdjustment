# Author: Clara Douzal SDSN
# Trade ADjustment for checking on extracted db before trade adjustment

# Creation FAO comodity balance FAO to compare with country NET historical submissions

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(readxl)
library(countrycode)
library(here)

# Upload data -------------------------------------------------------------
# 
# mapping_product <- read.csv(here("mapping_GAMS_FAO.csv"), 
#                             sep = ";", 
#                             fileEncoding="UTF-8-BOM") %>% 
#   mutate(FAO = as.character(FAO))
# map_country <- read_excel(here("Mapping_Country_FAO_w_FABLE_Scenathon2023.xlsx")) %>% 
#   mutate(ISO = countrycode(Country_FAO, origin = 'country.name', destination = 'iso3c'))
# 

#####################################
########### STEP 1 #################
####################################


#Could be using data from 1_data_demand prod_balance as it is the same historical data
# data_crop <- read.csv(here("FAOSTAT_crops.csv"))
# data_livestock <- read.csv(here("FAOSTAT_livestock.csv"))


# Assigning the right product name 

## To add mapping between FAO items names and Products names in the calculator
# data_crop <- data_crop %>% 
#   left_join( mapping_product, by = c("Item" = "FAO"), relationship = "many-to-many")
# 
# data_livestock <- data_livestock %>% 
#   left_join(mapping_product, by = c("Item" = "FAO"), relationship = "many-to-many")
# 
# ## Just use the prod_balance table to get the equivalent of data_crop and data_livestock
# #FAO_data <- rbind(data_crop, data_livestock) %>% 
# FAO_data <- 
#   #set the right names
#   mutate(ISO = countrycode(Country, origin = 'country.name', destination = 'iso3c')) %>% 
#   left_join(map_country, by = c("ISO")) %>% 
#   #get rid of country names and only use FABLE names
#   select(-Country) %>% 
#   rename(Country = Country_FABLE) %>% 
#   mutate(Year = as.factor(Year)) %>% 
#   #Aggregate by country, product, element, year
#   group_by(Country, FPRODUCT, Element, Year) %>% 
#   summarise(Value = sum(Value, na.rm = T)) %>% 
#   #those are things computed directly into the calculator
#   pivot_wider(names_from = Element, values_from = Value) %>% 
#   mutate(net_export = pmax(`Export Quantity` - `Import Quantity`, 0),
#          net_import = pmax(`Import Quantity` - `Export Quantity`, 0)) %>% 
#   select(-`Export Quantity`, -`Import Quantity`) %>% 
#   rename("Import Quantity" = net_import) %>% 
#   rename("Export Quantity" = net_export) %>% 
#   gather(key = "Element", value = "Historical", "Import Quantity", "Export Quantity") %>% 
#   rename(Product = FPRODUCT)

#
###get data from Scenathon_report, Byproduct TABLE
#Country, Product, Year, Import_quantity, Export_quantity

#Name of excel file where data from Scenathon_report, Byproduct TABLE is stored
#Country, Product, Year, Import_quantity, Export_quantity
db_name <- "20240308_FullProductDataBase"

#data <- read_excel(here(paste0(db_name, ".xlsx"))) %>% 
data <- read.csv(here(paste0(db_name, ".csv")), sep ="") %>% 
  #before trade adjustment
  filter(iteration == 4) %>% 
  rename(Country = country) %>% 
  mutate_at(vars(year:export_quantity), as.numeric) %>% 
  rename("Import Quantity" = import_quantity) %>% 
  rename("Export Quantity" = export_quantity) %>% 
  gather(key = "Element", value = "Submitted", "Import Quantity", "Export Quantity") %>% 
  mutate(Year = as.factor(year),
         Product = as.factor(product)) %>% 
  slice(which(Year %in% c(2000, 2005, 2010, 2015, 2020))) %>% 
  select(pathway, Country, Product, Year, Element, Submitted)

#Merge the Import and Export quantities from modelled by the 
#calculator with FAO historical export and imports quantities
#for years 2000, 2005 and 2010
# data_complete <- data %>% 
#   left_join(FAO_data, by = NULL) %>% 
#   mutate(Historical = Historical/1000,
#          Product = as.factor(Product),
#          Element = as.factor(Element))
# data_complete_filtered <-  data_complete %>% 
#   #keep only products that have values
#   slice(which(Product %in% 
#                 droplevels(aggregate(Historical ~ Product, data_complete, sum)[,1]))) %>% 
#   droplevels()

#This is the data set with historical and submitted trade values that will be used to compute trade imbalances in our methodology
saveRDS(data,#_complete_filtered, 
        here(paste0(gsub("-", "",Sys.Date()), "_data_net_historical_submitted.RDS")))


#--------------------------------------------------------------------------------------------------

#####################################
########### STEP 2 #################
####################################

###get data from Scenathon_report, Byproduct TABLE
#Country, Product, Year, Import_quantity, Export_quantity
data <- read.csv(here(paste0(db_name, ".csv")), sep ="") %>% 
  filter(iteration == 4) %>% 
  rename(Country = country)
  


#The dataset devloped at the end of step 1 (i.e. data complete)
trade_name_file <- paste0(gsub("-", "",Sys.Date()), "_data_net_historical_submitted.RDS")

historical <- readRDS(here(trade_name_file))


# Processing report tab ---------------------------------------------------

data <- data %>% 
  mutate(Year = as.numeric(year),
         Import_quantity = as.numeric(import_quantity),
         Export_quantity = as.numeric(export_quantity)) %>% 
  rename(Product = product) %>% 
  mutate(ALPHA3 = Country)

#Remove live animals
#remove product that we do not adjust
indexRmv <- which(data$Product %in% c("cattle        ", #some calculators ahve lots of spaces after the live animals names
                                      "chickens      ",
                                      "pigs          ",
                                      "sheep_goats         ",
                                      "chips_and__particles",
                                      "mech_pulp",
                                      "fiber_hard_other",
                                      "fiber_soft_other",
                                      "honey",
                                      "meat_other",
                                      "cottcake",
                                      "groundnutcake",
                                      "other_olscake",
                                      "palmkernelcake",
                                      "rapecake",
                                      "sunflcake",
                                      "citrus_other",
                                      "cereal_other",
                                      "cattle",
                                      "pigs",
                                      "chickens",
                                      "sheep_goats", 
                                      "oilpalmfruit",
                                      "clove",
                                      "sugarbeet",
                                      "sugarcane",
                                      "cottoil",
                                      "sisal",
                                      "other_oil",
                                      "abaca"))

# to trade for imbalance -> want to have for one year and one product the
# total import and total export to be roughly the same

historical <- historical %>% 
  mutate(Submitted = as.numeric(Submitted)) %>% 
  group_by(pathway, Year, Product, Element) %>% 
  #compute per year per product the total import and total export modelled in calculators
  summarise(Submitted = sum(Submitted, na.rm = T))


historical <- historical %>% 
  slice(which(Year == 2020)) %>% 
  pivot_wider(names_from = Element, 
              values_from = Submitted) %>% 
  data.frame() %>% 
  #compute relative difference of export quantities with respect to import quantities
  mutate(alpha.exp = (Export.Quantity - Import.Quantity)/Import.Quantity)

#define my imbalance table, associate an alpha to each product
imbalance <- select(historical, pathway, Product, alpha.exp)

Global_import <- aggregate(Import_quantity ~ pathway + Product + Year, data, sum)
Global_export <- aggregate(Export_quantity ~ pathway + Product + Year, data, sum)

Global_trade <- left_join(Global_import, 
                          Global_export, 
                          by = NULL) %>% 
  left_join(imbalance, 
            by = c("Product"= "Product", "pathway" = "pathway"), relationship ="many-to-many")

# Adjustment factor for exports:
#   o	If World Total Exportsp,a < World Total Importsp,a*1.2 => Adj factor exportsp,a = 1,
#   o	If World Total Exportsp,a > World Total Importsp,a*1.2 => Adj factor exportsp,a = World Total Importsp,a*1.2/World Total Exportsp,a
Global_trade$AdjFactorExport <- rep(NA, nrow(Global_trade))
boolean_threshold_pos <- (Global_trade$alpha.exp >= 0)
boolean_threshold_neg <- (Global_trade$alpha.exp < 0)

boolean_export_greater <- (Global_trade$Export_quantity >= Global_trade$Import_quantity)
boolean_import_greater <- (Global_trade$Import_quantity > Global_trade$Export_quantity)

#####
Global_trade <- Global_trade %>% 
  #case when 1) exports are higher than imports historically
  #          2) exports are higher than imports in calculators
  slice(which(boolean_threshold_pos & boolean_export_greater)) %>% 
  mutate(AdjFactorExport = ifelse(Export_quantity <= pmax(rep(1.1, length(Export_quantity)), (1 + alpha.exp)) * Import_quantity,
                                  1,
                                  Import_quantity * pmax(rep(1.1, length(Export_quantity)), (1+alpha.exp))/Export_quantity)) %>% 
  rbind.data.frame(Global_trade %>% 
                     #case when 1) exports are higher than imports historically
                     #          2) exports are lower than imports in calculators
                     slice(which(boolean_threshold_pos & boolean_import_greater)) %>% 
                     mutate(AdjFactorExport = ifelse(Export_quantity >= 0.9 * Import_quantity,
                                                     1,
                                                     ((0.9 * Import_quantity)/Export_quantity)))) %>% 
  rbind.data.frame(Global_trade %>% 
                     #case when 1) exports are lower than imports historically
                     #          2) exports are lower than imports in calculators
                     slice(which(boolean_threshold_neg & boolean_import_greater)) %>% 
                     mutate(AdjFactorExport = ifelse(Export_quantity >= pmin(rep(0.9, length(Export_quantity)), (1 + alpha.exp)) * Import_quantity,
                                                     1,
                                                     Import_quantity * pmin(rep(0.9, length(Export_quantity)), (1 + alpha.exp))/Export_quantity))) %>% 
  rbind.data.frame(Global_trade %>% 
                     #case when 1) exports are lower than imports historically
                     #          2) exports are higher than imports in calculators
                     slice(which(boolean_threshold_neg & boolean_export_greater)) %>% 
                     mutate(AdjFactorExport = ifelse(Export_quantity <= 1.1*Import_quantity,
                                                     1,
                                                     (1.1 * Import_quantity)/Export_quantity)))



### Adjust trade at the country level
data_adjust <- data %>% 
  left_join(Global_trade %>%  select(pathway, Product, Year, AdjFactorExport), by = NULL) %>% 
  mutate(ExportsAdj = Export_quantity * AdjFactorExport) %>%
  #keep historical years unadjusted
  mutate(ExportsAdj = ifelse(Year %in% c(2000, 2005, 2010, 2015, 2020), 
                             Export_quantity,
                             ExportsAdj))
data_adjust <- data_adjust[-indexRmv,]

Global_trade <- Global_trade %>% 
  mutate(AdjExport = Export_quantity*AdjFactorExport)


# Re-add product I removed that were not trade adjusted
data_untouched <- data %>% 
  slice(indexRmv) %>% 
  mutate(ExportsAdj =  Export_quantity)  %>% 
  select(pathway, ALPHA3, Product, Year, Import_quantity, ExportsAdj)

# Format our final dataset ------------------------------------------------
### Do not adjust historical data i.e. 2000, 2005 and 2010


#adjusted trade to use in SCENARIO DEFINITION sheet
finaldata <- data_adjust %>% 
  select(pathway, ALPHA3, Product, Year, Import_quantity, ExportsAdj) %>% 
  rbind(data_untouched) %>% 
  arrange(pathway, ALPHA3, Product, Year) %>% 
  rename(ImportsAdj = Import_quantity) %>% 
  data.frame()



write.xlsx(finaldata, paste0(outputpath,
                             gsub("-", "",Sys.Date()),
                             "_Adjusted_Trade.xlsx"),
           row.names = F)



