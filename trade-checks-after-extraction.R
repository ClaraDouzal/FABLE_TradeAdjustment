library(dplyr)
library(tidyr)
library(ggplot2)

trade <- read.csv("C:/Users/Clara Douzal/Downloads/240415_commodities.csv") 

df <- trade %>% 
  select(pathway, country, tradeadjustment, product, year, import_quantity, export_quantity) %>% 
  pivot_wider(names_from = tradeadjustment, values_from = c(import_quantity, export_quantity)) %>% 
  mutate(Comparison_Import = import_quantity_Yes - import_quantity_No) %>% 
  filter(Comparison_Import != 0) %>% 
  select(-export_quantity_Yes, -export_quantity_No) %>% 
  droplevels()

df_beef <- trade %>% 
  select(pathway, country, tradeadjustment, product, year, import_quantity, export_quantity) %>% 
  group_by(pathway, tradeadjustment, product, year) %>% 
  reframe(import_quantity=sum(import_quantity),
          export_quantity = sum(export_quantity)) %>% 
  mutate(alpha = import_quantity/export_quantity)
  

table(df$country)
table(df$country, df$pathway)

df_all <- trade %>% 
  select(pathway, country, tradeadjustment, product, year, import_quantity, export_quantity) %>% 
  pivot_wider(names_from = tradeadjustment, values_from = c(import_quantity, export_quantity)) %>% 
  mutate(Comp_Import = import_quantity_Yes - import_quantity_No) 

# xlsx::write.xlsx(df %>% data.frame(), paste0("C:/Users/Clara Douzal/Downloads/",
#                              gsub("-", "",Sys.Date()),
#                              "_Before_After_TradeAdjustment_ImportComparison.xlsx"),
#            row.names = F)


data <- read.csv("C:/Users/Clara Douzal/Downloads/240415_indicators.csv")

df_defor <- data %>% 
  select(pathway, country, tradeadjustment, year, forestchange) %>% 
  filter(year %in% c(2015, 2020, 2025,2030)) %>% 
  pivot_wider(names_from = year, values_from = c(forestchange)) %>% 
  mutate(Comparison_Defor_25_20 = `2025` - `2020`,
         Comparison_Defor_30_22 = `2030` - `2025`) 

data_deforestation <- data %>% 
  select(tradeadjustment, pathway, country, year, forestchange, calcforest)

ggplot(data_deforestation %>% filter(tradeadjustment == "Yes"))+
  geom_line(aes(x = year, y = forestchange, colour = country))+
  facet_wrap(~pathway)



for(cur_var in colnames(data %>% select(where(is.numeric)))){
p <- ggplot(data)+
  geom_line(aes(x = year, y = get(cur_var),
                  color = pathway, 
            linetype = tradeadjustment))+
              facet_wrap(~country)+
  ggtitle(cur_var)
plot(p)
}

