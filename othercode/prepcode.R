#Some data prep for the dashboard
library(tidyverse)
library(sf)

#Get ordered sector lists for each SIC digit level----

#Use SIC digit numbers from SIC lookup made elsewhere
SIClookup <- read_csv('https://github.com/DanOlner/ukcompare/raw/refs/heads/master/data/SIClookup.csv')

#check order is preserved... tick
unique(SIClookup$SIC_2DIGIT_NAME)

#Create a long version that can be filtered from the CH data when digit selected
SIClookup_long <- SIClookup %>% 
  select(contains('NAME')) %>% 
  pivot_longer(cols = everything(), names_to = 'SIC_digit', values_to = 'sector_name') %>% 
  mutate(
        SIC_digit = case_when(
          SIC_digit == "SIC_5DIGIT_NAME" ~ "5 digit",
          SIC_digit == "SIC_2DIGIT_NAME" ~ "2 digit",
          SIC_digit == "SIC_3DIGIT_NAME" ~ "3 digit",
          SIC_digit == "SIC_SECTION_NAME" ~ "Section"
        )#Match what's used in the dashboard
      )

#Tick
unique(SIClookup_long$SIC_digit)

#Check match in CH. Came from same lookup so should be fine.
ch <- readRDS('data/companieshouse_employees_n_sectors_southyorkshire_long.rds')

table(unique(SIClookup_long$sector_name) %in% unique(ch$sector_name))

#Check what those are... looks like a sensible list of those not present!
#Still, shouldn't matter when filtering in app
unique(SIClookup_long$sector_name)[!unique(SIClookup_long$sector_name) %in% unique(ch$sector_name)]


#Check just on sections and first selection
unique(SIClookup_long$sector_name[SIClookup_long$SIC_digit == 'Section'])


#Save
saveRDS(SIClookup_long,'data/SICorderedlookup.rds')
