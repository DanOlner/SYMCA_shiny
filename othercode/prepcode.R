#Some data prep for the dashboard
library(tidyverse)
library(sf)


#Convert SY boundaries to latlon----

# sy_boundaries <- st_read("data/mapdata/sy_localauthorityboundaries.shp")

#Convert to latlon and save as compressed
# sy_boundaries <- sy_boundaries %>% st_transform("EPSG:4326")
# saveRDS(sy_boundaries, 'data/mapdata/sy_localauthorityboundaries.rds')




# Prep ML sector classification version (2026) to match existing format----

# Via companies house open

# Just looking at the existing one, checking structure
ch = readRDS('data/companieshouse_employees_n_sectors_southyorkshire_long.rds')

ch <- ch %>% 
  mutate(
    employee_diff_percent = round(employee_diff_percent)
  )

# View sample...
ch %>% sample_n(200) %>% View


# OK, get the *much smaller, only ~1.5K firms* data via companies house open
# With the four bespoke categories in
chml = arrow::read_parquet('local/data/samplebatch_setfit_classified.parquet')

# Join with geocoded CH data (latest)
sy = readRDS('local/data/sy_ch_PROCESSED_Dec2025.rds')

sy = sy %>%
  inner_join(
    chml %>% select(CompanyName,CompanyNumber,accountcode,website,setfit_health_tech:setfit_best_sector),
    by = c('CompanyName','CompanyNumber','accountcode')
  )

# Clean invalid UTF-8 characters from text columns that break tmap
# (web-scraped text often contains malformed characters)
clean_utf8 <- function(x) {
  if (is.character(x)) {
    iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
  } else {
    x
  }
}

sy <- sy %>% mutate(across(where(is.character), clean_utf8))

# Very little of the data in ch columns actually gets used
# I think I only need these...
sy = sy %>% 
  select(Company,CompanyNumber,IncorporationDate,enddate,Employees_thisyear,Employees_lastyear,website,
         health_tech = setfit_health_tech,
         clean_energy = setfit_clean_energy,
         advanced_manufacturing = setfit_advanced_manufacturing,
         defence = setfit_defence
         ) %>% 
  mutate(
  employee_diff_percent = round(percent_change(Employees_lastyear,Employees_thisyear),1)
)

# Keep wide format - one row per firm, with percentile columns for each sector
# This allows independent filtering on each sector's percentile
sy = sy %>%
  mutate(
    health_tech_percentile = percent_rank(health_tech),
    clean_energy_percentile = percent_rank(clean_energy),
    advanced_manufacturing_percentile = percent_rank(advanced_manufacturing),
    defence_percentile = percent_rank(defence)
  )

# Convert to lonlat...
sy = sy %>% st_transform("EPSG:4326")

# OK, save for shiny use
saveRDS(sy, 'data/ch_ml_firms.rds')



#Prep companies house DF to be used live (order of this may be messy!)----

#Quick hack to check toggle switch works - 
#Use existing col name being used, reassign that to the two display columns
#So rename the original first
#Mutate: 1 - copy employees_thisyear into new col employees_mostrecent
#We'll then use "Employees_thisyear" as the stand in "variable column" to overwrite
# ch <- ch %>%
#   mutate(
#     employees_mostrecent = Employees_thisyear
#   )


#Make longer so most recent employee count and percent change are in one column so can filter on it
#65mb before... 90mb after. Args for not just making longer the whole time! Toh it's in server memory, not being sent to client
# chk <- ch %>%
#   pivot_longer(cols = c(Employees_thisyear,employee_diff_percent),names_to = 'display_val', values_to = 'value')

#Convert to latlon and resave
# ch <- ch %>% st_transform("EPSG:4326")

#Two entries with employee values filled in by error, from financials
#Remove
# ch <- ch %>% filter(!CompanyNumber %in% c('08638732','11756651'))
# saveRDS(ch,'data/companieshouse_employees_n_sectors_southyorkshire.rds')

#Make SIC digit names long, so selecting by digit level is a simple filter, not faffing with columns
# ch <- ch %>%
#   select(-c(SICCode.SicText_1:SICCode.SicText_4,SIC_5DIGIT_CODE,SIC_2DIGIT_CODE,SIC_2DIGIT_CODE_NUMERIC,SIC_3DIGIT_CODE,SIC_SECTION_LETTER,SIC_SECTION_CODE)) %>%
#   pivot_longer(cols = SIC_5DIGIT_NAME:SIC_SECTION_NAME, names_to = "SIC_digit", values_to = "sector_name", cols_vary = "slowest")
# 
# #That's duplicating values so is larger than the original, by about 50%
# pryr::object_size(ch)

#Update the digit values so is friendly in the dropdown. Here or column names, both fine!
# ch <- ch %>% 
#   mutate(
#     SIC_digit = case_when(
#       SIC_digit == "SIC_5DIGIT_NAME" ~ "5 digit",  
#       SIC_digit == "SIC_2DIGIT_NAME" ~ "2 digit",  
#       SIC_digit == "SIC_3DIGIT_NAME" ~ "3 digit",  
#       SIC_digit == "SIC_SECTION_NAME" ~ "Section"
#     )
#   )

#Save those digit names for the UI while we're here
# saveRDS(unique(ch$SIC_digit)[c(4,2,3,1)],'data/initialSICDigitNames.rds')

# ch <- ch %>%
#   mutate(
#     employee_diff_percent = ((Employees_thisyear - Employees_lastyear)/Employees_lastyear) * 100
#   )
# 
# #Keep only most recent account date to avoid duplication
# #(Duplicates from e.g. older accounts have varying employee numbers, so combining in final filter combo gets mismatched vals compared to what's asked via employee number slider)
# #TODO: process so time series of values from multiple accounts can be displayed
# ch <- ch %>%
#   group_by(CompanyNumber) %>%
#   filter(enddate == max(enddate)) %>%
#   ungroup()


# saveRDS(ch,'data/companieshouse_employees_n_sectors_southyorkshire_long.rds')



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
