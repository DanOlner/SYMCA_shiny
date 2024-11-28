#Testing random things
library(classInt)
source('othercode/functions.R')

#Test classinterval, get rounded integers----

ch.manuf <- readRDS('data/companieshouse_employees_n_sectors_southyorkshire.rds') %>% filter(SIC_SECTION_NAME == 'Manufacturing')

#.5s. We want integers!
fisher_breaks <- classIntervals(ch.manuf$Employees_thisyear, n = 7, style = "fisher")$brks

#Straight rounding?
fisher_breaks <- classIntervals(ch.manuf$Employees_thisyear, n = 7, style = "fisher")$brks %>% round



#Test inc function for console-plus-counts----
inc("test")
inc("test",1,2,3,"tap tap is this working? Hello??")

ct("test also")



#Checks on final filtered dataset----

#Do we have duplicate company numbers with differing employee counts in there?
#Possibly same company over different years - in which case, keep only most recent account

#Reason: combined subsets on company number is producing odd employee values that shouldn't be there
#If so, mostly a good problem to have - more accounts = longer time series

df <- readRDS('local/final_filtered.rds')

#Yep. MANY duplicates.
length(unique(df$CompanyNumber))

#view
df <- df %>% 
  group_by(CompanyNumber) %>% 
  mutate(dupcount = n()) %>% 
  ungroup() %>% 
  arrange(-dupcount) %>% 
  select(CompanyNumber,Company,enddate,Employees_thisyear,Employees_lastyear)


#repeat to check if those duplicates are in the original...
ch <- readRDS('data/companieshouse_employees_n_sectors_southyorkshire.rds')

ch <- ch %>% 
  group_by(CompanyNumber) %>% 
  mutate(dupcount = n()) %>% 
  ungroup() %>% 
  arrange(-dupcount) %>% 
  select(CompanyNumber,Company,enddate,Employees_thisyear,Employees_lastyear)

#... yes they are. Not something accidentally made in the combining.


#If we stick to only most recent account date for each of those, how many left?
df.mostrecent <- df %>% 
  group_by(CompanyNumber) %>% 
  filter(enddate == max(enddate))

#Only one duplicate here. Very rare to get dups...
#And I suspect the one dup would have same employee number anyway
length(unique(df.mostrecent$CompanyNumber))





#Test fisher/jenks scale diverging each side of zero----

#Approach one: can we just split at zero, fisher up each side and recombine? What does that look like?
ch.manuf <- readRDS('data/companieshouse_employees_n_sectors_southyorkshire.rds') %>% filter(SIC_SECTION_NAME == 'Manufacturing') %>%
  filter(
    !is.na(Employees_lastyear),
    Employees_lastyear > 4
  ) %>%
  mutate(employee_diff_percent = ((Employees_thisyear - Employees_lastyear)/Employees_lastyear) * 100)


#Split each side of zero for percent diff
fisher_breaks_pos <- classIntervals(ch.manuf$employee_diff_percent[ch.manuf$employee_diff_percent > 0], n = 4, style = "fisher")$brks %>% round()
fisher_breaks_neg <- classIntervals(ch.manuf$employee_diff_percent[ch.manuf$employee_diff_percent <= 0], n = 4, style = "fisher")$brks %>% round()

fisher_breaks_pos
fisher_breaks_neg






