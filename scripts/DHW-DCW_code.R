getwd()
setwd("/Users/glennd/Documents/AAlongtermgrowth")
temp <- read.csv("ctemp_sst_mod.csv")

## adding dhw package
devtools::install_github("dbca-wa/dbcaDHW")
library(dbcaDHW)

### Following vignette instructions here:https://dbca-wa.github.io/dbcaDHW/articles/making_dhw.html

https://dbca-wa.github.io/dbcaDHW/articles/making_dhw.html

library(tidyverse)
library(lubridate)
library(tibbletime)

## taking my dataset and formating 
str(temp)
sites <- names(temp)[5:9]
g_num <- length(sites) + 1
temp1 <- temp[,c(2,5:9)]
temp1$date1 <- as.Date(temp1$date1, '%d/%m/%y')

## Note this is MODIFIED FROM ORIGINAL DHW FORMULA
## create a mean of the maximum monthly mean each year at each site from "past period"
mmtdata <- temp1 %>%
  tidyr::gather("Site", "sst", 2:all_of(g_num)) %>%
  #dplyr::filter(date1 < "2013-12-31") %>% #define past
  dplyr::filter(date1 >= "1985-01-01" & date1 <= "1995-12-31") %>% #define past
  dplyr::mutate(month = month(date1),
                year = year(date1)) %>%
  dplyr::group_by(Site, year, month) %>%
  dplyr::summarise(avg = mean(sst)) %>% #obtain mthly avgs
  dplyr::group_by(Site, year) %>%
  #dplyr::reframe(mmmt = sort(avg, decreasing = TRUE)[1:3]) %>% #obtain top 3 monthly max mthly avg over this period
  #dplyr::group_by(site, year) %>%
  dplyr::summarise(mmmt = max(avg)) %>% #obtain max mthly avg each year over this period
  dplyr::group_by(Site) %>%
  dplyr::summarise(mmmt = mean(mmmt)) #average this value as threshold for DHW

# The DHW calculation is assisted by creating a function to do the work of a rolling 12 week “sum”. 
# A present period also needs to be defined. Again change this to suit your needs. Note, the code snippet below 
# focuses on temperatures that exceed the mmt by 1 degree or greater as per the Coral Reef Watch 
# example linked above. This may or may not suit your needs so adjust accordingly.

## function for rolling cusum of 12 weeks - 84 days)
dhw_calc_12 <- tibbletime::rollify(sum, window = 84)

## observation period, hotspot & dhw calc
dhw <- temp1 %>%
  tidyr::gather("Site", "sst", 2:all_of(g_num)) %>%
  dplyr::filter(date1 >= "2000-01-01" & date1 <= "2013-12-31") %>% #define present
  dplyr::mutate(month = month(date1)) %>%
  dplyr::full_join(mmtdata, by = "Site") %>%
  dplyr::mutate(hspt = sst - mmmt,
                hsptm = ifelse(hspt >= 1, hspt, 0),
                dhw = dhw_calc_12(hsptm)*(1/7)) %>% #hotspot based on greater than 1 degree
  dplyr::select(-month, -hsptm)

dhwm <- dhw %>%
  dplyr::mutate(year = year(date1)) %>%
  dplyr::group_by(Site, year) %>%
  dplyr::summarise(cummu_dhwm = sum(dhw, na.rm = TRUE))

write_csv(dhw, "mod_DHW.csv")
write_csv(dhwm, "mod_DHWM-year_site.csv")

## So unfortunately the hottest month in the entire dataset for each site was in the period 1985-1995 (i.e. the threshold)
## Temp over which DHW are defined. Therefore there are no DHW metrics to speak of. This was even when considering an average
## of the three hottest months over this period rather than just the single maximum value. This can be seen in the plot below:

mmtdata1 <- temp1 %>%
  tidyr::gather("Site", "sst", 2:all_of(g_num)) %>%
  #dplyr::filter(date1 < "2013-12-31") %>% #define past
  dplyr::filter(date1 >= "1985-01-01" & date1 <= "2013-12-31") %>% #define past
  dplyr::mutate(month = month(date1),
                year = year(date1)) %>%
  dplyr::group_by(Site, year, month) %>%
  dplyr::summarise(avg = mean(sst)) #%>% #obtain mthly avgs
  #dplyr::group_by(site)%>%
  #dplyr::summarise(mmmt = max(avg))
## Looking at monthly average data first
library(lattice)
xyplot(avg ~ year | Site, mmtdata1, type = "p", pch=20, layout=c(5,1), ylab = "Mean Monthly SST")


###NOw making the same modified cummu_dhwm metric but for dcwm (cooling) rather than warming.

mmtdata_cool <- temp1 %>%
  tidyr::gather("Site", "sst", 2:all_of(g_num)) %>%
  #dplyr::filter(date1 < "2013-12-31") %>% #define past
  dplyr::filter(date1 >= "1985-01-01" & date1 <= "1995-12-31") %>% #define past
  dplyr::mutate(month = month(date1),
                year = year(date1)) %>%
  dplyr::group_by(Site, year, month) %>%
  dplyr::summarise(avg = mean(sst)) %>% #obtain mthly avgs
  dplyr::group_by(Site, year) %>%
  #dplyr::reframe(mmmt = sort(avg, decreasing = TRUE)[1:3]) %>% #obtain top 3 monthly max mthly avg over this period
  #dplyr::group_by(site, year) %>%
  dplyr::summarise(mmmt = min(avg)) %>% #obtain max mthly avg each year over this period
  dplyr::group_by(Site) %>%
  dplyr::summarise(mmmt = mean(mmmt)) #average this value as threshold for DHW

dhw_calc_12 <- tibbletime::rollify(sum, window = 84)

## observation period, hotspot & dhw calc
dhw_cool <- temp1 %>%
  tidyr::gather("Site", "sst", 2:all_of(g_num)) %>%
  dplyr::filter(date1 >= "2000-01-01" & date1 <= "2013-12-31") %>% #define present
  dplyr::mutate(month = month(date1)) %>%
  dplyr::full_join(mmtdata_cool, by = "Site") %>%
  dplyr::mutate(hspt = sst - mmmt,
                hsptm = ifelse(hspt <= -1, hspt, 0),
                dcw = dhw_calc_12(hsptm)*(1/7)) %>% #hotspot based on less than 1 degree
  dplyr::select(-month, -hsptm)

dhwm_cool <- dhw_cool %>%
  dplyr::mutate(year = year(date1)) %>%
  dplyr::group_by(Site, year) %>%
  dplyr::summarise(cummu_dcwm = sum(dcw, na.rm = TRUE))

write_csv(dhw_cool, "mod_DCW.csv")
write_csv(dhwm_cool, "mod_DCWM-year_site.csv")

