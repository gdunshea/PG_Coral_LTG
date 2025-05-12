getwd()
setwd("/Users/glennd/Documents/AAlongtermgrowth")
temp <- read.csv("rawdata/ctemp_sst_mod.csv")

## adding dhw package
devtools::install_github("dbca-wa/dbcaDHW")
library(dbcaDHW)

### Following vignette instructions here:https://dbca-wa.github.io/dbcaDHW/articles/making_dhw.html

library(tidyverse)
library(lubridate)
library(tibbletime)

## taking my dataset and formating 
temp <- read.csv("rawdata/ctemp_sst_mod.csv")
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
  dplyr::filter(date1 >= "1985-01-01" & date1 <= "2013-12-31") %>% ## Using all dates to get dataset to plot
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



##playplots

# Step 1: Prepare monthly mean SST per site/year/month
mmtdata1 <- temp1 %>%
  tidyr::gather("Site", "sst", 2:all_of(g_num)) %>%
  filter(date1 >= "1985-01-01", date1 <= "2013-12-31") %>%
  mutate(month = month(date1),
         year = year(date1)) %>%
  group_by(Site, year, month) %>%
  summarise(avg = mean(sst), .groups = "drop")

# Step 2: Get min and max monthly avg SST per site
site_limits <- mmtdata1 %>%
  group_by(Site) %>%
  summarise(
    min_avg = min(avg, na.rm = TRUE),
    max_avg = max(avg, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Precompute annual Min and Max MMTs per site
mmt_extremes <- mmtdata1 %>%
  group_by(Site, year) %>%
  summarise(
    min_mmt = min(avg, na.rm = TRUE),
    max_mmt = max(avg, na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Plot with regression lines and min/max reference lines
xyplot(
  avg ~ year | Site,
  data = mmtdata1,
  type = "p",
  pch = 20,
  layout = c(5, 1),
  ylab = "Mean Monthly SST",
  main = "Mean Monthly SST Trends with LOWESS Fits (1985–2013)",
  panel = function(x, y, subscripts, ...) {
    panel.xyplot(x, y, ...)
    
    # Get current panel's site
    site_name <- as.character(mmtdata1$Site[subscripts][1])
    
    # Draw horizontal lines for min and max SST
    min_val <- site_limits$min_avg[site_limits$Site == site_name]
    max_val <- site_limits$max_avg[site_limits$Site == site_name]
    panel.abline(h = min_val, col = "blue", lty = 2)
    panel.abline(h = max_val, col = "red", lty = 2)
    
    # Extract annual extremes for LOWESS
    site_ext <- mmt_extremes %>% filter(Site == site_name)
    
    # Draw LOWESS curves
    panel.lines(lowess(site_ext$year, site_ext$min_mmt), col = "blue", lwd = 2)
    panel.lines(lowess(site_ext$year, site_ext$max_mmt), col = "red", lwd = 2)
  }
)

