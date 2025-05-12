### Formatting all ENV data to growth format
#### Getting moving average over a 60 day window then summarizing by year
setwd("/Users/gdunshea/AbuDhabi-longtermgrowth/Rdata/GulfLongTerm")
## Growth data first
onyr <- read.csv("maxvalues-year2000onwards.csv")
## Now ENV data to MA and summarize by year
str(onyr)
library(lattice)

##checcvk distribution of coral sizes
xyplot(height ~ dia | site, data = onyr,
       #group = year, #t="l",
       grid = TRUE, auto.key=list(space="top", columns=5, 
                                  title="Site", cex.title=1,
                                  lines=TRUE, points=FALSE))#,
plot(height ~ dia, data = onyr)

######$#######
#Using ZLEMA n = 14 : Moving average over a 14 day period
#but attempts to remove lag by subtracting data prior to (n-1)/2 periods 
#(default) to minimize the cumulative effect.
#
#############
detach("package:plyr", unload=TRUE)
library(zoo)
library(TTR)
temp <- read.csv("ctemp_sst_mod.csv")
str(temp)
temp1 <- temp[,5:9]
## Resetting variable names to make it easier to reshape dataset after adding moving averages
temp1 <- temp[,5:9]
colnames(temp1) <- paste(colnames(temp1), "raw", sep = ".")
temp <- cbind(temp[,c(1,2,3,4,10,11)], temp1)
str(temp)
### Getting 14-day moviong averages of each temp column
#temp$Delma.MA <- ZLEMA(temp$Delma.raw, n = 45)
#temp$RasGhanada.MA <- ZLEMA(temp$RasGhanada.raw, n = 45)
#temp$Saadiyat.MA <- ZLEMA(temp$Saadiyat.raw, n = 45)
#temp$AlAqah.MA <- ZLEMA(temp$AlAqah.raw, n = 45)
#temp$Dibba.MA <- ZLEMA(temp$Dibba.raw, n = 45)

library(tidyverse)
### Trying a centred moving average (rather than moving averages based on past data) to capture coldest and hottest periods
temp11 <- temp %>%
  select(date, date1, yearday, Year, month, season, Delma.raw, RasGhanada.raw, Saadiyat.raw, AlAqah.raw, Dibba.raw) %>%
  mutate(Delma.CMA = rollmean(Delma.raw, k = 61, fill = NA),
         RasGhanada.CMA = rollmean(RasGhanada.raw, k = 61, fill = NA),
         Saadiyat.CMA = rollmean(Saadiyat.raw, k = 61, fill = NA),
         AlAqah.CMA = rollmean(AlAqah.raw, k = 61, fill = NA),
         Dibba.CMA = rollmean(Dibba.raw, k = 61, fill = NA))
temp11

## formatting dat for plotting
temp11$date1 <- as.Date(temp11$date1, '%d/%m/%y')
str(temp)
require(ggplot2)
temp98 <- subset(temp11, Year ==2002 | Year ==2003)
ggplot( data = temp98, aes(date1, Delma.raw )) + geom_point() + 
  #geom_line(data = temp98, aes(date1, Delma.raw ), color = "red")+
  geom_line(data = temp98, aes(date1, Delma.CMA ), color = "blue")
str(temp11)
## ordering month factor appropriately
temp11$month <- factor(temp$month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
levels(temp11$month)
str(temp11)
head(temp11, n = 20)
## reshaping data so raw temps (Delma, RasGhanada, Saadiyat, AlAqah, Dibba) and 61 day centered averaged temps are in single
## columns with a site factor level instead (removing first 30 & last 30 days of data first to remove all NAs)
library(tidyverse)
head(temp11, n = 32)
tail(temp11, n = 31)
temp11 <- temp11[31:(nrow(temp11)-31),]
tail(temp11, n = 32)
head(temp11, n = 32)
templong <- temp11 %>% 
  gather("key", "value", -date, -date1, -yearday, -Year, -month, -season) %>% 
  separate(key, c("Site", "raw_CMA"), sep = "\\.") %>% 
  spread(raw_CMA, value)
str(templong)
head(templong)

##### RATES OF TEMPERATURE CHANGE (WARMING - WINTER TO SUMMER) AND COOLING (SUMMER TO WINTER) - using templong object to split by year + site
### The simplest way I could think of to do this was to fit a linear regression on the relationship between day of the year
## and temperature (so the measure of change is: Degrees C / Day -1) for warming coming out of winter and then for cooling coming out of summer.
## If the 'peak' of ther winter period and summer period is the coldest and warmest day respectively, these obviously vary (substantially) in
## some cases in each year, so a static value cannot be used across all years. The following code finds the coldest and hottest day each year and
## selects the 90 days after that period

### Looking at getting rates of change between winter and summer each year
xyplot(raw ~ yearday | as.factor(Year) , data = templong,
       group = Site, t="l",
       grid = TRUE, auto.key=list(space="top", columns=5, 
                                  title="Site", cex.title=1,
                                  lines=TRUE, points=FALSE))#,
## concentrating on 90 days after the lowest (rate of change from winter to summer) and highest (rate of change from summer to winter) temps 
## within each year
plot_df <- templong %>% 
  mutate(year = lubridate::year(date1)) %>%
  group_by(year, Site) %>%
  mutate(raw_min = +(raw == min(raw)),
         raw_max = +(raw == max(raw))) %>%
  ungroup() %>%
  mutate(raw_min = cumsum(raw_min - lag(raw_min, 90, default = 0)),
         raw_max = cumsum(raw_max - lag(raw_max, 90, default = 0)))
str(plot_df)
## plotting to make sure 90 days after coldest (blue) and hottest day of each year are detected (red) - checking a few different sites
ggplot(plot_df %>% filter(Site =="Delma"), aes(date1, raw)) + 
  geom_line() + 
  geom_vline(aes(xintercept = date1), plot_df %>% filter(Site =="Delma", raw_min > 0),
             alpha = 0.1, colour = "blue") +
  geom_vline(aes(xintercept = date1), plot_df %>% filter(Site =="Delma", raw_max > 0),
             alpha = 0.1, colour = "red")

ggplot(plot_df %>% filter(Site =="Dibba"), aes(date1, raw)) + 
  geom_line() + 
  geom_vline(aes(xintercept = date1), plot_df %>% filter(Site =="Dibba", raw_min > 0),
             alpha = 0.1, colour = "blue") +
  geom_vline(aes(xintercept = date1), plot_df %>% filter(Site =="Dibba", raw_max > 0),
             alpha = 0.1, colour = "red")


### subsetting raw data rise from mins at each site
raw_minroc <- subset(plot_df, raw_min > 0)
##checking 
xyplot(raw ~ yearday | as.factor(Year) , data = raw_minroc,
       group = Site, type ="l", xlim = c(0,200),
       grid = TRUE, auto.key=list(space="top", columns=5, 
                                  title="Site", cex.title=1,
                                  lines=TRUE, points=FALSE))
## seems like a few problems - eg, 2005 something weird has gone on with RasG and . Other instances is where the coldestr day is 
## on the 31st December, (RasG and Saadiyat) which bugs up the by year filtering, so need to remove 
## (RasG and Saadiyat, 31 dec 2004) running filtering again without those rows
nrow(templong)
filter(templong, date1 == "2004-12-31" & Site == "RasGhanada" )
filter(templong, date1 == "2004-12-31" & Site == "Saadiyat" )

templong <- templong %>%
  filter(date1 != "2004-12-31" | Site != "RasGhanada" ) %>%
  filter(date1 != "2004-12-31" |  Site != "Saadiyat")
nrow(templong)

plot_df <- templong %>% 
  mutate(year = lubridate::year(date1)) %>%
  group_by(year, Site) %>%
  mutate(raw_min = +(raw == min(raw)),
         raw_max = +(raw == max(raw))) %>%
  ungroup() %>%
  mutate(raw_min = cumsum(raw_min - lag(raw_min, 90, default = 0)),
         raw_max = cumsum(raw_max - lag(raw_max, 90, default = 0)))
str(plot_df)
## plotting to make sure 90 days after coldest (blue) and hottest day of each year are detected (red) - checking a few different sites
ggplot(plot_df %>% filter(Site =="Delma"), aes(date1, raw)) + 
  geom_line() + 
  geom_vline(aes(xintercept = date1), plot_df %>% filter(Site =="Delma", raw_min > 0),
             alpha = 0.1, colour = "blue") +
  geom_vline(aes(xintercept = date1), plot_df %>% filter(Site =="Delma", raw_max > 0),
             alpha = 0.1, colour = "red")

ggplot(plot_df %>% filter(Site =="Dibba"), aes(date1, raw)) + 
  geom_line() + 
  geom_vline(aes(xintercept = date1), plot_df %>% filter(Site =="Dibba", raw_min > 0),
             alpha = 0.1, colour = "blue") +
  geom_vline(aes(xintercept = date1), plot_df %>% filter(Site =="Dibba", raw_max > 0),
             alpha = 0.1, colour = "red")

##checking
raw_minroc <- subset(plot_df, raw_min > 0)
raw_maxroc <- subset(plot_df, raw_max > 0)
## winter to summer
xyplot(raw ~ yearday | as.factor(Year) , data = raw_minroc,
       group = Site, type ="l", xlim = c(0,200),
       grid = TRUE, auto.key=list(space="top", columns=5, 
                                  title="Site", cex.title=1,
                                  lines=TRUE, points=FALSE))
##Summer to winter
xyplot(raw ~ yearday | as.factor(Year) , data = raw_maxroc,
       group = Site, type ="l", #xlim = c(0,200),
       grid = TRUE, auto.key=list(space="top", columns=5, 
                                  title="Site", cex.title=1,
                                  lines=TRUE, points=FALSE))

## there is a problem with RagG and Al Aqah for summer to winter in 1985 but this will be discarded anyway (since coral data is only from 2000)
### Running regression within each year for Delma~yearday

## normalizing coldest (or hottest) day to day 1 before fitting regression so that intercepts are normalised to hottest/coldest day (as applicatble)

library(tidyverse)
library(data.table)
str(raw_minroc)
str(raw_maxroc)
raw_minroc$Site <- as.factor(raw_minroc$Site)
raw_maxroc$Site <- as.factor(raw_maxroc$Site)

raw_minroc <- raw_minroc %>%
  group_by(Site, year) %>%
  mutate(normday = rleid(date1))

raw_maxroc <- raw_maxroc %>%
  group_by(Site, year) %>%
  mutate(normday = rleid(date1))

## 
str(raw_minroc)
str(raw_maxroc)
levels(raw_maxroc$Site)
## Getting regression parameters for winter ROC (raw_minroc)
## Subsetting data into each site to site - there is some elegant way to break data down by two groups (year, Site) instead of one but I don't know
## it and can't find it easily and just want to keep moving
delminroc <- subset(raw_minroc, Site == "Delma")
rasminroc <- subset(raw_minroc, Site == "RasGhanada")
saaminroc <- subset(raw_minroc, Site == "Saadiyat")
alaminroc <- subset(raw_minroc, Site == "AlAqah")
dibminroc <- subset(raw_minroc, Site == "Dibba")

delfinalResult = purrr::map_df(unique(raw_minroc$year), function(modelYearSite){
  myModel = lm(raw ~ normday, data = delminroc %>% filter(year == modelYearSite) %>% select(-year))
  data.frame(year = modelYearSite, winRSquared = summary(myModel)$r.squared, winIntercept = myModel$coefficients[1], winSlope = myModel$coefficients[2])
})
rasfinalResult = purrr::map_df(unique(raw_minroc$year), function(modelYearSite){
  myModel = lm(raw ~ normday, data = rasminroc %>% filter(year == modelYearSite) %>% select(-year))
  data.frame(year = modelYearSite, winRSquared = summary(myModel)$r.squared, winIntercept = myModel$coefficients[1], winSlope = myModel$coefficients[2])
})
saafinalResult = purrr::map_df(unique(raw_minroc$year), function(modelYearSite){
  myModel = lm(raw ~ normday, data = saaminroc %>% filter(year == modelYearSite) %>% select(-year))
  data.frame(year = modelYearSite, winRSquared = summary(myModel)$r.squared, winIntercept = myModel$coefficients[1], winSlope = myModel$coefficients[2])
})
alafinalResult = purrr::map_df(unique(raw_minroc$year), function(modelYearSite){
  myModel = lm(raw ~ normday, data = alaminroc %>% filter(year == modelYearSite) %>% select(-year))
  data.frame(year = modelYearSite, winRSquared = summary(myModel)$r.squared, winIntercept = myModel$coefficients[1], winSlope = myModel$coefficients[2])
})
dibfinalResult = purrr::map_df(unique(raw_minroc$year), function(modelYearSite){
  myModel = lm(raw ~ normday, data = dibminroc %>% filter(year == modelYearSite) %>% select(-year))
  data.frame(year = modelYearSite, winRSquared = summary(myModel)$r.squared, winIntercept = myModel$coefficients[1], winSlope = myModel$coefficients[2])
})
levels(raw_minroc$Site)

## adding site factor to each subset
delfinalResult$Site <- rep("Delma", nrow(delfinalResult))
rasfinalResult$Site <- rep("RasGhanada", nrow(delfinalResult))
saafinalResult$Site <- rep("Saadiyat", nrow(delfinalResult))
alafinalResult$Site <- rep("AlAqah", nrow(delfinalResult))
dibfinalResult$Site <- rep("Dibba", nrow(delfinalResult))

wintroc <- rbind(delfinalResult, rasfinalResult, saafinalResult, alafinalResult, dibfinalResult)
wintroc
library(plyr)
ccold <- ddply(raw_minroc, .(year, Site) ,summarise, ccoldday=min(yearday))
wintrocfin <- merge(wintroc, ccold, by=c("year", "Site"))
detach("package:plyr", unload=TRUE)
### Looking for covarying parameters to know which can be included in the same model or not
pairs(wintrocfin[,3:6])
## So for winter, slope + intercept or intercept + ccoldday can be used in the same model but not slope + ccoldday
wintrocfin
### Done for winter to summer (Above - stored in wintrocfin) - now do for summer to winter and all other sites and add to growth

## Getting regression parameters for Summer ROC (raw_axroc)
## Subsetting data into each site to site - there is some elegant way to break data down by two groups (year, Site) instead of one but I don't know
## it and can't find it easily and just want to keep moving
delmaxroc <- subset(raw_maxroc, Site == "Delma")
rasmaxroc <- subset(raw_maxroc, Site == "RasGhanada")
saamaxroc <- subset(raw_maxroc, Site == "Saadiyat")
alamaxroc <- subset(raw_maxroc, Site == "AlAqah")
dibmaxroc <- subset(raw_maxroc, Site == "Dibba")

delfinalResult = purrr::map_df(unique(raw_maxroc$year), function(modelYearSite){
  myModel = lm(raw ~ normday, data = delmaxroc %>% filter(year == modelYearSite) %>% select(-year))
  data.frame(year = modelYearSite, sumRSquared = summary(myModel)$r.squared, sumIntercept = myModel$coefficients[1], sumSlope = myModel$coefficients[2])
})
rasfinalResult = purrr::map_df(unique(raw_maxroc$year), function(modelYearSite){
  myModel = lm(raw ~ normday, data = rasmaxroc %>% filter(year == modelYearSite) %>% select(-year))
  data.frame(year = modelYearSite, sumRSquared = summary(myModel)$r.squared, sumIntercept = myModel$coefficients[1], sumSlope = myModel$coefficients[2])
})
saafinalResult = purrr::map_df(unique(raw_maxroc$year), function(modelYearSite){
  myModel = lm(raw ~ normday, data = saamaxroc %>% filter(year == modelYearSite) %>% select(-year))
  data.frame(year = modelYearSite, sumRSquared = summary(myModel)$r.squared, sumIntercept = myModel$coefficients[1], sumSlope = myModel$coefficients[2])
})
alafinalResult = purrr::map_df(unique(raw_maxroc$year), function(modelYearSite){
  myModel = lm(raw ~ normday, data = alamaxroc %>% filter(year == modelYearSite) %>% select(-year))
  data.frame(year = modelYearSite, sumRSquared = summary(myModel)$r.squared, sumIntercept = myModel$coefficients[1], sumSlope = myModel$coefficients[2])
})
dibfinalResult = purrr::map_df(unique(raw_maxroc$year), function(modelYearSite){
  myModel = lm(raw ~ normday, data = dibmaxroc %>% filter(year == modelYearSite) %>% select(-year))
  data.frame(year = modelYearSite, sumRSquared = summary(myModel)$r.squared, sumIntercept = myModel$coefficients[1], sumSlope = myModel$coefficients[2])
})
levels(raw_maxroc$Site)
delfinalResult$Site <- rep("Delma", nrow(delfinalResult))
rasfinalResult$Site <- rep("RasGhanada", nrow(delfinalResult))
saafinalResult$Site <- rep("Saadiyat", nrow(delfinalResult))
alafinalResult$Site <- rep("AlAqah", nrow(delfinalResult))
dibfinalResult$Site <- rep("Dibba", nrow(delfinalResult))

summroc <- rbind(delfinalResult, rasfinalResult, saafinalResult, alafinalResult, dibfinalResult)
summroc
library(plyr)
warm <- ddply(raw_maxroc, .(year, Site) ,summarise, warmday=max(yearday))
summrocfin <- merge(summroc, warm, by=c("year", "Site"))
detach("package:plyr", unload=TRUE)
## As for winter checking for covariation between parameters
pairs(summrocfin[,3:6])
## For summer all parameters strongly covary so can only use one in per model
summrocfin


## Getting summaries over coldest 61 days in winter and hottest 61 days in summer  - finding the coldest and hottest time of the year
## from the day with the coldest and hottest record from the 14 day moving average and the 30 days plus and minus the coldest and hottest day of each year 
## (similar to rate of change) rather than months - so the coldest/hottest 61 days of the year
##################### winter - finding lowest temp day
templong$yearAD <- ifelse(templong$month == "DEC", templong$Year + 1, templong$Year)
sumwin_df <- templong %>% 
  mutate(year = lubridate::year(date1)) %>%
  group_by(yearAD, Site) %>%
  mutate(CMA_min = +(CMA == min(CMA)),
         CMA_max = +(CMA == max(CMA))) %>%
  ungroup()   
## Being screwed by the position of the first 1 in 1985 Delma - not even using 1985 data anyway, so hacking to change the position so the resulting rows corresponding
## to the first period dont have any negative values
print(sumwin_df, n = 40)
sumwin_df[[21,12]] 
sumwin_df[[21,12]] <- 0
sumwin_df[[21,12]] 
sumwin_df[[32,12]] <- 1
print(sumwin_df, n = 40)

## selecting 30 days before and 30 after lowest temp day
inds = which(sumwin_df$CMA_min == 1)
inds
# We use lapply() to get all rows for all indices, result is a list
rows <- lapply(inds, function(x) (x-30):(x+30))
str(rows)
rows
# With unlist() you get all relevant rows
sumwin_win <- sumwin_df[unlist(rows),]
# Giving all relevant rows a 1 value and re-merging with rest of dataset
sumwin_win[sumwin_win$CMA_min == 0, "CMA_min"] <- 1 
## this gives the 60 coldest days for each year, however, there is a problem because some days are in December of the previous year,
## so if summarizing by year (site), summaryes will be erroneous. Need to reassign year in this dataframe so that it means "year of 
## winter being measured"winter" so that the values in december will be included propoerly to the following years winter.
# easiest way is just too add one to the year column if the month is december
sumwin_win$yearAD <- ifelse(sumwin_win$month == "DEC", sumwin_win$Year + 1, sumwin_win$Year)


## sumwin_win contains coldest 61 days of each winter (majority year of season in yearAD field)

## Doing the same for summer (don't have to worry about cross over year values because in the middle of the year)
## selecting 30 days before and 30 after lowest temp day
inds = which(sumwin_df$CMA_max == 1)
# We use lapply() to get all rows for all indices, result is a list
rows <- lapply(inds, function(x) (x-30):(x+30))
# With unlist() you get all relevant rows
sumwin_sum <- sumwin_df[unlist(rows),]
# Giving all relevant rows a 1 value and re-merging with rest of dataset
sumwin_sum[sumwin_sum$CMA_min == 0, "CMA_max"] <- 1 
str(sumwin_sum)
sumwin_sum <- as.data.frame(sumwin_sum)
sumwin_win <- as.data.frame(sumwin_win)
temp1 <- merge(x = templong, y = sumwin_sum[ ,c("date1","Site", "CMA_max")], by = c("date1","Site"), all.x = TRUE)
temp1 <- merge(x = temp1, y = sumwin_win[ ,c("date1","Site", "CMA_min")], by = c("date1","Site"), all.x = TRUE)
str(temp1)

## Therefore temp1 is long form of temp with the hottest and coldest 61 days of each year marked in relation to to the hotest and coldest day, per site, per year
## Doing some graphical checks
ggplot(temp1 %>% filter(Site =="Delma"), aes(date1, raw)) + 
  geom_line() + 
  geom_vline(aes(xintercept = date1), temp1 %>% filter(Site =="Delma", CMA_min > 0),
             alpha = 0.1, colour = "blue") +
  geom_vline(aes(xintercept = date1), temp1 %>% filter(Site =="Delma", CMA_max > 0),
             alpha = 0.1, colour = "red")

ggplot(temp1 %>% filter(Site =="Dibba"), aes(date1, raw)) + 
  geom_line() + 
  geom_vline(aes(xintercept = date1), temp1 %>% filter(Site =="Dibba", CMA_min > 0),
             alpha = 0.1, colour = "blue") +
  geom_vline(aes(xintercept = date1), temp1 %>% filter(Site =="Dibba", CMA_max > 0),
             alpha = 0.1, colour = "red")

ggplot(temp1 %>% filter(Site =="AlAqah"), aes(date1, raw)) + 
  geom_line() + 
  geom_vline(aes(xintercept = date1), temp1 %>% filter(Site =="AlAqah", CMA_min > 0),
             alpha = 0.1, colour = "blue") +
  geom_vline(aes(xintercept = date1), temp1 %>% filter(Site =="AlAqah", CMA_max > 0),
             alpha = 0.1, colour = "red")

ggplot(temp1 %>% filter(Site =="RasGhanada"), aes(date1, raw)) + 
  geom_line() + 
  geom_vline(aes(xintercept = date1), temp1 %>% filter(Site =="RasGhanada", CMA_min > 0),
             alpha = 0.1, colour = "blue") +
  geom_vline(aes(xintercept = date1), temp1 %>% filter(Site =="RasGhanada", CMA_max > 0),
             alpha = 0.1, colour = "red")

ggplot(temp1 %>% filter(Site =="Saadiyat"), aes(date1, raw)) + 
  geom_line() + 
  geom_vline(aes(xintercept = date1), temp1 %>% filter(Site =="Saadiyat", CMA_min > 0),
             alpha = 0.1, colour = "blue") +
  geom_vline(aes(xintercept = date1), temp1 %>% filter(Site =="Saadiyat", CMA_max > 0),
             alpha = 0.1, colour = "red")

##Summer
xyplot(raw ~ yearday | as.factor(Year) , data = sumwin_sum,
       group = Site, type ="l", #xlim = c(0,200),
       grid = TRUE, auto.key=list(space="top", columns=5, 
                                  title="Site", cex.title=1,
                                  lines=TRUE, points=FALSE))

##winter - keeping in mind that points plotted in dec are from the previous years winter
xyplot(raw ~ yearday | as.factor(yearAD) , data = sumwin_win,
       group = Site, type ="p", xlim = c(0,100),
       cex=0.5,
       grid = TRUE, auto.key=list(space="top", columns=5, 
                                  title="Site", cex.title=1,
                                  lines=TRUE, points=FALSE))


## Looks fine 

str(temp1)
temp1$Site
###########################
temp1$Site <- as.factor(temp1$Site)
temp1$winteryear <- ifelse(temp1$month == "DEC", temp1$Year + 1, temp1$Year)
wtemp <- subset(temp1, CMA_min == 1)
stemp <- subset(temp1, CMA_max == 1)

## Winter
levels(temp1$Site)
str(Wdelt)
library(plyr)
Wdelt <- ddply(wtemp, .(winteryear, Site == "Delma") ,summarise, WtmaxM=max(CMA), WtminM=min(CMA), WtavM =mean(CMA), WtsdM = sd(CMA),
               WtmaxR=max(raw), WtminR=min(raw), WtavR =mean(raw), WtsdR = sd(raw))
names(Wdelt)[2] <- "TF"
Wdelt <- subset(Wdelt, TF ==TRUE)
Wdelt <- Wdelt[,-2]

Wras <- ddply(wtemp, .(winteryear,Site == "RasGhanada") ,summarise, WtmaxM=max(CMA), WtminM=min(CMA), WtavM =mean(CMA), WtsdM = sd(CMA),
              WtmaxR=max(raw), WtminR=min(raw), WtavR =mean(raw), WtsdR = sd(raw))
names(Wras)[2] <- "TF"
Wras <- subset(Wras, TF ==TRUE)
Wras <- Wras[,-2]

Wsaa <- ddply(wtemp, .(winteryear, Site == "Saadiyat") ,summarise, WtmaxM=max(CMA), WtminM=min(CMA), WtavM =mean(CMA), WtsdM = sd(CMA),
              WtmaxR=max(raw), WtminR=min(raw), WtavR =mean(raw), WtsdR = sd(raw))
names(Wsaa)[2] <- "TF"
Wsaa <- subset(Wsaa, TF ==TRUE)
Wsaa <- Wsaa[,-2]

Wala <- ddply(wtemp, .(winteryear,Site == "AlAqah") ,summarise, WtmaxM=max(CMA), WtminM=min(CMA), WtavM =mean(CMA), WtsdM = sd(CMA),
              WtmaxR=max(raw), WtminR=min(raw), WtavR =mean(raw), WtsdR = sd(raw))
names(Wala)[2] <- "TF"
Wala <- subset(Wala, TF ==TRUE)
Wala <- Wala[,-2]

Wdib <- ddply(wtemp, .(winteryear,Site == "Dibba") ,summarise, WtmaxM=max(CMA), WtminM=min(CMA), WtavM =mean(CMA), WtsdM = sd(CMA),
              WtmaxR=max(raw), WtminR=min(raw), WtavR =mean(raw), WtsdR = sd(raw))
names(Wdib)[2] <- "TF"
Wdib <- subset(Wdib, TF ==TRUE)
Wdib <- Wdib[,-2]

## Summer
Sdelt <- ddply(stemp, .(Year, Site == "Delma") ,summarise, StmaxM=max(CMA), StminM=min(CMA), StavM =mean(CMA), StsdM = sd(CMA),
               StmaxR=max(raw), StminR=min(raw), StavR =mean(raw), StsdR = sd(raw))
names(Sdelt)[2] <- "TF"
Sdelt <- subset(Sdelt, TF ==TRUE)
Sdelt <- Sdelt[,-2]

Sras <- ddply(stemp, .(Year, Site == "RasGhanada"),summarise, StmaxM=max(CMA), StminM=min(CMA), StavM =mean(CMA), StsdM = sd(CMA),
              StmaxR=max(raw), StminR=min(raw), StavR =mean(raw), StsdR = sd(raw))
names(Sras)[2] <- "TF"
Sras <- subset(Sras, TF ==TRUE)
Sras <- Sras[,-2]

Ssaa <- ddply(stemp, .(Year, Site == "Saadiyat") ,summarise, StmaxM=max(CMA), StminM=min(CMA), StavM =mean(CMA), StsdM = sd(CMA),
              StmaxR=max(raw), StminR=min(raw), StavR =mean(raw), StsdR = sd(raw))
names(Ssaa)[2] <- "TF"
Ssaa <- subset(Ssaa, TF ==TRUE)
Ssaa <- Ssaa[,-2]

Sala <- ddply(stemp, .(Year, Site == "AlAqah") ,summarise, StmaxM=max(CMA), StminM=min(CMA), StavM =mean(CMA), StsdM = sd(CMA),
              StmaxR=max(raw), StminR=min(raw), StavR =mean(raw), StsdR = sd(raw))
names(Sala)[2] <- "TF"
Sala <- subset(Sala, TF ==TRUE)
Sala <- Sala[,-2]

Sdib <- ddply(stemp, .(Year, Site == "Dibba") ,summarise, StmaxM=max(CMA), StminM=min(CMA), StavM =mean(CMA), StsdM = sd(CMA),
              StmaxR=max(raw), StminR=min(raw), StavR =mean(raw), StsdR = sd(raw))
names(Sdib)[2] <- "TF"
Sdib <- subset(Sdib, TF ==TRUE)
Sdib <- Sdib[,-2]


## Structure of the growth dataset
str(onyr)
str(Sdelt)
str(Wdelt)
levels(onyr$site)
## making same variables in temp data to merge datasets
Wdelt$site <- as.factor(c(rep("Delma", 34)))
Wras$site <- as.factor(c(rep("RasGhanada", 34)))
Wsaa$site <- as.factor(c(rep("Saadiyat", 34)))
Wala$site <- as.factor(c(rep("AlAqah", 34)))
Wdib$site <- as.factor(c(rep("Dibba", 34)))
Sdelt$site <- as.factor(c(rep("Delma", 34)))
Sras$site <- as.factor(c(rep("RasGhanada", 34)))
Ssaa$site <- as.factor(c(rep("Saadiyat", 34)))
Sala$site <- as.factor(c(rep("AlAqah", 34)))
Sdib$site <- as.factor(c(rep("Dibba", 34)))

Wtsumall <- rbind(Wdelt, Wras, Wsaa, Wala, Wdib)
Stsumall <- rbind(Sdelt, Sras, Ssaa, Sala, Sdib)

str(Wtsumall)
Wtsumall <- na.omit(Wtsumall)
Wtsumall$WtrangeM <- Wtsumall$WtmaxM - Wtsumall$WtminM
Wtsumall$WtrangeR <- Wtsumall$WtmaxR - Wtsumall$WtminR
str(Wtsumall)
str(Stsumall)
Stsumall <- na.omit(Stsumall)
Stsumall$StrangeM <- Stsumall$StmaxM - Stsumall$StminM
Stsumall$StrangeR <- Stsumall$StmaxR - Stsumall$StminR
str(Stsumall)

## Getting some more variables in common between all datasets to merge
str(onyr)
onyr$site <- as.factor(onyr$site)
str(onyr)
str(Wtsumall)
names(Wtsumall)[1] <- "year"
str(onyr)
str(Stsumall)
names(Stsumall)[1] <- "year"
onyr$Site <- onyr$site

str(onyr)
str(Wtsumall)
onyr$Site <- as.factor(onyr$Site)
Wtsumall$year <- as.integer(Wtsumall$year)
test <- merge(onyr, Wtsumall, by = c("year", "site"), all.x=TRUE)
test <- merge(test, Stsumall, by = c("year", "site"), all.x=TRUE)
str(test)
str(summrocfin)
names(wintrocfin)[2] <- "site"
names(summrocfin)[2] <- "site"
test <- merge(test, wintrocfin, by = c("year", "site"), all.x=TRUE)
test <- merge(test, summrocfin, by = c("year", "site"), all.x=TRUE)

str(test)
head(test, n= 50)


############ That is all temperature metrics summarized (including rates of changes 90 days post yearly min and max temperature) and added
######## to the coral growth data

### Current speed data
cursp <- read.csv("hycom_current_speed_mod.csv")
str(cursp)
head(cursp)
cursp$tf <- complete.cases(cursp) 
cursp <- subset(cursp, tf =="TRUE")
str(cursp)
### Getting 14-day moviong averages of each cursp column
cursp1 <- cursp[,c(4:8)]
colnames(cursp1) <- paste(colnames(cursp1), "raw", sep = ".")
cursp <- cbind(cursp[,c(1,2,3,9,10,11)], cursp1)
str(cursp)

cursp$curspap <- na.approx(cursp$curspap) 
cursp$Delma.MA <- ZLEMA(cursp$Delma.raw, n = 14)
cursp$RasGhanada.MA <- ZLEMA(cursp$RasGhanada.raw, n = 14)
cursp$Saadiyat.MA <- ZLEMA(cursp$Saadiyat.raw, n = 14)
cursp$AlAqah.MA <- ZLEMA(cursp$AlAqah.raw, n = 14)
cursp$Dibba.MA <- ZLEMA(cursp$Dibba.raw, n = 14)
## formatting dat for plotting
cursp$month <- factor(cursp$month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
cursp$date1 <- as.Date(cursp$date, '%d/%m/%y')
str(cursp)
cursp <- cursp[14:nrow(cursp),]
head(cursp, n = 20)
cursplong <- cursp %>% 
  gather("key", "value", -Time, -date, -date1, -yearday, -Year, -month, -season) %>% 
  separate(key, c("Site", "raw_MA"), sep = "\\.") %>% 
  spread(raw_MA, value)
str(cursplong)
head(templong)
xyplot(raw ~ yearday | as.factor(Year) , data = cursplong,
       group = Site, type ="l", #xlim = c(0,200),
       grid = TRUE, auto.key=list(space="top", columns=5, 
                                  title="Site", cex.title=1,
                                  lines=TRUE, points=FALSE))
require(ggplot2)
cursp98 <- subset(cursp, Year ==1998)
ggplot( data = cursp98, aes(date1, Delma.raw )) + geom_line()
ggplot()+ 
  geom_line(data = cursp98, aes(date1, Delma.raw ), color = "red")+
  geom_line(data = cursp98, aes(date1, Delma.MA ), color = "blue")
## Getting range, min, max per year per site
str(cursp)
library(dplyr)
str(cursp)
library(plyr)

### Getting moving average sumamries over the whole year, max, min, av, sd
delt <- ddply(cursp, .(Year) ,summarise, csmaxM=max(Delma.MA), csminM=min(Delma.MA), csavM =mean(Delma.MA), cssdM = sd(Delma.MA))
ras <- ddply(cursp, .(Year) ,summarise, csmaxM=max(RasGhanada.MA), csminM=min(RasGhanada.MA), csavM =mean(RasGhanada.MA), cssdM = sd(RasGhanada.MA))
saa <- ddply(cursp, .(Year) ,summarise, csmaxM=max(Saadiyat.MA), csminM=min(Saadiyat.MA), csavM =mean(Saadiyat.MA), cssdM = sd(Saadiyat.MA))
ala <- ddply(cursp, .(Year) ,summarise, csmaxM=max(AlAqah.MA), csminM=min(AlAqah.MA), csavM =mean(AlAqah.MA), cssdM = sd(AlAqah.MA))
dib <- ddply(cursp, .(Year) ,summarise, csmaxM=max(Dibba.MA), csminM=min(Dibba.MA), csavM =mean(Dibba.MA), cssdM = sd(Dibba.MA))

## Getting summaries over coldest 61 days in winter (measured from 30 days +/- lowest temp) and hottest 61 in summer (measured from 30 days +/- highest temp)
str(cursplong)
str(temp1)
cursplong1 <- merge(cursplong, temp1[,c(1,2,10,11,12)], by = c("date1", "Site"), all.x=TRUE)
str(cursplong1)
cursplong1$winteryear <- ifelse(cursplong1$month == "DEC" & cursplong1$CMA_min == 1, cursplong1$Year + 1, cursplong1$Year)
wcursp <- subset(cursplong1, CMA_min == 1)
scursp <- subset(cursplong1, CMA_max == 1)

str(wcursp)
## Winter
library(plyr)
Wdelt <- ddply(wcursp, .(winteryear, Site == "Delma") ,summarise, WcsmaxM=max(MA), WcsminM=min(MA), WcsavM =mean(MA), WcssdM = sd(MA),
               WcsmaxR=max(raw), WcsminR=min(raw), WcsavR =mean(raw), WcssdR = sd(raw))
names(Wdelt)[2] <- "TF"
Wdelt <- subset(Wdelt, TF ==TRUE)
Wdelt <- Wdelt[,-2]

Wras <- ddply(wcursp, .(winteryear,Site == "RasGhanada") ,summarise, WcsmaxM=max(MA), WcsminM=min(MA), WcsavM =mean(MA), WcssdM = sd(MA),
              WcsmaxR=max(raw), WcsminR=min(raw), WcsavR =mean(raw), WcssdR = sd(raw))
names(Wras)[2] <- "TF"
Wras <- subset(Wras, TF ==TRUE)
Wras <- Wras[,-2]

Wsaa <- ddply(wcursp, .(winteryear, Site == "Saadiyat") ,summarise, WcsmaxM=max(MA), WcsminM=min(MA), WcsavM =mean(MA), WcssdM = sd(MA),
              WcsmaxR=max(raw), WcsminR=min(raw), WcsavR =mean(raw), WcssdR = sd(raw))
names(Wsaa)[2] <- "TF"
Wsaa <- subset(Wsaa, TF ==TRUE)
Wsaa <- Wsaa[,-2]

Wala <- ddply(wcursp, .(winteryear,Site == "AlAqah") ,summarise, WcsmaxM=max(MA), WcsminM=min(MA), WcsavM =mean(MA), WcssdM = sd(MA),
              WcsmaxR=max(raw), WcsminR=min(raw), WcsavR =mean(raw), WcssdR = sd(raw))
names(Wala)[2] <- "TF"
Wala <- subset(Wala, TF ==TRUE)
Wala <- Wala[,-2]

Wdib <- ddply(wcursp, .(winteryear,Site == "Dibba") ,summarise, WcsmaxM=max(MA), WcsminM=min(MA), WcsavM =mean(MA), WcssdM = sd(MA),
              WcsmaxR=max(raw), WcsminR=min(raw), WcsavR =mean(raw), WcssdR = sd(raw))
names(Wdib)[2] <- "TF"
Wdib <- subset(Wdib, TF ==TRUE)
Wdib <- Wdib[,-2]
## Summer
Sdelt <- ddply(scursp, .(Year, Site == "Delma") ,summarise, ScsmaxM=max(MA), ScsminM=min(MA), ScsavM =mean(MA), ScssdM = sd(MA),
               ScsmaxR=max(raw), ScsminR=min(raw), ScsavR =mean(raw), ScssdR = sd(raw))
names(Sdelt)[2] <- "TF"
Sdelt <- subset(Sdelt, TF ==TRUE)
Sdelt <- Sdelt[,-2]

Sras <- ddply(scursp, .(Year, Site == "RasGhanada"),summarise, ScsmaxM=max(MA), ScsminM=min(MA), ScsavM =mean(MA), ScssdM = sd(MA),
              ScsmaxR=max(raw), ScsminR=min(raw), ScsavR =mean(raw), ScssdR = sd(raw))
names(Sras)[2] <- "TF"
Sras <- subset(Sras, TF ==TRUE)
Sras <- Sras[,-2]

Ssaa <- ddply(scursp, .(Year, Site == "Saadiyat") ,summarise, ScsmaxM=max(MA), ScsminM=min(MA), ScsavM =mean(MA), ScssdM = sd(MA),
              ScsmaxR=max(raw), ScsminR=min(raw), ScsavR =mean(raw), ScssdR = sd(raw))
names(Ssaa)[2] <- "TF"
Ssaa <- subset(Ssaa, TF ==TRUE)
Ssaa <- Ssaa[,-2]

Sala <- ddply(scursp, .(Year, Site == "AlAqah") ,summarise, ScsmaxM=max(MA), ScsminM=min(MA), ScsavM =mean(MA), ScssdM = sd(MA),
              ScsmaxR=max(raw), ScsminR=min(raw), ScsavR =mean(raw), ScssdR = sd(raw))
names(Sala)[2] <- "TF"
Sala <- subset(Sala, TF ==TRUE)
Sala <- Sala[,-2]

Sdib <- ddply(scursp, .(Year, Site == "Dibba") ,summarise, ScsmaxM=max(MA), ScsminM=min(MA), ScsavM =mean(MA), ScssdM = sd(MA),
              ScsmaxR=max(raw), ScsminR=min(raw), ScsavR =mean(raw), ScssdR = sd(raw))
names(Sdib)[2] <- "TF"
Sdib <- subset(Sdib, TF ==TRUE)
Sdib <- Sdib[,-2]

## Structure of the growth dataset
str(test)
str(Sdelt)
str(Wdelt)
## making same variables in cursp data to merge datasets
Wdelt$site <- as.factor(c(rep("Delma", nrow(Wdelt))))
Wras$site <- as.factor(c(rep("RasGhanada", nrow(Wras))))
Wsaa$site <- as.factor(c(rep("Saadiyat", nrow(Wsaa))))
Wala$site <- as.factor(c(rep("AlAqah", nrow(Wala))))
Wdib$site <- as.factor(c(rep("Dibba", nrow(Wdib))))
Sdelt$site <- as.factor(c(rep("Delma", nrow(Sdelt))))
Sras$site <- as.factor(c(rep("RasGhanada", nrow(Sras))))
Ssaa$site <- as.factor(c(rep("Saadiyat", nrow(Ssaa))))
Sala$site <- as.factor(c(rep("AlAqah", nrow(Sala))))
Sdib$site <- as.factor(c(rep("Dibba", nrow(Sdib))))

Wcssumall <- rbind(Wdelt, Wras, Wsaa, Wala, Wdib)
Scssumall <- rbind(Sdelt, Sras, Ssaa, Sala, Sdib)

str(Wcssumall)
Wcssumall <- na.omit(Wcssumall)
Wcssumall$WcsrangeM <- Wcssumall$WcsmaxM - Wcssumall$WcsminM
Wcssumall$WcsrangeR <- Wcssumall$WcsmaxR - Wcssumall$WcsminR
str(Wcssumall)
Scssumall <- na.omit(Scssumall)
Scssumall$AScsrangeM <- Scssumall$ScsmaxM - Scssumall$ScsminM
str(Scssumall)

Wcssumall$year <- Wcssumall$winteryear
Scssumall$year <- Scssumall$Year
Wcssumall$Site <- Wcssumall$site
Scssumall$Site <- Scssumall$site

test <- merge(test, Wcssumall, by = c("year", "site"), all.x=TRUE)
test <- merge(test, Scssumall, by = c("year", "site"), all.x=TRUE)

str(test)

## removing all repeated columns (site and year with .x and .y suffixes)
test <- test[,-grep(pattern="\\.x",colnames(test))] 
test <- test[,-grep(pattern="\\.y",colnames(test))] 
head(test, n= 50)

## adding minimum age data (the last year the growth records go back to, so standardized min age as much as it can be)
minage <- read.csv("minage.csv")
str(minage)
minage <- minage[,c(1,4)]
test <- merge(test, minage, by = "colony", all.x=TRUE)

### Making the ENV dataset lag a year which means adding plus 1 to year in current data (so last years
### enviro data matches up with next years growth data)
Wtsumallm1 <- Wtsumall
Wtsumallm1$Year <- Wtsumallm1$Year+1
Wtsumallm1$year <- Wtsumallm1$year+1
Stsumallm1 <- Stsumall
Stsumallm1$Year <- Stsumallm1$Year+1
Stsumallm1$year <- Stsumallm1$year+1
wintrocfinm1 <- wintrocfin
wintrocfinm1$Year <- wintrocfinm1$Year+1
wintrocfinm1$year <- wintrocfinm1$year+1
summrocfinm1 <- summrocfin
summrocfinm1$Year <- summrocfinm1$Year+1
summrocfinm1$year <- summrocfinm1$year+1
Wcssumallm1 <- Wcssumall
Wcssumallm1$Year <- Wcssumallm1$Year+1
Wcssumallm1$year <- Wcssumallm1$year+1
Scssumallm1 <- Scssumall
Scssumallm1$Year <- Scssumallm1$Year+1
Scssumallm1$year <- Scssumallm1$year+1

## Merging with growth data - year the same in ENV and growth (for one year lag)
str(onyr)
testlag1 <- merge(onyr, Wtsumallm1, by = c("year", "site"), all.x=TRUE)
testlag1 <- merge(testlag1, Stsumallm1, by = c("year", "site"), all.x=TRUE)
testlag1 <- merge(testlag1, wintrocfinm1, by = c("year", "site"), all.x=TRUE)
testlag1 <- merge(testlag1, summrocfinm1, by = c("year", "site"), all.x=TRUE)
testlag1 <- merge(testlag1, Wcssumallm1, by = c("year", "site"), all.x=TRUE)
testlag1 <- merge(testlag1, Scssumallm1, by = c("year", "site"), all.x=TRUE)

str(testlag1)
## removing all repeated columns (site and year with .x and .y suffixes)
testlag1 <- testlag1[,-grep(pattern="\\.x",colnames(testlag1))] 
testlag1 <- testlag1[,-grep(pattern="\\.y",colnames(testlag1))] 

testlag1 <- merge(testlag1, minage, by = "colony", all.x=TRUE)

## adding gulf variable with R script
test$gulf<-ifelse (test$site == "AlAqah" | test$site =="Dibba", "oman", "gulf")
testlag1$gulf<-ifelse (testlag1$site == "AlAqah" | testlag1$site =="Dibba", "oman", "gulf")

str(test)
str(testlag1)

head(test, n = 20)
head(testlag1)

## getting complete cases for calcification - same year data ### ALL INITIALLY MODELLED VARIABLES
test$tf <- complete.cases(test) 
testcalc <- subset(test, tf =="TRUE")
str(test)
str(testcalc)
##

testtib <- as_tibble(test)
testcalc <- testtib %>% select(colony, year, Site, track, sample, calc,extension,StavR,WtavR,WtsdR,winSlope,StsdR,sumSlope,WcsavR,WcssdR,ScsavR,ScssdR,ccoldday, warmday,
                               gulf,height)
head(testcalc)
testcalc <- as.data.frame(testcalc)
str(testcalc)
testcalc$tf <- complete.cases(testcalc)
str(testcalc)
testcalc <- subset(testcalc, tf =="TRUE")
str(testcalc)

## getting complete cases for extension - same year data
testtib <- as_tibble(test)
colnames(testtib)
testex <- testtib %>% dplyr::select(colony, year, Site, track, sample, calc,extension,StavR,WtavR,WtsdR,winSlope,StsdR,sumSlope,WcsavR,WcssdR,ScsavR,ScssdR,ccoldday, warmday,
                                    gulf,height)
head(testex)
testex <- as.data.frame(testex)
str(testex)
testex$tf <- complete.cases(testex)
str(testex)
testex <- subset(testex, tf =="TRUE")
str(testex)

## getting complete cases for  density - same year data
testtib <- as_tibble(test)
testden <- testtib %>% dplyr::select(colony, year, Site, track, sample, calc,density,extension, StavR,WtavR,WtsdR,winSlope,StsdR,sumSlope,WcsavR,WcssdR,ScsavR,ScssdR, 
                                     gulf,dia,height)
head(testden)
testden <- as.data.frame(testden)
str(testden)
testden$tf <- complete.cases(testden)
str(testden)
testden <- subset(testden, tf =="TRUE")
str(testden)

#### Want to look at two last temperature metrics describing the length of extreme seasons per year
str(templong)
templong$gulf<-ifelse (templong$Site == "AlAqah" | templong$Site =="Dibba", "oman", "gulf")
boxplot(templong$raw~templong$gulf)

# temp quartiles in the gulf
quantile(templong$raw[templong$gulf == "gulf"], prob=c(.05,.1,.5,.9,.95))
quantile(templong$raw[templong$gulf == "oman"], prob=c(.05, .1,.5,.9,.95))

## Getting the number of days each year at each site where temp is above the 90th and below the 10th percentile of gulf temperatures
## For this using  20 years from 1985 to 2005 to get quantiles for temps

templong20 <- subset(templong, Year <2006)
summary(templong20)

#temp quantiles in the Guulf from 1985 - 2005
quantile(templong20$raw[templong$gulf == "gulf"], prob=c(.05,.1,.5,.9,.95))

day95 <- templong %>% 
  group_by(Year, Site) %>% 
  filter(raw > 33.88) %>% 
  dplyr::summarize(count95 = n())

day90 <- templong %>% 
  group_by(Year, Site) %>% 
  filter(raw > 33.26) %>% 
  dplyr::summarize(count90 = n())

day10 <- templong %>% 
  group_by(Year, Site) %>% 
  filter(raw < 21.56) %>% 
  dplyr::summarize(count10 = n())

day05 <- templong %>% 
  group_by(Year, Site) %>% 
  filter(raw < 20.57) %>% 
  dplyr::summarize(count05 = n())

xyplot(count90 ~ Year | Site, data = day90,
       #group = year, #t="l",
       grid = TRUE, auto.key=list(space="top", columns=5, 
                                  title="Site", cex.title=1,
                                  lines=TRUE, points=FALSE))#,

xyplot(count05 ~ Year | Site, data = day05,
       #group = year, #t="l",
       grid = TRUE, auto.key=list(space="top", columns=5, 
                                  title="Site", cex.title=1,
                                  lines=TRUE, points=FALSE))#,

### Merging with the existing climate/growth data from above: testcalc, testex, testden

str(testcalc)

testcalc <- merge(testcalc, day95, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)
testcalc <- merge(testcalc, day90, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)
testcalc <- merge(testcalc, day10, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)
testcalc <- merge(testcalc, day05, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)

str(testex)

testex <- merge(testex, day95, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)
testex <- merge(testex, day90, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)
testex <- merge(testex, day10, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)
testex <- merge(testex, day05, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)

str(testden)

testden <- merge(testden, day95, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)
testden <- merge(testden, day90, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)
testden <- merge(testden, day10, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)
testden <- merge(testden, day05, by.x = c("year", "Site"), by.y = c("Year", "Site"), all.x=TRUE)

testlag1 <- testlag1[,-grep(pattern="\\.x",colnames(testlag1))] 
testlag1 <- testlag1[,-grep(pattern="\\.y",colnames(testlag1))] 
