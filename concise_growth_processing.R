### 1. Assign site labels and combine seasonal summaries

Wcssumall <- bind_rows(
  Wdelt %>% mutate(Site = "Delma"),
  Wras %>% mutate(Site = "RasGhanada"),
  Wsaa %>% mutate(Site = "Saadiyat"),
  Wala %>% mutate(Site = "AlAqah"),
  Wdib %>% mutate(Site = "Dibba")
)

Scssumall <- bind_rows(
  Sdelt %>% mutate(Site = "Delma"),
  Sras %>% mutate(Site = "RasGhanada"),
  Ssaa %>% mutate(Site = "Saadiyat"),
  Sala %>% mutate(Site = "AlAqah"),
  Sdib %>% mutate(Site = "Dibba")
)

### 2. Clean and add derived variables

Wcssumall <- Wcssumall %>%
  drop_na() %>%
  mutate(
    WcsrangeM = WcsmaxM - WcsminM,
    WcsrangeR = WcsmaxR - WcsminR,
    year = winteryear
  )

Scssumall <- Scssumall %>%
  drop_na() %>%
  mutate(
    AScsrangeM = ScsmaxM - ScsminM,
    year = Year
  )

### 3. Merge seasonal summaries with main growth data

test <- test %>%
  left_join(Wcssumall, by = c("year", "site")) %>%
  left_join(Scssumall, by = c("year", "site")) %>%
  select(-matches("\\.x$|\\.y$"))

### 4. Add minimum age data

minage <- read.csv("minage.csv") %>% select(colony, minage = 4)
test <- left_join(test, minage, by = "colony")

### 5. Create lagged versions of environmental data

Wtsumallm1 <- Wtsumall %>% mutate(across(c(Year, year), ~ . + 1))
Stsumallm1 <- Stsumall %>% mutate(across(c(Year, year), ~ . + 1))
wintrocfinm1 <- wintrocfin %>% mutate(across(c(Year, year), ~ . + 1))
summrocfinm1 <- summrocfin %>% mutate(across(c(Year, year), ~ . + 1))
Wcssumallm1 <- Wcssumall %>% mutate(across(c(Year, year), ~ . + 1))
Scssumallm1 <- Scssumall %>% mutate(across(c(Year, year), ~ . + 1))

### 6. Merge lagged ENV with growth data

testlag1 <- onyr %>%
  left_join(Wtsumallm1, by = c("year", "site")) %>%
  left_join(Stsumallm1, by = c("year", "site")) %>%
  left_join(wintrocfinm1, by = c("year", "site")) %>%
  left_join(summrocfinm1, by = c("year", "site")) %>%
  left_join(Wcssumallm1, by = c("year", "site")) %>%
  left_join(Scssumallm1, by = c("year", "site")) %>%
  select(-matches("\\.x$|\\.y$")) %>%
  left_join(minage, by = "colony")

### 7. Add gulf region classification

test <- test %>% mutate(gulf = if_else(site %in% c("AlAqah", "Dibba"), "oman", "gulf"))
testlag1 <- testlag1 %>% mutate(gulf = if_else(site %in% c("AlAqah", "Dibba"), "oman", "gulf"))

### 8. Filter complete cases for model inputs

testcalc <- test %>%
  filter(complete.cases(.)) %>%
  select(colony, year, Site, track, sample, calc, extension, StavR, WtavR, WtsdR, winSlope,
         StsdR, sumSlope, WcsavR, WcssdR, ScsavR, ScssdR, ccoldday, warmday, gulf, height)

testex <- test %>%
  filter(complete.cases(.)) %>%
  select(colony, year, Site, track, sample, calc, extension, StavR, WtavR, WtsdR, winSlope,
         StsdR, sumSlope, WcsavR, WcssdR, ScsavR, ScssdR, ccoldday, warmday, gulf, height)

testden <- test %>%
  filter(complete.cases(.)) %>%
  select(colony, year, Site, track, sample, calc, density, extension, StavR, WtavR, WtsdR, 
         winSlope, StsdR, sumSlope, WcsavR, WcssdR, ScsavR, ScssdR, gulf, dia, height)

### 9. Temperature extreme quantiles and frequency summaries

templong <- templong %>%
  mutate(gulf = if_else(Site %in% c("AlAqah", "Dibba"), "oman", "gulf"))

day95 <- templong %>% group_by(Year, Site) %>% filter(raw > 33.88) %>% summarise(count95 = n())
day90 <- templong %>% group_by(Year, Site) %>% filter(raw > 33.26) %>% summarise(count90 = n())
day10 <- templong %>% group_by(Year, Site) %>% filter(raw < 21.56) %>% summarise(count10 = n())
day05 <- templong %>% group_by(Year, Site) %>% filter(raw < 20.57) %>% summarise(count05 = n())

### 10. Merge quantiles with final datasets

merge_quantiles <- function(df) {
  df %>%
    left_join(day95, by = c("year" = "Year", "Site")) %>%
    left_join(day90, by = c("year" = "Year", "Site")) %>%
    left_join(day10, by = c("year" = "Year", "Site")) %>%
    left_join(day05, by = c("year" = "Year", "Site"))
}

testcalc <- merge_quantiles(testcalc)
testex   <- merge_quantiles(testex)
testden  <- merge_quantiles(testden)