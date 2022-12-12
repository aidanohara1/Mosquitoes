library(tidyverse)

quito <- read.csv("WNV__Mosquito_Test.csv")

quito15 <- filter(quito,quito$'SEASON.YEAR'==2015)
View(quito15)

ggplot(data = quito22, aes(x=LATITUDE, y = LONGITUDE, size = NUMBER.OF.MOSQUITOES)) + 
  geom_jitter(width = 0.05, height = 0.05) + 
  theme_bw()

#https://mattherman.info/blog/point-in-poly/


library(tmap)
library(tmaptools)
library(tidycensus)
library(tidyverse)
library(sf)

chiTownMedianIncome <- get_acs(
  geography = "tract", #geography level
  variables = c("B25105_001","B01003_001"), #specific variables
  table = NULL, #"group" variables and tables
  year = 2015,
  state = "IL", # state, FIPS or abbreviation
  county = "Cook", # a vector of counties 
  # If you want countie from multiple states,
  # state = "multiple"s
  geometry = TRUE,# to include shape files
  shift_geo = FALSE, # For Alaska and Hawaii
  summary_var = NULL, # Variable for comparisons
  key = Sys.getenv('CENSUS_KEY'), # YOUR CENSUS KEY
  moe_level = 90, # Margin of Error
  survey = "acs5", # acs1, acs3, or acs5
  show_call = FALSE, # For debug
)

chiTownMedianIncome <- chiTownMedianIncome %>%
  filter(!is.na(estimate))

quito15 <- quito15 %>%
  filter(!is.na(LATITUDE))

map1 <- tm_shape(chiTownMedianIncome) + 
  tm_polygons("estimate", palette = "viridis", n = 5)
map1


chiTownSF <- st_as_sf(chiTownMedianIncome)

quito15SF <- st_as_sf(quito15, coords = c('LONGITUDE','LATITUDE')) %>%
  st_set_crs('NAD83') %>%
  st_transform(st_crs(chiTownSF))

ggplot() +
  geom_sf(data = chiTownSF$geometry) +
  geom_sf(data = quito15SF$geometry) +
  theme_minimal()

trap_in_tract <- st_join(quito15SF, chiTownSF, join = st_within, left = F)
View(trap_in_tract)




map2 <- tm_shape(trap_in_tract) + 
  tm_polygons("estimate", palette = "viridis", n = 5) +
  tm_dots("NUMBER.OF.MOSQUITOES")
map2


chiTMed <- st_drop_geometry(chiTownMedianIncome)
ttt <- st_drop_geometry(trap_in_tract)


quitosPerTractSF <- trap_in_tract %>% group_by(NAME) %>% summarise(totalMosq = sum(NUMBER.OF.MOSQUITOES))

quitosPerTract <- ttt %>% group_by(NAME,RESULT) %>% summarise(totalMosq = sum(NUMBER.OF.MOSQUITOES)) 

justTrappedTracts <- filter(chiTMed, chiTMed$NAME == quitosPerTract$NAME)

withEstimates <- inner_join(quitosPerTract, chiTMed)

positivePerTract <- trap_in_tract %>% group_by(NAME,RESULT) %>% summarise(totalMosq = sum(NUMBER.OF.MOSQUITOES))

speciesPerTract <- trap_in_tract %>% group_by(NAME,SPECIES) %>% summarise(totalMosq = sum(NUMBER.OF.MOSQUITOES))


# everyYear <- quito %>% mutate



trapperPerWeek <- quito %>% group_by(SEASON.YEAR,TRAP,WEEK) %>% summarise(totalMosq = sum(NUMBER.OF.MOSQUITOES))

quitosPerTract <- ttt %>% group_by(NAME,WEEK) %>% summarise(totalMosq = sum(NUMBER.OF.MOSQUITOES))
withEstimates <- inner_join(quitosPerTract, chiTMed)
# withEstimates <- pivot_wider(withEstimates, names_from = RESULT, values_from = totalMosq)
# withEstimates$positive <- replace_na(withEstimates$positive, 0)
# withEstimates$negative <- replace_na(withEstimates$negative, 0)


ggplot(withEstimates)+geom_density(alpha=0.3)+
  aes(x=log(totalMosq),color=NAME)+facet_wrap(~NAME)+theme(legend.position="none")+geom_rug()+
  xlab("totalMosq")+geom_vline(xintercept=mean(log(withEstimates$totalMosq)),color="red",lty=2)

library(lme4)
lmer_fit <- lmer(estimate ~ totalMosq + (1|NAME),data=withEstimates)

lmer_fit2 <- lmer(totalMosq ~ estimate, data = withEstimates)
