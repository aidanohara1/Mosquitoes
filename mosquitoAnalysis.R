library(tidyverse)

quito <- read.csv("WNV__Mosquito_Test.csv")
quito15 <- filter(quito,quito$'SEASON.YEAR'==2015) %>%
  filter(!is.na(LATITUDE))


library(tidycensus)
library(sf)
library(units)
library(stringr)
library(lme4)

chicago <- get_acs(
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

chicagoBetter <- chicago %>% select(-moe) %>% 
  pivot_wider(names_from = 'variable',
              values_from = 'estimate') %>%
  mutate(area = set_units(st_area(.), km^2)) %>%
  filter(B01003_001 > 0) %>%
  mutate(density = (B01003_001/area))
chicagoBetter <- chicagoBetter %>%
  mutate(NAME = sapply(strsplit(chicagoBetter$NAME,","), `[`, 1))

colnames(chicagoBetter) <- c("id","NAME","geometry",
                             "population","medincome",
                             "area","density")

#Geographic Merge
chiSF <- st_as_sf(chicagoBetter)
quito15SF <- st_as_sf(quito15, coords = c('LONGITUDE','LATITUDE')) %>%
  st_set_crs('NAD83') %>%
  st_transform(st_crs(chiSF))

tractsWithTraps <- st_join(quito15SF, chiSF, join = st_within, left = F)

twt <- st_drop_geometry(tractsWithTraps)

perWeeks <- twt %>% group_by(NAME,WEEK,RESULT) %>% 
  summarise(totalMosq = sum(NUMBER.OF.MOSQUITOES)) %>%
  inner_join(chicagoBetter) %>%
  pivot_wider(names_from = RESULT,
              values_from = totalMosq) 

perWeeks$positive <- replace_na(perWeeks$positive, 0)
perWeeks$negative <- replace_na(perWeeks$negative, 0)

perWeeks <- mutate(perWeeks, totalMosq = positive + negative) %>%
  mutate(logTotal = log(totalMosq))

perWeeks <- filter(perWeeks, id != 17031832500)
  


gG <- ggplot(perWeeks)+geom_density(alpha=0.3)+
  aes(x=log(totalMosq),color=NAME)+facet_wrap(~NAME)+theme(legend.position="none")+geom_rug()+
  xlab("log of total mosquitoes")+geom_vline(xintercept=mean(perWeeks$logTotal),color="red",lty=2)

lmer_fit <- lmer(totalMosq ~ log(density) + log(medincome)  + (1|NAME),data=perWeeks)
lmer_fit2 <- lmer(totalMosq ~ log(medincome)  + (1|NAME),data=perWeeks)
lmer_fit3 <- lmer(totalMosq ~ log(density) + (1|NAME),data=perWeeks)


tractData <- group_by(perWeeks, NAME) %>%
  summarise(medincome=unique(medincome),
            density = unique(density),
            mean=mean(totalMosq), 
            ymin=max((mean(totalMosq)-sd(totalMosq)),0),
            ymax=mean(totalMosq)+sd(totalMosq))

tractWithInts <- data.frame(tractData,
                            summary(lm(totalMosq~-1+factor(NAME),data=perWeeks))$coefficients[,1:2])

ggg1<-ggplot(tractWithInts)+
  geom_pointrange()+
  aes(x=medincome,y=Estimate,ymin=Estimate-Std..Error,ymax=Estimate+Std..Error,color=factor(NAME))+
  geom_smooth(method="lm",se=FALSE,aes(group=1))+xlab("medincome")+ylab("mean+/-se mosquitoes")+ 
  theme(legend.position = "none")
ggg1

ggg2<-ggplot(tractWithInts)+
  geom_pointrange()+
  aes(x=density,y=Estimate,ymin=Estimate-Std..Error,ymax=Estimate+Std..Error,color=factor(NAME))+
  geom_smooth(method="lm",se=FALSE,aes(group=1))+xlab("density")+ylab("mean+/-se mosquitoes")+ 
  theme(legend.position = "none")
ggg2


