v <- sessionInfo()
print(v)
gc()
colors()
rm(list=ls())
library(readr)
library(ggplot2); library(ggthemes)
library(dplyr) #change data frames
library(readxl) #data from excel file, too tedious to manually enter data
library(lme4) #glmm feed
library(lubridate) #for dates handle
library(gridExtra) #compare ggplots
library(maps); library(mapproj) #graph US infection visual
CountyMaskMandate <- read_excel("CountyMaskMandateData.xlsx")
#View(CountyMaskMandate) #Tab of full dataset
MichiganMaskMandate  <- CountyMaskMandate %>% filter (state_name=="MICHIGAN")
#View(MichiganMaskMandate) #Reduced #2nd 84 rows

USCOVIDWeekly <- read.csv("COVIDWeekly.csv") #import and headers on
USCOVIDWeekly <- USCOVIDWeekly %>% filter(date >= as.Date("2020-02-28") & date <= as.Date("2022-09-30"))
colnames(USCOVIDWeekly)[5] <- 'ChangeinWeeklyInfections' #rename for convenience/simplicity
#View(USCOVIDWeekly) #already mi data filtered from website https://data.cdc.gov/Public-Health-Surveillance/Weekly-COVID-19-County-Level-of-Community-Transmis/jgk8-6dpn
USCOVIDWeekly$fips_code <- as.numeric(USCOVIDWeekly$fips_code)
USCOVIDWeekly$ChangeinWeeklyInfections <- as.numeric(USCOVIDWeekly$ChangeinWeeklyInfections)
USCOVIDWeekly$ChangeinWeeklyInfections <- ifelse(is.na(USCOVIDWeekly$ChangeinWeeklyInfections) | USCOVIDWeekly$ChangeinWeeklyInfections == 'suppressed', 0, USCOVIDWeekly$ChangeinWeeklyInfections)

#new change variable
tempCOVID <- as.Date(USCOVIDWeekly$date, "%Y-%m-%d") #"%m/%d/%y"
head(tempCOVID)
USCOVIDWeekly$date <- tempCOVID
USCOVIDWeekly$LastWeekDate<- as.Date(USCOVIDWeekly$date)-7
tempnewinfect <-  USCOVIDWeekly[ c("date","fips_code","ChangeinWeeklyInfections")] %>%
rename( LastWeekDate=date, InfectionsLastWeek = ChangeinWeeklyInfections )
USCOVIDWeekly<- left_join(USCOVIDWeekly,tempnewinfect,
by =c('fips_code'='fips_code',"LastWeekDate"="LastWeekDate" ))
USCOVIDWeekly$InfectRateChanges <- USCOVIDWeekly$ChangeinWeeklyInfections -USCOVIDWeekly$InfectionsLastWeek


CountyInfo1<- read.csv("United_States_COVID-19_Community_Levels_by_County.csv",header=T)
CountyInfo <- CountyInfo1 %>% group_by(county_fips) %>% 
  summarise(county_population= mean(county_population) )

USCOVIDWeekly<- left_join(USCOVIDWeekly,CountyInfo[,c("county_fips","county_population")], 
                          by =c('fips_code'='county_fips' ))  

MICOVIDWeekly <- filter(USCOVIDWeekly, state_name == 'Michigan')
USCOVIDWeekly$state_abb <- state.abb[match(USCOVIDWeekly$state_name,state.name)]
MICOVIDWeekly$state_abb <- state.abb[match(MICOVIDWeekly$state_name,state.name)]

names(CountyMaskMandate)[3]<-names(USCOVIDWeekly)[3]
countrywide <- semi_join(USCOVIDWeekly, CountyMaskMandate, by = "fips_code")
countynomaskdata <- anti_join(USCOVIDWeekly, CountyMaskMandate, by = "fips_code")
#View(countrywide); View(countynomaskdata)

#countrywide
p1<- ggplot(countrywide, aes(x = date, y = InfectRateChanges, 
                             group =fips_code, color=state_name)) + geom_line() +
  theme(legend.position="none") + 
  labs(title = "All US Counties Infection Rates", 
       x = "Date", y = "Change in Weekly Infection Rate Per 100K") +
  ylim(-1000, 1250)
p1

weighted.mean2<-function (x,w,na.rm=T){ xw<-x*w; inc<-!is.na(xw); sum(xw[inc])/sum(w[inc])}
#countrywide weigh average
p2<- ggplot(countrywide%>% group_by(state_name,date )%>%
              summarise(InfectRateChanges2= weighted.mean2(InfectRateChanges,county_population)
              ), aes(x = date, y = InfectRateChanges2, 
                     group =state_name, color=state_name)) + geom_line() +
  theme(legend.position="none")+
  labs(title = " US States Infection Rates", 
       x = "Date", y = "Change in Weekly Infection Rate Per 100K")
p2
#p2 boxplot
tempmonthlydata <- countrywide
tempmonthlydate <- format(tempmonthlydata$date, "%Y-%m")
tempmonthlydata$index <- as.factor(tempmonthlydate)
#create index of months for x while group by date plan
tempmonthlydata1 <- tempmonthlydata %>%
  group_by(index, fips_code) %>%
  summarize(InfectRateChanges2 = weighted.mean2(InfectRateChanges,county_population))
ggplot(tempmonthlydata1, aes(x = index, y = InfectRateChanges2))+
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  labs(x = "date", y = "Change in Weekly Infection Rate Per 100K",
       title = "Monthly US Counties Infection Counts")

#mi counties
p3<- ggplot(countrywide%>% filter(state_name=="Michigan") , aes(x = date, y = InfectRateChanges,                                                               group =fips_code, color=state_name)) + 
  geom_line() + 
  geom_vline(xintercept = as.numeric(as.Date("2020-07-10")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2021-06-24")), linetype = "dashed", color = "red") +
  theme(legend.position="none") +
  labs(title = "Michigan Counties", 
       x = "Date", y = "Change in Weekly Infection Rate Per 100K")
p3

#weigh average mi
p4 <- ggplot(countrywide%>% group_by(state_name,date )%>% filter(state_name=="Michigan") %>%
               summarise(InfectRateChanges2= weighted.mean2(InfectRateChanges,county_population)
               ), aes(x = date, y = InfectRateChanges2, 
                      group =state_name, color='black')) + 
  geom_line( aes(size =1.5)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-10")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2021-06-24")), linetype = "dashed", color = "red") +
  theme(legend.position="none")+ labs(title = "Michigan", 
                                      x = "Date", y = "Change in Weekly Infection Rate Per 100K")
p4

#https://martinfowler.com/articles/2021-muted-spaghetti.html
temp.data <- countrywide%>% filter(state_name=="Michigan") %>%arrange(fips_code)
cty.ppl.MI<- temp.data %>% #filter( fips_code   == "26001")%>% 
  select(county_population)/
  sum(temp.data %>% filter( date == "2020-06-03")%>% 
        select(county_population))

p4a <- ggplot(temp.data%>% group_by(date)  %>%
                summarise(InfectRateChanges= 
                            weighted.mean2(InfectRateChanges,county_population))
              , aes(x = date, y = InfectRateChanges)) + 
  geom_line(data =temp.data #%>%   filter( fips_code   == "26001")
            ,aes(group = fips_code)
            ,color = "#F8766D",  alpha = cty.ppl.MI[[1]]*50) + #county_population*5) +
  geom_line( aes(color=I('black'))) +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-10")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2021-06-24")), linetype = "dashed", color = "red") +
  theme(legend.position="none")+ labs(title = "Michigan Counties Infection Rate", 
                                      x = "Date", y = "Change in Weekly Infection Rate Per 100K")
p4a

#See what is done badly
TopCountyAverage <- countrywide %>% 
  filter(state_name == "Michigan") %>% 
  group_by(county_name) %>% 
  summarize(TopAverageWeeklyInfections = mean(InfectRateChanges, na.rm = TRUE)) %>% 
  top_n(10, TopAverageWeeklyInfections) %>%  # Select the top 10 counties
  select(county_name)  # Only keep the fips_code column

# Use ggplot's facet_wrap() function to create a small multiples plot
p5 <- ggplot(countrywide %>% 
               filter(state_name == "Michigan", county_name %in% TopCountyAverage$county_name),
             aes(x = date, y = InfectRateChanges)) +
  geom_line(color='red') + geom_vline(xintercept = as.numeric(as.Date("2020-07-10")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2021-06-24")), linetype = "dashed", color = "black") +
  facet_wrap(~ county_name, ncol = 2) +
  labs(title = "Top 10 Counties in Michigan",
       xlab="Date", ylab="Change in Weekly Infection Rate Per 100K")
p5

#See what is done well
BottomCountyAverage <- countrywide %>% 
  filter(state_name == "Michigan") %>% 
  group_by(county_name) %>% 
  summarize(BottomAverageWeeklyInfections = mean(InfectRateChanges, na.rm = TRUE)) %>% 
  arrange(BottomAverageWeeklyInfections) %>%  # Sort the data in ascending order
  head(10) %>%  # Select the first 10 rows (lowest averages)
  select(county_name)  # Only keep the fips_code column

# Use ggplot's facet_wrap() function to create a small multiples plot
p6 <- ggplot(countrywide %>% 
               filter(state_name == "Michigan", county_name %in% BottomCountyAverage$county_name),
             aes(x = date, y = InfectRateChanges)) +
  geom_line(color='blue') + geom_vline(xintercept = as.numeric(as.Date("2020-07-10")), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2021-06-24")), linetype = "dashed", color = "black") +
  facet_wrap(~ county_name, ncol = 2) +
  labs(title = "Bottom 10 Counties in Michigan",
       xlab="Date", ylab="Change in Weekly Infection Rate Per 100K")
p6

TopBottom <- countrywide %>% 
  filter(state_name == "Michigan", county_name %in% c(TopCountyAverage$county_name, BottomCountyAverage$county_name))
TopBottom$group <- ifelse(TopBottom$county_name %in% TopCountyAverage$county_name, "Top 10", "Bottom 10")


tempdata <- TopBottom
top_data <- tempdata %>% filter(group == "Top 10")
bottom_data <- tempdata %>% filter(group == "Bottom 10")
### the proportion as of the Michigan population
pplMI.top    <- c(top_data %>% arrange(fips_code) %>%
                    select(county_population) / countrywide %>%
                    filter(date == "2020-06-03" & state_abb == "MI") %>%
                    select(county_population) %>%
                    sum())[[1]]^0.2    

pplMI.bottom <- c(bottom_data %>% arrange(fips_code) %>%
                    select(county_population) / countrywide %>%
                    filter(date == "2020-06-03") %>%
                    select(county_population) %>%
                    sum())[[1]]^0.2  

# Create plot and the color
p7a <- ggplot(tempdata %>%
                group_by(date) %>%
                summarise(InfectRateChanges = weighted.mean2(InfectRateChanges, county_population)),
              aes(x = date, y = InfectRateChanges)) +
  geom_line(data = top_data %>% arrange(fips_code) , aes(group = fips_code), color = "rosybrown2", alpha = pplMI.top) +
  
  geom_line(data = bottom_data %>% arrange(fips_code) , aes(group = fips_code), color = "steelblue2", alpha = pplMI.bottom) +
  
  geom_line( data = top_data %>% group_by(date )%>%
               summarise(InfectRateChanges= weighted.mean2(InfectRateChanges,county_population)),
             color = "red") +
  
  geom_line( data = bottom_data %>% group_by(date )%>%
               summarise(InfectRateChanges= weighted.mean2(InfectRateChanges,county_population)),
             color = "blue") +
  
  geom_vline(xintercept = as.numeric(as.Date("2020-07-10")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2021-06-24")), linetype = "dashed", color = "red") +
  theme(legend.position = "none") +
  labs(title = "Michigan Trend Between 10 Poor and Fair Performing Counties",
       x = "Date",
       y = "Change in Weekly Infection Rate Per 100K")

p7a


#Any county MI
p8 <- ggplot(countrywide %>% 
               filter(state_name == "Michigan", county_name == "Macomb County"),
             aes(x = date, y = InfectRateChanges)) +
  geom_line() + 
  geom_vline(xintercept = as.numeric(as.Date("2020-07-10")), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2021-06-24")), linetype = "dashed", color = "red") +
  labs(title = "County",
       x="Date",y="Change in Weekly Infection Rate Per 100K")
p8

Vaccine <- read.csv("COVIDVACCINE.csv") #import and headers on
Vaccine <- rename(Vaccine, PercentWeeklyVaccinations = Series_Complete_Pop_Pct)
#View(Vaccine)
tempVaccine <- as.Date(Vaccine$Date, "%m/%d/%Y") 
head(tempVaccine)
Vaccine$Date <- tempVaccine
Vaccine <- Vaccine[Vaccine$FIPS != "UNK", ]
Vaccine$FIPS <- as.numeric(Vaccine$FIPS)
MIVaccine <- filter(Vaccine, Recip_State == 'MI')
#View(MIVaccine)

finalcoviddataset <- MICOVIDWeekly[, c("date", "county_name", "fips_code", "InfectRateChanges")]
finalcoviddataset$ActiveMaskMandateNumeric <- ifelse(finalcoviddataset$date >= "2020-04-27" & finalcoviddataset$date <= "2021-06-24", 1, 0)
finalcoviddataset$ActiveMaskMandate <- as.factor(finalcoviddataset$ActiveMaskMandate)#maskindicator
finalcoviddataset <- left_join(finalcoviddataset, MIVaccine, by=c('date'='Date', 'fips_code'='FIPS'))
finalcoviddataset$PercentWeeklyVaccinations[is.na(finalcoviddataset$PercentWeeklyVaccinations)] <- 0
finalcoviddataset$VariantDeltaNumeric <- ifelse(finalcoviddataset$date >= "2020-10-01", 1, 0)
finalcoviddataset$VariantDelta <- as.factor(finalcoviddataset$VariantDelta)
finalcoviddataset$VariantOmicronNumeric <- ifelse(finalcoviddataset$date >= "2021-11-01", 1, 0)
finalcoviddataset$VariantOmicron <- as.factor(finalcoviddataset$VariantOmicron)
#View(finalcoviddataset)
maskanalyze <- finalcoviddataset[ , c(1, 2, 3, 4, 6, 22, 86, 88)]
#View(maskanalyze)
maskanalyze <- maskanalyze %>% arrange(date)
maskanalyze$InfectRateChanges[maskanalyze$InfectRateChanges == 0] <- 0.0001
maskanalyze$PercentWeeklyVaccinations[maskanalyze$PercentWeeklyVaccinations == 0] <- 0.0001

#compare states covid with county pop

statewidebyDate <- USCOVIDWeekly%>% group_by( state_name,date )%>%
  summarise(state_abb = unique(state_abb),
            MeanInfectionRates= weighted.mean2(
              x= InfectRateChanges,
              w= county_population,  na.rm = TRUE))

## garbage collection after summary
gc() 

statewide <- statewidebyDate %>% filter(date > as.Date("2020-02-26")) %>%
  group_by( state_name) %>% 
  summarise(state_abb = unique(state_abb),
            MeanInfectionRates= mean(MeanInfectionRates,na.rm=TRUE)) 
#View(statewide)

countywide <- USCOVIDWeekly %>% filter(date > as.Date("2020-02-26")) %>%
  group_by( fips_code ) %>% 
  summarise(
    state_name   = unique(state_name),
    state_abb    = unique(state_abb),
    county_name  = unique(county_name),
    MeanInfectionRates= mean(InfectRateChanges ,na.rm=TRUE))
#View(countywide)

gc()

ggplot(statewide, aes(x = state_name, y = MeanInfectionRates)) +
  geom_col() +
  coord_flip(xlim = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "State Name", y = "Mean Change in Infection Rate")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 8))

gc() #use to reduce memory storage

library(socviz)
MICOVIDVisualMap <- MICOVIDWeekly[, -c(5,7)]   ## do not remove the date 
county_map_1 <- county_map %>% mutate(fips_code = as.numeric(id))%>%
  left_join(county_data[, c("id","state")], by="id" )
#View(county_map_1)

USCOVIDVisualMap <- USCOVIDWeekly[, -c(5,7)]
library( usmap) #https://cran.r-project.org/web/packages/usmap/vignettes/introduction.html
USMap<- usmap::us_map()
FullUSMap <- left_join( USMap, statewide[,-1],by =c( 'abbr'="state_abb") )

#US States

temps <- median((FullUSMap %>%
                    select(MeanInfectionRates))[[1]] ,na.rm=TRUE)
p <- ggplot(data = FullUSMap,
            mapping = aes(x = x, y = y, fill = MeanInfectionRates, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
p2 <- p1 + scale_fill_gradient2(low = "blue", mid="purple", high = "red", midpoint=temps, breaks=seq(0,1.5,0.25))#0.7
p2 + labs(fill = "Mean Change in Infection Rates by States") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_map() + theme(legend.position = "bottom")

#US Counties

FullCountyMap <- left_join( county_map_1, countywide[,c(1,5)],
                            by =c('fips_code'="fips_code") )
tempc <- median((FullCountyMap %>%
                   select(MeanInfectionRates))[[1]] ,na.rm=TRUE)
p <- ggplot(data = FullCountyMap,
            mapping = aes(x = long, y = lat, fill = MeanInfectionRates, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
p2 <- p1 + scale_fill_gradient2(low = "blue", mid="purple", high = "red", midpoint=tempc)#0.61
p2 + labs(fill = "Mean Change in Infection Rates by Counties") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_map() + theme(legend.position = "bottom")


#MI counties focus


MICountyMap <- FullCountyMap %>% filter(state =="MI")
tempmi <- median((MICountyMap %>%
                    select(MeanInfectionRates))[[1]] ,na.rm=TRUE)
p <- ggplot(data = MICountyMap,
            mapping = aes(x = long, y = lat, fill = MeanInfectionRates, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
p2 <- p1 + scale_fill_gradient2(low = "blue", mid="purple", high = "red", midpoint=tempmi)
p2 + labs(fill = "Change in Mean Infection Rates by Counties") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_map() + theme(legend.position = "bottom")

# Here is the code for comparing before, during, and after
countywide.before <- left_join( county_map_1,
                                (USCOVIDWeekly %>% filter(date > as.Date("2020-02-28") & date <= as.Date("2020-07-10") ) %>%
                                   group_by( fips_code ) %>%
                                   summarise(
                                     state_name   = unique(state_name),
                                     state_abb    = unique(state_abb),
                                     county_name  = unique(county_name),
                                     MeanInfectionRates= mean(InfectRateChanges ,na.rm=TRUE)))[,c(1,5)],
                                by =c('fips_code'="fips_code") )

countywide.during <- left_join( county_map_1, (USCOVIDWeekly %>%
                                                 filter(date > as.Date("2020-07-10") & date <= as.Date("2021-06-24") ) %>%
                                                 group_by( fips_code ) %>%
                                                 summarise(
                                                   state_name   = unique(state_name),
                                                   state_abb    = unique(state_abb),
                                                   county_name  = unique(county_name),
                                                   MeanInfectionRates= mean(InfectRateChanges ,na.rm=TRUE)))[,c(1,5)],
                                by =c('fips_code'="fips_code") )

countywide.after <- left_join( county_map_1, (USCOVIDWeekly %>%
                                                filter( date >as.Date("2021-06-24") ) %>% 
                                                group_by( fips_code ) %>%
                                                summarise(
                                                  state_name   = unique(state_name),
                                                  state_abb    = unique(state_abb),
                                                  county_name  = unique(county_name),
                                                  MeanInfectionRates= mean(InfectRateChanges ,na.rm=TRUE)))[,c(1,5)],
                               by =c('fips_code'="fips_code") )

countywide.before$period<-"1)Before Mask Mandate (7/10/20)"
countywide.during$period<-"2)During Mask Mandate"
countywide.after$period <-"3)After Mask Mandate (6/24/21)"

FullCountyMap1 <- rbind(countywide.before, countywide.during,countywide.after)

temp.m <- median((FullCountyMap1%>% filter(state =="MI")%>%
select(MeanInfectionRates))[[1]] ,na.rm=TRUE)

MICountyMap1 <- FullCountyMap1 %>% filter(state =="MI")
pa <- ggplot(data = MICountyMap1,
             mapping = aes(x = long, y = lat, fill = MeanInfectionRates,
                           group = group))
p1a <- pa + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
p2a <- p1a + scale_fill_gradient2(low = "blue", mid="purple", high = "red", midpoint=temp.m)
p2a + labs(fill = "Mean Change in Infection Rates by Counties") +
  # guides(fill = guide_legend(nrow = 1)) +
  theme_map() +
  facet_wrap(~period) +
  theme(legend.position = "bottom")

#separate data frame needed
if(FALSE){
  p0 <- ggplot(data = FullUSMap,
               mapping = aes(x = long, y = lat,
                             group = group, fill = MeanInfectionRates )) #fill=region has black/white
  p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +  #coord makes x and y better
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
    guides(fill = guide_colorbar(title = "Percentage", barwidth = 1, barheight = 5))
  p1 + labs(title = "Mean Weekly COVID Infection Density By US States") + theme_map() + labs(fill = "Percentage")
  p2 <- p1 + scale_fill_gradient(low = "white", high = "#CB454A") +
    labs(title = "Mean Weekly COVID Infection Density By US States") 
  p2 + theme_map() + labs(fill = "Percentage")   }


library(corrplot) #now installed
investigatevars <- finalcoviddataset[, c('InfectRateChanges', 'ActiveMaskMandate', 'Administered_Dose1_Pop_Pct', 
'PercentWeeklyVaccinations', 'Booster_Doses_Vax_Pct','VariantDelta',"VariantOmicron")]
investigatevars[is.na(investigatevars)] <- 0


ggpairs(investigatevars, labeller = label_wrap_gen(10))

density <- c('R','R','R','R','U','R','U','R','U','R','R','R','U','R','R','U','R','U','R','U',
             'U','R','R','U','R','R','R','R','R','R','R','R','R','R','U','R','R','R','R','U',
             'R','R','R','R','R','R','R','R','R','R','R','R','R','U','R','R','U','U','U','R',
             'U','U','U','R','R','R','R','U','R','R','U','R','R','R','R','R','R','R','U','R',
             'R','U','R')
countydensity <- maskanalyze %>% slice(1:83) %>% select(county_name) %>% mutate(new_column = density)
maskanalyze <- left_join(maskanalyze, countydensity, by = "county_name")
colnames(maskanalyze)[9] <- 'PopulationDensity'
maskanalyze$PopulationDensityValue <- ifelse(maskanalyze$PopulationDensity == "R", 0, ifelse(maskanalyze$PopulationDensity == "U", 1, NA))

gc()

#how to model response
hist(maskanalyze$InfectRateChanges, main = 'Change in Weekly Infection Rate', xlab = 'Change in Hundreds of Thousands of Cases')
hist(maskanalyze$PercentWeeklyVaccinations, main = 'Change in Weekly Infection Rate', xlab = 'Change in Hundreds of Thousands of Cases')
hist(finalcoviddataset$ActiveMaskMandateNumeric, main = 'Change in Weekly Infection Rate', xlab = 'Change in Hundreds of Thousands of Cases')
hist(maskanalyze$PopulationDensityValue, main = 'Change in Weekly Infection Rate', xlab = 'Change in Hundreds of Thousands of Cases')

empty <- lmer(InfectRateChanges ~ 1 + (1 | county_name), data = maskanalyze)
active <- lmer(InfectRateChanges ~ ActiveMaskMandate + (1 | county_name), data = maskanalyze)
vaccine <- lmer(InfectRateChanges ~ 1 + PercentWeeklyVaccinations + (1 | county_name), data = maskanalyze)
density <- lmer(InfectRateChanges ~ PopulationDensityValue + (1 | county_name), data = maskanalyze)
active_vaccine <- lmer(InfectRateChanges ~ ActiveMaskMandate + PercentWeeklyVaccinations + (1 | county_name), data = maskanalyze)
active_density <- lmer(InfectRateChanges ~ ActiveMaskMandate + PopulationDensityValue + (1 | county_name), data = maskanalyze)
vaccine_density <- lmer(InfectRateChanges ~ PercentWeeklyVaccinations + PopulationDensityValue + (1 | county_name), data = maskanalyze)
active_vaccine_density <- lmer(InfectRateChanges ~ ActiveMaskMandate + PercentWeeklyVaccinations + PopulationDensityValue + (1 | county_name), data = maskanalyze)

library(pbkrtest) #see direction in model building ask
KRmodcomp(active, empty)#work still
gc()
KRmodcomp(vaccine, empty) #work
gc()
KRmodcomp(density, empty) #work
gc()
KRmodcomp(active_vaccine, active) #work
gc()
KRmodcomp(active_vaccine, vaccine) #work
gc()
KRmodcomp(active_density, active) #work
gc()
KRmodcomp(active_density, density) #work
gc()
KRmodcomp(vaccine_density, vaccine) #work
gc()
KRmodcomp(vaccine_density, density) #work
gc()
KRmodcomp(active_vaccine_density, active_vaccine)#work
gc()
KRmodcomp(active_vaccine_density, active_density) #work
gc()
KRmodcomp(active_vaccine_density, vaccine_density) #work
gc()


#Use instead
library(lmerTest)
lrtest1<-anova(active, empty); lrtest1
lrtest2<-anova(vaccine, empty); lrtest2
lrtest3<-anova(density, empty); lrtest3
lrtest4<-anova(active_vaccine, active); lrtest4
lrtest5<-anova(active_vaccine, vaccine); lrtest5
lrtest6<-anova(active_density, active); lrtest6
lrtest7<-anova(active_density, density); lrtest7
lrtest8<-anova(vaccine_density, vaccine); lrtest8
lrtest9<-anova(vaccine_density, density); lrtest9
lrtest10<-anova(active_vaccine_density, active_vaccine); lrtest10
lrtest11<-anova(active_vaccine_density, active_density); lrtest11
lrtest12<-anova(active_vaccine_density, vaccine_density); lrtest12

## S3 method for class 'lmerModLmerTest'
lmerfull <- lmer(InfectRateChanges ~ ActiveMaskMandate 
                 + PercentWeeklyVaccinations +(1+ActiveMaskMandate|county_name)
                 +(PercentWeeklyVaccinations|county_name)+VariantDelta+VariantOmicron, data=maskanalyze)
step(
  lmerfull,
  #direction = 'backward',
  ddf = "Satterthwaite",
  alpha.random = 0.1,
  alpha.fixed = 0.05,
  reduce.fixed = TRUE,
  reduce.random = TRUE,
  keep = "ActiveMaskMandate"
)

#summary for aic or bic given as well so no summary
# random slope to test mask mandate effectiveness or necessity 
activeneed <- lmer(InfectRateChanges ~ ActiveMaskMandate + (1 | county_name) + (ActiveMaskMandate|county_name), data = maskanalyze)
vaccineneed <-lmer(InfectRateChanges ~ PercentWeeklyVaccinations + (1 | county_name) + (PercentWeeklyVaccinations|county_name), data = maskanalyze)
densityneed <-lmer(InfectRateChanges ~ PopulationDensityValue + (1 | county_name) + (PopulationDensityValue|county_name), data = maskanalyze)
#test random slope(response = fixed effect fixed intercept random slope)
summary(activeneed);summary(vaccineneed);summary(densityneed)

#best model diagnostics
best<-lme4::lmer(InfectRateChanges ~ (1+ActiveMaskMandate|county_name)+ActiveMaskMandate + PercentWeeklyVaccinations+VariantOmicron+VariantDelta, data=maskanalyze)
summary(best)
coef(best)
ranef(best)
plot(resid(best) ~ fitted(best),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqnorm(resid(best))

library(car)
cd <- cooks.distance(best)
# Plot Cook's distance
plot(cd, type = "o", pch = 20, main = "Cook's distance plot")
abline(h = 4/length(cd), lty = 2, col = "red")

h <- lm.influence(best)@hat
 influence.merMod(best)
# Plot leverage
plot(h, type = "o", pch = 20, main = "Leverage plot")
abline(h = 2 * p/n, lty = 2, col = "red")

# Identify outliers
outliers <- abs(resid(best)) > 2 * sd(resid(best))
# Plot residuals vs. fitted values with outliers in red
plot(fitted(best), resid(best), pch = ifelse(outliers, 20, 1), 
     col = ifelse(outliers, "red", "black"), 
     xlab = "Fitted", ylab = "Residuals")

# Calculate correlation matrix
cor_mat <- cor(model.matrix(best))
# Plot correlation matrix
library(corrplot)
corrplot(cor_mat, method = "color", type = "lower", 
         tl.cex = 0.8, tl.col = "black")

# Extract fixed effects model matrix and residuals
X <- model.matrix(best)
y <- resid(best)

# Create scatter plots of residuals against each predictor
par(mfrow = c(2, 2))
for (i in 1:ncol(X)) {
  # Check for missing values
  if (any(is.na(X[, i])) || any(is.na(y))) {
    next
  }
  plot(X[, i], y, xlab = colnames(X)[i], ylab = "Residuals")
}

#Scale-location heteroscedasicity
sqrt_abs_resid <- sqrt(abs(resid(best)))
plot(fitted(best), sqrt_abs_resid, pch = 20, 
     xlab = "Fitted", ylab = "sqrt(|Residuals|)")
abline(h = c(-1, 1), lty = 2, col = "red")

