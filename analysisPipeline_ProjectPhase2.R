# If libraries are not present uncomment the following section
    #Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre7") # for 64-bit version
    #install.packages(c("tidyverse", "dplyr", "rJava", "xlsx", "ggplot2", "lattice", "lme4", "lmerTest", "R.utils", "gtrendsR", "lubridate", "nlme", "drc", "tibble", "scales", "Hmisc", "rms"))

# Load libraries
library(tidyverse)
library(dplyr)
library(rJava)
library(xlsx)
library(ggplot2)
library(R.utils)
library(gtrendsR)
library(lubridate)
library(tibble)
library(lattice)
library(lme4)
library(lmerTest)
library(nlme)
library(drc)

################################ Hyperparameters ###############################

# !!!!! Country name must be valid. Use 'unique(OxGovResp$CountryName' to retreive valid country names!!!!!
# !!!!! Country tag must be valid. Use 'data(countries)' to retreive valid codes !!!!!

# Country to include in the analysis 
CountryList <- c('Australia',
                 'Canada',
                 'United Kingdom',
                 'United States')
CountryTag <- c('AU', 'CA', 'GB', 'US')

# Internet search terms
SearchTerms <- c('depression', 'anxiety', 'suicide', 'worry', 'fear', 'sad') # 'worried', 'angst', 'anxiety', 'depressed', 'anxiety symptoms', 'depression symptoms')

# Time period
Start <- '2020-01-01'
End <- '2021-05-31'

################################################################################
########################## Google Trends data set  #############################
################################################################################

# Control for matching length of country names and country tags.
if (length(CountryList) != length(CountryTag)) {
  stop('The list of country names and their tags do not match!')
}

# Control for matching length of country names and country tags.
for (k in length(CountryList)){
  if (substr(CountryList[k], 1, 1) != substr(CountryTag[k], 1, 1)) {
    stop('The country names and country tags do not match in their sequence!')
  }
}

########################## Google Trends Extractor #############################
get_daily_gtrend <- function(keyword, geo, from, to) {
  if (ymd(to) >= floor_date(Sys.Date(), 'month')) {
    to <- floor_date(ymd(to), 'month') - days(1)
    
    if (to < from) {
      stop('Specifying \'to\' date in the current month is not allowed')
    }
  }
  
  mult_m <- gtrends(keyword = keyword, geo = geo, time = paste(from, to))$interest_over_time %>%
    mutate(hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
    group_by(month = floor_date(date, 'month'), keyword) %>%
    summarise(hits = sum(hits)) %>%
    ungroup() %>%
    mutate(ym = format(month, '%Y-%m'),
           mult = hits / max(hits)) %>%
    select(month, ym, keyword, mult) %>%
    as_tibble()
  
  pm <- tibble(s = seq(ymd(from), ymd(to), by = 'month'), 
               e = seq(ymd(from), ymd(to), by = 'month') + months(1) - days(1))
  
  raw_trends_m <- tibble()
  
  for (i in seq(1, nrow(pm), 1)) {
    curr <- gtrends(keyword, geo = geo, time = paste(pm$s[i], pm$e[i]))
    print(paste('for', pm$s[i], pm$e[i], 'retrieved', count(curr$interest_over_time), 'days of data (all keywords)'))
    raw_trends_m <- rbind(raw_trends_m,
                          curr$interest_over_time)
  }
  
  trend_m <- raw_trends_m %>%
    select(date, keyword, hits) %>%
    mutate(ym = format(date, '%Y-%m'),
           hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
    as_tibble()
  
  trend_res <- trend_m %>%
    left_join(mult_m) %>%
    mutate(est_hits = hits * mult) %>%
    select(date, keyword, est_hits) %>%
    as_tibble() %>%
    mutate(date = as.Date(date))
  
  return(trend_res)
}

################### Constructs the gTrends data set ############################
for (i in 1:length(CountryTag)) { # for all countries
  for (j in 1:length(SearchTerms)) { # for all search terms 
    term <- data.frame(get_daily_gtrend(SearchTerms[j], CountryTag[i], Start, End)) # request single serch term for an individual country from Google Trends
    
    if (j == 1){ # constructs the first data frame for the search terms
      terms.country <- data.frame(Country = CountryList[i], CountryTag = CountryTag[i], Date = term$date)
    }
    
    terms.country <- cbind(terms.country,term$est_hits)
    names(terms.country)[names(terms.country)=="term$est_hits"] <- SearchTerms[j]
  }
  
  if (i == 1) { # constructs the first data frame for the countries
    gTrends <- terms.country
  } 
  else {
    gTrends <- rbind(gTrends, terms.country)
  }
}
# save the data set
gTrends[,1:2] <- lapply(gTrends[,1:2], function(x) {as.factor(as.character(x))}) # change 'Country' and 'CountryTag' to factors
write.csv(gTrends,'C:/Users/DK/Desktop/Social Computing/Project/gTrends.csv', row.names=FALSE)

############################# Load the data set ################################

gTrends <- read.csv('C:/Users/DK/Desktop/Social Computing/Project/gTrends.csv', header=TRUE)

# transform dates to Oxford dates
OxDateStart <- paste(substr(Start, 1, 4),  substr(Start, 6, 7), substr(Start, 9, 10), sep="")
OxDateEnd <- paste(substr(End, 1, 4),  substr(End, 6, 7), substr(End, 9, 10), sep="")

OxDate <- gTrends$Date
# Convert gTrend dates to Oxford dates 
for (x in 1:length(gTrends$Date)) {
  OxDate[x] <- as.numeric(paste(substr(gTrends$Date[x], 1, 4),
                                      substr(gTrends$Date[x], 6, 7),
                                      substr(gTrends$Date[x], 9, 10),
                                      sep=""))
}
gTrends <- tibble::add_column(gTrends, OxDate, .after = which(colnames(gTrends) == 'Date'))
gTrends <- dplyr::filter(gTrends, OxDate >= OxDateStart & OxDate <= OxDateEnd)

################################################################################
######## Oxford-COVID-19-Government-Response-Policy data set ###################
################################################################################

# Loading OxCGRT_latest data set
OxGovResp <- read.csv('C:/Users/DK/Desktop/Social Computing/Project/OxCGRT_latest.csv', header=TRUE)

# Filtering OxGovResp             
OxGovResp <- dplyr::filter(OxGovResp, 
                    CountryName %in% CountryList
                    & OxGovResp$Jurisdiction == "NAT_TOTAL",
                    Date >= OxDateStart
                    & Date <= OxDateEnd) %>%
  dplyr::select(CountryName,
                Date, 
                C1_School.closing, 
                C2_Workplace.closing, 
                C3_Cancel.public.events, 
                C4_Restrictions.on.gatherings,
                C5_Close.public.transport,
                C6_Stay.at.home.requirements,
                C7_Restrictions.on.internal.movement,
                C8_International.travel.controls,
                GovernmentResponseIndex,
                ContainmentHealthIndex,
                EconomicSupportIndex)

# JHU daily cases/deaths 
COVIDCases <- read.csv('C:/Users/DK/Desktop/Social Computing/Project/CovidCasesDataset.csv', header=TRUE)

############################# Merging data sets ################################

# Check if Oxford Dates and gTrend dates match
if (!is_empty(which(is.na(match(OxGovResp$Date, gTrends$OxDate))))) {
  stop('Oxfort and Google Trends data sets do not match in their dates (e.g. not the same length)!')
}

# Merge the OxGovResp COVIDCases
OxGovResp <- tibble::add_column(arrange(OxGovResp, CountryName),
                              dplyr::select(arrange(COVIDCases, Country), Daily.Cases, Daily.Deaths),
                              .before = which(colnames(OxGovResp) == 'GovernmentResponseIndex'))
# Merge the OxGovResp and gTrends
rawdata <- tibble::add_column(arrange(OxGovResp, CountryName),
                              dplyr::select(arrange(gTrends, Country), Date, all_of(SearchTerms)),
                              .after = which(colnames(OxGovResp) == 'Date'))

# rename column names to variable abbrivations
cName.Base <- c('Country', 'OxDate', 'Date')
cName.Dep <- c()
cName.Ind <- c("C1","C2","C3","C4","C5","C6","C7","C8")
cName.ConCOV <- c('DailyCases', 'DailyDeaths')
cName.ConIdx <- c('GovRespIdx', 'ConHealthIdx', 'EcoSupIdx')

for (j in 1:length(SearchTerms)) {
  cName.Dep <- c(cName.Dep, substr(SearchTerms[j], 1, 4))
}

colnames(rawdata) <- c(cName.Base,cName.Dep,cName.Ind, cName.ConCOV, cName.ConIdx)
                        #C1 = School closure, C2 = Workplace closure, C3 = Cancel of public events, C4 = Restriction on gatherings
                        #C5 = Closure of public transport, C6 = Stay at home requirements, C7 = Restriction on internal movements, C8 = International travel control
# save the raw data set
#write.csv(rawdata,'C:/Users/DK/Desktop/Social Computing/Project/rawdata.csv',  row.names=FALSE)
#rawdata <- read.csv('C:/Users/DK/Desktop/Social Computing/Project/rawdata.csv',  header=TRUE)
#################### Pre-processing and data set construction ##################

# Construct the corrected data set: cordata
cordata <- na.omit(rawdata) # remove Na
cordata <- add_column(cordata, data.frame(cbind( # construct Months and Days variable
  Month = substr(cordata$OxDate, 3, 6), 
  Day = substr(cordata$OxDate, 5,8))), 
  .after = which(colnames(cordata)== "OxDate")) 
cName.Base <- c('Country', 'OxDate', 'Date', 'Month', 'Day')

# normalized cases (per inhabitants)
Abs.pop <- c(25822100, 39110000, 67886011, 331002651)/100000 # 1: Australia, 2: Canada, 3: United Kingdom, 4: United States
norm.DailyCases <- cordata$DailyCases

for (Country_ in CountryList) {
  cordata$DailyCases[cordata$Country == Country_] <- round((cordata$DailyCases[cordata$Country == Country_]
                                                      /Abs.pop[which(CountryList == Country_)]), 2)
}

for (Country_ in CountryList) {
  cordata$DailyDeaths[cordata$Country == Country_] <- round((cordata$DailyDeaths[cordata$Country == Country_]
                                                      /(Abs.pop[which(CountryList == Country_)])), 2)
}

# categorize governmental indeces 
sel <- grepl("Idx",names(cordata))
cordata[sel] <- round(cordata[sel])
cordata[sel] <- lapply(cordata[sel], function(x) ifelse(x >= 0 & x < 20, 0, x))
cordata[sel] <- lapply(cordata[sel], function(x) ifelse(x >= 20 & x < 40, 1, x))
cordata[sel] <- lapply(cordata[sel], function(x) ifelse(x >= 40 & x < 60, 2, x))
cordata[sel] <- lapply(cordata[sel], function(x) ifelse(x >= 60 & x < 80, 3, x))
cordata[sel] <- lapply(cordata[sel], function(x) ifelse(x >= 80 & x <= 100, 4, x))


# Construct new data set: bicordata
bicordata <- cordata
bicordata[cName.Ind] <- lapply(bicordata[cName.Ind], function(x) { # binary policies (Yes/No)
  ifelse (!isZero(x), x/x, 0) 
})

# save the raw data set
write.csv(cordata,'C:/Users/DK/Desktop/Social Computing/Project/cordata.csv', row.names=FALSE)
write.csv(bicordata,'C:/Users/DK/Desktop/Social Computing/Project/bicordata.csv', row.names=FALSE)
#cordata <- read.csv('C:/Users/DK/Desktop/Social Computing/Project/cordata.csv', header=TRUE)
#bicordata <- read.csv('C:/Users/DK/Desktop/Social Computing/Project/bicordata.csv', header=TRUE)
########## Data Visualization: Term popularity per date per country ############
searchTermPlots <- list()
Dep.StartLoc <- length(cName.Base)
for (sTerm_ in SearchTerms) {
  qnt <- quantile(cordata[[sTerm_]], c(.05, .25, .75, .95))
  
  searchTermPlots[[sTerm_]] <- ggplot(cordata, aes(x = as.Date(Date),
                                                   y = cordata[, Dep.StartLoc + which(SearchTerms == sTerm_)],
                                                   group = Country,
                                                   color = Country)) +
    ylim(19,100) +
    geom_line(size = 0.4) +
    geom_smooth(aes(x = as.Date(Date),
                    y = cordata[, Dep.StartLoc + which(SearchTerms == sTerm_)]),
                method = lm, 
                formula = y ~ splines::bs(x, 50),
                se = TRUE,
                size = 1.3) +
    facet_wrap( ~ Country, ncol = 4) + 
    labs(x = paste('Time periode from', first(cordata$Date), 'to', last(cordata$Date)),
         y = paste('Search term Popularity of:', SearchTerms[which(SearchTerms == sTerm_)])) +
    geom_hline(yintercept = qnt[1], colour = "red") +
    geom_hline(yintercept = qnt[4], colour = "red") +
    geom_hline(yintercept = qnt[2], colour = "lightgreen") +
    geom_hline(yintercept = qnt[3], colour = "lightgreen") +
    theme_classic() +
    theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold'),
          axis.title.y = element_text(color = 'black', size = 12, face = 'bold'),
          legend.position = "none",
          panel.grid.major.x = element_line(color = 'gray90',
                                            size = 0.3,
                                            linetype = 1),
          panel.grid.major.y = element_line(color = 'gray90',
                                            size = 0.3,
                                            linetype = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(fill = "gray99")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  print(searchTermPlots[[sTerm_]])
  ggsave(searchTermPlots[[sTerm_]], 
         file=paste0("Plot_", sTerm_,".png"), 
         path = 'C:/Users/DK/Desktop/Social Computing/Project/figures',
         width = 50, height = 10, units = "cm", dpi=300)
}

############ Data Visualization: Policy level per date per country #############
IndPlots <- list()
Ind.StartLoc <- length(c(cName.Base, cName.Dep))

PolicyList <- c('(C1) School closing',
                '(C2) Workplace closing',
                '(C3) Cancellation of public events',
                '(C4) Restrictions on gatherings',
                '(C5) Closing of public transport',
                '(C6) Stay-at-home requirements',
                '(C7) Internal mobility restrictions',
                '(C8) International travel control')

for (Ind_ in cName.Ind) {
  IndPlots[[Ind_]] <- ggplot(cordata, aes(x = as.Date(Date),
                                          y = as.numeric(cordata[,Ind.StartLoc + which(cName.Ind == Ind_)]),
                                          group = Country,
                                          color = Country)) +
    scale_y_continuous(limits = c(0,max(cordata[Ind_]) + max(cordata[Ind_])/10),
                       breaks = seq(0, max(cordata[Ind_])+1, by = 1)) +
    geom_line(size = 0.4) +
    facet_wrap( ~ Country, ncol = 4) + 
    labs(x = paste('Time periode from\n', first(cordata$Date), 'to', last(cordata$Date)),
         y = paste(PolicyList[which(cName.Ind == Ind_)]))+
    theme_classic() +
    theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold'),
          axis.title.y = element_text(color = 'black', size = 12, face = 'bold'),
          legend.position = "none",
          panel.grid.major.x = element_line(color = 'gray90',
                                            size = 0.3,
                                            linetype = 1),
          panel.grid.major.y = element_line(color = 'gray90',
                                            size = 0.3,
                                            linetype = 1),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "gray99")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

  print(IndPlots[[Ind_]])
  ggsave(IndPlots[[Ind_]], 
         file=paste0("Plot_", Ind_,".png"), 
         path = 'C:/Users/DK/Desktop/Social Computing/Project/figures', 
         width = 50, height = 7.5, units = "cm", dpi=300)
  
}

######## Data Visualization: Daily Cases/Deaths per date per country ###########
ConCOVPlots <- list()
ConCOV.StartLoc <- length(c(cName.Base, cName.Dep, cName.Ind))
m.cName.ConCOV <- c('Daily Cases', 'Daily Deaths')

for (ConCOV_ in m.cName.ConCOV) {
  ConCOVPlots[[ConCOV_]] <- ggplot(cordata, aes(x = as.Date(Date),
                                                   y = cordata[, ConCOV.StartLoc + which(m.cName.ConCOV == ConCOV_)],
                                                   group = Country,
                                                   color = Country)) +
    #scale_y_continuous(labels = label_number(suffix = " K")) +
    geom_line(size = 0.4) +
    facet_wrap( ~ Country, ncol = 4) + 
    labs(x = paste('Time periode from\n', first(cordata$Date), 'to', last(cordata$Date)),
         y = paste(m.cName.ConCOV[which(m.cName.ConCOV == ConCOV_)], 'per 100.000 inhabitants')) +
    theme_classic() +
    theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold'),
          axis.title.y = element_text(color = 'black', size = 12, face = 'bold'),
          legend.position = "none",
          panel.grid.major.x = element_line(color = 'gray90',
                                            size = 0.3,
                                            linetype = 1),
          panel.grid.major.y = element_line(color = 'gray90',
                                            size = 0.3,
                                            linetype = 1),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "gray99")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(ConCOVPlots[[ConCOV_]])
  ggsave(ConCOVPlots[[ConCOV_]], 
         file=paste0("Plot_", ConCOV_,".png"), 
         path = 'C:/Users/DK/Desktop/Social Computing/Project/figures', 
         width = 50, height = 10, units = "cm", dpi=300)
}

############### Data Visualization: Index per date per country #################
ConIdxPlots <- list()
ConIdx.StartLoc <- length(c(cName.Base, cName.Dep, cName.Ind, cName.ConCOV)) 

for (ConIdx_ in cName.ConIdx) {
  ConIdxPlots[[ConIdx_]] <- ggplot(cordata, aes(x = as.Date(Date),
                                                y = as.numeric(cordata[, ConIdx.StartLoc + which(cName.ConIdx == ConIdx_)]),
                                                group = Country,
                                                color = Country)) +
    scale_y_continuous(limits = c(0,max(cordata[Ind_]) + max(cordata[Ind_])/10),
                       breaks = seq(0, max(cordata[Ind_])+1, by = 1)) +
    geom_line(size = 0.4) +
    facet_wrap( ~ Country) + 
    labs(x = paste('Time periode from\n', first(cordata$Date), 'to', last(cordata$Date)),
         y= paste(colnames(cordata[ConIdx.StartLoc + which(cName.ConIdx == ConIdx_)]))) +
    theme_classic() +
    theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold'),
          axis.title.y = element_text(color = 'black', size = 12, face = 'bold'),
          panel.grid.major.x = element_line(color = 'gray90',
                                            size = 0.3,
                                            linetype = 1),
          panel.grid.minor.x = element_blank(),
          plot.background = element_rect(fill = "gray99")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%m-%y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  print(ConIdxPlots[[ConIdx_]])
  ggsave(ConIdxPlots[[ConIdx_]], 
         file=paste0("Plot_", ConIdx_,".png"),
         path = 'C:/Users/DK/Desktop/Social Computing/Project/figures',
         width = 50, height = 8.5, units = "cm", dpi=300)
}

################################################################################
############################# Other stuff ######################################
################################################################################

############################### load gTrends ###################################
"
gTrends_daily <- read.csv('C:/Users/DK/Desktop/Social Computing/Project/gTrends.csv', header=TRUE)
gTrends_daily <- dplyr::select(gTrends_daily, !worrying)
gTrends_daily <- dplyr::select(gTrends_daily, !X)
gTrends_daily <- dplyr::select(gTrends_daily, !OxDate)
OxDate <- gTrends_daily$Date
# Convert gTrend dates to Oxford dates 
for (x in 1:length(gTrends_daily$Date)) {
  OxDate[x] <- as.numeric(paste(substr(gTrends_daily$Date[x], 1, 4),
                                substr(gTrends_daily$Date[x], 6, 7),
                                substr(gTrends_daily$Date[x], 9, 10),
                                sep=""))
}
gTrends_daily <- tibble::add_column(gTrends_daily, OxDate, .after = which(colnames(gTrends_daily) == 'Date'))

gTrends_daily <- dplyr::filter(gTrends_daily, 
                           OxDate >= OxDateStart
                           & OxDate <= OxDateEnd)
gTrends_daily <- cbind(cbind(data.frame(CountryTag = gTrends$Country), data.frame(Date = gTrends$date), gTrends[,2:7]))
gTrends_daily <- data.frame(Country = OxGovResp$CountryName, gTrends)
write.csv(gTrends_daily,'C:/Users/DK/Desktop/Social Computing/Project/gTrends.csv', row.names=FALSE)
"

########################### COVID CASES DATA SET ###############################
"
COVIDCases[1538,3] <- round(1993 + (2146 - 1993)/2)
COVIDCases[1499,3] <- round(2713 + (3124 - 2713)/2)
write.csv(COVIDCases, 'C:/Users/DK/Desktop/Social Computing/Project/CovidCasesDataset.csv',  row.names=FALSE)
"

################### Data Visualization: Grouped policies #######################

"
ggplot(data = cordata, mapping = aes(x = as.Date(Date), y = as.numeric(C1), fill = '(C1) School closing')) +
geom_area(stat = 'identity', alpha = 0.5, color = 'red', size = 0.5) +
geom_area(aes(y = as.numeric(C2), fill = '(C2) Workplace closing'), stat = 'identity', alpha = 0.3, color = 'blue', size = 0.5) +
facet_wrap( ~ Country, ncol = 4) + 
  labs(x = paste('Time periode from\n', first(cordata$Date), 'to', last(cordata$Date)),
       y = paste('Level of the C1 and C2'))+
  theme_classic() +
  theme(axis.title.x = element_text(color = 'black', size = 12, face = 'bold'),
        axis.title.y = element_text(color = 'black', size = 12, face = 'bold'),
        legend.position = 'none',
        panel.grid.major.x = element_line(color = 'gray90',
                                          size = 0.3,
                                          linetype = 1),
        panel.grid.major.y = element_line(color = 'gray90',
                                          size = 0.3,
                                          linetype = 1),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = 'gray99')) +
  scale_x_date(date_breaks = '1 month', date_labels = "%m-%y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
"