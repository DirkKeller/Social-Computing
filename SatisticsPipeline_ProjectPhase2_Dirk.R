
cordata <- read.csv('C:/Users/DK/Desktop/Social Computing/Project/cordata.csv', header=TRUE)
bicordata <- read.csv('C:/Users/DK/Desktop/Social Computing/Project/bicordata.csv', header=TRUE)

################################################################################
################################ Statistics ####################################
################################################################################

# model parameter
dataset <- cordata
searchTerm <- dataset$sad

########### Power estimation: Monte Carlo Markov Chain Simulation ##############

to.fac <- c('Country', 'Month', 'GovRespIdx', 'ConHealthIdx', 'EcoSupIdx', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8')
to.num <- c('OxDate', 'Day', 'DailyCases', 'DailyDeaths', 'depr', 'anxi', 'suic', 'worr', 'fear', 'sad')

# transform to factor, numeric and date
dataset['Date'] <- lapply(dataset['Date'], function(x) {as.Date(as.character(x))}) 
dataset[to.fac] <- lapply(dataset[to.fac], function(x) {as.factor(as.character(x))}) 

library(dplyr)
library(lattice)
library(lme4)
library(lmerTest)
library(nlme)
library(drc)
library(Hmisc)
library(rms)



######################### Model construction ################################### 
SearchTerms <- c('depression', 'anxiety', 'suicide', 'worry', 'fear', 'sad') # 'worried', 'angst', 'anxiety', 'depressed', 'anxiety symptoms', 'depression symptoms')
qqPlots <- list()
for (sTerm_ in SearchTerms)
for (sTerm_ in SearchTerms) {
  qqPlots[[sTerm_]] <- qqmath(x = ~ eval(parse(text = substr(sTerm_, 1, 4))) |C4,
                              data = dataset,
                              ylab = paste(sTerm_))
  print(qqPlots[[sTerm_]])
}


pilot_m1 <- lmer(searchTerm ~  C1 + (1 | Month:Country), dataset)


pilot_m1 <- lmer(searchTerm ~ 1 + (1 | Country), dataset, REML = FALSE)
pilot_m2 <- lmer(searchTerm ~ 1 + (1 | Country) + (1|Country:EcoSupIdx), dataset, REML = FALSE)
pilot_m3 <- lmer(searchTerm ~ 1 + (1 | Country) + (1|Country:ConHealthIdx), dataset, REML = FALSE)
pilot_m4 <- lmer(searchTerm ~ 1 + (1 | Country) + (1|Country:EcoSupIdx) + (1|Country:ConHealthIdx), dataset, REML = FALSE)
#pilot_m1 <- lmer(searchTerm ~ 1 + (1 | Country) + (1|EcoSupIdx) + (1|ConHealthIdx), dataset)
#pilot_m1 <- lmer(searchTerm ~ 1 + (1 | Country) + (1|EcoSupIdx) + (1|ConHealthIdx), dataset)
anova(pilot_m1, pilot_m2, pilot_m3, pilot_m4)

#summary(pilot_m4)
residuals(pilot_m4)

# basic visual diagnostic
dotplot(ranef(pilot_m4)) # Check assumption 1: Residuals of randome effects should be normally distributed/have a mean of 0
plot(ranef(pilot_m4))
plot(pilot_m4) # Checking assumption 2: Homocedasticy 

m1    <- lmer(as.numeric(searchTerm) ~ 1 + C1 + (1 | Country) + (1|Country:EcoSupIdx) + (1|Country:ConHealthIdx), dataset)
m2    <- lmer(as.numeric(searchTerm) ~ 1 + C1 + C2 + (1 | Country) + (1|Country:EcoSupIdx) + (1|Country:ConHealthIdx), dataset)
m3    <- lmer(as.numeric(searchTerm) ~ 1 + C1 + C2 + C3 + (1 | Country) + (1|Country:EcoSupIdx) + (1|Country:ConHealthIdx), dataset)
m4    <- lmer(as.numeric(searchTerm) ~ 1 + C1 + C2 + C3 + C4 + (1 | Country) + (1|Country:EcoSupIdx) + (1|Country:ConHealthIdx), dataset)
m5    <- lmer(as.numeric(searchTerm) ~ 1 + C1 + C2 + C3 + C4 + C5 + (1 | Country) + (1|Country:EcoSupIdx) + (1|Country:ConHealthIdx), dataset)
m6    <- lmer(as.numeric(searchTerm) ~ 1 + C1 + C2 + C3 + C4 + C5 + C6 + (1 | Country) + (1|Country:EcoSupIdx) + (1|Country:ConHealthIdx), dataset)
m7    <- lmer(as.numeric(searchTerm) ~ 1 + C1 + C2 + C3 + C4 + C5 + C6 + C7 + (1 | Country) + (1|Country:EcoSupIdx) + (1|Country:ConHealthIdx), dataset)
m8    <- lmer(as.numeric(searchTerm) ~ 1 + C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + (1 | Country) + (1|Country:EcoSupIdx) + (1|Country:ConHealthIdx), dataset)

anova(m1, m2, m3, m4, m5,  m6, m7, m8)
summary(m8, correlation = TRUE)



# attempts to do nonlinear models
pilot_m1 <- lme(searchTerm ~ rcs(OxDate, 20) + C1,
                random =  ~1 | Country,
                #method = "REML",
                data = dataset)

summary(pilot_m1)


rsc_fit_knots <- rcspline.eval(
  dataset$OxDate,
  nk = 20,
  inclx = TRUE,
  knots.only=TRUE
)


rcs(OxDate, 20)

searchTermPlots <- list()
Dep.StartLoc <- 5
for (sTerm_ in SearchTerms) {
  qnt <- quantile(cordata[[sTerm_]], c(.05, .25, .75, .95))
  
  searchTermPlots[[sTerm_]] <- ggplot(cordata, aes(x = Oxdate,
                                                   y = cordata[, Dep.StartLoc + which(SearchTerms == sTerm_)],
                                                   group = Country,
                                                   color = Country)) +
    ylim(19,100) +
    geom_line(size = 0.4) +
    geom_smooth(aes(x = Oxdate,
                    y = cordata[, Dep.StartLoc + which(SearchTerms == sTerm_)]), color = 'black',
                method = lm, 
                formula = y ~ splines::bs(as.numeric(x), 20),
                se = TRUE,
                size = 1.3) +
    facet_wrap( ~ Country, ncol = 4) + 
    labs(x = paste('Time periode from', first(cordata$Date), 'to', last(cordata$Date)),
         y = paste('Search term Popularity of:', SearchTerms[which(SearchTerms == sTerm_)])) +
    geom_vline(xintercept = rsc_fit_knots, colour = "red") +
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
         file=paste0("Plot_results_", sTerm_,".png"), 
         path = 'C:/Users/DK/Desktop/Social Computing/Project/figures',
         width = 50, height = 10, units = "cm", dpi=300)
}






pilot_m <- lme(fixed = searchTerm ~ rcs(OxDate, 5):C1 
               + rcs(OxDate, 5):C2
               + rcs(OxDate, 5):C3 
               + rcs(OxDate, 5):C4
               + rcs(OxDate, 5):C5
               + rcs(OxDate, 5):C6
               + rcs(OxDate, 5):C7
               + rcs(OxDate, 5):C7
               + rcs(OxDate, 5):C8,
               random = ~ 1|Country,
               #list(Country = ~1, Month=~1, ConHealthIdx= ~1, EcoSupIdx= ~ConHealthIdx),
               #~ 1|Country, # list(Country = ~1, Month=~1, ConHealthIdx= ~1, EcoSupIdx= ~ConHealthIdx),
               #method = "REML",
               data = dataset)
summary(pilot_m)
plot(pilot_m)


pilot_m <- lme(fixed = searchTerm ~ rcs(OxDate, 20):C1,
               random = ~ 1|Country,
               #list(Country = ~1, Month=~1, ConHealthIdx= ~1, EcoSupIdx= ~ConHealthIdx),
               #~ 1|Country, # list(Country = ~1, Month=~1, ConHealthIdx= ~1, EcoSupIdx= ~ConHealthIdx),
               #method = "REML",
               data = dataset)
summary(pilot_m)
plot(pilot_m)



rcspline.eval(
  dataset$OxDate,
  nk = 5,
  inclx = TRUE,
  knots.only=TRUE
)



pilot_m1 <- lme(fixed = searchTerm ~ rcs(OxDate, 5) + C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8,
                random = ~ 1|Month/Country,
                #method = "REML",
                data = dataset)
pilot_m2 <- lme(fixed = searchTerm ~ rcs(OxDate, 5) + C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8,
                random = ~ 1|ConHealthIdx/Month/Country,
                #method = "REML",
                data = dataset)
pilot_m3 <- lme(fixed = searchTerm ~ rcs(OxDate, 5) + C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8,
                random = ~ 1|EcoSupIdx/Month/Country,
                #method = "REML",
                data = dataset)

pilot_m <- lme(fixed = searchTerm ~ rcs(OxDate, 20) + C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8,
               random = ~ 1|Country,
               data = dataset)

#print(summary(pilot_m), correlation=FALSE)
anova(pilot_m, pilot_m1, pilot_m2, pilot_m3) # Non-sequential analysis of variance


plot(pilot_m1)

