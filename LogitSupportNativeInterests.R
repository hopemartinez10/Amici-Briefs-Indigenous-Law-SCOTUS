#Load Libraries
library(tidyverse)
library(haven)
library(stargazer)
library(dplyr)
library(tidyr)
library(ggeffects)
library(interflex)
library(marginaleffects)
library(viridisLite)
library(aod)
library(dotwhisker)
library(lmtest)
library(sandwich)


#Load Survey Data
setwd("/Users/hmartinez10/OneDrive - Georgia State University (1)/Data 2024/ICWA")
Amicus_Briefs <- read.csv("Final20082023Briefs.csv")

#logistic regression models of whether non native vs native groups are in
#support or against ICWA
Amicus_Briefs$Tribe_Third_State <- as.factor(Amicus_Briefs$Tribe_Third_State)
Amicus_Briefs <- Amicus_Briefs |>
  filter(Tribe_Third_State!= "4")
Amicus_Briefs <- Amicus_Briefs |>
  filter(Support_Native_interest!= "3")
#Descriptives
ggplot(Amicus_Briefs, aes(x = Support_Native_interest)) +
  geom_histogram(stat = "count", fill = "white", colour = "black")

ggplot(Amicus_Briefs, aes(x = Tribe_Third_State)) +
  geom_histogram(stat = "count", fill = "white", colour = "black") +
  labs(title = "", x = NULL, y = "Number of Briefs")

Amicus_Briefs$Year <- as.factor(Amicus_Briefs$Year)

ggplot(data = Amicus_Briefs, aes(x = Year)) +
  geom_line(stat = "count")

ggplot(Amicus_Briefs, aes(x = Year)) +
  geom_histogram(stat = "count", fill = "white", colour = "black")


Amicus_Briefs <- Amicus_Briefs %>%
  mutate(Tribe_Third_State = recode(Tribe_Third_State,
                                    "1" = "Native Brief",
                                    "3" = "State Brief",
                                    "2" = "Other Brief"))

Amicus_Briefs %>% count(Tribe_Third_State) 

m1 <- glm(Support_Native_interest ~ relevel(Tribe_Third_State, ref = "Native Brief"), data = Amicus_Briefs)
summary(m1)



#robust SE

cov_m2 <- vcovHC(m1, method = "HC3")
rob_m2 <- sqrt(diag(cov_m2))

wald.test(b = coef(m1), Sigma = vcov(m1), Terms = 2:length(m1$coefficients))

AIC(m1)
BIC(m1)

#Create WALD function 
##WALDFunctions for stargazer
# To call the correct number of stars from the p value of the wald test
wald.test.stars<- function(pvalue){
  if(pvalue < 0.1 & pvalue >= 0.05){return("*")
  } else if(pvalue < 0.05 & pvalue >= 0.01){return("*")
  } else if(pvalue < 0.01){return("*")
  } else {return(" ")}
}

# Pulling the chi2 statistic from the wald.test
stargazer.wald.chi<- function(model){
  require(aod)
  w1 <- wald.test(b = coef(model), Sigma = vcov(model), Terms = 2:length(model$coefficients))
  w1chi <- w1$result$chi2[1]
  return(format(round(w1chi, 3), nsmall=3)) 
}

# Pulling the significance level from the wald.test 
stargazer.wald.sig<- function(model){
  require(aod)
  w1<-wald.test(b = coef(model), Sigma = vcov(model), Terms = 2:length(model$coefficients))
  w1p<-w1$result$chi2[3]
  starw1<-wald.test.stars(w1p)
  return(starw1)
}
# Putting everything together for stargazer
stargazer.wald.output <- function(model){
  out<-paste(stargazer.wald.chi(model), stargazer.wald.sig(model))
  return(out)}
  
#Use Wald function in stargazer
wald_m1 <- stargazer.wald.output(m1)

stargazer(m1,
          out = "table.html",
          title = "Amici Briefs Supporting Native Interests",
          dep.var.labels = c("Brief Support"),
          ci.level = 0.95,
          star.cutoffs = c(0.05),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "",
          notes = "Cell entries are logit coefficient estimates with robust standard errors in parentheses.[*]",
          add.lines=list(
            c("Wald $\\chi^{2}$", stargazer.wald.output(m1)
            )))


#create plot
#ggpredict(m1, terms = c("Tribe_Third_State")) |> plot() + labs(title = "Probability Briefs Support Native Interests", x = "", y = "Supports Native Interests") + theme_bw()
ggpredict(m1, terms = c("Tribe_Third_State")) |> plot() + labs(title = "", x = "Submitted Briefs in Native Cases", y = "Probability of Support for Native Interests") + theme()

#test assumptions
plot(ICWA_Support)
#Frequency of state amicus briefs (for / against) influence upon Supreme Court

Amicus_Briefs$State <- as.numeric(Amicus_Briefs$State)
Amicus_Briefs$Other <- as.numeric(Amicus_Briefs$Other)
Amicus_Briefs$Native <- as.numeric(Amicus_Briefs$Native)
hist(Amicus_Briefs$State, breaks = c(2))
hist(Amicus_Briefs$Other, breaks = c(2))
hist(Amicus_Briefs$Native, breaks = c(2))

#Amicus_Briefs <- Amicus_Briefs |>
  filter(Tribe_Third_State!= "1")

m2 <- glm(Support_Native_interest ~ relevel(Tribe_Third_State, ref = "2"), data = Amicus_Briefs)
cov_m3 <- vcovHC(m2, method = "HC3")
rob_m3 <- sqrt(diag(cov_m3))

wald.test(b = coef(m2), Sigma = vcov(m2), Terms = 2:length(m2$coefficients))
wald_m2 <- stargazer.wald.output(m2)
stargazer(m2,
          out = "table.html",
          title = "Predicting Probability of Amici Brief Supporting Native Interests",
          dep.var.labels = c("Supports Native Interests"),
          ci.level = 0.95,
          se = list(rob_m3),
          star.cutoffs = c(0.05),
          notes.align = "l",
          notes.append = FALSE,
          notes.label = "Notes",
          notes = "p < 0.05. Robust standard errors in parentheses",
          add.lines=list(
            c("Wald $\\chi^{2}$", stargazer.wald.output(m2)
            )))



ggplot(Amicus_Briefs, aes(x = Tribe_Third_State)) +
  geom_histogram(stat = "count", fill = "white", colour = "black") +
  labs(title = "", x = NULL, y = "Number of Briefs")
