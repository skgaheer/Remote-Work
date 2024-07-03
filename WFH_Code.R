#install.packages("ivreg", dependencies = TRUE)
#install.packages("xtable")
#install.packages("stargazer")
library(stargazer)
install.packages("pacman")
# some useful settings
knitr::opts_chunk$set(echo = TRUE, tidy.opts = list(width.cutoff = 55), tidy = TRUE, fig.align="center")
#get rid of scientific notation
options(scipen=100)
#tells which part of code has error for knit to pdf
options(tinytext.verbose=TRUE)
#loading packages
library(pacman)
p_load(gamlr)
setwd("/Users/emeraldwaters/Documents/Classwork/Unemployment")
# Load all datasets in one at a time
# Subset data and write.csv
library(data.table)
## 2013
Person13a <- read.csv("Person13/ss13pusa.csv")

Person13b <- read.csv("Person13/ss13pusb.csv")

Person13 <- bind_rows(Person13a, Person13b)

Person13 <- Person13[, c("SPORDER", "SERIALNO", "ST", "SEX", "MARHYP", "ESR", "INDP", 
              "MSP", "PAOC", "JWTR", "SCHL", "PINCP", "RAC1P", "AGEP", "OC")]

write.csv(Person13, "Person13.csv",row.names=FALSE)
rm(list = ls())
## 2016

Person16a <- read.csv("Person16/ss16pusa.csv")
  
Person16b <- read.csv("Person16/ss16pusb.csv")
  
Person16 <- bind_rows(Person16a, Person16b)
  
Person16 <- Person16[, c("SPORDER", "SERIALNO", "ST", "SEX", "MARHYP", "ESR", "INDP", 
              "MSP", "PAOC", "SCHL", "PINCP", "RAC1P", "AGEP", "OC", "JWTR")]


write.csv(Person16, "Person16.csv",row.names=FALSE)
rm(list = ls())
## 2019
Person19a <- read.csv("Person19/psam_pusa.csv")
Person19b <- read.csv("Person19/psam_pusb.csv")

Person19 <- bind_rows(Person19a, Person19b)

Person19 <- Person19[, c("SPORDER", "SERIALNO", "ST", "SEX", "MARHYP", "ESR", "INDP", 
                  "MSP", "PAOC", "SCHL", "PINCP", "RAC1P", "AGEP", "OC", "JWTRNS")]

write.csv(Person19, "Person19.csv",row.names=FALSE)
rm(list = ls())
## 2022
Person22a <- read.csv("Person22/psam_pusa.csv")
Person22b <- read.csv("Person22/psam_pusb.csv")

Person22 <-bind_rows(Person22a, Person22b)

Person22 <- Person22[, c("SPORDER", "SERIALNO", "ST", "SEX", "MARHYP", "ESR", "INDP", 
                  "MSP", "PAOC", "SCHL", "PINCP", "RAC1P", "AGEP", "OC", "JWTRNS")]

write.csv(Person22, "Person22.csv",row.names=FALSE)
rm(list = ls())
# 2013 Household

HH13a <- read.csv("Householddta/2013hus/ss13husa.csv")

HH13b <- read.csv("Householddta/2013hus/ss13husb.csv")

HH13 <- rbind(HH13a, HH13b)

HH13 <- HH13[,c ("SERIALNO", "FINCP", "ACCESS", "BROADBND", 
                   "LAPTOP", "SATELLITE", "GRPIP", "FIBEROP", "MODEM")]

HH13$HISPEED <- HH13$FIBEROP
HH13$HISPEED[HH13$MODEM==1] <- 1
HH13$HISPEED[is.na(HH13$MODEM)] <- NA

HH13 <- subset(HH13, select = -FIBEROP)
HH13 <- subset(HH13, select = -MODEM)

write.csv(HH13, "HH13.csv")
rm(list= ls())

# 2016 HH
HH16a <- read.csv("Householddta/2016hus/ss16husa.csv")
HH16b <- read.csv("Householddta/2016hus/ss16husb.csv")

HH16 <- bind_rows(HH16a, HH16b)

HH16 <- HH16[,c ("SERIALNO", "FINCP", "ACCESS", "BROADBND", 
                   "LAPTOP", "SATELLITE", "GRPIP", "HISPEED")]
write.csv(HH16, "HH16.csv")
rm(list = ls())
# 2019

HH19a <- read.csv("Householddta/2019hus/psam_husa.csv")
HH19b <- read.csv("Householddta/2019hus/psam_husb.csv")

HH19 <- bind_rows(HH19a, HH19b)

HH19 <- HH19[,c ("SERIALNO", "FINCP", "ACCESS", "BROADBND", 
                   "LAPTOP", "SATELLITE", "GRPIP", "HISPEED")]
write.csv(HH19, "HH19.csv")
rm(list = ls())
# 2022

HH22a <- read.csv("Householddta/2022hus/psam_husa.csv")
HH22b <- read.csv("Householddta/2022hus/psam_husb.csv")

HH22 <- bind_rows(HH22a, HH22b)

HH22 <- HH22[,c ("SERIALNO", "FINCP", "ACCESSINET", "BROADBND", 
                   "LAPTOP", "SATELLITE", "GRPIP", "HISPEED")]


write.csv(HH22, "HH22.csv")
rm(list = ls())
########
Person13 <- read.csv("Person13.csv")
Person16 <- read.csv("Person16.csv")
Person19 <- read.csv("Person19.csv")
Person22 <- read.csv("Person22.csv")

# add year
library(dplyr)
Person13 <- mutate(Person13, YEAR = 2013)
Person16 <- mutate(Person16, YEAR = 2016)
Person19 <- mutate(Person19, YEAR = 2019)
Person22 <- mutate(Person22, YEAR = 2022)

Person13$JWTRNS <- Person13$JWTR
Person13 <- subset(Person13, select = -JWTR)

Person16$JWTRNS <- Person16$JWTR
Person16 <- subset(Person16, select = -JWTR)

Person13$SERIALNO <- factor(Person13$SERIALNO)
Person16$SERIALNO <- factor(Person16$SERIALNO)
Person19$SERIALNO <- factor(Person19$SERIALNO)
Person22$SERIALNO <- factor(Person22$SERIALNO)

################################################################################
#HOUSEHOLD 

HH13 <- read.csv("HH13.csv")
HH16 <- read.csv("HH16.csv")
HH19 <- read.csv("HH19.csv")
HH22 <- read.csv("HH22.csv")

# add year
library(dplyr)
HH13 <- mutate(HH13, YEAR = 2013)
HH16 <- mutate(HH16, YEAR = 2016)
HH19 <- mutate(HH19, YEAR = 2019)
HH22 <- mutate(HH22, YEAR = 2022)

HH22$ACCESS <- HH22$ACCESSINET
HH22 <- subset(HH22, select = -ACCESSINET)

HH13$SERIALNO <- as.character(HH13$SERIALNO)
HH16$SERIALNO <- as.character(HH16$SERIALNO)
HH19$SERIALNO <- as.character(HH19$SERIALNO)
HH22$SERIALNO <- as.character(HH22$SERIALNO)

# MERGE each year's person and hh together
#2013
# Sort the person data frame by serialno
Person13 <- Person13 %>%
  arrange(SERIALNO)
  
# Sort the household data frame by serialno
HH13 <- HH13 %>%
  arrange(SERIALNO)
#Merging
Person13$per <- TRUE
US13 <- merge(Person13, HH13, by = "SERIALNO", all.x = TRUE)

#2016
# Sort the person data frame by serialno
  Person16 <- Person16 %>%
    arrange(SERIALNO)
  
# Sort the household data frame by serialno
HH16 <- HH16 %>%
  arrange(SERIALNO)
#Merging
Person16$per <- TRUE
US16 <- merge(Person16, HH16, by = "SERIALNO", all.x = TRUE)

#2019
# Sort the person data frame by serialno
Person19 <- Person19 %>%
  arrange(SERIALNO)

# Sort the household data frame by serialno
HH19 <- HH19 %>%
  arrange(SERIALNO)
#Merging
Person19$per <- TRUE
US19 <- merge(Person19, HH19, by = "SERIALNO", all.x = TRUE)

#2022
# Sort the person data frame by serialno
Person22 <- Person22 %>%
  arrange(SERIALNO)

# Sort the household data frame by serialno
HH22 <- HH22 %>%
  arrange(SERIALNO)
#Merging
Person22$per <- TRUE
US22 <- merge(Person22, HH22, by = "SERIALNO", all.x = TRUE)



library(data.table)
FinalUS <- bind_rows(US13, US16, US19, US22)
##########################################################################
#VARIABLE ADJUSTMENTS
FinalUS$YEAR <- FinalUS$YEAR.x
FinalUS$YEAR <- factor(FinalUS$YEAR)
FinalUS <- FinalUS[ ,-c(15,17,18,20,21,23,26)]
FinalUS$HISPEED[FinalUS$HISPEED==2] <- 0
write.csv(FinalUS, "Final_US.csv", row.names=FALSE)
rm(list=ls())
FinalUS <- read.csv("Final_US.csv")
#restrict data to ages above 16
FinalUS <- FinalUS[FinalUS$AGEP >= 16,]
FinalUS$LFP <- ifelse(FinalUS$ESR==6,0,1)
FinalUS$WFH <- ifelse(FinalUS$JWTRNS==11,1,0)
FinalUS$Female <- ifelse(FinalUS$SEX==2,1,0)
FinalUS <- subset(FinalUS, select=-SEX)
FinalUS <- subset(FinalUS, select=-JWTRNS)
#### FIRST REGRESSIONS####################################################
library("ivreg")
RF <- lm(LFP ~ HISPEED, data = FinalUS)
summary(RF)

FS <- lm(WFH ~ HISPEED, data = FinalUS)
summary(FS) #only statistically significant with all industries

FinalUS$ACCESSWFH <- ifelse((FinalUS$HISPEED==1 & FinalUS$LAPTOP==1),1,0)
summary(FinalUS$ACCESSWFH)

#yi - LFP
#zi - HISPEED
#di - WFH


#only want industries where there isn't anyone who works from home who doesn't have internet

noncompliantind <- as.vector(na.omit(FinalUS$INDP[(FinalUS$WFH==1 & FinalUS$HISPEED==0)]))
noncompliantind <- unique(noncompliantind)
Industrydta <- FinalUS[!FinalUS$INDP %in% noncompliantind,]

twost <- FinalUS[FinalUS$ST==28 | FinalUS$ST==47,] #this data compares Missippi and Tennessee
#bc they are close geographically but T has 10 percentage point higher internet rate

iv <- ivreg(LFP ~ WFH + YEAR| HISPEED + YEAR, data = twost)
ivf <- ivreg(LFP ~ WFH + YEAR | HISPEED + YEAR, data = FinalUS[FinalUS$Female==1,])
ivc <- ivreg(LFP ~ WFH + YEAR | HISPEED + YEAR, data = FinalUS[FinalUS$PAOC > 1 & FinalUS$PAOC<4,])
indiv <- ivreg(LFP ~ WFH + YEAR | HISPEED + YEAR, data = Industrydta)
indivf <- ivreg(LFP ~ WFH + YEAR | HISPEED + YEAR, data = Industrydta[Industrydta$Female==1,])
indivc <- ivreg(LFP ~ WFH + YEAR | HISPEED + YEAR, data = Industrydta[Industrydta$PAOC>1 & Industrydta$PAOC < 4,])
summary(iv)

stargazer(iv,ivf,ivc,indiv,indivf, title="Regression Results",
          align=TRUE, dep.var.labels=c("Labor Force Participation","Female Labor Force Participation"),
          covariate.labels=c("Works From Home","Has High Speed Internet",
                             "Year"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)

################################################################################
#SUMMARY STATISTICS

library(dplyr)
internetbyst <- FinalUS %>% group_by(ST) %>% summarise(mean = mean(na.omit(HISPEED)))

internetbyst$ST[internetbyst$mean==max(internetbyst$mean)]#Rhode Island
internetbyst$ST[internetbyst$mean==min(internetbyst$mean)] #Mississippi

#Tennessee 0.76, Missippi 0.64
#Overall
female <- mean(FinalUS$Female)
#51.5% female
wfh <- mean(na.omit(FinalUS$WFH))
#8.11 % of workers work from home 
lfp <- mean(FinalUS$LFP)
#59.49 % in the labor force
flfp <- mean(FinalUS$LFP[FinalUS$Female==1])
# Female Labor force participation is 55%
mlfp <- mean(FinalUS$LFP[FinalUS$Female==0])
#Male LFP is 63.9%
internet <- mean(na.omit(FinalUS$HISPEED))
#78.7% has access to highspeed internet
Means_table<-rbind(female, wfh, lfp, flfp, mlfp, internet)
#2013
female13 <- mean(FinalUS$Female[FinalUS$YEAR==2013])
#51.7% female
wfh13 <- mean(na.omit(FinalUS$WFH[FinalUS$YEAR==2013]))
# 4.6% of workers work from home 
lfp13 <- mean(FinalUS$LFP[FinalUS$YEAR==2013])
# 60.2% in the labor force
flfp13 <- mean(FinalUS$LFP[FinalUS$Female==1 & FinalUS$YEAR==2013])
# Female Labor force participation is 55.8%
mlfp13 <- mean(FinalUS$LFP[FinalUS$Female==0 & FinalUS$YEAR==2013])
#Male LFP is 65.1%
internet13 <- mean(na.omit(FinalUS$HISPEED[FinalUS$YEAR==2013]))
# 63.77% has access to highspeed internet
Means_table13<-rbind(female13, wfh13, lfp13, flfp13, mlfp13, internet13)

#2016
female16 <- mean(FinalUS$Female[FinalUS$YEAR==2016])
#51.6% female
wfh16 <- mean(na.omit(FinalUS$WFH[FinalUS$YEAR==2016]))
# 5.38% of workers work from home 
lfp16 <- mean(FinalUS$LFP[FinalUS$YEAR==2016])
# 59.63% in the labor force
flfp16 <- mean(FinalUS$LFP[FinalUS$Female==1 & FinalUS$YEAR==2016])
# Female Labor force participation is 55.3%
mlfp16 <- mean(FinalUS$LFP[FinalUS$Female==0 & FinalUS$YEAR==2016])
#Male LFP is 64.3%
internet16 <- mean(na.omit(FinalUS$HISPEED[FinalUS$YEAR==2016]))
# 83% has access to highspeed internet
Means_table16<-rbind(female16, wfh16, lfp16, flfp16, mlfp16, internet16)
#2019
female19 <- mean(FinalUS$Female[FinalUS$YEAR==2019])
#51.4% female
wfh19 <- mean(na.omit(FinalUS$WFH[FinalUS$YEAR==2019]))
# 6% of workers work from home 
lfp19 <- mean(FinalUS$LFP[FinalUS$YEAR==2019])
# 59.76% in the labor force
flfp19 <- mean(FinalUS$LFP[FinalUS$Female==1 & FinalUS$YEAR==2019])
# Female Labor force participation is 55.6%
mlfp19 <- mean(FinalUS$LFP[FinalUS$Female==0 & FinalUS$YEAR==2019])
#Male LFP is 64.18%
internet19 <- mean(na.omit(FinalUS$HISPEED[FinalUS$YEAR==2019]))
# 82.3% has access to highspeed internet
Means_table19<-rbind(female19, wfh19, lfp19, flfp19, mlfp19, internet19)
#2022
female22 <- mean(FinalUS$Female[FinalUS$YEAR==2022])
#51.3% female
wfh22 <- mean(na.omit(FinalUS$WFH[FinalUS$YEAR==2022]))
# 15.7% of workers work from home 
lfp22 <- mean(FinalUS$LFP[FinalUS$YEAR==2022])
# 58.39% in the labor force
flfp22 <- mean(FinalUS$LFP[FinalUS$Female==1 & FinalUS$YEAR==2022])
# Female Labor force participation is 54.5%
mlfp22 <- mean(FinalUS$LFP[FinalUS$Female==0 & FinalUS$YEAR==2022])
#Male LFP is 62.5%
internet22 <- mean(na.omit(FinalUS$HISPEED[FinalUS$YEAR==2022]))
# 83.5% has access to highspeed internet
Means_table22<-rbind(female22, wfh22, lfp22, flfp22, mlfp22, internet22)

Allmeans <- cbind(Means_table, Means_table13, Means_table16, Means_table19, Means_table22)
colnames(Allmeans) <- c("Overall", "2013", "2016", "2019", "2022")

library(knitr)
kable(Allmeans)
library(xtable)

xtable(Allmeans, type="latex")
###############################################################################
# BALANCE CHECK

library("MatchIt")
library("dplyr")

FinalUS <- read.csv("/Users/sgaheer/Downloads/FinalUS 2/FinalUS.csv")
FinalUS$unemp <- ifelse(FinalUS$ESR==3,1,0)
FinalUS$WFH[FinalUS$unemp==1] <- 0

set.seed(0)
dta_sample <- FinalUS[sample(nrow(dta), 100000), ]

dta_sample <- dta_sample %>% rename(State = "ST") 
dta_sample <- dta_sample %>% rename(Family_Income = "FINCP") 
dta_sample <- dta_sample %>% rename(Year_Last_Married = "MARHYP") 
dta_sample <- dta_sample %>% rename(Employment_Status = "ESR") 
dta_sample <- dta_sample %>% rename(Industry = "INDP") 
dta_sample <- dta_sample %>% rename(Marital_Status = "MSP") 
dta_sample <- dta_sample %>% rename(Presence_and_Age_of_Own_Children = "PAOC")
dta_sample <- dta_sample %>% rename(Education_Status = "SCHL") 
dta_sample <- dta_sample %>% rename(Race = "RAC1P") 
dta_sample <- dta_sample %>% rename(Age = "AGEP") 
dta_sample <- dta_sample %>% rename(Perc_Gross_Inc = "GRPIP") 
dta_sample <- dta_sample %>% rename(HighSpeed_Internet = "HISPEED") 

m.out <- matchit(HighSpeed_Internet ~ State + Female + Industry +
                   Presence_and_Age_of_Own_Children + 
                   Education_Status + Race + Age + Family_Income + Perc_Gross_Inc, data = dta_sample,
                 replace = TRUE)

library(xtable)
bal.tab <- summary(m.out, standardize = TRUE)
bal.df <- as.data.frame(bal.tab$sum.matched)
bal.tex <- xtable(bal.df, caption = "Balance Table with p-values", label = "tab:balance")
print(bal.tex, type = "latex", file = "balancetabless(1).tex")
#############################################################################
# POST LASSO
# run lasso Y on X, find non zero betas
# run second lasso D on X, find non zero betaso
# regress Y on D with set of controls found with first 2 steps
library(gamlr)
library(data.table)

# set.seed(0)

New_US <- subset(FinalUS, select = c("LFP", "HISPEED", "ST", "FINCP", "Female", "INDP", "MSP", "PAOC", 
                                     "SCHL", "AGEP", "RAC1P", "LAPTOP", "GRPIP", "YEAR"))

controls <- model.matrix(LFP ~ HISPEED + ST + Female + 
                           INDP + PAOC + SCHL + RAC1P + AGEP + FINCP + LAPTOP + GRPIP + YEAR , data=New_US)[,-1]

y_model <- cv.gamlr(controls, New_US$LFP) #step 1

d_model <- cv.gamlr(controls, New_US$HISPEED) #step 2

d_coefs <- coef(d_model, select='min')[-1] #step 3
d_nonzero_coef_indices <- which(d_coefs !=0) 

y_coefs <- coef(y_model, select='min')[-1] 
y_nonzero_coef_indices <- which(y_coefs != 0)

nonzero_coef_indices <- union(y_nonzero_coef_indices, d_nonzero_coef_indices)

# dataframe of outcome, treatment, and selected controls
data2 <- data.frame(LFP=New_US$LFP, HISPEED= New_US$HISPEED, as.matrix(controls[,nonzero_coef_indices]))

#step 4
postlasso <- glm(LFP ~ ., data= data2)

#results
summary(postlasso)$coef['HISPEED',]

#Controls used
dim(data2)

#Counterfactual prediction
cfact_data <- data2
cfact_data$HISPEED <- 1 
pred <- predict(postlasso, newdata = cfact_data)
mean(pred)
mean(data2$LFP)

########################################################################## VISUALIZATIONS

# Rename variables:
FinalUS <- FinalUS %>% rename(State = "ST") 
FinalUS <- FinalUS %>% rename(Family_Income = "FINCP") 
FinalUS <- FinalUS %>% rename(Year_Last_Married = "MARHYP") 
FinalUS <- FinalUS %>% rename(Employment_Status = "ESR") 
FinalUS <- FinalUS %>% rename(Industry_Code = "INDP") 
FinalUS <- FinalUS %>% rename(Marital_Status = "MSP") 
FinalUS <- FinalUS %>% rename(Presence_and_Age_of_Own_Children = "PAOC") 
FinalUS <- FinalUS %>% rename(Education_Status = "SCHL") 
FinalUS <- FinalUS %>% rename(Total_Person_Income = "PINCP") 
FinalUS <- FinalUS %>% rename(Race = "RAC1P") 
FinalUS <- FinalUS %>% rename(Age = "AGEP") 
FinalUS <- FinalUS %>% rename(Only_Child = "OC") 
FinalUS <- FinalUS %>% rename(Perc_Gross_Inc = "GRPIP") 
FinalUS <- FinalUS %>% rename(Work_From_Home = "WFH") 

library(ggplot2)

# subset data
subset_2013 <- subset(FinalUS, YEAR == 2013)
subset_2016 <- subset(FinalUS, YEAR == 2016)
subset_2019 <- subset(FinalUS, YEAR == 2019)
subset_2022 <- subset(FinalUS, YEAR == 2022)

################################################################################# 2013
# calculate the percentage of women that work from home in each state
library(dplyr)
women_data <- subset_2013 %>% filter(Female == 1)

women_data <- women_data %>%
  mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))

percentage_women_working_from_home <- women_data %>%
  group_by(State) %>%
  summarize(
    total_women = n(),
    women_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home = (women_working_from_home / total_women) * 100
  )
state_lookup <- data.frame(
  state_code = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 72),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
)

percentage_women_working_from_home <- percentage_women_working_from_home %>%
  left_join(state_lookup, by = c("State" = "state_code"))

ggplot(data = percentage_women_working_from_home, aes(x = reorder(state_name, percentage_working_from_home), y = percentage_working_from_home)) +
  geom_bar(stat = "identity", fill = "lightpink", color = "black") +
  coord_flip() +  
  theme_minimal() +
  labs(title = "Percentage of Women Working from Home by State",
       x = "State",
       y = "Percentage Working from Home (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################# 2016
library(dplyr)
women_data <- subset_2016 %>% filter(Female == 1)

women_data <- women_data %>%
  mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))

percentage_women_working_from_home <- women_data %>%
  group_by(State) %>%
  summarize(
    total_women = n(),
    women_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home = (women_working_from_home / total_women) * 100
  )
state_lookup <- data.frame(
  state_code = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 72),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
)

percentage_women_working_from_home <- percentage_women_working_from_home %>%
  left_join(state_lookup, by = c("State" = "state_code"))

ggplot(data = percentage_women_working_from_home, aes(x = reorder(state_name, percentage_working_from_home), y = percentage_working_from_home)) +
  geom_bar(stat = "identity", fill = "lightpink", color = "black") +
  coord_flip() +  
  theme_minimal() +
  labs(title = "Percentage of Women Working from Home by State",
       x = "State",
       y = "Percentage Working from Home (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################# 2019
library(dplyr)
women_data <- subset_2019 %>% filter(Female == 1)

women_data <- women_data %>%
  mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))

percentage_women_working_from_home <- women_data %>%
  group_by(State) %>%
  summarize(
    total_women = n(),
    women_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home = (women_working_from_home / total_women) * 100
  )
state_lookup <- data.frame(
  state_code = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 72),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
)

percentage_women_working_from_home <- percentage_women_working_from_home %>%
  left_join(state_lookup, by = c("State" = "state_code"))

ggplot(data = percentage_women_working_from_home, aes(x = reorder(state_name, percentage_working_from_home), y = percentage_working_from_home)) +
  geom_bar(stat = "identity", fill = "lightpink", color = "black") +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Percentage of Women Working from Home by State",
       x = "State",
       y = "Percentage Working from Home (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


################################################################################# 2022

library(dplyr)
women_data <- subset_2022 %>% filter(Female == 1)

women_data <- women_data %>%
  mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))

percentage_women_working_from_home <- women_data %>%
  group_by(State) %>%
  summarize(
    total_women = n(),
    women_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home = (women_working_from_home / total_women) * 100
  )
state_lookup <- data.frame(
  state_code = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 72),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
)

percentage_women_working_from_home <- percentage_women_working_from_home %>%
  left_join(state_lookup, by = c("State" = "state_code"))

ggplot(data = percentage_women_working_from_home, aes(x = reorder(state_name, percentage_working_from_home), y = percentage_working_from_home)) +
  geom_bar(stat = "identity", fill = "lightpink", color = "black") +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Percentage of Women Working from Home by State",
       x = "State",
       y = "Percentage Working from Home (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################# 
################################################################################# 
################################################################################# 2013
FinalUS <- read.csv("/Users/sgaheer/Downloads/FinalUS 2/FinalUS.csv")
FinalUS$unemp <- ifelse(FinalUS$ESR==3,1,0)
FinalUS$WFH[FinalUS$unemp==1] <- 0

# Rename variables:
FinalUS <- FinalUS %>% rename(State = "ST") 
FinalUS <- FinalUS %>% rename(Family_Income = "FINCP") 
FinalUS <- FinalUS %>% rename(Year_Last_Married = "MARHYP") 
FinalUS <- FinalUS %>% rename(Employment_Status = "ESR") 
FinalUS <- FinalUS %>% rename(Industry_Code = "INDP") 
FinalUS <- FinalUS %>% rename(Marital_Status = "MSP") 
FinalUS <- FinalUS %>% rename(Presence_and_Age_of_Own_Children = "PAOC") 
FinalUS <- FinalUS %>% rename(Education_Status = "SCHL") 
FinalUS <- FinalUS %>% rename(Total_Person_Income = "PINCP") 
FinalUS <- FinalUS %>% rename(Race = "RAC1P") 
FinalUS <- FinalUS %>% rename(Age = "AGEP") 
FinalUS <- FinalUS %>% rename(Only_Child = "OC") 
FinalUS <- FinalUS %>% rename(Perc_Gross_Inc = "GRPIP") 
FinalUS <- FinalUS %>% rename(Work_From_Home = "WFH") 

subset_2013 <- subset(FinalUS, YEAR == 2013)
subset_2016 <- subset(FinalUS, YEAR == 2016)
subset_2019 <- subset(FinalUS, YEAR == 2019)
subset_2022 <- subset(FinalUS, YEAR == 2022)

library(dplyr)
library(ggplot2)
library(tidyr)

women_data <- subset_2013 %>% filter(Female == 1)
men_data <- subset_2013 %>% filter(Female == 0)

women_data <- women_data %>% mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))
men_data <- men_data %>% mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))

percentage_women_working_from_home <- women_data %>%
  group_by(State) %>%
  summarize(
    total_women = n(),
    women_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home_women = (women_working_from_home / total_women) * 100
  )

percentage_men_working_from_home <- men_data %>%
  group_by(State) %>%
  summarize(
    total_men = n(),
    men_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home_men = (men_working_from_home / total_men) * 100
  )

percentage_working_from_home <- percentage_women_working_from_home %>%
  left_join(percentage_men_working_from_home, by = "State")

state_lookup <- data.frame(
  state_code = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 72),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
)

percentage_working_from_home <- percentage_working_from_home %>%
  left_join(state_lookup, by = c("State" = "state_code"))

percentage_working_from_home_long <- percentage_working_from_home %>%
  gather(key = "Gender", value = "Percentage_Working_From_Home", 
         percentage_working_from_home_women, percentage_working_from_home_men)

percentage_working_from_home_long$Gender <- recode(percentage_working_from_home_long$Gender, 
                                                   "percentage_working_from_home_women" = "Women", 
                                                   "percentage_working_from_home_men" = "Men")

ggplot(data = percentage_working_from_home_long, aes(x = reorder(state_name, Percentage_Working_From_Home), y = Percentage_Working_From_Home, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("Women" = "lightpink", "Men" = "lightblue")) +
  coord_flip() +  
  theme_minimal() +
  labs(title = "Percentage of Women and Men Working from Home by State in 2013",
       x = "State",
       y = "Percentage Working from Home (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


################################################################################# 2016

women_data <- subset_2016 %>% filter(Female == 1)
men_data <- subset_2016 %>% filter(Female == 0)

women_data <- women_data %>% mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))
men_data <- men_data %>% mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))

percentage_women_working_from_home <- women_data %>%
  group_by(State) %>%
  summarize(
    total_women = n(),
    women_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home_women = (women_working_from_home / total_women) * 100
  )

percentage_men_working_from_home <- men_data %>%
  group_by(State) %>%
  summarize(
    total_men = n(),
    men_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home_men = (men_working_from_home / total_men) * 100
  )

percentage_working_from_home <- percentage_women_working_from_home %>%
  left_join(percentage_men_working_from_home, by = "State")

state_lookup <- data.frame(
  state_code = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 72),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
)

percentage_working_from_home <- percentage_working_from_home %>%
  left_join(state_lookup, by = c("State" = "state_code"))

percentage_working_from_home_long <- percentage_working_from_home %>%
  gather(key = "Gender", value = "Percentage_Working_From_Home", 
         percentage_working_from_home_women, percentage_working_from_home_men)

percentage_working_from_home_long$Gender <- recode(percentage_working_from_home_long$Gender, 
                                                   "percentage_working_from_home_women" = "Women", 
                                                   "percentage_working_from_home_men" = "Men")

ggplot(data = percentage_working_from_home_long, aes(x = reorder(state_name, Percentage_Working_From_Home), y = Percentage_Working_From_Home, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("Women" = "lightpink", "Men" = "lightblue")) +
  coord_flip() +  
  theme_minimal() +
  labs(title = "Percentage of Women and Men Working from Home by State in 2016",
       x = "State",
       y = "Percentage Working from Home (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################# 2019

women_data <- subset_2019 %>% filter(Female == 1)
men_data <- subset_2019 %>% filter(Female == 0)

women_data <- women_data %>% mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))
men_data <- men_data %>% mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))

percentage_women_working_from_home <- women_data %>%
  group_by(State) %>%
  summarize(
    total_women = n(),
    women_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home_women = (women_working_from_home / total_women) * 100
  )

percentage_men_working_from_home <- men_data %>%
  group_by(State) %>%
  summarize(
    total_men = n(),
    men_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home_men = (men_working_from_home / total_men) * 100
  )

percentage_working_from_home <- percentage_women_working_from_home %>%
  left_join(percentage_men_working_from_home, by = "State")

state_lookup <- data.frame(
  state_code = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 72),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
)

percentage_working_from_home <- percentage_working_from_home %>%
  left_join(state_lookup, by = c("State" = "state_code"))

percentage_working_from_home_long <- percentage_working_from_home %>%
  gather(key = "Gender", value = "Percentage_Working_From_Home", 
         percentage_working_from_home_women, percentage_working_from_home_men)

percentage_working_from_home_long$Gender <- recode(percentage_working_from_home_long$Gender, 
                                                   "percentage_working_from_home_women" = "Women", 
                                                   "percentage_working_from_home_men" = "Men")

ggplot(data = percentage_working_from_home_long, aes(x = reorder(state_name, Percentage_Working_From_Home), y = Percentage_Working_From_Home, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("Women" = "lightpink", "Men" = "lightblue")) +
  coord_flip() +  
  theme_minimal() +
  labs(title = "Percentage of Women and Men Working from Home by State in 2019",
       x = "State",
       y = "Percentage Working from Home (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################# 2022

women_data <- subset_2022 %>% filter(Female == 1)
men_data <- subset_2022 %>% filter(Female == 0)

women_data <- women_data %>% mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))
men_data <- men_data %>% mutate(Work_From_Home = ifelse(is.na(Work_From_Home), 0, Work_From_Home))

percentage_women_working_from_home <- women_data %>%
  group_by(State) %>%
  summarize(
    total_women = n(),
    women_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home_women = (women_working_from_home / total_women) * 100
  )

percentage_men_working_from_home <- men_data %>%
  group_by(State) %>%
  summarize(
    total_men = n(),
    men_working_from_home = sum(Work_From_Home == 1),
    percentage_working_from_home_men = (men_working_from_home / total_men) * 100
  )

percentage_working_from_home <- percentage_women_working_from_home %>%
  left_join(percentage_men_working_from_home, by = "State")

state_lookup <- data.frame(
  state_code = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 72),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")
)

percentage_working_from_home <- percentage_working_from_home %>%
  left_join(state_lookup, by = c("State" = "state_code"))

percentage_working_from_home_long <- percentage_working_from_home %>%
  gather(key = "Gender", value = "Percentage_Working_From_Home", 
         percentage_working_from_home_women, percentage_working_from_home_men)

percentage_working_from_home_long$Gender <- recode(percentage_working_from_home_long$Gender, 
                                                   "percentage_working_from_home_women" = "Women", 
                                                   "percentage_working_from_home_men" = "Men")

ggplot(data = percentage_working_from_home_long, aes(x = reorder(state_name, Percentage_Working_From_Home), y = Percentage_Working_From_Home, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("Women" = "lightpink", "Men" = "lightblue")) +
  coord_flip() +  # Flip the coordinates to place the labels vertically
  theme_minimal() +
  labs(title = "Percentage of Women and Men Working from Home by State in 2022",
       x = "State",
       y = "Percentage Working from Home (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################# 
################################################################################# 
################################################################################# 

library(dplyr)
library(ggplot2)
library(tidyr)

data_years <- list(subset_2013 = subset_2013, subset_2016 = subset_2016, subset_2019 = subset_2019, subset_2022 = subset_2022)

calculate_percentage_working_from_home <- function(data, year) {

  women_data <- data %>% filter(Female == 1)
  men_data <- data %>% filter(Female == 0)
  
  women_data <- women_data %>% mutate(WFH = ifelse(is.na(WFH), 0, WFH))
  men_data <- men_data %>% mutate(WFH = ifelse(is.na(WFH), 0, WFH))
  
  women_working_from_home <- women_data %>%
    summarize(
      total_women = n(),
      women_working_from_home = sum(WFH == 1),
      percentage_working_from_home_women = (women_working_from_home / total_women) * 100
    )
  
  men_working_from_home <- men_data %>%
    summarize(
      total_men = n(),
      men_working_from_home = sum(WFH == 1),
      percentage_working_from_home_men = (men_working_from_home / total_men) * 100
    )
  
  data.frame(
    Year = year,
    Gender = c("Women", "Men"),
    Percentage_Working_From_Home = c(women_working_from_home$percentage_working_from_home_women, men_working_from_home$percentage_working_from_home_men)
  )
}

percentage_working_from_home_all_years <- bind_rows(
  lapply(names(data_years), function(year) calculate_percentage_working_from_home(data_years[[year]], year))
)

ggplot(data = percentage_working_from_home_all_years, aes(x = Year, y = Percentage_Working_From_Home, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("Women" = "lightpink", "Men" = "lightblue")) +
  theme_minimal() +
  labs(title = "Percentage of Women and Men Working from Home by Year",
       x = "Year",
       y = "Percentage Working from Home (%)")




