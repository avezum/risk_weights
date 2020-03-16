##-----------------------------------------------------------------------------##
## Project: Bank Regulation and Capital Structure                              ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Date: 10/10/2018                                                            ##
## Description: this file opens, merges and saves in .Rda format the following ## 
## datasets: Dealscan, EBA-transparency exercises                              ##
##-----------------------------------------------------------------------------##

##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##

rm(list = ls())                               ## Clean enviroment.
setwd("../Research/Projects/Risk_weight/")    ## Set working directory.
options(java.parameters = "-Xmx4g" )          ## Increase java memory to load excel files.
library(tidyverse)                            ## Load the libraries.
library(XLConnect)
library(data.table)

##=============================================================================##
## Cleaning                                                                    ##
##=============================================================================##

load("Data/Dataset.Rda")


##########################################################################

matched.names.matrix <- matched.names.matrix.Parent[matched.names.matrix.Parent$jw<0.20,]
matched.names.matrix <- subset(matched.names.matrix,is.na(s2.i)==FALSE)
matched.names.matrix <- subset(matched.names.matrix,select = EBA:DealScan)
names(Facility.df)[names(Facility.df)=="Lender.Parent.Name"] = "name"
names(Facility.df)[names(Facility.df)=="Lender.Parent.Operating.Country"] = "parent.country"
names(EBA.df)[names(EBA.df)=="desc_country"] = "parent.country"
names(EBA.df)[names(EBA.df)=="name"] = "EBA"

aux.date <- subset(Tranche.df,select = c("Tranche.Id","Tranche.Active.Date","Country.Of.Syndication"))

# Merge
aux <-  matched.names.matrix
names(aux)[names(aux)=="DealScan"] = "name"
Facility.name.matched.df <- merge(Facility.df,aux,by="name")
Facility.date.matched.df <- merge(Facility.name.matched.df,aux.date,by="Tranche.Id")
Facility.date.matched.df <- mutate(Facility.date.matched.df,year=year(Facility.date.matched.df$Tranche.Active.Date))
Facility.date.matched.df <- mutate(Facility.date.matched.df,month=month(Facility.date.matched.df$Tranche.Active.Date))
Facility.date.matched.df <- mutate(Facility.date.matched.df,semester=ifelse(month > 6, 12, 6))
names(Facility.date.matched.df)[names(Facility.date.matched.df)=="Country.Of.Syndication"] = "borrower.country"
EBA.df <- mutate(EBA.df,year=as.numeric(substr(EBA.df$period,0,4)))
EBA.df <- mutate(EBA.df,semester=as.numeric(substr(EBA.df$period,5,6)))

start_time <- Sys.time()
bank.df <- merge(Facility.date.matched.df,EBA.df,by=c("EBA","year","semester","parent.country","borrower.country"))

end_time <- Sys.time()

print(end_time - start_time)

save(bank.df,file="Data/Dataset.Rda")



bank.df$exposure <- NULL
names(bank.df) <- tolower(names(bank.df))

## Remove defaulted exposures from IRB exposures 
bank.df <- mutate(bank.df,
                  exposure_adj = ifelse(is.na(exposure_default),exposure,exposure-exposure_default))

bank.df <- mutate(bank.df,
                  risk_exposure_adj = ifelse(is.na(risk_exposure_default),
                                             risk_exposure,risk_exposure-risk_exposure_default))

## Create Risk weight variable
bank.df <- mutate(bank.df,risk_weight = risk_exposure_adj/exposure_adj)


## Remove negative risk weights
bank.df  <- bank.df [complete.cases(bank.df$risk_weight),]
bank.df  <- bank.df [bank.df$risk_weight>0,]

## Create risk calculation method factor
bank.df <- mutate(bank.df,approach = factor(ifelse(portfolio==1,"SA","IRB")))

## Create SSM factor
bank.df <- mutate(bank.df,ssm.dummy = as.numeric(ifelse(ssm=="Y",1,0)))


## Create loan share variable
bank.df$lender.share... <- as.numeric(as.character(bank.df$lender.share...))
bank.df$loan.amount <- bank.df$lender.share...*bank.df$tranche.amount..mm./100
bank.df$loan.amount <- Winsorize(bank.df$loan.amount,probs=c(0.01,0.99),na.rm = TRUE)

bank.df$tranche.amount..mm. <- Winsorize(bank.df$tranche.amount..mm.,probs=c(0.01,0.99),na.rm = TRUE)


## Create factors for fixed effects
bank.df$tranche.id <- factor(bank.df$tranche.id)
bank.df$iso_code<- factor(bank.df$iso_code)
bank.df$country.x<- factor(bank.df$country.x)
bank.df$period <- factor(bank.df$period)
bank.df$eba <- factor(bank.df$eba)

# Create default rate
bank.df$default_rate <- bank.df$risk_exposure_default/bank.df$risk_exposure
bank.df$default_rate_w <- Winsorize(bank.df$default_rate,probs=c(0.05,0.95),na.rm = TRUE)


# Create risk weight average

bank.df <- bank.df %>%                                               ## Mean for each host-destination combination
  group_by(borrower.country,parent.country, year,semester,portfolio) %>%
  mutate(N = n(),
    mean.rw.host_dest = mean(risk_weight,na.rm=TRUE)-risk_weight/N)


bank.df <- bank.df %>%                                               ## Mean for each bank by portfolio
  group_by(eba, year,semester,portfolio) %>%
  mutate(mean.rw.bank = mean(risk_weight,na.rm=TRUE),
         mean.loan.bank = mean(loan.amount,na.rm=TRUE),
         mean.default.bank = mean(default_rate,na.rm=TRUE),
         mean.exposure.bank = mean(exposure,na.rm=TRUE))

bank.df <- bank.df %>%                                               ## Mean for each bank total
  group_by(eba, year,semester) %>%
  mutate(mean.default.bank = mean(default_rate,na.rm=TRUE),
         mean.exposure.bank = mean(exposure,na.rm=TRUE))

bank.df <- bank.df %>%                                               ## Mean of the mean for each bank (for each host)
  group_by(parent.country, year,semester,portfolio) %>%
  mutate(N = n(),
    mean.rw.host = mean(mean.rw.bank,na.rm=TRUE)-mean.rw.bank/N)

bank.df <- bank.df %>%                                               ## Mean for each loan
  group_by(tranche.id,portfolio) %>%
  mutate(mean.rw.tranche = weighted.mean(risk_weight,loan.amount,na.rm=TRUE))


bank.df <- bank.df %>%                                               ## Mean for each destination
  group_by(borrower.country,year,semester,portfolio) %>%
  mutate(mean.rw.dest = mean(risk_weight,na.rm=TRUE),
         mean.default.dest = mean(default_rate,na.rm=TRUE))

save(bank.df,file="Data/Dataset_clean.Rda")