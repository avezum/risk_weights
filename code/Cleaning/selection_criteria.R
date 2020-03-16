rm(list = ls())                               ## Clean enviroment.
setwd("../Research/Projects/Risk_weight/")    ## Set working directory.
library(tidyverse)
library(xlsx)
load("Data/Facility.Rda")
load("Data/Tranche.Rda")

names(Facility.df) <- tolower(names(Facility.df))
names(Tranche.df) <- tolower(names(Tranche.df))

names(Facility.df)[names(Facility.df)=="lender.parent.name"] = "name"
names(Facility.df)[names(Facility.df)=="lender.parent.operating.country"] = "parent.country"


aux.date <- subset(Tranche.df,select = c("tranche.id","tranche.active.date","country.of.syndication"))

# Merge


Facility.date.matched.df <- merge(Facility.df,aux.date,by="tranche.id")



Facility.date.matched.df$lender.share... <- as.numeric(as.character(Facility.date.matched.df$lender.share...))
Facility.date.matched.df  <- Facility.date.matched.df[complete.cases(Facility.date.matched.df$lender.share...),]

Facility.date.matched.df$loan.amount <- Facility.date.matched.df$lender.share...*Facility.date.matched.df$tranche.amount..mm./100

Facility.mean.df <- Facility.date.matched.df %>%                                            
  group_by(name, parent.country,country.of.syndication) %>%
  summarise(sum = sum(loan.amount,na.rm=TRUE),
            N = n())

#Facility.mean.df <- filter(Facility.mean.df,parent.country != country.of.syndication)

n_countries.df <- Facility.mean.df %>%                                            
  group_by(name, parent.country) %>%
  summarise(sum = sum(sum,na.rm=TRUE),
            N_deals = sum(N,na.rm=TRUE),
            N_countries = n())


n_countries.df <- filter(n_countries.df, N_deals>99,N_countries>0)

n_countries.df <- n_countries.df %>%                                            
  group_by(parent.country) %>%
  summarise(N_banks = n())

n_countries.df <- filter(n_countries.df,N_banks>2)


write.csv(n_countries.df, "country_selection.csv",row.names = FALSE)
