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

rm(list = ls())                                                                ## Clean enviroment,
setwd("../Research/Projects/Risk_weight/")                                     ## set working directory,
options(java.parameters = "-Xmx4g" )                                           ## increase java memory to load 
library(tidyverse)                                                             ## excel files and,
library(XLConnect)                                                             ## load the libraries.
library(data.table)
library(foreign)

##============================================================================##
## Dealscan                                                                   ##
##============================================================================##

for(i in c("Facility","Deal","Tranche")) {                                    ## I save one dataframe for each level
data.path  <- "../../../data/Financial/Dealscan/Dealscan_Raw/"                ## within a deal: facility (bank level),
data.names <-  list.files(path = paste0(data.path,i,"/"),pattern="*.xls")     ## tranche (loan level) and, deal (group
data.list  <-  lapply(paste0(data.path,i,"/",data.names),                     ## of loans that are signed in a deal).
                readWorksheetFromFile,sheet = 1)
assign(paste0(i,".df"), rbindlist(data.list, use.names=TRUE))
setnames(get(paste0(i,".df")),tolower(names(get(paste0(i,".df")))))                                       
save(list=paste0(i,".df"),file=paste0("Data/",i,".Rda"))
rm(data.list)
}

##============================================================================##
## EBA-tranparency exercises                                                  ##
##============================================================================##

data.path <- "../../../data/Financial/EBA-transparency_exercise/"             ## Files' location.
data.name <- "/tr_cre.csv"                                                    ## Files' name.

for(i in 2015:2017) {
  assign(paste0("credit_",i), read.csv(paste0(data.path,i,data.name)))        ## I set variables' name to 
  setnames(get(paste0("credit_", i)),                                         ## lower-case so we have no
           tolower(names(get(paste0("credit_",i)))))                          ## problem when merging files.                
}                                                                                 
EBA.df <- rbindlist(lapply(ls(pattern="credit_*"), get), fill = TRUE) 

EBA.ID.df <- readWorksheetFromFile(paste0(data.path,"2017/TR_Metadata.xlsx"), ## This file contain the match 
                                   1,startRow=2)                              ## between different identifiers 
                                                                              ## used by EBA in different exercises.
setnames(EBA.ID.df, tolower(names(EBA.ID.df)))                                       

EBA.ID.df <- tibble::rowid_to_column(EBA.ID.df, "id")                         ## I create a unique identifier. 

EBA.ID.df[,c("finrep","fin_year_end","periods","tr_16","tr_15")] <- NULL      ## A bit of cleaning before turning
                                                                              ## the dataset to long format.
EBA.ID.df <- melt(EBA.ID.df, 
                  id.vars = c("id","country","desc_country","ssm","name"),
                  variable.name = "index", 
                  value.name = "lei_code")
EBA.ID.df[,c("index")] <- NULL                                                ## We need to remove the index                                            
EBA.ID.df <- unique(EBA.ID.df)                                                ## variable, NA and, non-unique 
EBA.ID.df <- na.omit(EBA.ID.df)                                               ## observations to merge correctly.

EBA.df <- merge(EBA.ID.df,EBA.df,by="lei_code",all = TRUE)                       
EBA.df[,c("label","footnote","status","perf_status")] <- NULL   
rm(list = ls(pattern="credit_*"))                                                 

EBA.country.df <- readWorksheetFromFile(paste0(data.path,                     ## This file contain the match 
                                               "2017/TR_Metadata.xlsx"),      ## between country number identifier
                                   4,startRow=2)                              ## used by EBA and country name.
                                                                              
setnames(EBA.country.df, tolower(names(EBA.country.df)))                      ## Variables are set to lower case
names(EBA.country.df)[names(EBA.country.df)=="country"] <-  "country.y"       ## and names are changed to merge.
names(EBA.country.df)[names(EBA.country.df)=="label"] <-  "borrower.country"
EBA.df <- merge(EBA.country.df,EBA.df,by="country.y",all = TRUE) 

variables <- c("0522","0532","0512","0542")                                   ## Identifier numbers for variables of 
EBA.df$item <- sapply(EBA.df$item,toString)                                   ## interest (check lines 94-97) are
EBA.df$item_global <- sapply(EBA.df$item,str_sub,start= -4)                   ## taken from file TR_Data_Dictionary.
EBA.df$item <- NULL                                                            

EBA.df <- subset(EBA.df,                                                      ## I keep only corporate lending 
                      subset = EBA.df$item_global %in% variables &            ## (item 303).
                        EBA.df$exposure %in% 303)

EBA.df <- dcast(EBA.df, lei_code + ... ~ item_global, value.var="amount")     ## Split Item variables in exposures 
EBA.df <- plyr::rename(EBA.df,c("0522"="Exposure",                            ## variables and rename them.
                                "0532"="Risk_exposure",
                                "0512"="Exposure_default",
                                "0542"="Risk_exposure_default"))

save(EBA.df,file="Data/EBA.Rda")

