##-----------------------------------------------------------------------------##
## Project: Bank Regulation and Capital Structure                              ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Date: 12/10/2018                                                            ##
## Description: this file matches Facility.Rda and EBA.Rda from Data_Import.R  ## 
## by name and location of banks                                               ##
##-----------------------------------------------------------------------------##

##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##
setwd("../Research/Projects/Risk_weight/")                                     ## set working directory,
options(java.parameters = "-Xmx4g" )                                           ## increase java memory to load 
library(tidyverse)                                                             ## excel files, load libraries and,
library(reshape2)                                                              ## load data imported (see 
library(stringr)                                                               ## Data_Import.R).
library(data.table)
library(DescTools)
library(stringdist)
library(compiler)
library(doParallel) 
library(iterators)



rm(list = ls())   
load("Data/Facility.Rda")
load("Data/EBA.Rda")

Facility.df <- Facility.df %>% 
  rename(name = lender.parent.name)




##=============================================================================##
## Name match                                                                  ##
##=============================================================================##

df1 <- EBA.df %>% 
  select(name) %>% 
  mutate(name = as.character(toupper(name)),
         name = str_replace(name, " \\[.*\\]", "")) %>%
  unique(.) %>%
  na.omit(.)
row.names(df1)<-1:nrow(df1)
df1.list <- split(df1[['name']], (as.numeric(rownames(df1))-1) %/% 10)

df2 <- Facility.df %>% 
  select(name) %>% 
  mutate(name = as.character(toupper(name)),
         name = str_replace(name, " \\[.*\\]", "")) %>% 
  unique(.)%>% 
  na.omit(.)
row.names(df2)<-1:nrow(df2)

distance.methods<-c('osa')
match.matrix.long <- NULL

ptime <- system.time({for(m in 1:length(distance.methods)){ 
  
  min.distance <-  foreach(i=df1.list, .combine=rbind, 
                           .packages=c('stringdist','data.table')) %dopar% {
                    distance.matrix <- stringdistmatrix(i,df2[['name']],
                                                        method = distance.methods[m])
                    data.table(distance = apply(distance.matrix, 1, min),
                               columns = apply(distance.matrix, 1, which.min))}
  
  match.matrix.long <-rbind(data.table(df1 = df1[['name']],                  
                                       df2 = df2[min.distance[["columns"]], ], 
                                       distance = min.distance[["distance"]],
                                       method = distance.methods[m]),
                            match.matrix.long)
  
}})[3]

match.matrix <- as.data.table(pivot_wider(match.matrix.long, 
                                          names_from = method, 
                                          values_from = distance)) 


##=============================================================================##
## Name match  test                                                                ##
##=============================================================================##
rm(list=setdiff(ls(), "oldtime"))  
start_time <- Sys.time()
                                                              ## Clean enviroment,
load("Data/Facility.Rda")
load("Data/EBA.Rda")

Facility.df <- Facility.df %>% 
  rename(name = lender.parent.name)


df1 <- EBA.df %>% 
  select(name) %>% 
  mutate(name = as.character(toupper(name)),
         name = str_replace(name, " \\[.*\\]", "")) %>%
  unique(.) %>%
  na.omit(.)
row.names(df1)<-1:nrow(df1)

df2 <- Facility.df %>% 
  select(name) %>% 
  mutate(name = as.character(toupper(name)),
         name = str_replace(name, " \\[.*\\]", "")) %>% 
  unique(.)%>% 
  na.omit(.)
row.names(df2)<-1:nrow(df2)

rm(list=setdiff(ls(), c("df1","df2", "oldtime", "start_time"))) 
distance.methods<-c('osa')
distance.matrix <- matrix(nrow = nrow(df1),              
                          ncol = nrow(df2))
match.matrix.long <- NULL

myFunc <- function(x){
  match(as.data.frame(min.distance)[x, ], as.data.frame(distance.matrix)[x, ])
}
myFunc <- cmpfun(myFunc)
for(m in 1:length(distance.methods)){ 
  distance.matrix <- stringdistmatrix(df1[['name']],df2[['name']],
                                      method = distance.methods[m])
  min.distance <- apply(distance.matrix, 1, min)
  rows <- rownames(as.data.frame(min.distance))
  columns <-apply(as.data.frame(rows), 1, FUN=myFunc ) 
  rm(distance.matrix)
  match.matrix.long <-rbind(data.table(df1 = df1[rows, ],                  
                                       df2 = df2[columns, ], 
                                       dist = min.distance,
                                       method = distance.methods[m]),
                            match.matrix.long)
  
}

match.matrix <- as.data.table(pivot_wider(match.matrix.long, 
                                          names_from = method, 
                                          values_from = dist)) 

end_time <- Sys.time()
print(end_time - start_time)
oldtime

##=============================================================================##
## Name match 3                                                                ##
##=============================================================================##
start_time <- Sys.time()
distance.methods<-c('osa')
match.matrix.long<-NULL
for(m in 1:length(distance.methods))                                           ## I create a matrix for each 
{                                                                              ## distance method that has the 
  distance.matrix<-matrix(NA, nrow = length(df1$name),                    ## respective distance for each
                          ncol = length(df2$name))                        ## combination of the two datasets.
  for(i in 1:length(df2$name)) {
    for(j in 1:length(df1$name)) { 
      distance.matrix[j,i]<-stringdist(tolower(df2[i,]),tolower(df1[j,]),
                                       method = distance.methods[m])      
    }  
  }
  
  min.distance<-apply(distance.matrix, 1, base::min)                            
  
  for(r in 1:nrow(distance.matrix))
  {
    column <-match(min.distance[r],distance.matrix[r,])                         ## This line of code garantees that              
    row <-r                                                                  ## I keep track of each matching pair.
    match.matrix.long <-rbind(data.frame(row.i=row.i,                          ## One minimum pair in one distance 
                                   row.j=row.j,                                ## method could also be a minimum in 
                                   DealScan=df1[row.j,],                  ## another method.
                                   EBA=df2[row.i,], 
                                   adist=min.distance[r],
                                   method=distance.methods[m]),
                        match.matrix.long)
  }
}

end_time <- Sys.time()
print(end_time - start_time)

match.matrix <-dcast(match.matrix.long,row.i+row.j+DealScan+EBA~method,             ## I set dataset in long format to
                          value.var = "adist")                                 ## better compare metrics.




save(match.matrix,file="Data/EBA_Dealscan_match.Rda")



shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
str_extract(shopping_list, "\\d")
