library("tidyverse")
library("readxl")
library("car")
library("dplyr")
library("plyr")

rm(list=ls())
setwd("M:/Desktop/data")


### Importing all data

abnamro<-read_xlsx("ABN AMRO Group N.V. data.xlsx",  skip=1) %>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
MPS<- read_xlsx("Banca Monte dei Paschi di Siena SpA [MPS] data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
Bilbao<- read_xlsx("Banco Bilbao Vizcaya Argentaria SA  data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
Barclays<- read_xlsx("Barclays PLC data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
Bayern<- read_xlsx("Bayern LB data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
BNP<-read_xlsx("BNP Paribas data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
Commerzbank <-read_xlsx("Commerzbank AG data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
CAC<-read_xlsx("Credit Agricole Corporate & Investment Bank SA data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
CS<-read_xlsx("Credit Suisse AG data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
DB<- read_xlsx("Deutsche Bank data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
Dexia<- read_xlsx("Dexia SA data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
DZ<- read_xlsx("DZ Bank Group data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
HSBC<- read_xlsx("HSBC Holdings plc.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
ING<- read_xlsx("ING Group data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
ISP<- read_xlsx("Intesa Sanpaolo SpA [ISP] data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
LBBW<- read_xlsx("Landesbank Baden-Wurttemberg [LBBW] data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
Helaba<- read_xlsx("Landesbank Hessen-Thueringen GZ [Helaba] data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
Lloyds<- read_xlsx("Lloyds Banking Group PLC data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
Natixis<- read_xlsx("Natixis SA data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
NORDLB<- read_xlsx("NORDLB data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
Rabo<- read_xlsx("Rabobank data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
RBS<- read_xlsx("RBS data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
SG<- read_xlsx("Societe Generale SA data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
SCB<- read_xlsx("Standared Chartered Bank PLC data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
UBI<- read_xlsx("UBI Banca Group data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
UBS<- read_xlsx("UBS Group AG data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")
UC<- read_xlsx("UniCredit Group data.xlsx", skip=1)%>%
  select("Bank","date","Method","Type of Borrower","CPCR","SL","Location of Borrower","Exposure at default - mln (a)","Risk exposure amount or RWA  - mln  (b) = (d)/8%")


###Cleaning The merged dataset


merged<- rbind(abnamro, Barclays, Bayern,Bilbao,BNP,CAC,Commerzbank, CS,DB, Dexia, DZ, Helaba, ING, HSBC, ISP, LBBW, Lloyds, MPS, Natixis, NORDLB, Rabo, RBS, SCB, SG,UBI,UBS, UC)
merged$`Type of Borrower` <-recode(merged$`Type of Borrower`, "'Corporate' = 0; 'Corporates' = 0;'Total' = 1")
merged$Method <-recode(merged$Method, "'IRB' = 1; 'AIRB' = 1; 'FIRB'= 1 ;'SA' = 0")
merged$`Location of Borrower` <- recode(merged$`Location of Borrower`,"'Aggregate'= 1; 'Aggregated'=1")
names(merged)[8]<-"EAD"
names(merged)[9]<-"RWA"
merged<- merged %>% filter(merged$`Location of Borrower` == 1) 
merged$RWA<- as.numeric(merged$RWA)
merged$EAD<- recode(merged$EAD,"NA = 0")
merged$RWA<- recode(merged$RWA,"NA = 0")


### Creating the corporate portfolio dataset

corporate<- merged %>% filter(merged$`Type of Borrower` ==0)
corporate_sum <- ddply(corporate,c("date","Bank"), numcolwise(sum))
names(corporate_sum) [5] <-  "Total_EAD"
names(corporate_sum) [6] <-  "Total_RWA"
corporate_sum$Total_RW <- corporate_sum$Total_RWA / corporate_sum$Total_EAD
corporate<- ddply(corporate, c("date","Bank","Method"), numcolwise(sum))
corporate <- merge(corporate,corporate_sum, by= c("date","Bank","Type of Borrower"))
names(corporate)[4]<-"Method"
corporate$Method.y <- NULL
corporate$EAD_percentage<- corporate$EAD/corporate$Total_EAD
corporate<- corporate %>% filter(corporate$EAD_percentage !=1, corporate$EAD_percentage !=0) 
corporate$RWA_percentage<- corporate$RWA/corporate$Total_RWA
corporate<- corporate %>% filter(corporate$RWA_percentage !=1, corporate$RWA_percentage !=0) 
corporate<- corporate %>% filter(corporate$Method ==1)


### Creating the total portfolio dataset

total<- merged %>% filter(merged$`Type of Borrower` ==1)
total_sum <- ddply(total,c("date","Bank"), numcolwise(sum))
names(total_sum) [5] <-  "Total_EAD"
names(total_sum) [6] <-  "Total_RWA"
total_sum$Total_RW <- total_sum$Total_RWA / total_sum$Total_EAD
total<- ddply(total, c("date","Bank","Method"), numcolwise(sum))
total <- merge(total,total_sum, by= c("date","Bank"))
names(total)[3]<-"Method"
names(total)[4]<-"Type of Borrower"
total$Method.y <- NULL
total$`Type of Borrower.y` <- NULL
total$`Type of Borrower`<- recode(total$`Type of Borrower`, "2=1")
total$EAD_percentage<- total$EAD/total$Total_EAD
total<- total %>% filter(total$EAD_percentage !=1, total$EAD_percentage !=0) 
total$RWA_percentage<- total$RWA/total$Total_RWA
total<- total %>% filter(total$RWA_percentage !=1, total$RWA_percentage !=0) 
total<- total %>% filter(total$Method ==1)


### Testing correlation in Corporate Portfolio
cor.test(corporate$EAD_percentage,corporate$Total_RW)
reg1<- lm(corporate$Total_RW ~ corporate$EAD_percentage)
summary(reg1)


### Testing correlation in Total Porfolio

cor.test(total$EAD_percentage,total$Total_RW)
reg2<- lm(total$Total_RW ~ total$EAD_percentage)
summary(reg2)

