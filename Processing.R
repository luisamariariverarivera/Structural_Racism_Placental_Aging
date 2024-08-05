#clean up
rm(list=ls())

## Make ICE Variables

#load packages
library(tidycensus)
library(tidyverse)
library(tigris)
library(acs)
library(tmap)
library(growthstandards)

p <- read_csv("data_raw/candle_psyc.csv")
a <- read_csv("data_raw/amend.csv")
c<-read_csv("data_raw/PLAC_ages_CPC_clean.csv")
i<-read_csv("data_raw/newinc.csv")
ct <- read_csv("data_raw/cell_type.csv")
b <- read_csv("data_raw/birth.csv")



##recode the ID to match naming convention for lab samples


c$STUDYID<-as.numeric(c$STUDYID)
i$STUDYID<-as.numeric(i$studyid)
p$STUDYID <- as.numeric(p$STUDYID)
a$STUDYID <- as.numeric(a$STUDYID)
ct$STUDYID <- as.numeric(ct$sample_id)
b$STUDYID <- as.numeric(b$STUDYID)

 
##merge in data

d1<-merge(p,i, by= "STUDYID")
d2<-merge(d1, c, by="STUDYID")
d3<-merge(d2, a, by="STUDYID")
d4 <- merge (d3, ct, by="STUDYID", all.x=TRUE)
d5 <- merge (d4, b, by="STUDYID", all.x=TRUE)

d<-d5
rm(d1,d2,d3,d4,d5,ct,c,p,i,a, b)



d$ICEincome<-as.numeric(d$ICEincome)
d$ICErace<-as.numeric(d$ICErace)
d$ICEraceinc<-as.numeric(d$ICEraceinc)
d$age_difference_CPC<-d$DNAme_GA_CPC-d$gestage
d$delivery<-substr(d$delclass,1,1)
d$delivery<-as.factor(d$delivery)
levels(d$delivery) <- c("Term", "Spontaneous preterm", "PROM preterm", "PROM c-section", "Preterm Fetal Ind", "Preterm  Maternal Ind") 
d$black<-ifelse(d$aa==1, "African American", "Not AA")
d$bmi <- as.numeric(d$M1MB_BMI)
d$hypertension<-ifelse(d$M3LD_GESTHTN4==1, "Hypertension", "No Hypertension")
d$diabetes<-ifelse(d$mdiabetes==1, "Gestational Diabetes", "No Diabetes")
d$term<-ifelse(d$delivery=="Term",1,0)
d$term<-as.factor(d$term)
levels(d$term)<-c("Preterm", "Full Term")
d$csex<-substr(d$csex, 1, 1)
d$csex<-ifelse(d$csex=="1", 1,0)
d$csex<-factor(d$csex,levels = c(0,1),
               labels = c("Female", "Male"))
d$adjusted_income<-as.numeric(d$adjusted_income)
d$minc<-factor(d$minc, levels =c(1,2,3,4,5,6,7,8,9,10,11),
               labels = c("<4.9k", "5-9.9k", "10-14.9k", "15-19.9k", "20-24.9k", "25-34.9k", "35- 44.9k", "45- 54.9k", "55 - 64.9k", "65-74.9k", ">75k"))

d$meduc<-factor(d$meduc, levels=c(1,2,3,4,5), labels = c("<HS", "HS/GED", "Technical School","College", "Graduate/Professional"))
d$hs <- ifelse(d$meduc== 1|d$meduc==2,1,0)
d$mrace<-factor(d$mrace, levels= c(1,2,3), labels = c("Identifies as Black", "Identifies as White", "Does not identify as White or Black"))
d$other_race<-d$`Other race`
d$aces_pa <- d$tleq12
d$aces_fv <- d$tleq13
d$aces_sa <- d$tleq15
d$worsttleq <- d$tleq21s
d_tleq <- df %>% select(starts_with("tleq")) 
df$tleqtot <- rowSums(d_tleq)
d$smoke <- d$M1MB_TOBACCO
d$smoke <- ifelse(d$smoke=="0:No", 0, 1)
d$bmi <- as.numeric(d$M1MB_BMI)
d$length <- as.numeric(d$M3NSF_Birthlength)
d$weight <- as.numeric(d$M3NSF_Birthweight)
d$sex <- ifelse(d$csex == "Male", "M", 
                ifelse(d$csex == "Female", "F", NA))
d$gestage_days <- d$gestage*7
d$weightkg <- d$weight/1000

##calculate zscore for newborn weight and length using INTERGROWTH standards
d$zweight <- igb_wtkg2zscore(d$gestage_days, d$weightkg, sex = d$csex)
d$pctweight <- igb_wtkg2centile(d$gestage_days, d$weightkg, sex = d$csex)
d$zlength <- igb_lencm2zscore(d$gestage_days, d$length, sex = d$csex)
d$pctlength <- igb_lencm2centile(d$gestage_days, d$length, sex = d$csex)
hist(d$zweight)

## select variables for analysis and save dataset

df<-d %>%
  select( mage, mtotpreg, csex, hs, term, delivery, hypertension, diabetes, bmi, white, aa, asian, other_race, mhisp, mrace,gestage, tleq1, tleq2, tleq3, tleq4, tleq5, tleq6, tleq7, tleq8, tleq9, tleq10,tleq11, tleq14, tleq16, tleq17, tleq18, tleq19, tleq20, worsttleq, tleqtot, ICErace, ICEraceinc, ICEincome, adjusted_income, age_difference_CPC,residuals_CPC, Trophoblasts, Hofbauer,Endothelial,nRBC, Syncytiotrophoblast, CellType_PC1, CellType_PC2, CellType_PC3, CellType_PC4, CellType_PC5, meduc , smoke, zweight, zlength)

write_csv(df, "data_cooked/df.csv")

rm(d)




