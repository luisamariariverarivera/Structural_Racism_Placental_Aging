source("Processing.R")


library(brms)
library(apaTables)
library(tidyverse)
library(ggplot2)
library(mice)
library(dplyr)
library(qgraph)
library(Matrix)
library(VIM)
library(psych)
library(performance)
library(corrplot)
library(htmlTable)
library(circlize)


##make a logit function
logit <- function(x) {
  log(x) - log(1 - x)
}

## calculate trauma summary scores

d_tleq <- df %>% select(starts_with("tleq")) 
df$tleqtot <- rowSums(d_tleq)


## how much missing data is there?
colSums(is.na(df))/677*100


## make a missing data plot


dmissing<-df %>%
  select( mage, mtotpreg, csex, hs, term, bmi, mrace, gestage,  tleqtot, ICErace, ICEraceinc, age_difference_CPC)

mice_plot <- aggr(dmissing, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(dmissing), cex.axis=.7,
                    gap=2, ylab=c("Missing data","Pattern"))



##test for mcar

dmcar<-df %>%
  select( mage, mtotpreg, white, aa, gestage,  tleqtot, ICErace, ICEraceinc, age_difference_CPC )

mcar(dmcar)


##data not MCAR, don't use imputation

##Describe the whole sample and explain your decision to subset on Black and White moms

table(df$mrace)/677*100
table(df$mhisp)/677*100
table(df$mhisp,df$mrace)


## Visually inspect cell heterogeneity PCs and predicted proportions

plot(df$age_difference_CPC, df$CellType_PC1, xlab= "Placental Age Acceleration", ylab= "Cell Type PC1", col=df$mrace, legend=TRUE)
plot(df$age_difference_CPC, df$CellType_PC2,xlab= "Placental Age Acceleration", ylab= "Cell Type PC2", col=df$mrace, legend=TRUE)
plot(df$age_difference_CPC, df$CellType_PC3, xlab= "Placental Age Acceleration", ylab= "Cell Type PC3", col=df$mrace, legend=TRUE)
plot(df$age_difference_CPC, df$CellType_PC4, xlab= "Placental Age Acceleration", ylab= "Cell Type PC4", col=df$mrace, legend=TRUE)
plot(df$age_difference_CPC, df$CellType_PC5, xlab= "Placental Age Acceleration", ylab= "Cell Type PC5", col=df$mrace, legend=TRUE)

## examine distribution of age accelleration outcome (in weeks)
hist(df$age_difference_CPC)

## standardize continuous variables, transform ICE and stress measures


df$ICErace_s <- (df$ICErace + 1)/2
df$ICErace_s <- ifelse(df$ICErace_s == 0, min(df$ICErace_s[df$ICErace_s > 0]), df$ICErace_s)
df$ICEraceinc_s <- (df$ICEraceinc + 1)/2
df$ICEraceinc_s <- ifelse(df$ICEraceinc_s == 0, min(df$ICEraceinc_s[df$ICErace_s > 0]), df$ICEraceinc_s)
df$mage_s <- scale(df$mage)[,1]
df$mtotpreg_s <- scale(df$mtotpreg)[,1]
df$adjusted_income_s <- scale(df$adjusted_income)[,1]
df$bmi <- as.numeric(df$bmi)
df$bmi_s <- scale(df$bmi)[,1]
df$tleqtot_s <- scale(df$tleqtot)
df$pc1_s <- scale(df$CellType_PC1)[,1]
df$pc2_s <- scale(df$CellType_PC2)[,1]
df$pc3_s <- scale(df$CellType_PC3)[,1]
df$gestage <- scale(df$gestage)



 ##make 2 complete case datasets stratified by race, one with centered (for analysis) and one with untransformed variables (for reporting)

d_black <- df %>% filter(mrace=="Identifies as Black") %>% select(c(age_difference_CPC, ICErace_s,ICEraceinc_s, mage_s, mtotpreg_s,tleqtot_s, adjusted_income_s, bmi_s, hs, csex, pc1_s, pc2_s, pc3_s, smoke, gestage, diabetes, hypertension, zweight, zlength, mrace))
d_black <- d_black[complete.cases(d_black),]

d_white <- df %>% filter(mrace=="Identifies as White") %>% select(c(age_difference_CPC, tleqtot_s, ICErace_s,ICEraceinc_s, mage_s, mtotpreg_s, adjusted_income_s,hs, csex, smoke, bmi_s, pc1_s, pc2_s, pc3_s, gestage, diabetes, hypertension, zweight, zlength, mrace))
d_white <- d_white[complete.cases(d_white),]




d_black2 <- df %>% filter(mrace=="Identifies as Black") %>% select(c(age_difference_CPC, tleqtot, ICErace,ICEraceinc, mage, mtotpreg, adjusted_income, bmi, csex, mrace, meduc, smoke,gestage, pc1_s, pc2_s, pc3_s, hypertension, diabetes, weight, length))
d_black2 <- d_black2[complete.cases(d_black2),]

d_white2 <- df %>% filter(mrace=="Identifies as White") %>% select(c(age_difference_CPC, tleqtot,ICErace,ICEraceinc, mage, mtotpreg, adjusted_income, bmi, csex, mrace, meduc, smoke, gestage, pc1_s, pc2_s, pc3_s, hypertension, diabetes, weight, length))
d_white2 <- d_white2[complete.cases(d_white2),]



d_comb <- bind_rows(d_black, d_white)
d_comb2 <- bind_rows(d_black2, d_white2)


##Report descriptive statistics

library(furniture)
furniture::table1
t1 <- table1(d_comb2, mage, mtotpreg, meduc, adjusted_income, bmi, csex,gestage, tleqtot, smoke, ICErace, ICEraceinc, hypertension, diabetes, length, weight,
       age_difference_CPC, splitby = ~mrace, output = "pandoc", test= TRUE, format_number = TRUE, export = "Table_1")

##make a density plot for ICE variables and age acelleration

p1 <- ggplot(data=d_comb2, aes(x=ICErace, group= mrace, fill=mrace)) +
  geom_density(adjust=1.5, alpha=.5, color=NA) +
  scale_x_continuous(limits=c(-1,1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size=16) +
theme(legend.title=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      legend.position = "top")  + 
  ylab("")
    
p1

p2 <- ggplot(data=d_comb2, aes(x=ICEraceinc, group= mrace, fill=mrace)) +
  geom_density(adjust=1.5, alpha=.5, color=NA) +
  scale_x_continuous(limits=c(-1,1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal(base_size=16) +
  theme(legend.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "top")  + 
  ylab("")

p2

library(patchwork)

p1 + p2 + plot_layout(guides="collect") & theme(legend.position = 'top')


p3 <- ggplot(data=d_comb2, aes(x=age_difference_CPC, group= mrace, fill=mrace)) +
  geom_density(adjust=1.5, alpha=.5, color=NA) +
  theme_minimal(base_size=16) +
  theme(legend.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "top")  + 
  ylab("") + 
  xlab("Placental Age Acceleration")

p3


p4 <- ggplot(data=d_comb2, aes(x=pc1_s, group= mrace, fill=mrace)) +
  geom_density(adjust=1.5, alpha=.5, color=NA) +
  theme_minimal(base_size=16) +
  theme(legend.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "top")  + 
  ylab("") + 
  xlab(" Cell type PC1")

p4

p5 <- ggplot(data=d_comb2, aes(x=pc2_s, group= mrace, fill=mrace)) +
  geom_density(adjust=1.5, alpha=.5, color=NA) +
  theme_minimal(base_size=16) +
  theme(legend.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "top")  + 
  ylab("") + 
  xlab(" Cell type PC2")

p5




##correlation table 
d_comb2$aa <- ifelse(d_comb2$mrace == "Identifies as Black", 1, 0)

d_corr<-d_comb2 %>%select(mage, bmi, mtotpreg, gestage, adjusted_income, tleqtot, ICErace, ICEraceinc, age_difference_CPC, pc1_s)

d_corr_w<-d_white2 %>%select(mage, bmi, mtotpreg, gestage, adjusted_income, tleqtot,, ICErace, ICEraceinc, age_difference_CPC, pc1_s)

d_corr_b<-d_black2 %>%select(mage, bmi, mtotpreg, gestage, adjusted_income, tleqtot, ICErace, ICEraceinc, age_difference_CPC, pc1_s)

apa.cor.table(d_corr,filename="correlation_tot.doc")
apa.cor.table(d_corr_w, filename="correlation_w.doc")
apa.cor.table(d_corr_b,filename="correlation_b.doc")

## lets see these as cord plots because I would like a better look

library(qgraph)


cor_mat <- cor(d_corr, use="complete.obs")


labs <- c("age", "BMI", "tot preg","gestage", "income", "tleq", "ICERace", "ICERaceinc", " Placenta Aging", "Cell PC1") # short names here
names <- c("Maternal age", "BMI", "Total pregnancies", "Gestational Age", "Income", "Stressful life events", "Segregation", "Racial income inequity", "Placental epigenetic age acceleration", "Cell Type PC1") # long names here

qgraph(cor_mat, minimum="sig", alpha=0.05, graph="cor", sampleSize=nrow(d_corr), edge.labels=T, labels=labs, nodeNames=names)


##poster script

par(bg=NA)
png("qgraph_cormat_combined.png", width = 8, height=6, units="in", res=600)

qgraph(cor_mat, minimum="sig", alpha=0.05, graph="cor", sampleSize=nrow(d_corr_b), edge.labels=T, labels=labs, nodeNames=names, bg="transparent", edge.label.bg="#2F5597")

dev.off()


##Black participants


cor_mat <- cor(d_corr_b, use="complete.obs")


labs <- c("age", "BMI", "tot preg","gestage", "income", "tleq", "ICERace", "ICERaceinc", " Placenta Aging", "Cell PC1") # short names here
names <- c("Maternal age", "BMI", "Total pregnancies", "Gestational Age", "Income", "Stressful life events", "Segregation", "Racial income inequity", "Placental epigenetic age acceleration", "Cell Type PC1") # long names here

qgraph(cor_mat, minimum="sig", alpha=0.05, graph="cor", sampleSize=nrow(d_corr_b), edge.labels=T, labels=labs, nodeNames=names)



##poster script

par(bg=NA)
png("qgraph_cormat_black.png", width = 8, height=6, units="in", res=600)

qgraph(cor_mat, minimum="sig", alpha=0.05, graph="cor", sampleSize=nrow(d_corr_b), edge.labels=T, labels=labs, nodeNames=names, bg="transparent", edge.label.bg="#2F5597")

dev.off()

cor_mat <- cor(d_corr_w, use="complete.obs")

labs <- c("age", "gestage", "BMI", "tot preg", "income",  "tleq", "ICERace", "ICERaceinc", "Placenta Aging") # short names here
names <- c("Maternal age", "Gestational age", "BMI", "Total pregnancies", "Income", "Stressful life events", "Segregation", "Racial income inequity", "Placental epigenetic age acceleration") # long names here

qgraph(cor_mat, minimum="sig", alpha=0.05, graph="cor", sampleSize=nrow(d_corr_w), edge.labels=T, labels=labs, nodeNames=names)

##poster script
par(bg=NA)
png("qgraph_cormat_white.png", width = 8, height=6, units="in", res=600)

qgraph(cor_mat, minimum="sig", alpha=0.05, graph="cor", sampleSize=nrow(d_corr_w), edge.labels=T, labels=labs, nodeNames=names, bg="transparent", edge.label.bg="#2F5597")


dev.off()


## do race-stratified linear regression and check model fit

##segregation combined
segregation_combined_covar <-  lm(age_difference_CPC~ mage_s + csex +mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICErace_s + adjusted_income_s, data= d_comb)

summary(segregation_combined_covar)

check_model(segregation_combined_covar)

segregation_combined_nocovar <-  lm(age_difference_CPC~ mage_s +  tleqtot_s + ICErace_s , data= d_comb)

summary(segregation_combined_nocovar)

check_model(segregation_combined_nocovar)

### QQ plots look bad, try robust methods

##segregation combined robust
segregation_combined_covar <-  lmrob(age_difference_CPC~ mage_s + csex +mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICErace_s + adjusted_income_s, data= d_comb)

summary(segregation_combined_covar)

check_model(segregation_combined_covar)

segregation_combined_nocovar <-  lmrob(age_difference_CPC~ mage_s +  tleqtot_s + ICErace_s , data= d_comb)

summary(segregation_combined_nocovar)

check_model(segregation_combined_nocovar)


##segregation Black
segregation_black_covar <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICErace_s + adjusted_income_s, data= d_black)

summary(segregation_black_covar)

segregation_black_nocovar <- lm(age_difference_CPC~ mage_s + csex + pc1_s + tleqtot_s + ICErace_s, data= d_black)

summary(segregation_black_nocovar)

##segregation white

segregation_white_covar <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICErace_s + adjusted_income_s, data= d_white)

summary(segregation_white_covar)

segregation_white_nocovar <- lm(age_difference_CPC~ mage_s + csex + pc1_s + tleqtot_s + ICErace_s, data= d_white)

summary(segregation_white_nocovar)


####RaceInc

##full sample
raceinc_combined_nocovar <- lm(age_difference_CPC~ mage_s + csex  + pc1_s  + tleqtot_s + ICEraceinc_s, data= d_comb)

summary(raceinc_combined_nocovar)

raceinc_combined_covar <- lm(age_difference_CPC~ mage_s + csex +mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICEraceinc_s + adjusted_income_s, data= d_comb)

summary(raceinc_combined_covar)

raceinc_combined_covar <- lm(age_difference_CPC~ mage_s + csex +mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICEraceinc_s + adjusted_income_s, data= d_comb)

summary(raceinc_combined_covar)


##RaceInc Black
segregation_black_covar <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICEraceinc_s + adjusted_income_s, data= d_black)

summary(segregation_black_covar)

segregation_black_nocovar <- lm(age_difference_CPC~ mage_s + csex + pc1_s + tleqtot_s + ICEraceinc_s, data= d_black)

summary(segregation_black_nocovar)

##Raceinc white

segregation_white_covar <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICEraceinc_s + adjusted_income_s, data= d_white)

summary(segregation_white_covar)

segregation_white_nocovar <- lm(age_difference_CPC~ mage_s + csex + pc1_s + tleqtot_s + ICEraceinc_s, data= d_white)

summary(segregation_white_nocovar)


library(performance)


check_model(segregation_black_nocovar)


# ##ICErace
# 
# ##Black mothers
# m1 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s, data= d_black )
# summary(m1)
# 
# m2 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke+ pc1_s + pc2_s + pc3_s + tleqtot_s, data= d_black)
# 
# summary(m2)
# 
# m3 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICErace_s, data= d_black)
# 
# summary(m3)
# 
# apa.reg.table(m1, filename= "m1_black.doc")
# apa.reg.table(m2, filename= "m2_black.doc")
# apa.reg.table(m3, filename= "m3_black.doc")
# 
# aic_black <- AIC(m1,m2,m3)
# aic_black
# 
# ## white mothers
# 
# m4 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s, data= d_white )
# summary(m4)
# 
# m5 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke+ pc1_s + pc2_s + pc3_s + tleqtot_s, data= d_white)
# summary(m5)
# 
# m6 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + pc1_s + pc2_s + pc3_s + tleqtot_s + ICErace_s + smoke, data= d_white)
# summary(m6)
# 
# AIC(m4,m5,m6)
# 
# apa.reg.table(m4, filename= "m4_white.doc")
# apa.reg.table(m5, filename= "m5_white.doc")
# apa.reg.table(m6, filename= "m6_white.doc")
# 
# 
# 
# ##ICEraceinc
# 
# ##Black mothers
# m7 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s, data= d_black )
# summary(m7)
# 
# m8 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + pc1_s + pc2_s + pc3_s + smoke + tleqtot_s, data= d_black)
# summary(m8)
# 
# m9 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + pc1_s + pc2_s + pc3_s  + smoke + ICEraceinc_s, data= d_black)
# summary(m9)
# 
# apa.reg.table(m7, filename= "m7_black.doc")
# apa.reg.table(m8, filename= "m8_black.doc")
# apa.reg.table(m9, filename= "m9_black.doc")
# 
# aic_black <- AIC(m7,m8,m9)
# aic_black
# 
# ## white mothers
# 
# m10 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke+ pc1_s + pc2_s + pc3_s, data= d_white )
# summary(m10)
# 
# m11 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + + tleqtot_s, data= d_white)
# summary(m11)
# 
# m12 <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke+ pc1_s + pc2_s + pc3_s + tleqtot_s + ICEraceinc_s, data= d_white)
# summary(m12)
# 
# AIC(m10,m11,m12)
# 
# apa.reg.table(m10, filename= "m10_white.doc")
# apa.reg.table(m11, filename= "m11_white.doc")
# apa.reg.table(m12, filename= "m12_white.doc")
# 



model_brms <- brm(
  age_difference_CPC ~ ICErace_s + tleqtot_s + mage_s + pc1_s,
  data = d_comb,
  family = student()
)

# Summarize the model
summary(model_brms)

residuals_brms <- residuals(model_brms, summary = FALSE)
# If residuals is a matrix, extract the first column
if (is.matrix(residuals_brms)) {
  residuals_brms <- residuals_brms[, 1]
  
  qqnorm(residuals_brms)
  qqline(residuals_brms, col = "red")
}

look <- ggplot(data = data.frame(residuals = residuals_brms), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  labs(title = "QQ Plot of Residuals from Bayesian Model",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

print(look)


Let's try a different approach to extract residuals from a brms model and create a QQ plot. The brms package stores residuals in the fitted model object, and we can extract them using the residuals() function.

Here's a step-by-step guide:
  
  Fit your Bayesian model using brms.
Extract the residuals.
Create a QQ plot of the residuals.
Example with Detailed Steps
Fit the Bayesian Model:
  r
Copy code
# Load necessary libraries
library(brms)
library(ggplot2)

# Example data (replace with your actual data)
# your_data <- data.frame(
#   response_variable = ...,
#   predictor_variable = ...
# )

# Fit a Bayesian robust linear model using a t-distribution
model_brms <- brm(
  response_variable ~ predictor_variable,
  data = your_data,
  family = student()
)

# Summarize the model
summary(model_brms)


# Posterior predictive check with density overlay

pp_check(model_brms, type = "dens_overlay")

# Posterior predictive check with histogram
pp_check(model_brms, type = "hist")

# Posterior predictive check with boxplot
pp_check(model_brms, type = "boxplot")
