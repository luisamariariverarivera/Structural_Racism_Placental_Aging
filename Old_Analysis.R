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

d_black <- df %>% filter(mrace=="Identifies as Black") %>% select(c(age_difference_CPC, ICErace_s,ICEraceinc_s, mage_s, mtotpreg_s,tleqtot_s, adjusted_income_s, bmi_s, hs, csex, pc1_s, pc2_s, pc3_s, smoke, gestage, diabetes, hypertension, zweight, zlength))
d_black <- d_black[complete.cases(d_black),]

d_white <- df %>% filter(mrace=="Identifies as White") %>% select(c(age_difference_CPC, tleqtot_s, ICErace_s,ICEraceinc_s, mage_s, mtotpreg_s, adjusted_income_s,hs, csex, smoke, bmi_s, pc1_s, pc2_s, pc3_s, gestage, diabetes, hypertension, zweight, zlength))
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

segregation_combined <-  lm(age_difference_CPC~ mage_s + csex +gestage +mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICErace_s + adjusted_income_s, data= d_comb)


summary(segregation_combined)

segregation_black <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICErace_s + adjusted_income_s, data= d_black)

summary(segregation_black)

segregation_white <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICErace_s + adjusted_income_s, data= d_white)

summary(segregation_white)

raceinc_combined <- lm(age_difference_CPC~ mage_s + csex + mtotpreg_s + bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s + ICEraceinc_s + adjusted_income_s, data= d_comb)

summary(raceinc_combined)



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

## Do model diagnostics 

check_model(m1) ## repeat for each model 



##Distributional Model of ICErace

##Full sample

formula <- bf(age_difference_CPC ~ mage_s + csex  + mtotpreg_s +
                bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s +
                ICErace_s + adjusted_income_s,
              sigma ~ mage_s + csex + mtotpreg_s +
                bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s +
                ICErace_s + adjusted_income_s)

family <- gaussian()

prior <- prior(normal(0, 0.5), class = "b") + prior(normal(0, 0.5), class = "b", dpar = "sigma")

fit <- brm(formula = formula,
           data = d_comb,
           family = family,
           prior = prior,
           chains = 4,     # Number of Markov chains
           iter = 2000,    # Number of iterations per chain
           warmup = 1000,  # Number of warmup iterations
           cores = 4,      # Number of cores to use
           seed = 123)     # Random seed for reproducibility


fit2 <- brm(formula = age_difference_CPC ~ mage_s + csex  + mtotpreg_s +
             bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s +
             ICErace_s + adjusted_income_s,
           data = d_comb,
           family = family,
           chains = 4,     # Number of Markov chains
           iter = 2000,    # Number of iterations per chain
           warmup = 1000,  # Number of warmup iterations
           cores = 4,      # Number of cores to use
           seed = 123)     # Random seed for reproducibility

summary(fit)

plot(fit)



pp_check(fit)  # Posterior predictive checks

library(bayesplot)
library(ggplot2)
library(brms)
library(reshape2)

## Plot the fixed effects 
fixed_effects <- fixef(fit)
fixed_effects_df <- as.data.frame(fixed_effects)
fixed_effects_df$Parameter <- rownames(fixed_effects_df)
sigma_parameters <- grepl("sigma_", fixed_effects_df$Parameter)
fixed_effects_mean <- fixed_effects_df[!sigma_parameters, ]
fixed_effects_sigma <- fixed_effects_df[sigma_parameters, ]
names(fixed_effects_mean) <- c("Estimate", "Est.Error", "Q2.5", "Q97.5", "Parameter")
names(fixed_effects_sigma) <- c("Estimate", "Est.Error", "Q2.5", "Q97.5", "Parameter")

segregation_full_mean <- ggplot(fixed_effects_mean, aes(x = Estimate, y = reorder(Parameter, Estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2) +
  theme_minimal() +
  labs(
    title = "Posterior Distributions of the Population-Level Effects (Mean)",
    x = "Estimate",
    y = "Parameter"
  )

segregation_full_sigma <- ggplot(fixed_effects_sigma, aes(x = Estimate, y = reorder(Parameter, Estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2) +
  theme_minimal() +
  labs(
    title = "Posterior Distributions of the Population-Level Effects (Sigma)",
    x = "Estimate",
    y = "Parameter"
  )
library(patchwork)
segregation_full <- segregation_full_mean+segregation_full_sigma
segregation_full
# Extract posterior samples
posterior_samples <- as.matrix(fit)


##Make some custom labels

custom_labels <- c(
  "b_sigma_mage_s" = "Maternal Age",
  "b_sigma_csexMale" = "Child Sex (Male)",
  "b_sigma_mtotpreg_s" = "Total Pregnancies",
  "b_sigma_bmi_s" = "BMI",
  "b_sigma_smoke" = "Smoking Status",
  "b_sigma_pc1_s" = "Cell Type PC1",
  "b_sigma_pc2_s" = "Cell Type PC2",
  "b_sigma_pc3_s" = "Cell Type PC3",
  "b_sigma_tleqtot_s" = "Traumatic Life Events Total",
  "b_sigma_ICErace_s" = "Residential Segregation (ICERace)",
  "b_sigma_adjusted_income_s" = "Maternal Income"
)

xlim_custom <- c(-0.6, 0.6)
# Create density plots of the parameter estimates
library(bayesplot)
library(ggplot2)

# Extract posterior samples
posterior_samples <- as.matrix(fit)

# Create density plots of the parameter estimates s


plot_mu <- mcmc_areas(
  posterior_samples,
  pars = c("b_mage_s", "b_csexMale", "b_mtotpreg_s", "b_bmi_s", "b_smoke", "b_adjusted_income_s",
           "b_pc1_s", "b_pc2_s", "b_pc3_s", "b_tleqtot_s", "b_ICErace_s"),
  prob = 0.95  # 95% credible intervals
) + 
  ggtitle("1") +
  theme_minimal() +
  scale_y_discrete(labels = custom_labels)+
  xlim(xlim_custom)

plot_mu

plot_sigma <- mcmc_areas(
  posterior_samples,
  pars = c("b_sigma_mage_s", "b_sigma_csexMale", "b_sigma_mtotpreg_s", "b_sigma_bmi_s", "b_sigma_smoke", "b_sigma_adjusted_income_s",
           "b_sigma_pc1_s", "b_sigma_pc2_s", "b_sigma_pc3_s", "b_sigma_tleqtot_s", "b_sigma_ICErace_s"),
  prob = 0.95  # 95% credible intervals
) + 
  ggtitle("2") +
  theme_minimal() +
  scale_y_discrete(labels = custom_labels)+
  xlim(xlim_custom)



plot_sigma

plot_mu + plot_sigma


####But can you do it as an overlapping density plot??


library(bayesplot)
library(ggplot2)
library(dplyr)
library(tidyr)

# Extract posterior samples
posterior_samples <- as.matrix(fit)

# Extract mean parameters (excluding b_sigma_ parameters)
mean_params <- posterior_samples[, grepl("^b_", colnames(posterior_samples)) & !grepl("^b_sigma_", colnames(posterior_samples))]

# Extract sigma parameters
sigma_params <- posterior_samples[, grepl("^b_sigma_", colnames(posterior_samples))]

# Convert to data frames
mean_df <- as.data.frame(mean_params)
sigma_df <- as.data.frame(sigma_params)

# Add a column to identify the type of parameter
mean_df$type <- "Mean"
sigma_df$type <- "Sigma"

mean_long <- mean_df %>%
  pivot_longer(everything(), names_to = "param", values_to = "value") %>%
  mutate(type = "Mean")

sigma_long <- sigma_df %>%
  pivot_longer(everything(), names_to = "param", values_to = "value") %>%
  mutate(type = "Sigma")

# Combine data frames
combined_df <- bind_rows(mean_long, sigma_long)

# Create density plots with overlapping densities
plot_combined <- ggplot(combined_df, aes(x = value, fill = type, color = type)) +
  geom_density(alpha = 0.5) +  # Overlapping density with transparency
  facet_wrap(~param, scales = "free", ncol = 2) +  # Facet by parameter
  labs(title = "Posterior Distributions of Mean and Sigma Coefficients",
       x = "Value",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top")

# Display the plot
plot_combined


# Display the plot
plot_combined

##Do it all again for ICEraceinc

formula <- bf(age_difference_CPC ~ mage_s + csex  + mtotpreg_s +
                bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s +
                ICErace_s + adjusted_income_s,
              sigma ~ mage_s + csex + mtotpreg_s +
                bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s +
                ICErace_s + adjusted_income_s)

family <- gaussian()

fit <- brm(formula = formula,
           data = d_comb,
           family = family,
           chains = 4,     # Number of Markov chains
           iter = 2000,    # Number of iterations per chain
           warmup = 1000,  # Number of warmup iterations
           cores = 4,      # Number of cores to use
           seed = 123)     # Random seed for reproducibility

summary(fit)

plot(fit)



pp_check(fit)  # Posterior predictive checks

library(bayesplot)
library(ggplot2)
library(brms)
library(reshape2)

## Plot the fixed effects 
fixed_effects <- fixef(fit)
fixed_effects_df <- as.data.frame(fixed_effects)
fixed_effects_df$Parameter <- rownames(fixed_effects_df)
sigma_parameters <- grepl("sigma_", fixed_effects_df$Parameter)
fixed_effects_mean <- fixed_effects_df[!sigma_parameters, ]
fixed_effects_sigma <- fixed_effects_df[sigma_parameters, ]
names(fixed_effects_mean) <- c("Estimate", "Est.Error", "Q2.5", "Q97.5", "Parameter")
names(fixed_effects_sigma) <- c("Estimate", "Est.Error", "Q2.5", "Q97.5", "Parameter")

segregation_full_mean <- ggplot(fixed_effects_mean, aes(x = Estimate, y = reorder(Parameter, Estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2) +
  theme_minimal() +
  labs(
    title = "Posterior Distributions of the Population-Level Effects (Mean)",
    x = "Estimate",
    y = "Parameter"
  )

segregation_full_sigma <- ggplot(fixed_effects_sigma, aes(x = Estimate, y = reorder(Parameter, Estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height = 0.2) +
  theme_minimal() +
  labs(
    title = "Posterior Distributions of the Population-Level Effects (Sigma)",
    x = "Estimate",
    y = "Parameter"
  )
library(patchwork)
segregation_full <- segregation_full_mean+segregation_full_sigma
segregation_full
# Extract posterior samples
posterior_samples <- as.matrix(fit)

# Create density plots of the parameter estimates
mcmc_areas(
  posterior_samples,
  pars = c("b_mage_s", "b_csexMale", "b_mtotpreg_s", "b_bmi_s", "b_smoke", 
           "b_pc1_s", "b_pc2_s", "b_pc3_s", "b_tleqtot_s", "b_ICErace_s", 
           "b_adjusted_income_s"),
  prob = 0.95  # 95% credible intervals
) + 
  ggtitle("Posterior Distributions of the Coefficients") +
  theme_minimal()


mcmc_areas(
  posterior_samples,
  pars = c("b_sigma_mage_s", "b_sigma_csexMale", "b_sigma_mtotpreg_s", "b_sigma_bmi_s", "b_sigma_smoke", 
           "b_sigma_pc1_s", "b_sigma_pc2_s", "b_sigma_pc3_s", "b_sigma_tleqtot_s", "b_sigma_ICErace_s", 
           "b_sigma_adjusted_income_s"),
  prob = 0.95  # 95% credible intervals
) + 
  ggtitle("Posterior Distributions of the Mean Coefficients") +
  theme_minimal()






## Black mothers

formula <- bf(age_difference_CPC ~ mage_s + csex  + mtotpreg_s +
                bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s +
                ICErace_s + adjusted_income_s,
              sigma ~ mage_s + csex + mtotpreg_s +
                bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s +
                ICErace_s + adjusted_income_s)

family <- gaussian()

fit <- brm(formula = formula,
           data = d_black,
           family = family,
           chains = 4,     # Number of Markov chains
           iter = 2000,    # Number of iterations per chain
           warmup = 1000,  # Number of warmup iterations
           cores = 4,      # Number of cores to use
           seed = 123)     # Random seed for reproducibility

summary(fit)

plot(fit)

pp_check(fit)  # Posterior predictive checks


##white moms


formula <- bf(age_difference_CPC ~ mage_s + csex  + mtotpreg_s +
                bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s +
                ICErace_s + adjusted_income_s,
              sigma ~ mage_s + csex + mtotpreg_s +
                bmi_s + smoke + pc1_s + pc2_s + pc3_s + tleqtot_s +
                ICErace_s + adjusted_income_s)

family <- gaussian()

fit <- brm(formula = formula,
           data = d_white,
           family = family,
           chains = 4,     # Number of Markov chains
           iter = 2000,    # Number of iterations per chain
           warmup = 1000,  # Number of warmup iterations
           cores = 4,      # Number of cores to use
           seed = 123)     # Random seed for reproducibility

summary(fit)

plot(fit)

pp_check(fit)  # Posterior predictive checks

