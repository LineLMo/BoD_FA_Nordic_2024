###############################
######## M E T A ############## PREVALENCE
###############################

#First a model for prevalence, also used for severity and resolution prevalence
#Data sets for each food item and country is found in file "Data sets for meta"

#Then models for age of onset and age of resolve
#If studies provided more than one age, data sets are found in "Data sets for meta"

#Packages
library(metafor)
library(dplyr)

#####
# Dataset
studies <- data.frame(
  year = c("HertzenA","HertzenB","Pyrhonen09","IsolauriA","IsolauriB","IsolauriC","IsolauriD","Jarvenpaa","KajosaariA","KajosaariB","KajosaariC","KajosaariD"),
  n = c(365,357,3308,100,100,100,100,1563,261,202,200,203), 
  prevalence = c(0.00042,0.00392,0.01792,0.0045,0.0022,0.0005,0.00355,0.00208,0.0032,0.008,0.0032,0),  
  median = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,0.0021,0.0006,0.00002,0.0015,0.00145,NA,NA,NA,NA),
  upper_CI = c(NA,NA,NA,0.0082,0.0054,0.00275,0.007,0.0032,NA,NA,NA,NA)
)


#If only lower and upper CI is available: calculate prevalence. Otherwise, just use prevalence.
studies <- studies %>% 
  mutate(
    prevalence = ifelse(is.na(prevalence) & !is.na(lower_CI) & !is.na(upper_CI),
                        (lower_CI + upper_CI) / 2,
                        prevalence)
  )

# Compute missing mean prevalence (if only median is available)
studies <- studies %>%
  mutate(
    prevalence = ifelse(is.na(prevalence) & !is.na(median), median, prevalence),
    SE = case_when(
      !is.na(prevalence) ~ sqrt(prevalence * (1 - prevalence) / n),  # If prevalence is given
      !is.na(lower_CI) & !is.na(upper_CI) ~ (upper_CI - lower_CI) / 3.92,  # If CI is given
      TRUE ~ NA_real_
    )
  )


# Remove rows with missing values. If SD (SE) cannot be calculated, the row is removed.
studies <- studies %>% filter(!is.na(prevalence) & !is.na(SE))

# Convert prevalence to logit scale
##PLO for data between 0 and 1. Logit proportion?
##xi = cases in a study -> prevalence*participants.
##sei = standard deviation -> SE
studies <- escalc(measure = "PLO", xi = prevalence * n, ni = n, sei = SE, data = studies) 

# Run random-effects meta-analysis
##yi is the logit-transformed prevalence?
##vi is the variance of this new prevalence
res <- rma(yi, vi, data = studies, method = "REML",control=list(maxiter=100000,stepadj=0.88),verbose=TRUE)  #verbose = visualise #100000 number iterations #0.88 as close to 1 as possible

# Get the estimated pooled prevalence (logit scale)
pred <- predict(res, transf = transf.ilogit)

#print the result in a simple way by using cat. 2f gives to digits.
cat(sprintf("Estimated Prevalence: %.2f%% (95%% CI: %.2f%% - %.2f%%)", 
            pred$pred * 100, pred$ci.lb * 100, pred$ci.ub * 100))

# Summary and forest plot
summary(res)
forest(res)







########################################
########## A G E ############### Weight by total participants
#######################################
# O N S E T ###

########Age of onset P E A N U T #####
#Data
studies <- data.frame(
  study = c("Eller", "Sicherer", "Rio", "Vereda"),
  n = c(562,5149,260,115),          # Sample size
  mean_age = c(NA,2.46,NA,NA),        #in years
  median_age = c(1.5,1.167, 5.6,3)   #in years
)
n_total<-sum(studies$n)
studies$proportion<-studies$n/n_total
studies$weight_ave<-studies$median_age*studies$proportion
AoOPA<-sum(studies$weight_ave)
AoOPA<-round(AoOPA,digits = 3)   #1.422

########Age of onset T R E E N U T #####
#Data
#Rio
median(7,6.2,5.4,6.8,8.1) #7

studies <- data.frame(
  study = c("Sicherer","Rio"),
  n = c(5149,260),          # Sample size
  mean_age = c(NA,NA),        #in years
  median_age = c(3,7)   #in years
)
n_total<-sum(studies$n)
studies$proportion<-studies$n/n_total
studies$weight_ave<-studies$median_age*studies$proportion
AoOTN<-sum(studies$weight_ave)
AoOTN<-round(AoOTN,digits = 3)  #3.192


########Age of onset M I L K #####
#Data
studies <- data.frame(
  study = c("Host","Santos"),
  n = c(1749,139),          # Sample size
  mean_age = c(NA,NA),        #in years
  median_age = c(0.077,0.292)   #in years
)
n_total<-sum(studies$n)
studies$proportion<-studies$n/n_total
studies$weight_ave<-studies$median_age*studies$proportion
AoOCM<-sum(studies$weight_ave)
AoOCM<-round(AoOCM,digits = 3)   #0.093



########################################
########## A G E ############### Weight by total participants
#######################################
### R E S O L V E #####

#Data sets for studies including more than one age found in file "Data sets for meta"

### Age of resolve P E A N U T ###
#Data
studies <- data.frame(
  study = c("Peters","Hasan","Skolnick"),
  n = c(156,6,223),          # Sample size
  mean_age = c(NA,NA,NA),        #in years
  median_age = c(5.332,14.014,6)   #in years
)
n_total<-sum(studies$n)
studies$proportion<-studies$n/n_total
studies$weight_ave<-studies$median_age*studies$proportion
AoRPA<-sum(studies$weight_ave)
AoRPA<-round(AoRPA,digits = 3)   #5.854 years


### Age of resolve M I L K ###
#Data
studies <- data.frame(
  study = c("Host","Skripak","Wood","Kaczmarski","Kubota","Schoemaker","Saarinen"),
  n = c(39,807,244,291,80,55,118),          # Sample size
  mean_age = c(NA,NA,NA,NA,NA,NA,NA),        #in years
  median_age = c(2.379,9.681,5.25,5,10.539,1,3.348)   #in years
)
n_total<-sum(studies$n)
studies$proportion<-studies$n/n_total
studies$weight_ave<-studies$median_age*studies$proportion
AoRCM<-sum(studies$weight_ave)
AoRCM<-round(AoRCM,digits = 3)   # 7.304 years
#MILK, Removed Skripack that only has IgE which causes later resolve 
#Data
studies <- data.frame(
  study = c("Host","Wood","Kaczmarski","Kubota","Schoemaker","Saarinen"),
  n = c(39,244,291,80,55,118),          # Sample size
  mean_age = c(NA,NA,NA,NA,NA,NA),        #in years
  median_age = c(2.379,5.25,5,10.539,1,3.348)   #in years
)
n_total<-sum(studies$n)
studies$proportion<-studies$n/n_total
studies$weight_ave<-studies$median_age*studies$proportion
AoRCM<-sum(studies$weight_ave)
AoRCM<-round(AoRCM,digits = 3)   # 4.984 years




