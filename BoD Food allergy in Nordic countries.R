#############################################################
##Burden of Disease of Food allergy in The Nordic Countries##
#############################################################

##This script contains data and distributions used to calculate the BoD 
# for food allergies in the Nordic countries.
##First, data and parameters are defined generally, for food, and for country.
##Then, mortality and YLL is calculated
##Further down, the model of YLD and DALY is calculated for each country.
##The order is Denmark, Sweden, Norway, Finland, and Iceland.
##The last part calculates the total DALY per country, and the total DALY per food. 

#Packages
library(mc2d)

########################################
############ D A T A ###################

###### GENERAL DATA
nunc <- 1e4 #Number of iterations for uncertainty   

# FA Disability weight #Assumptions used to combine different symptoms #GBD
#Build non-severe DW
set.seed(123)
non_low <- rpert(nunc, min = 0.005, mode = 0.011, max = 0.021) #Mild to moderate GERD
set.seed(123)
non_up <- rpert(nunc, min = 0.031, mode = 0.049, max = 0.072) #Uncomplicated diabetes mellites type 1
#Mean of non-severe DW
DWnon <- (non_low+non_up)/2 #mean = 0.031 (95CI: 0.023 - 0.039)
c(mean(DWnon), quantile(DWnon, probs = c(0.025, 0.975)))
#Build severe DW
set.seed(123)
sev_low <- rpert(nunc, min = 0.018, mode = 0.03, max = 0.046) #Mild anxiety disorders 
set.seed(123)
sev_up <- rpert(nunc, min = 0.125, mode = 0.188, max = 0.267) #Moderate atopic dermatitis arthritis 
#Mean of non-severe DW
DWsev <- (sev_low+sev_up)/2 #mean = 0.111 (95CI: 0.086 - 0.137)
c(mean(DWsev), quantile(DWsev, probs = c(0.025, 0.975)))

# Tree nut
#Tree nut severity
set.seed(123)
TNSevere <- rpert(nunc, min = 0.3278, mode = 0.5011, max = 0.6742) #mean of studies of anaphylaxis
TNnonSevere<-1-TNSevere #no anaphylaxis
#Tree nut Age of onset and Age of resolve
AoO_TN<-3.19 #years  #weighted by number of participants #See R file "Meta studies" 
AoR_TN<-10 #based on one study
set.seed(123)
TNResolve <- rpert(nunc, min = 0.04, mode = 0.089, max = 0.16) #incidence of resolve

# Peanut
#Peanut severity
set.seed(123)
PASevere <- rpert(nunc,min = 0.2692, mode = 0.2969, max = 0.3261) #mean of meta-analysis studies of anaphylaxis
PAnonSevere<-1-PASevere #no anaphylaxis
#Peanut Age of onset and Age of resolve
AoO_PA<-1.42 #years  #weighted by number of participants #See R file "Meta studies" 
AoR_PA<-5.854 #years  #weighted by number of particiapnts #See R file "Meta studies"
set.seed(123)
PAResolve <- rpert(nunc, min = 0.1836, mode = 0.2768, max = 0.3943) #meta-analysis #incidence of resolve

# Milk
#Milk severity
set.seed(123)
CMSevere <- rpert(nunc, min = 0.0504, mode = 0.1798, max = 0.4755) #mean of meta-analysis studies of anaphylaxis
CMnonSevere<-1-CMSevere #no anaphylaxis
#Milk Age of onset and Age of resolve
AoO_CM<-0.093 #years  #weighted by number of participants #See R file "Meta studies" 
AoR_CM<-4.984 #years  #weighted by number of participants #See R file "Meta studies"
set.seed(123)
CMResolve <- rpert(nunc, min = 0.6134, mode = 0.7518, max = 0.8526) #incidence of resolve


###### COUNTRY SPECIFIC DATA
#DENMARK
DKpopulation<-5995464 #Population of DK 2025 feburary. #Danmarks Statistik
DKpop100<-DKpopulation/100000 #per 100 000
DKlifeexpect <- mean(c(79.85,83.96)) #life expect of the Danish population 2023 at birth. Mean of male and female
print(DKlifeexpect) #for Age 0 = 81.905 years old
#Prevalence
set.seed(123)
TN_DK_prev <- rpert(nunc, min = 0.001, mode = 0.0076,max = 0.0543)
set.seed(123)
TNW_DK_prev <- rpert(nunc, min = 0.0104, mode = 0.0255,max = 0.0610)
set.seed(123)
PA_DK_prev <- rpert(nunc, min = 0.0034, mode = 0.0051,max = 0.0077)
set.seed(123)
CM_DK_prev <- rpert(nunc, min = 0.0019, mode = 0.0035,max = 0.0063)

#SWEDEN
SEpopulation<-10588020 #Population of SE 2025 january #scb.se
SEpop100<-SEpopulation/100000 #per 100 000
SElifeexpect <- mean(c(81.58,84.9)) #life expect of the Swedish population 2023 #mean of male and female
print(SElifeexpect) #for Age 0 = 83.24 years old
#Prevalence
set.seed(123)
TN_SE_prev <- rpert(nunc, min = 0.0023, mode = 0.0040,max = 0.0069)
set.seed(123)
TNW_SE_prev <- rpert(nunc, min = 0.0018, mode = 0.0045,max = 0.011)
set.seed(123)
PA_SE_prev <- rpert(nunc, min = 0.0027, mode = 0.0037,max = 0.0051)
set.seed(123)
CM_SE_prev <- rpert(nunc, min = 0.0024, mode = 0.0045,max = 0.0083)
CM_FI_prev <- rpert(nunc, min = 0.0024, mode = 0.0045,max = 0.0083)

#NORWAY
NOpopulation<-5594340 #Population of NO fourth quarter 2024 #ssb.no
NOpop100<-NOpopulation/100000 #per 100 000
NOlifeexpect <- mean(c(81.59,84.8)) #life expect of the Swedish population 2023 #mean male and female
print(NOlifeexpect) #for Age 0 = 83.195 years old
#Prevalence
set.seed(123)
TN_NO_prev <- rpert(nunc, min = 0.0006, mode = 0.003,max = 0.0147)
set.seed(123)
PA_NO_prev <- rpert(nunc, min = 0.0009, mode = 0.0029,max = 0.0091)
set.seed(123)
CM_NO_prev <- rpert(nunc, min = 0.0083, mode = 0.0114,max = 0.0156)

#FINLAND
FIpopulation<-5638675 #Population of FI January 2025  #stat.fi
FIpop100<-FIpopulation/100000 #per 100 000
FIlifeexpect <- mean(c(78.96,84.19)) #life expect of the Swedish population 2023. Mean male and female
print(FIlifeexpect) #for Age 0 = 81.575 years old
#Prevalence
set.seed(123)
TN_FI_prev <- rpert(nunc, min = 0.0015, mode = 0.0037,max = 0.0091)
set.seed(123)
PA_FI_prev <- rpert(nunc, min = 0.0068, mode = 0.0129,max = 0.0243)
set.seed(123)
CM_FI_prev <- rpert(nunc, min = 0.0023, mode = 0.0049,max = 0.0103)

#ICELAND
ICpopulation<-389444 #Population of IC January 2025  #statice.is
ICpop100<-ICpopulation/100000 #per 100 000
IClifeexpect <- mean(c(81.1,83.9)) #life expect of the Swedish population 2023 #mean of male and female
print(IClifeexpect) #for Age 0 = 82.5 years old
#Prevalence
set.seed(123)
TN_IC_prev <- rpert(nunc, min = 0.0002, mode = 0.0006,max = 0.0023)
set.seed(123)
TNW_IC_prev <- rpert(nunc, min = 0.0002, mode = 0.0009,max = 0.0036)
set.seed(123)
PA_IC_prev <- rpert(nunc, min = 0.0006, mode = 0.0018,max = 0.0058)
set.seed(123)
CM_IC_prev <- rpert(nunc, min = 0.0005, mode = 0.0017,max = 0.0064)




##### Mortality model #########
###############################
#####Calculate YLL independent on other parameters####
###############################
# Mortality rates per 100 000  #Not country dependent
set.seed(123)
TN_Mortality <- rpert(nunc, min = 0, mode = 0.00132, max = 0.00368) #Mortality rate per 100 000  #Not dependent on population size.
set.seed(123)
PA_Mortality <- rpert(nunc, min = 0, mode = 0.00233, max = 0.00737) #Mortality rate per 100 000  #Not dependent on population size.
set.seed(123)
CM_Mortality <- rpert(nunc, min = 0, mode = 0.00125, max = 0.00563) #Mortality rate per 100 000  #Not dependent on population size.

#Mortality cases per country TNA and WTNA
MorTNDK<-TN_Mortality*DKpopulation/100000
c(mean(MorTNDK), quantile(MorTNDK, probs = c(0.025, 0.975))) #DK
MorTNSE<-TN_Mortality*SEpopulation/100000
c(mean(MorTNSE), quantile(MorTNSE, probs = c(0.025, 0.975))) #SE
MorTNNO<-TN_Mortality*NOpopulation/100000
c(mean(MorTNNO), quantile(MorTNNO, probs = c(0.025, 0.975))) #NO
MorTNFI<-TN_Mortality*FIpopulation/100000
c(mean(MorTNFI), quantile(MorTNFI, probs = c(0.025, 0.975))) #FI
MorTNIC<-TN_Mortality*ICpopulation/100000
c(mean(MorTNIC), quantile(MorTNIC, probs = c(0.025, 0.975))) #IC

#Mortality cases per country PA
MorPADK<-PA_Mortality*DKpopulation/100000
c(mean(MorPADK), quantile(MorPADK, probs = c(0.025, 0.975))) #DK
MorPASE<-PA_Mortality*SEpopulation/100000
c(mean(MorPASE), quantile(MorPASE, probs = c(0.025, 0.975))) #SE
MorPANO<-PA_Mortality*NOpopulation/100000
c(mean(MorPANO), quantile(MorPANO, probs = c(0.025, 0.975))) #NO
MorPAFI<-PA_Mortality*FIpopulation/100000
c(mean(MorPAFI), quantile(MorPAFI, probs = c(0.025, 0.975))) #FI
MorPAIC<-PA_Mortality*ICpopulation/100000
c(mean(MorPAIC), quantile(MorPAIC, probs = c(0.025, 0.975))) #IC

#Mortality cases per country CM
MorCMDK<-CM_Mortality*DKpopulation/100000
c(mean(MorCMDK), quantile(MorCMDK, probs = c(0.025, 0.975))) #DK
MorCMSE<-CM_Mortality*SEpopulation/100000
c(mean(MorCMSE), quantile(MorCMSE, probs = c(0.025, 0.975))) #SE
MorCMNO<-CM_Mortality*NOpopulation/100000
c(mean(MorCMNO), quantile(MorCMNO, probs = c(0.025, 0.975))) #NO
MorCMFI<-CM_Mortality*FIpopulation/100000
c(mean(MorCMFI), quantile(MorCMFI, probs = c(0.025, 0.975))) #FI
MorCMIC<-CM_Mortality*ICpopulation/100000
c(mean(MorCMIC), quantile(MorCMIC, probs = c(0.025, 0.975))) #IC


# Standard Expected Years Of Life Lost                         
# Dependent on the age a person dies, then gives how many years lost.  #Mean of male and female GDB
SEYLL_01_04 <- 89.41
SEYLL_05_09 <- 84.52
SEYLL_10_14 <- 79.53
SEYLL_15_19 <- 74.54
SEYLL_20_24 <- 69.54
SEYLL_25_29 <- 64.60
SEYLL_30_34 <- 59.63
SEYLL_35_39 <- 54.67
SEYLL_40_44 <- 49.73
SEYLL_45_49 <- 44.81
SEYLL_50_54 <- 39.92
SEYLL_55_59 <- 35.07
SEYLL_60_64 <- 30.25
SEYLL_65_69 <- 25.49
SEYLL_70_74 <- 20.77
SEYLL_75_79 <- 16.43

###Age of death
#Extract value of SEYLL based on AoD
#Tree nut
AoD_TN<-20.58 #years
SEYLL_TN<-SEYLL_20_24
#Peanut
AoD_PA<-20.29 #years
SEYLL_PA<-SEYLL_20_24
#Milk
AoD_CM<-9.25 #years
SEYLL_CM<-SEYLL_05_09


#### Calculate YLL as YLL = nr of deaths * SEYLL
#YLL per 100 000 in each country
#YLL Tree nut
YLL_TN<-TN_Mortality*SEYLL_TN
c(mean(YLL_TN), quantile(YLL_TN, probs = c(0.025, 0.975)))
#YLL Peanut
YLL_PA<-PA_Mortality*SEYLL_PA
c(mean(YLL_PA), quantile(YLL_PA, probs = c(0.025, 0.975)))
#YLL Milk
YLL_CM<-CM_Mortality*SEYLL_CM
c(mean(YLL_CM), quantile(YLL_CM, probs = c(0.025, 0.975)))

#YLL for each country
#Total YLL Tree nut
YLL_TN_DK<-MorTNDK*SEYLL_TN
c(mean(YLL_TN_DK), quantile(YLL_TN_DK, probs = c(0.025, 0.975)))
YLL_TN_SE<-MorTNSE*SEYLL_TN
c(mean(YLL_TN_SE), quantile(YLL_TN_SE, probs = c(0.025, 0.975)))
YLL_TN_NO<-MorTNNO*SEYLL_TN
c(mean(YLL_TN_NO), quantile(YLL_TN_NO, probs = c(0.025, 0.975)))
YLL_TN_FI<-MorTNFI*SEYLL_TN
c(mean(YLL_TN_FI), quantile(YLL_TN_FI, probs = c(0.025, 0.975)))
YLL_TN_IC<-MorTNIC*SEYLL_TN
c(mean(YLL_TN_IC), quantile(YLL_TN_IC, probs = c(0.025, 0.975)))

#Total YLL Peanut
YLL_PA_DK<-MorPADK*SEYLL_PA
c(mean(YLL_PA_DK), quantile(YLL_PA_DK, probs = c(0.025, 0.975)))
YLL_PA_SE<-MorPASE*SEYLL_PA
c(mean(YLL_PA_SE), quantile(YLL_PA_SE, probs = c(0.025, 0.975)))
YLL_PA_NO<-MorPANO*SEYLL_PA
c(mean(YLL_PA_NO), quantile(YLL_PA_NO, probs = c(0.025, 0.975)))
YLL_PA_FI<-MorPAFI*SEYLL_PA
c(mean(YLL_PA_FI), quantile(YLL_PA_FI, probs = c(0.025, 0.975)))
YLL_PA_IC<-MorPAIC*SEYLL_PA
c(mean(YLL_PA_IC), quantile(YLL_PA_IC, probs = c(0.025, 0.975)))

#Total YLL Milk
YLL_CM_DK<-MorCMDK*SEYLL_CM
c(mean(YLL_CM_DK), quantile(YLL_CM_DK, probs = c(0.025, 0.975)))
YLL_CM_SE<-MorCMSE*SEYLL_CM
c(mean(YLL_CM_SE), quantile(YLL_CM_SE, probs = c(0.025, 0.975)))
YLL_CM_NO<-MorCMNO*SEYLL_CM
c(mean(YLL_CM_NO), quantile(YLL_CM_NO, probs = c(0.025, 0.975)))
YLL_CM_FI<-MorCMFI*SEYLL_CM
c(mean(YLL_CM_FI), quantile(YLL_CM_FI, probs = c(0.025, 0.975)))
YLL_CM_IC<-MorCMIC*SEYLL_CM
c(mean(YLL_CM_IC), quantile(YLL_CM_IC, probs = c(0.025, 0.975)))



########Incidence and DALY model################################

#################################################################
############ P E A N U T  in  D E N M A R K #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
DK_Dur_PA<-((1-PAResolve)*(DKlifeexpect-AoO_PA)+(PAResolve*(AoR_PA-AoO_PA))) #Duration weighted by lifelong and resolve proportion
#mean((1-PAResolve)*(DKlifeexpect-AoO_PA))+mean((PAResolve*(AoR_PA-AoO_PA)))
DKInc_inc_PA<-PA_DK_prev/DK_Dur_PA #Incidence of PA in % (rate)
c(mean(DKInc_inc_PA)*100,quantile(DKInc_inc_PA*100,probs = c(0.025,0.975))) #result of incidence rate
# Incidence number of people    #Multiply inc with target population
DKInc_PA<-DKpopulation*DKInc_inc_PA #Danish population
DKInc_PA<- round(DKInc_PA) #Rounded number so we don't have 0.x people.
c(round(mean(DKInc_PA)),round(quantile(DKInc_PA, probs = c(0.025, 0.975))))    #Incidence in DK
c(round(mean(DKInc_PA/DKpop100)),round(quantile(DKInc_PA/DKpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
DKNonsev_inc<-DKInc_PA*(1-PASevere) #People with allergy has a non-severe form
DKNonsev_inc <- round(DKNonsev_inc) # #Rounded number so we don't have 0.x people

DKlife_nonPA<-DKNonsev_inc*(1-PAResolve) #number of people with lifelong non-sev PA
DKlife_nonPA <-round(DKlife_nonPA) # #Rounded number so we don't have 0.x people
DKres_nonPA <-DKNonsev_inc*PAResolve #number of people with resolved non-sev PA
DKres_nonPA <-round(DKres_nonPA) # #Rounded number so we don't have 0.x people

# Severe
DKSev_inc<-DKInc_PA*PASevere #People with allergy has a severe form = anaphylaxis
DKSev_inc <-round(DKSev_inc)#Rounded number so we don't have 0.x people

DKlife_sevPA<-DKSev_inc*(1-PAResolve) #number of people with lifelong severe PA
DKlife_sevPA <-round(DKlife_sevPA)#Rounded number so we don't have 0.x people
DKres_sevPA<-DKSev_inc*PAResolve #number of people with resolved severe PA
DKres_sevPA<-round(DKres_sevPA)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
DKYLD_PA_non_life <- DKlife_nonPA * (DKlifeexpect-AoO_PA) * DWnon #DKlifeexpect when age 0 = 81.905 mean
#Non-severe resolve #A
DKYLD_PA_non_res <- DKres_nonPA *(AoR_PA-AoO_PA)*DWnon
#Severe lifelong #C
DKYLD_PA_sev_life <- DKlife_sevPA * (DKlifeexpect-AoO_PA) * DWsev
#Severe resolve #D
DKYLD_PA_sev_res <- DKres_sevPA *(AoR_PA-AoO_PA) * DWsev

#Total YLD for PA in DK
DK_YLD_PA<-(DKYLD_PA_non_life + DKYLD_PA_non_res + DKYLD_PA_sev_life + DKYLD_PA_sev_res)
c(mean(DK_YLD_PA), quantile(DK_YLD_PA, probs = c(0.025, 0.975)))
#YLD per 100 000 in DK
c(mean(DK_YLD_PA/DKpop100), quantile(DK_YLD_PA/DKpop100, probs = c(0.025, 0.975)))
#             2.5%    97.5% 
#28.65758 18.84456 40.78155 


#YLL is from mortality calculations


#Calculate DALY = YLD + YLL
DK_DALY_PA_DK<- mean(DK_YLD_PA)+mean(YLL_PA_DK)
DK_DALY_PA_lo_DK <- quantile(DK_YLD_PA, probs = c(0.025))+quantile(YLL_PA_DK, probs = c(0.025))
DK_DALY_PA_up_DK <- quantile(DK_YLD_PA, probs = c(0.975))+quantile(YLL_PA_DK, probs = c(0.975))
print(c(DK_DALY_PA_DK,DK_DALY_PA_lo_DK,DK_DALY_PA_up_DK))
#             2.5%    97.5% 
#1729.746 1131.935 2468.224      #Total DALY for PA in Denmark

DK_DALY_PA <- mean(DK_YLD_PA/DKpop100)+mean(YLL_PA)
DK_DALY_PA_lo <- quantile(DK_YLD_PA/DKpop100, probs = c(0.025))+quantile(YLL_PA, probs = c(0.025))
DK_DALY_PA_up <- quantile(DK_YLD_PA/DKpop100, probs = c(0.975))+quantile(YLL_PA, probs = c(0.975))
print(c(DK_DALY_PA,DK_DALY_PA_lo,DK_DALY_PA_up))
#             2.5%    97.5% 
#28.85092 18.87985 41.16818        #Peanuts in Denmark pr 100 000

DK_DALYc_PA <- (mean(DK_YLD_PA)+mean(YLL_PA_DK))/mean(DKInc_PA)
DK_DALYc_PA_lo <- quantile(DK_YLD_PA/DKInc_PA, probs = c(0.025))+quantile(YLL_PA_DK/mean(DKInc_PA), probs = c(0.025))
DK_DALYc_PA_up <- quantile(DK_YLD_PA/DKInc_PA, probs = c(0.975))+quantile(YLL_PA_DK/mean(DKInc_PA), probs = c(0.975))
print(c(DK_DALYc_PA,DK_DALYc_PA_lo,DK_DALYc_PA_up))
#             2.5%    97.5% 
#3.240796 2.511762 4.030450         #Peanuts in Denmark pr case

#Proportion of YLD in DALY per 100 000
propYLD_DK_PA<-(DK_YLD_PA/DKpop100)/DK_DALY_PA
mean(propYLD_DK_PA)*100



#################################################################
############ TREE NUT  in  D E N M A R K #####################
# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
DK_Dur_TN<-((1-TNResolve)*(DKlifeexpect-AoO_TN)+TNResolve*(AoR_TN-AoO_TN)) #Duration weighted by lifelong and resolve
DKInc_inc_TN<-TN_DK_prev/DK_Dur_TN #Incidence of TN in %
c(mean(DKInc_inc_TN)*100,quantile(DKInc_inc_TN*100,probs = c(0.025,0.975))) #result of inc rate in real %
# Incidence number of people     #Multiply inc with target population
DKInc_TN<-DKpopulation*DKInc_inc_TN #Whole population
DKInc_TN<- round(DKInc_TN) #Rounded number so we don't have 0.x people.
c(round(mean(DKInc_TN)),round(quantile(DKInc_TN, probs = c(0.025, 0.975))))
c(round(mean(DKInc_TN/DKpop100)),round(quantile(DKInc_TN/DKpop100, probs = c(0.025, 0.975))))  #Incidence per 100 000

# Non-severe
DKNonsevTN_inc<-DKInc_TN*(1-TNSevere) #People with allergy has a non-severe form
DKNonsevTN_inc <- round(DKNonsevTN_inc) # #Rounded number so we don't have 0.x people

DKlife_nonTN<-DKNonsevTN_inc*(1-TNResolve) #number of people with lifelong non-sev PA
DKlife_nonTN <-round(DKlife_nonTN) # #Rounded number so we don't have 0.x people
DKres_nonTN <-DKNonsevTN_inc*TNResolve #number of people with resolved non-sev PA
DKres_nonTN <-round(DKres_nonTN) # #Rounded number so we don't have 0.x people

# Severe
DKSev_incTN<-DKInc_TN*TNSevere #People with allergy has a severe form = anaphylaxis
DKSev_incTN <-round(DKSev_incTN)#Rounded number so we don't have 0.x people

DKlife_sevTN<-DKSev_incTN*(1-TNResolve) #number of people with lifelong severe PA
DKlife_sevTN <-round(DKlife_sevTN)#Rounded number so we don't have 0.x people
DKres_sevTN<-DKSev_incTN*TNResolve #number of people with resolved severe PA
DKres_sevTN<-round(DKres_sevTN)#Rounded number so we don't have 0.x people

# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
DKYLD_TN_non_life <- DKlife_nonTN * (DKlifeexpect-AoO_TN) * DWnon #DKlifeexpect when age 0 = 81.905 mean
#Non-severe resolve #A
DKYLD_TN_non_res <- DKres_nonTN *(AoR_TN-AoO_TN)*DWnon
#Severe lifelong #C
DKYLD_TN_sev_life <- DKlife_sevTN * (DKlifeexpect-AoO_TN) * DWsev
#Severe resolve #D
DKYLD_TN_sev_res <- DKres_sevTN *(AoR_TN-AoO_TN) * DWsev

#Total YLD for TN in DK
DK_YLD_TN<-(DKYLD_TN_non_life + DKYLD_TN_non_res + DKYLD_TN_sev_life + DKYLD_TN_sev_res)
c(mean(DK_YLD_TN), quantile(DK_YLD_TN, probs = c(0.025, 0.975)))
c(mean(DK_YLD_TN/DKpop100), quantile(DK_YLD_TN/DKpop100, probs = c(0.025, 0.975)))
#               2.5%     97.5% 
#101.39041  14.80639 251.72562  #YLD per 100 000


#YLL is calculated at mortality

#Calculate DALY = YLD + YLL
DK_DALY_TN_DK <- mean(DK_YLD_TN)+mean(YLL_TN_DK)
DK_DALY_TN_lo_DK <- quantile(DK_YLD_TN, probs = c(0.025))+quantile(YLL_TN_DK, probs = c(0.025))
DK_DALY_TN_up_DK <- quantile(DK_YLD_TN, probs = c(0.975))+quantile(YLL_TN_DK, probs = c(0.975))
print(c(DK_DALY_TN_DK,DK_DALY_TN_lo_DK,DK_DALY_TN_up_DK))
#                 2.5%      97.5% 
#6085.0494   888.9939 15104.1351     #Total DALY for TN in Denmark

DK_DALY_TN <- mean(DK_YLD_TN/DKpop100)+mean(YLL_TN)
DK_DALY_TN_lo <- quantile(DK_YLD_TN/DKpop100, probs = c(0.025))+quantile(YLL_TN, probs = c(0.025))
DK_DALY_TN_up <- quantile(DK_YLD_TN/DKpop100, probs = c(0.975))+quantile(YLL_TN, probs = c(0.975))
print(c(DK_DALY_TN,DK_DALY_TN_lo,DK_DALY_TN_up))
#               2.5%     97.5% 
#101.49422  14.82777 251.92604       #Nuts in Denmark per 100 000

DK_DALYc_TN <- (mean(DK_YLD_TN)+mean(YLL_TN_DK))/mean(DKInc_TN)
DK_DALYc_TN_lo <- quantile(DK_YLD_TN/DKInc_TN, probs = c(0.025))+quantile(YLL_TN_DK/mean(DKInc_TN), probs = c(0.025))
DK_DALYc_TN_up <- quantile(DK_YLD_TN/DKInc_TN, probs = c(0.975))+quantile(YLL_TN_DK/mean(DKInc_TN), probs = c(0.975))
print(c(DK_DALYc_TN,DK_DALYc_TN_lo,DK_DALYc_TN_up))
#             2.5%    97.5% 
#5.105081 3.864021 6.534932      #Nuts in Denmark per case

#Proportion of YLD in DALY per 100 000
propYLD_DK_TN<-(DK_YLD_TN/DKpop100)/DK_DALY_TN
mean(propYLD_DK_TN)*100


#################################################################
############ Worst case TREE NUT  in  D E N M A R K #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
DK_Dur_TNW<-((1-TNResolve)*(DKlifeexpect-AoO_TN)+TNResolve*(AoR_TN-AoO_TN)) #Duration weighted by lifelong and resolve
DKInc_inc_TNW<-TNW_DK_prev/DK_Dur_TNW #Incidence of worst TN in %
c(mean(DKInc_inc_TNW)*100,quantile(DKInc_inc_TNW*100,probs = c(0.025,0.975))) #result inc rate real %
# Incidence number of people      #Multiply inc with target population
DKInc_TNW<-DKpopulation*DKInc_inc_TNW #Danish population
DKInc_TNW<- round(DKInc_TNW) #Rounded number so we don't have 0.x people.
c(round(mean(DKInc_TNW)),round(quantile(DKInc_TNW, probs = c(0.025, 0.975))))
c(round(mean(DKInc_TNW/DKpop100)),round(quantile(DKInc_TNW/DKpop100, probs = c(0.025, 0.975)))) #Incidence per 100 000

# Non-severe
DKNonsevTNW_inc<-DKInc_TNW*(1-TNSevere) #People with allergy has a non-severe form
DKNonsevTNW_inc <- round(DKNonsevTNW_inc) # #Rounded number so we don't have 0.x people

DKlife_nonTNW<-DKNonsevTNW_inc*(1-TNResolve) #number of people with lifelong non-sev PA
DKlife_nonTNW <-round(DKlife_nonTNW) # #Rounded number so we don't have 0.x people
DKres_nonTNW <-DKNonsevTNW_inc*TNResolve #number of people with resolved non-sev PA
DKres_nonTNW <-round(DKres_nonTNW) # #Rounded number so we don't have 0.x people

# Severe
DKSev_incTNW<-DKInc_TNW*TNSevere #People with allergy has a severe form = anaphylaxis
DKSev_incTNW <-round(DKSev_incTNW)#Rounded number so we don't have 0.x people

DKlife_sevTNW<-DKSev_incTNW*(1-TNResolve) #number of people with lifelong severe PA
DKlife_sevTNW <-round(DKlife_sevTNW)#Rounded number so we don't have 0.x people
DKres_sevTNW<-DKSev_incTNW*TNResolve #number of people with resolved severe PA
DKres_sevTNW<-round(DKres_sevTNW)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
DKYLD_TNW_non_life <- DKlife_nonTNW * (DKlifeexpect-AoO_TN) * DWnon #DKlifeexpect when age 0 = 81.905 mean
#Non-severe resolve #A
DKYLD_TNW_non_res <- DKres_nonTNW *(AoR_TN-AoO_TN)*DWnon
#Severe lifelong #C
DKYLD_TNW_sev_life <- DKlife_sevTNW * (DKlifeexpect-AoO_TN) * DWsev
#Severe resolve #D
DKYLD_TNW_sev_res <- DKres_sevTNW *(AoR_TN-AoO_TN) * DWsev

#Total YLD for TN in DK
DK_YLD_TNW<-(DKYLD_TNW_non_life + DKYLD_TNW_non_res + DKYLD_TNW_sev_life + DKYLD_TNW_sev_res)
c(mean(DK_YLD_TNW), quantile(DK_YLD_TNW, probs = c(0.025, 0.975)))
c(mean(DK_YLD_TNW/DKpop100), quantile(DK_YLD_TNW/DKpop100, probs = c(0.025, 0.975)))
#               2.5%     97.5% 
#204.68398  92.34423 360.15457    #YLD per 100 000

#YLL is from mortality calculations

#Calculate DALY = YLD + YLL
DK_DALY_TNW_DK <- mean(DK_YLD_TNW)+mean(YLL_TN_DK)
DK_DALY_TNW_lo_DK <- quantile(DK_YLD_TNW, probs = c(0.025))+quantile(YLL_TN_DK, probs = c(0.025))
DK_DALY_TNW_up_DK <- quantile(DK_YLD_TNW, probs = c(0.975))+quantile(YLL_TN_DK, probs = c(0.975))
print(c(DK_DALY_TNW_DK,DK_DALY_TNW_lo_DK,DK_DALY_TNW_up_DK))
#               2.5%     97.5% 
#12277.978  5537.747 21604.953            #Total DALY for TN in Denmark

DK_DALY_TNW <- mean(DK_YLD_TNW/DKpop100)+mean(YLL_TN)
DK_DALY_TNW_lo <- quantile(DK_YLD_TNW/DKpop100, probs = c(0.025))+quantile(YLL_TN, probs = c(0.025))
DK_DALY_TNW_up <- quantile(DK_YLD_TNW/DKpop100, probs = c(0.975))+quantile(YLL_TN, probs = c(0.975))
print(c(DK_DALY_TNW,DK_DALY_TNW_lo,DK_DALY_TNW_up))
#               2.5%     97.5% 
#204.78779  92.36562 360.35499      #Worst case nut Denmark per 100 000

DK_DALYc_TNW <- (mean(DK_YLD_TNW)+mean(YLL_TN_DK))/mean(DKInc_TNW)
DK_DALYc_TNW_lo <- quantile(DK_YLD_TNW/DKInc_TNW, probs = c(0.025))+quantile(YLL_TN_DK/mean(DKInc_TNW), probs = c(0.025))
DK_DALYc_TNW_up <- quantile(DK_YLD_TNW/DKInc_TNW, probs = c(0.975))+quantile(YLL_TN_DK/mean(DKInc_TNW), probs = c(0.975))
print(c(DK_DALYc_TNW,DK_DALYc_TNW_lo,DK_DALYc_TNW_up))
#             2.5%    97.5% 
#5.098561 3.862332 6.528565       #Worst case Treenuts in Denmark per case

#Proportion of YLD in DALY per 100 000
propYLD_DK_TNW<-(DK_YLD_TNW/DKpop100)/DK_DALY_TNW
mean(propYLD_DK_TNW)*100


#################################################################
############ M I L K  in  D E N M A R K #########################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
DK_Dur_CM<-((1-CMResolve)*(DKlifeexpect-AoO_CM)+CMResolve*(AoR_CM-AoO_CM)) #Duration weighted by lifelong and resolve
DKInc_inc_CM<-CM_DK_prev/DK_Dur_CM #Incidence of CM in %
c(mean(DKInc_inc_CM)*100,quantile(DKInc_inc_CM*100,probs = c(0.025,0.975))) #result inc rate real %
# Incidence number of people         #Multiply inc with target population
DKInc_CM<-DKpopulation*DKInc_inc_CM #Danish population
DKInc_CM<- round(DKInc_CM) #Rounded number so we don't have 0.x people.
c(round(mean(DKInc_CM)),round(quantile(DKInc_CM, probs = c(0.025, 0.975))))  #Incidence in DK
c(round(mean(DKInc_CM/DKpop100)),round(quantile(DKInc_CM/DKpop100, probs = c(0.025, 0.975)))) #Incidence per 100 000 in DK

# Non-severe
DKNonsevCM_inc<-DKInc_CM*(1-CMSevere) #People with allergy has a non-severe form
DKNonsevCM_inc <- round(DKNonsevCM_inc) # #Rounded number so we don't have 0.x people

DKlife_nonCM<-DKNonsevCM_inc*(1-CMResolve) #number of people with lifelong non-sev PA
DKlife_nonCM <-round(DKlife_nonCM) # #Rounded number so we don't have 0.x people
DKres_nonCM <-DKNonsevCM_inc*CMResolve #number of people with resolved non-sev PA
DKres_nonCM <-round(DKres_nonCM) # #Rounded number so we don't have 0.x people

# Severe
DKSev_incCM<-DKInc_CM*CMSevere #People with allergy has a severe form = anaphylaxis
DKSev_incCM <-round(DKSev_incCM)#Rounded number so we don't have 0.x people

DKlife_sevCM<-DKSev_incCM*(1-CMResolve) #number of people with lifelong severe PA
DKlife_sevCM <-round(DKlife_sevCM)#Rounded number so we don't have 0.x people
DKres_sevCM<-DKSev_incCM*CMResolve #number of people with resolved severe PA
DKres_sevCM<-round(DKres_sevCM)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
DKYLD_CM_non_life <- DKlife_nonCM * (DKlifeexpect-AoO_CM) * DWnon #DKlifeexpect when age 0 = 81.905 mean
#Non-severe resolve #A
DKYLD_CM_non_res <- DKres_nonCM *(AoR_CM-AoO_CM)*DWnon
#Severe lifelong #C
DKYLD_CM_sev_life <- DKlife_sevCM * (DKlifeexpect-AoO_CM) * DWsev
#Severe resolve #D
DKYLD_CM_sev_res <- DKres_sevCM *(AoR_CM-AoO_CM) * DWsev

#Total YLD for CM in DK
DK_YLD_CM<-(DKYLD_CM_non_life + DKYLD_CM_non_res + DKYLD_CM_sev_life + DKYLD_CM_sev_res)
c(mean(DK_YLD_CM), quantile(DK_YLD_CM, probs = c(0.025, 0.975)))
c(mean(DK_YLD_CM/DKpop100), quantile(DK_YLD_CM/DKpop100, probs = c(0.025, 0.975)))
#             2.5%    97.5% 
#17.53504  9.43724 28.72188  #YLD per 100 000

#YLL is found in mortality calculations

#Calculate DALY = YLD + YLL
DK_DALY_CM_DK <- mean(DK_YLD_CM)+mean(YLL_CM_DK)
DK_DALY_CM_lo_DK <- quantile(DK_YLD_CM, probs = c(0.025))+quantile(YLL_CM_DK, probs = c(0.025))
DK_DALY_CM_up_DK <- quantile(DK_YLD_CM, probs = c(0.975))+quantile(YLL_CM_DK, probs = c(0.975))
print(c(DK_DALY_CM_DK,DK_DALY_CM_lo_DK,DK_DALY_CM_up_DK))
#               2.5%     97.5% 
#1060.2972  567.0048 1741.8014     #Total DALY for CM in Denmark

DK_DALY_CM <- mean(DK_YLD_CM/DKpop100)+mean(YLL_CM)
DK_DALY_CM_lo <- quantile(DK_YLD_CM/DKpop100, probs = c(0.025))+quantile(YLL_CM, probs = c(0.025))
DK_DALY_CM_up <- quantile(DK_YLD_CM/DKpop100, probs = c(0.975))+quantile(YLL_CM, probs = c(0.975))
print(c(DK_DALY_CM,DK_DALY_CM_lo,DK_DALY_CM_up))
#             2.5%    97.5% 
#17.68499  9.45723 29.05199       #Milk in Denmark per 100 000

DK_DALYc_CM <- (mean(DK_YLD_CM)+mean(YLL_CM_DK))/mean(DKInc_CM)
DK_DALYc_CM_lo <- quantile(DK_YLD_CM/DKInc_CM, probs = c(0.025))+quantile(YLL_CM_DK/mean(DKInc_CM), probs = c(0.025))
DK_DALYc_CM_up <- quantile(DK_YLD_CM/DKInc_CM, probs = c(0.975))+quantile(YLL_CM_DK/mean(DKInc_CM), probs = c(0.975))
print(c(DK_DALYc_CM,DK_DALYc_CM_lo,DK_DALYc_CM_up))
#               2.5%     97.5% 
#1.1458249 0.7228723 1.7535992   #Milk in Denmark pr case

#Proportion of YLD in DALY per 100 000
propYLD_DK_CM<-(DK_YLD_CM/DKpop100)/DK_DALY_CM
mean(propYLD_DK_CM)*100







#################################################################
############ P E A N U T  in  S W E D E N #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
SE_Dur_PA<-((1-PAResolve)*(SElifeexpect-AoO_PA)+PAResolve*(AoR_PA-AoO_PA)) #Duration weighted by lifelong and resolve
SEInc_inc_PA<-PA_SE_prev/SE_Dur_PA #Incidence of PA in %
c(mean(SEInc_inc_PA*100),quantile(SEInc_inc_PA*100,probs = c(0.025,0.975))) #result inc rate real %
# Incidence number of people    #Multiply inc with target population
SEInc_PA<-SEpopulation*SEInc_inc_PA #Swedish population
SEInc_PA<- round(SEInc_PA) #Rounded number so we don't have 0.x people.
c(round(mean(SEInc_PA)),round(quantile(SEInc_PA, probs = c(0.025, 0.975))))  #Incidence in SE
c(round(mean(SEInc_PA/SEpop100)),round(quantile(SEInc_PA/SEpop100, probs = c(0.025, 0.975)))) #Incidence per 100 000 in SE

# Non-severe
SENonsev_inc<-SEInc_PA*(1-PASevere) #People with allergy has a non-severe form
SENonsev_inc <- round(SENonsev_inc) # #Rounded number so we don't have 0.x people

SElife_nonPA<-SENonsev_inc*(1-PAResolve) #number of people with lifelong non-sev PA
SElife_nonPA <-round(SElife_nonPA) # #Rounded number so we don't have 0.x people
SEres_nonPA <-SENonsev_inc*PAResolve #number of people with resolved non-sev PA
SEres_nonPA <-round(SEres_nonPA) # #Rounded number so we don't have 0.x people

# Severe
SESev_inc<-SEInc_PA*PASevere #People with allergy has a severe form = anaphylaxis
SESev_inc <-round(SESev_inc)#Rounded number so we don't have 0.x people

SElife_sevPA<-SESev_inc*(1-PAResolve) #number of people with lifelong severe PA
SElife_sevPA <-round(SElife_sevPA)#Rounded number so we don't have 0.x people
SEres_sevPA<-SESev_inc*PAResolve #number of people with resolved severe PA
SEres_sevPA<-round(SEres_sevPA)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
SEYLD_PA_non_life <- SElife_nonPA * (SElifeexpect-AoO_PA) * DWnon #SElifeexpect when age 0 = 83.24 mean
#Non-severe resolve #A
SEYLD_PA_non_res <- SEres_nonPA *(AoR_PA-AoO_PA)*DWnon
#Severe lifelong #C
SEYLD_PA_sev_life <- SElife_sevPA * (SElifeexpect-AoO_PA) * DWsev
#Severe resolve #D
SEYLD_PA_sev_res <- SEres_sevPA *(AoR_PA-AoO_PA) * DWsev

#Total YLD for PA in SE
SE_YLD_PA<-(SEYLD_PA_non_life + SEYLD_PA_non_res + SEYLD_PA_sev_life + SEYLD_PA_sev_res)
c(mean(SE_YLD_PA), quantile(SE_YLD_PA, probs = c(0.025, 0.975)))
c(mean(SE_YLD_PA/SEpop100), quantile(SE_YLD_PA/SEpop100, probs = c(0.025, 0.975)))
#             2.5%    97.5% 
#20.54027 14.41285 27.78307  #YLD per 100 000

#YLL is from mortality calculations

#Calculate DALY = YLD + YLL
SE_DALY_PA_SE <- mean(SE_YLD_PA)+mean(YLL_PA_SE)
SE_DALY_PA_lo_SE <- quantile(SE_YLD_PA, probs = c(0.025))+quantile(YLL_PA_SE, probs = c(0.025))
SE_DALY_PA_up_SE <- quantile(SE_YLD_PA, probs = c(0.975))+quantile(YLL_PA_SE, probs = c(0.975))
print(c(SE_DALY_PA_SE,SE_DALY_PA_lo_SE,SE_DALY_PA_up_SE))
#             2.5%    97.5% 
#2195.279 1529.773 2982.613      #Total DALY for PA in Sweden

SE_DALY_PA <- mean(SE_YLD_PA/SEpop100)+mean(YLL_PA)
SE_DALY_PA_lo <- quantile(SE_YLD_PA/SEpop100, probs = c(0.025))+quantile(YLL_PA, probs = c(0.025))
SE_DALY_PA_up <- quantile(SE_YLD_PA/SEpop100, probs = c(0.975))+quantile(YLL_PA, probs = c(0.975))
print(c(SE_DALY_PA,SE_DALY_PA_lo,SE_DALY_PA_up))
#             2.5%    97.5% 
#20.73361 14.44815 28.16970     #Peanuts in Sweden pr 100 000

SE_DALYc_PA <- (mean(SE_YLD_PA)+mean(YLL_PA_SE))/mean(SEInc_PA)
SE_DALYc_PA_lo <- quantile(SE_YLD_PA/SEInc_PA, probs = c(0.025))+quantile(YLL_PA_SE/mean(SEInc_PA), probs = c(0.025))
SE_DALYc_PA_up <- quantile(SE_YLD_PA/SEInc_PA, probs = c(0.975))+quantile(YLL_PA_SE/mean(SEInc_PA), probs = c(0.975))
print(c(SE_DALYc_PA,SE_DALYc_PA_lo,SE_DALYc_PA_up))
#             2.5%    97.5% 
#3.300082 2.555531 4.113335   #Peanuts in Sweden pr case

#Proportion of YLD in DALY per 100 000
propYLD_SE_PA<-(SE_YLD_PA/SEpop100)/SE_DALY_PA
mean(propYLD_SE_PA)*100


#################################################################
############ TREE NUT  in  S W E D E N #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
SE_Dur_TN<-((1-TNResolve)*(SElifeexpect-AoO_TN)+TNResolve*(AoR_TN-AoO_TN)) #Duration weighted by lifelong and resolve
SEInc_inc_TN<-TN_SE_prev/SE_Dur_TN #Incidence of TN in %
c(mean(SEInc_inc_TN*100),quantile(SEInc_inc_TN*100,probs = c(0.025,0.975))) #result inc rate real %
# Incidence number of people     #Multiply inc with target population
SEInc_TN<-SEpopulation*SEInc_inc_TN #Swedish population
SEInc_TN<- round(SEInc_TN) #Rounded number so we don't have 0.x people.
c(round(mean(SEInc_TN)),round(quantile(SEInc_TN, probs = c(0.025, 0.975)))) #Incidence in SE
c(round(mean(SEInc_TN/SEpop100)),round(quantile(SEInc_TN/SEpop100, probs = c(0.025, 0.975)))) #Incidence per 100 000 in SE

# Non-severe
SENonsevTN_inc<-SEInc_TN*(1-TNSevere) #People with allergy has a non-severe form
SENonsevTN_inc <- round(SENonsevTN_inc) # #Rounded number so we don't have 0.x people

SElife_nonTN<-SENonsevTN_inc*(1-TNResolve) #number of people with lifelong non-sev PA
SElife_nonTN <-round(SElife_nonTN) # #Rounded number so we don't have 0.x people
SEres_nonTN <-SENonsevTN_inc*TNResolve #number of people with resolved non-sev PA
SEres_nonTN <-round(SEres_nonTN) # #Rounded number so we don't have 0.x people

# Severe
SESev_incTN<-SEInc_TN*TNSevere #People with allergy has a severe form = anaphylaxis
SESev_incTN <-round(SESev_incTN)#Rounded number so we don't have 0.x people

SElife_sevTN<-SESev_incTN*(1-TNResolve) #number of people with lifelong severe PA
SElife_sevTN <-round(SElife_sevTN)#Rounded number so we don't have 0.x people
SEres_sevTN<-SESev_incTN*TNResolve #number of people with resolved severe PA
SEres_sevTN<-round(SEres_sevTN)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
SEYLD_TN_non_life <- SElife_nonTN * (SElifeexpect-AoO_TN) * DWnon #SElifeexpect when age 0 = 83.24 mean
#Non-severe resolve #A
SEYLD_TN_non_res <- SEres_nonTN *(AoR_TN-AoO_TN)*DWnon
#Severe lifelong #C
SEYLD_TN_sev_life <- SElife_sevTN * (SElifeexpect-AoO_TN) * DWsev
#Severe resolve #D
SEYLD_TN_sev_res <- SEres_sevTN *(AoR_TN-AoO_TN) * DWsev

#Total YLD for TN in SE
SE_YLD_TN<-(SEYLD_TN_non_life + SEYLD_TN_non_res + SEYLD_TN_sev_life + SEYLD_TN_sev_res)
c(mean(SE_YLD_TN), quantile(SE_YLD_TN, probs = c(0.025, 0.975)))
c(mean(SE_YLD_TN/SEpop100), quantile(SE_YLD_TN/SEpop100, probs = c(0.025, 0.975)))
#             2.5%    97.5% 
#29.69739 17.73081 45.24473  #YLD per 100 000

#YLL is calculated at mortality

#Calculate DALY = YLD + YLL
SE_DALY_TN_SE <- mean(SE_YLD_TN)+mean(YLL_TN_SE)
SE_DALY_TN_lo_SE <- quantile(SE_YLD_TN, probs = c(0.025))+quantile(YLL_TN_SE, probs = c(0.025))
SE_DALY_TN_up_SE <- quantile(SE_YLD_TN, probs = c(0.975))+quantile(YLL_TN_SE, probs = c(0.975))
print(c(SE_DALY_TN_SE,SE_DALY_TN_lo_SE,SE_DALY_TN_up_SE))
#             2.5%    97.5% 
#3155.356 1879.606 4811.742        #Total DALY for TN in Sweden

SE_DALY_TN <- mean(SE_YLD_TN/SEpop100)+mean(YLL_TN)
SE_DALY_TN_lo <- quantile(SE_YLD_TN/SEpop100, probs = c(0.025))+quantile(YLL_TN, probs = c(0.025))
SE_DALY_TN_up <- quantile(SE_YLD_TN/SEpop100, probs = c(0.975))+quantile(YLL_TN, probs = c(0.975))
print(c(SE_DALY_TN,SE_DALY_TN_lo,SE_DALY_TN_up))
#             2.5%    97.5% 
#29.80119 17.75220 45.44515           #Nuts in Sweden pr 100 000

SE_DALYc_TN <- (mean(SE_YLD_TN)+mean(YLL_TN_SE))/mean(SEInc_TN)
SE_DALYc_TN_lo <- quantile(SE_YLD_TN/SEInc_TN, probs = c(0.025))+quantile(YLL_TN_SE/mean(SEInc_TN), probs = c(0.025))
SE_DALYc_TN_up <- quantile(SE_YLD_TN/SEInc_TN, probs = c(0.975))+quantile(YLL_TN_SE/mean(SEInc_TN), probs = c(0.975))
print(c(SE_DALYc_TN,SE_DALYc_TN_lo,SE_DALYc_TN_up))
#             2.5%    97.5% 
#5.197785 3.932241 6.666819     #Treenuts in Sweden pr case

#Proportion of YLD in DALY per 100 000
propYLD_SE_TN<-(SE_YLD_TN/SEpop100)/SE_DALY_TN
mean(propYLD_SE_TN)*100


#################################################################
############ Worst case TREE NUT  in  S W E D E N #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
SE_Dur_TNW<-((1-TNResolve)*(SElifeexpect-AoO_TN)+TNResolve*(AoR_TN-AoO_TN)) #Duration weighted by lifelong and resolve
SEInc_inc_TNW<-TNW_SE_prev/SE_Dur_TN #Incidence of worst TN in %
c(mean(SEInc_inc_TNW*100),quantile(SEInc_inc_TNW*100,probs = c(0.025,0.975))) #result inc rate real %
# Incidence number of people     #Multiply inc with target population
SEInc_TNW<-SEpopulation*SEInc_inc_TNW #Swedish population
SEInc_TNW<- round(SEInc_TNW) #Rounded number so we don't have 0.x people.
c(round(mean(SEInc_TNW)),round(quantile(SEInc_TNW, probs = c(0.025, 0.975)))) #Incidence in SE
c(round(mean(SEInc_TNW/SEpop100)),round(quantile(SEInc_TNW/SEpop100, probs = c(0.025, 0.975)))) #Incidence per 100 000 in SE

# Non-severe
SENonsevTNW_inc<-SEInc_TNW*(1-TNSevere) #People with allergy has a non-severe form
SENonsevTNW_inc <- round(SENonsevTNW_inc) # #Rounded number so we don't have 0.x people

SElife_nonTNW<-SENonsevTNW_inc*(1-TNResolve) #number of people with lifelong non-sev PA
SElife_nonTNW <-round(SElife_nonTNW) # #Rounded number so we don't have 0.x people
SEres_nonTNW <-SENonsevTNW_inc*TNResolve #number of people with resolved non-sev PA
SEres_nonTNW <-round(SEres_nonTNW) # #Rounded number so we don't have 0.x people

# Severe
SESev_incTNW<-SEInc_TNW*TNSevere #People with allergy has a severe form = anaphylaxis
SESev_incTNW <-round(SESev_incTNW)#Rounded number so we don't have 0.x people

SElife_sevTNW<-SESev_incTNW*(1-TNResolve) #number of people with lifelong severe PA
SElife_sevTNW <-round(SElife_sevTNW)#Rounded number so we don't have 0.x people
SEres_sevTNW<-SESev_incTNW*TNResolve #number of people with resolved severe PA
SEres_sevTNW<-round(SEres_sevTNW)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
SEYLD_TNW_non_life <- SElife_nonTNW * (SElifeexpect-AoO_TN) * DWnon #SElifeexpect when age 0 = 83.24 mean
#Non-severe resolve #A
SEYLD_TNW_non_res <- SEres_nonTNW *(AoR_TN-AoO_TN)*DWnon
#Severe lifelong #C
SEYLD_TNW_sev_life <- SElife_sevTNW * (SElifeexpect-AoO_TN) * DWsev
#Severe resolve #D
SEYLD_TNW_sev_res <- SEres_sevTNW *(AoR_TN-AoO_TN) * DWsev

#Total YLD for TN in SE
SE_YLD_TNW<-(SEYLD_TNW_non_life + SEYLD_TNW_non_res + SEYLD_TNW_sev_life + SEYLD_TNW_sev_res)
c(mean(SE_YLD_TNW), quantile(SE_YLD_TNW, probs = c(0.025, 0.975)))
c(mean(SE_YLD_TNW/SEpop100), quantile(SE_YLD_TNW/SEpop100, probs = c(0.025, 0.975)))
#             2.5%    97.5% 
#36.36417 16.26018 64.87775  #YLD per 100 000

#YLL is from mortality calculations

#Calculate DALY = YLD + YLL
SE_DALY_TNW_SE <- mean(SE_YLD_TNW)+mean(YLL_TN_SE)
SE_DALY_TNW_lo_SE <- quantile(SE_YLD_TNW, probs = c(0.025))+quantile(YLL_TN_SE, probs = c(0.025))
SE_DALY_TNW_up_SE <- quantile(SE_YLD_TNW, probs = c(0.975))+quantile(YLL_TN_SE, probs = c(0.975))
print(c(SE_DALY_TNW_SE,SE_DALY_TNW_lo_SE,SE_DALY_TNW_up_SE))
#             2.5%    97.5% 
#3861.236 1723.895 6890.490    #Total DALY for worst TN in Sweden

SE_DALY_TNW <- mean(SE_YLD_TNW/SEpop100)+mean(YLL_TN)
SE_DALY_TNW_lo <- quantile(SE_YLD_TNW/SEpop100, probs = c(0.025))+quantile(YLL_TN, probs = c(0.025))
SE_DALY_TNW_up <- quantile(SE_YLD_TNW/SEpop100, probs = c(0.975))+quantile(YLL_TN, probs = c(0.975))
print(c(SE_DALY_TNW,SE_DALY_TNW_lo,SE_DALY_TNW_up))
#             2.5%    97.5% 
#36.46797 16.28156 65.07817    #Worst case nut Sweden

SE_DALYc_TNW <- (mean(SE_YLD_TNW)+mean(YLL_TN_SE))/mean(SEInc_TNW)
SE_DALYc_TNW_lo <- quantile(SE_YLD_TNW/SEInc_TNW, probs = c(0.025))+quantile(YLL_TN_SE/mean(SEInc_TNW), probs = c(0.025))
SE_DALYc_TNW_up <- quantile(SE_YLD_TNW/SEInc_TNW, probs = c(0.975))+quantile(YLL_TN_SE/mean(SEInc_TNW), probs = c(0.975))
print(c(SE_DALYc_TNW,SE_DALYc_TNW_lo,SE_DALYc_TNW_up))
#             2.5%    97.5% 
#5.197742 3.931123 6.663445      #Worst case Tree nuts in Sweden pr case

#Proportion of YLD in DALY per 100 000
propYLD_SE_TNW<-(SE_YLD_TNW/SEpop100)/SE_DALY_TNW
mean(propYLD_SE_TNW)*100




#################################################################
############ M I L K  in  S W E D E N #########################
# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
SE_Dur_CM<-((1-CMResolve)*(SElifeexpect-AoO_CM)+CMResolve*(AoR_CM-AoO_CM)) #Duration weighted by lifelong and resolve
SEInc_inc_CM<-CM_SE_prev/SE_Dur_CM #Incidence of CM in %
c(mean(SEInc_inc_CM*100),quantile(SEInc_inc_CM*100,probs = c(0.025,0.975))) #result inc rate real %
# Incidence number of people       #Multiply inc with target population
SEInc_CM<-SEpopulation*SEInc_inc_CM #Swedish population
SEInc_CM<- round(SEInc_CM) #Rounded number so we don't have 0.x people.
c(round(mean(SEInc_CM)),round(quantile(SEInc_CM, probs = c(0.025, 0.975)))) #Incidence in SE
c(round(mean(SEInc_CM/SEpop100)),round(quantile(SEInc_CM/SEpop100, probs = c(0.025, 0.975)))) #Incidence per 100 000 in SE

# Non-severe
SENonsevCM_inc<-SEInc_CM*(1-CMSevere) #People with allergy has a non-severe form
SENonsevCM_inc <- round(SENonsevCM_inc) # #Rounded number so we don't have 0.x people

SElife_nonCM<-SENonsevCM_inc*(1-CMResolve) #number of people with lifelong non-sev PA
SElife_nonCM <-round(SElife_nonCM) # #Rounded number so we don't have 0.x people
SEres_nonCM <-SENonsevCM_inc*CMResolve #number of people with resolved non-sev PA
SEres_nonCM <-round(SEres_nonCM) # #Rounded number so we don't have 0.x people

# Severe
SESev_incCM<-SEInc_CM*CMSevere #People with allergy has a severe form = anaphylaxis
SESev_incCM <-round(SESev_incCM)#Rounded number so we don't have 0.x people

SElife_sevCM<-SESev_incCM*(1-CMResolve) #number of people with lifelong severe PA
SElife_sevCM <-round(SElife_sevCM)#Rounded number so we don't have 0.x people
SEres_sevCM<-SESev_incCM*CMResolve #number of people with resolved severe PA
SEres_sevCM<-round(SEres_sevCM)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
SEYLD_CM_non_life <- SElife_nonCM * (SElifeexpect-AoO_CM) * DWnon #SElifeexpect when age 0 = 83.24 mean
#Non-severe resolve #A
SEYLD_CM_non_res <- SEres_nonCM *(AoR_CM-AoO_CM)*DWnon
#Severe lifelong #C
SEYLD_CM_sev_life <- SElife_sevCM * (SElifeexpect-AoO_CM) * DWsev
#Severe resolve #D
SEYLD_CM_sev_res <- SEres_sevCM *(AoR_CM-AoO_CM) * DWsev

#Total YLD for CM in SE
SE_YLD_CM<-(SEYLD_CM_non_life + SEYLD_CM_non_res + SEYLD_CM_sev_life + SEYLD_CM_sev_res)
c(mean(SE_YLD_CM), quantile(SE_YLD_CM, probs = c(0.025, 0.975)))
c(mean(SE_YLD_CM/SEpop100), quantile(SE_YLD_CM/SEpop100, probs = c(0.025, 0.975)))
#             2.5%    97.5% 
#22.64503 12.16113 37.25356   #YLD per 100 000

#YLL is found in mortality calculations

#Calculate DALY = YLD + YLL
SE_DALY_CM_SE <- mean(SE_YLD_CM)+mean(YLL_CM_SE)
SE_DALY_CM_lo_SE <- quantile(SE_YLD_CM, probs = c(0.025))+quantile(YLL_CM_SE, probs = c(0.025))
SE_DALY_CM_up_SE <- quantile(SE_YLD_CM, probs = c(0.975))+quantile(YLL_CM_SE, probs = c(0.975))
print(c(SE_DALY_CM_SE,SE_DALY_CM_lo_SE,SE_DALY_CM_up_SE))
#             2.5%    97.5% 
#2413.538 1289.739 3979.366      #Total DALY for CM in Sweden

SE_DALY_CM <- mean(SE_YLD_CM/SEpop100)+mean(YLL_CM)
SE_DALY_CM_lo <- quantile(SE_YLD_CM/SEpop100, probs = c(0.025))+quantile(YLL_CM, probs = c(0.025))
SE_DALY_CM_up <- quantile(SE_YLD_CM/SEpop100, probs = c(0.975))+quantile(YLL_CM, probs = c(0.975))
print(c(SE_DALY_CM,SE_DALY_CM_lo,SE_DALY_CM_up))
#             2.5%    97.5% 
#22.79498 12.18112 37.58367    #Milk in Sweden per 100 000

SE_DALYc_CM <- (mean(SE_YLD_CM)+mean(YLL_CM_SE))/mean(SEInc_CM)
SE_DALYc_CM_lo <- quantile(SE_YLD_CM/SEInc_CM, probs = c(0.025))+quantile(YLL_CM_SE/mean(SEInc_CM), probs = c(0.025))
SE_DALYc_CM_up <- quantile(SE_YLD_CM/SEInc_CM, probs = c(0.975))+quantile(YLL_CM_SE/mean(SEInc_CM), probs = c(0.975))
print(c(SE_DALYc_CM,SE_DALYc_CM_lo,SE_DALYc_CM_up))
#               2.5%     97.5% 
#1.1585179 0.7322915 1.7732876   #Milk in Sweden pr case

#Proportion of YLD in DALY per 100 000
propYLD_SE_CM<-(SE_YLD_CM/SEpop100)/SE_DALY_CM
mean(propYLD_SE_CM)*100







#################################################################
############ P E A N U T  in  N O R W A Y #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
NO_Dur_PA<-((1-PAResolve)*(NOlifeexpect-AoO_PA)+PAResolve*(AoR_PA-AoO_PA)) #Duration weighted by lifelong and resolve
NOInc_inc_PA<-PA_NO_prev/NO_Dur_PA #Incidence of PA in %
c(mean(NOInc_inc_PA*100),quantile(NOInc_inc_PA*100,probs = c(0.025,0.975))) #Result inc rate in real %
# Incidence number of people    #Multiply inc with target population
NOInc_PA<-NOpopulation*NOInc_inc_PA #Norwegian population
NOInc_PA<- round(NOInc_PA) #Rounded number so we don't have 0.x people.
c(round(mean(NOInc_PA)),round(quantile(NOInc_PA, probs = c(0.025, 0.975))))    #Incidence in DK
c(round(mean(NOInc_PA/NOpop100)),round(quantile(NOInc_PA/NOpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
NONonsev_inc<-NOInc_PA*(1-PASevere) #People with allergy has a non-severe form
NONonsev_inc <- round(NONonsev_inc) # #Rounded number so we don't have 0.x people

NOlife_nonPA<-NONonsev_inc*(1-PAResolve) #number of people with lifelong non-sev PA
NOlife_nonPA <-round(NOlife_nonPA) # #Rounded number so we don't have 0.x people
NOres_nonPA <-NONonsev_inc*PAResolve #number of people with resolved non-sev PA
NOres_nonPA <-round(NOres_nonPA) # #Rounded number so we don't have 0.x people

# Severe
NOSev_inc<-NOInc_PA*PASevere #People with allergy has a severe form = anaphylaxis
NOSev_inc <-round(NOSev_inc)#Rounded number so we don't have 0.x people

NOlife_sevPA<-NOSev_inc*(1-PAResolve) #number of people with lifelong severe PA
NOlife_sevPA <-round(NOlife_sevPA)#Rounded number so we don't have 0.x people
NOres_sevPA<-NOSev_inc*PAResolve #number of people with resolved severe PA
NOres_sevPA<-round(NOres_sevPA)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
NOYLD_PA_non_life <- NOlife_nonPA * (NOlifeexpect-AoO_PA) * DWnon #NOlifeexpect when age 0 = 83.195 mean
#Non-severe resolve #A
NOYLD_PA_non_res <- NOres_nonPA *(AoR_PA-AoO_PA)*DWnon
#Severe lifelong #C
NOYLD_PA_sev_life <- NOlife_sevPA * (NOlifeexpect-AoO_PA) * DWsev
#Severe resolve #D
NOYLD_PA_sev_res <- NOres_sevPA *(AoR_PA-AoO_PA) * DWsev

#Total YLD for PA in NO
NO_YLD_PA<-(NOYLD_PA_non_life + NOYLD_PA_non_res + NOYLD_PA_sev_life + NOYLD_PA_sev_res)
c(mean(NO_YLD_PA), quantile(NO_YLD_PA, probs = c(0.025, 0.975)))
c(mean(NO_YLD_PA/NOpop100), quantile(NO_YLD_PA/NOpop100, probs = c(0.025, 0.975)))
#               2.5%     97.5% 
#19.649276  6.887449 37.860320     #YLD per 100 000

#YLL is from mortality calculations


#Calculate DALY = YLD + YLL
NO_DALY_PA_NO <- mean(NO_YLD_PA)+mean(YLL_PA_NO)
NO_DALY_PA_lo_NO <- quantile(NO_YLD_PA, probs = c(0.025))+quantile(YLL_PA_NO, probs = c(0.025))
NO_DALY_PA_up_NO <- quantile(NO_YLD_PA, probs = c(0.975))+quantile(YLL_PA_NO, probs = c(0.975))
print(c(NO_DALY_PA_NO,NO_DALY_PA_lo_NO,NO_DALY_PA_up_NO))
#               2.5%     97.5% 
#1110.0633  387.2819 2139.6643        #Total DALY for PA in Norway

NO_DALY_PA <- mean(NO_YLD_PA/NOpop100)+mean(YLL_PA)
NO_DALY_PA_lo <- quantile(NO_YLD_PA/NOpop100, probs = c(0.025))+quantile(YLL_PA, probs = c(0.025))
NO_DALY_PA_up <- quantile(NO_YLD_PA/NOpop100, probs = c(0.975))+quantile(YLL_PA, probs = c(0.975))
print(c(NO_DALY_PA,NO_DALY_PA_lo,NO_DALY_PA_up))
#               2.5%     97.5% 
#19.842614  6.922746 38.246948       #PA in Norway pr 100 000

NO_DALYc_PA <- (mean(NO_YLD_PA)+mean(YLL_PA_NO))/mean(NOInc_PA)
NO_DALYc_PA_lo <- quantile(NO_YLD_PA/NOInc_PA, probs = c(0.025))+quantile(YLL_PA_NO/mean(NOInc_PA), probs = c(0.025))
NO_DALYc_PA_up <- quantile(NO_YLD_PA/NOInc_PA, probs = c(0.975))+quantile(YLL_PA_NO/mean(NOInc_PA), probs = c(0.975))
print(c(NO_DALYc_PA,NO_DALYc_PA_lo,NO_DALYc_PA_up))
#             2.5%    97.5% 
#3.298268 2.555034 4.113981     #PA in Norway pr case

#Proportion of YLD in DALY per 100 000
propYLD_NO_PA<-(NO_YLD_PA/NOpop100)/NO_DALY_PA
mean(propYLD_NO_PA)*100



#################################################################
############ TREE NUT  in  N O R W A Y #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
NO_Dur_TN<-((1-TNResolve)*(NOlifeexpect-AoO_TN)+TNResolve*(AoR_TN-AoO_TN)) #Duration weighted by lifelong and resolve
NOInc_inc_TN<-TN_NO_prev/NO_Dur_TN #Incidence of TN in %
c(mean(NOInc_inc_TN*100),quantile(NOInc_inc_TN*100,probs = c(0.025,0.975)))
# Incidence number of people     #Multiply inc with target population
NOInc_TN<-NOpopulation*NOInc_inc_TN #Norwegian population
NOInc_TN<- round(NOInc_TN) #Rounded number so we don't have 0.x people.
c(round(mean(NOInc_TN)),round(quantile(NOInc_TN, probs = c(0.025, 0.975))))    #Incidence in NO
c(round(mean(NOInc_TN/NOpop100)),round(quantile(NOInc_TN/NOpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
NONonsevTN_inc<-NOInc_TN*(1-TNSevere) #People with allergy has a non-severe form
NONonsevTN_inc <- round(NONonsevTN_inc) # #Rounded number so we don't have 0.x people

NOlife_nonTN<-NONonsevTN_inc*(1-TNResolve) #number of people with lifelong non-sev PA
NOlife_nonTN <-round(NOlife_nonTN) # #Rounded number so we don't have 0.x people
NOres_nonTN <-NONonsevTN_inc*TNResolve #number of people with resolved non-sev PA
NOres_nonTN <-round(NOres_nonTN) # #Rounded number so we don't have 0.x people

# Severe
NOSev_incTN<-NOInc_TN*TNSevere #People with allergy has a severe form = anaphylaxis
NOSev_incTN <-round(NOSev_incTN)#Rounded number so we don't have 0.x people

NOlife_sevTN<-NOSev_incTN*(1-TNResolve) #number of people with lifelong severe PA
NOlife_sevTN <-round(NOlife_sevTN)#Rounded number so we don't have 0.x people
NOres_sevTN<-NOSev_incTN*TNResolve #number of people with resolved severe PA
NOres_sevTN<-round(NOres_sevTN)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
NOYLD_TN_non_life <- NOlife_nonTN * (NOlifeexpect-AoO_TN) * DWnon #NOlifeexpect when age 0 = 83.195 mean
#Non-severe resolve #A
NOYLD_TN_non_res <- NOres_nonTN *(AoR_TN-AoO_TN)*DWnon
#Severe lifelong #C
NOYLD_TN_sev_life <- NOlife_sevTN * (NOlifeexpect-AoO_TN) * DWsev
#Severe resolve #D
NOYLD_TN_sev_res <- NOres_sevTN *(AoR_TN-AoO_TN) * DWsev

#Total YLD for TN in NO
NO_YLD_TN<-(NOYLD_TN_non_life + NOYLD_TN_non_res + NOYLD_TN_sev_life + NOYLD_TN_sev_res)
c(mean(NO_YLD_TN), quantile(NO_YLD_TN, probs = c(0.025, 0.975)))
c(mean(NO_YLD_TN/NOpop100), quantile(NO_YLD_TN/NOpop100, probs = c(0.025, 0.975)))
#               2.5%     97.5% 
#32.315126  7.210951 72.783457    #YLD per 100 000

#YLL is calculated at mortality


#Calculate DALY = YLD + YLL
NO_DALY_TN_NO <- mean(NO_YLD_TN)+mean(YLL_TN_NO)
NO_DALY_TN_lo_NO <- quantile(NO_YLD_TN, probs = c(0.025))+quantile(YLL_TN_NO, probs = c(0.025))
NO_DALY_TN_up_NO <- quantile(NO_YLD_TN, probs = c(0.975))+quantile(YLL_TN_NO, probs = c(0.975))
print(c(NO_DALY_TN_NO,NO_DALY_TN_lo_NO,NO_DALY_TN_up_NO))
#               2.5%     97.5% 
#1813.6253  404.6015 4082.9662         #Total DALY for TN in Norway

NO_DALY_TN <- mean(NO_YLD_TN/NOpop100)+mean(YLL_TN)
NO_DALY_TN_lo <- quantile(NO_YLD_TN/NOpop100, probs = c(0.025))+quantile(YLL_TN, probs = c(0.025))
NO_DALY_TN_up <- quantile(NO_YLD_TN/NOpop100, probs = c(0.975))+quantile(YLL_TN, probs = c(0.975))
print(c(NO_DALY_TN,NO_DALY_TN_lo,NO_DALY_TN_up))
#               2.5%     97.5% 
#32.418933  7.232337 72.983877     # Nuts in Norway per 100 000

NO_DALYc_TN <- (mean(NO_YLD_TN)+mean(YLL_TN_NO))/mean(NOInc_TN)
NO_DALYc_TN_lo <- quantile(NO_YLD_TN/NOInc_TN, probs = c(0.025))+quantile(YLL_TN_NO/mean(NOInc_TN), probs = c(0.025))
NO_DALYc_TN_up <- quantile(NO_YLD_TN/NOInc_TN, probs = c(0.975))+quantile(YLL_TN_NO/mean(NOInc_TN), probs = c(0.975))
print(c(NO_DALYc_TN,NO_DALYc_TN_lo,NO_DALYc_TN_up))
#             2.5%    97.5% 
#5.193838 3.931622 6.667139     #Treenuts in Norway pr case

#Proportion of YLD in DALY per 100 000
propYLD_NO_TN<-(NO_YLD_TN/NOpop100)/NO_DALY_TN
mean(propYLD_NO_TN)*100


#################################################################
############ M I L K  in  N O R W A Y #########################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
NO_Dur_CM<-((1-CMResolve)*(NOlifeexpect-AoO_CM)+CMResolve*(AoR_CM-AoO_CM)) #Duration weighted by lifelong and resolve
NOInc_inc_CM<-CM_NO_prev/NO_Dur_CM #Incidence of CM in %
c(mean(NOInc_inc_CM*100),quantile(NOInc_inc_CM*100,probs = c(0.025,0.975))) #Result inc rate in real %
# Incidence number of people       #Multiply inc with target population
NOInc_CM<-NOpopulation*NOInc_inc_CM #Norwegian population
NOInc_CM<- round(NOInc_CM) #Rounded number so we don't have 0.x people.
c(round(mean(NOInc_CM)),round(quantile(NOInc_CM, probs = c(0.025, 0.975))))    #Incidence in DK
c(round(mean(NOInc_CM/NOpop100)),round(quantile(NOInc_CM/NOpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
NONonsevCM_inc<-NOInc_CM*(1-CMSevere) #People with allergy has a non-severe form
NONonsevCM_inc <- round(NONonsevCM_inc) # #Rounded number so we don't have 0.x people

NOlife_nonCM<-NONonsevCM_inc*(1-CMResolve) #number of people with lifelong non-sev PA
NOlife_nonCM <-round(NOlife_nonCM) # #Rounded number so we don't have 0.x people
NOres_nonCM <-NONonsevCM_inc*CMResolve #number of people with resolved non-sev PA
NOres_nonCM <-round(NOres_nonCM) # #Rounded number so we don't have 0.x people

# Severe
NOSev_incCM<-NOInc_CM*CMSevere #People with allergy has a severe form = anaphylaxis
NOSev_incCM <-round(NOSev_incCM)#Rounded number so we don't have 0.x people

NOlife_sevCM<-NOSev_incCM*(1-CMResolve) #number of people with lifelong severe PA
NOlife_sevCM <-round(NOlife_sevCM)#Rounded number so we don't have 0.x people
NOres_sevCM<-NOSev_incCM*CMResolve #number of people with resolved severe PA
NOres_sevCM<-round(NOres_sevCM)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
NOYLD_CM_non_life <- NOlife_nonCM * (NOlifeexpect-AoO_CM) * DWnon #NOlifeexpect when age 0 = 83.195 mean
#Non-severe resolve #A
NOYLD_CM_non_res <- NOres_nonCM *(AoR_CM-AoO_CM)*DWnon
#Severe lifelong #C
NOYLD_CM_sev_life <- NOlife_sevCM * (NOlifeexpect-AoO_CM) * DWsev
#Severe resolve #D
NOYLD_CM_sev_res <- NOres_sevCM *(AoR_CM-AoO_CM) * DWsev

#Total YLD for CM in NO
NO_YLD_CM<-(NOYLD_CM_non_life + NOYLD_CM_non_res + NOYLD_CM_sev_life + NOYLD_CM_sev_res)
c(mean(NO_YLD_CM), quantile(NO_YLD_CM, probs = c(0.025, 0.975)))
c(mean(NO_YLD_CM/NOpop100), quantile(NO_YLD_CM/NOpop100, probs = c(0.025, 0.975)))
#             2.5%    97.5% 
#54.80715 35.19840 79.78273   #YLD per 100 000

#YLL is found in mortality calculations

#Calculate DALY = YLD + YLL
NO_DALY_CM_NO <- mean(NO_YLD_CM)+mean(YLL_CM_NO)
NO_DALY_CM_lo_NO <- quantile(NO_YLD_CM, probs = c(0.025))+quantile(YLL_CM_NO, probs = c(0.025))
NO_DALY_CM_up_NO <- quantile(NO_YLD_CM, probs = c(0.975))+quantile(YLL_CM_NO, probs = c(0.975))
print(c(NO_DALY_CM_NO,NO_DALY_CM_lo_NO,NO_DALY_CM_up_NO))
#             2.5%    97.5% 
#3074.487 1970.236 4481.785           #Total DALY for CM in Norway

NO_DALY_CM <- mean(NO_YLD_CM/NOpop100)+mean(YLL_CM)
NO_DALY_CM_lo <- quantile(NO_YLD_CM/NOpop100, probs = c(0.025))+quantile(YLL_CM, probs = c(0.025))
NO_DALY_CM_up <- quantile(NO_YLD_CM/NOpop100, probs = c(0.975))+quantile(YLL_CM, probs = c(0.975))
print(c(NO_DALY_CM,NO_DALY_CM_lo,NO_DALY_CM_up))
#             2.5%    97.5% 
#54.95710 35.21839 80.11284       #Milk in Norway per 100 000

NO_DALYc_CM <- (mean(NO_YLD_CM)+mean(YLL_CM_NO))/mean(NOInc_CM)
NO_DALYc_CM_lo <- quantile(NO_YLD_CM/NOInc_CM, probs = c(0.025))+quantile(YLL_CM_NO/mean(NOInc_CM), probs = c(0.025))
NO_DALYc_CM_up <- quantile(NO_YLD_CM/NOInc_CM, probs = c(0.975))+quantile(YLL_CM_NO/mean(NOInc_CM), probs = c(0.975))
print(c(NO_DALYc_CM,NO_DALYc_CM_lo,NO_DALYc_CM_up))
#               2.5%     97.5% 
#1.1585247 0.7317443 1.7652291     #Milk in Norway pr case


#Proportion of YLD in DALY per 100 000
propYLD_NO_CM<-(NO_YLD_CM/NOpop100)/NO_DALY_CM
mean(propYLD_NO_CM)*100





#################################################################
############ P E A N U T  in  F I N L A N D #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
FI_Dur_PA<-((1-PAResolve)*(FIlifeexpect-AoO_PA)+PAResolve*(AoR_PA-AoO_PA)) #Duration weighted by lifelong and resolve
FIInc_inc_PA<-PA_FI_prev/FI_Dur_PA #Incidence of PA in %
c(mean(FIInc_inc_PA*100),quantile(FIInc_inc_PA*100,probs = c(0.025,0.975))) #Result inc rate real %
# Incidence number of people    #Multiply inc with target population
FIInc_PA<-FIpopulation*FIInc_inc_PA #Finnish population
FIInc_PA<- round(FIInc_PA) #Rounded number so we don't have 0.x people.
c(round(mean(FIInc_PA)),round(quantile(FIInc_PA, probs = c(0.025, 0.975))))    #Incidence in DK
c(round(mean(FIInc_PA/FIpop100)),round(quantile(FIInc_PA/FIpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
FINonsev_inc<-FIInc_PA*(1-PASevere) #People with allergy has a non-severe form
FINonsev_inc <- round(FINonsev_inc) # #Rounded number so we don't have 0.x people

FIlife_nonPA<-FINonsev_inc*(1-PAResolve) #number of people with lifelong non-sev PA
FIlife_nonPA <-round(FIlife_nonPA) # #Rounded number so we don't have 0.x people
FIres_nonPA <-FINonsev_inc*PAResolve #number of people with resolved non-sev PA
FIres_nonPA <-round(FIres_nonPA) # #Rounded number so we don't have 0.x people

# Severe
FISev_inc<-FIInc_PA*PASevere #People with allergy has a severe form = anaphylaxis
FISev_inc <-round(FISev_inc)#Rounded number so we don't have 0.x people

FIlife_sevPA<-FISev_inc*(1-PAResolve) #number of people with lifelong severe PA
FIlife_sevPA <-round(FIlife_sevPA)#Rounded number so we don't have 0.x people
FIres_sevPA<-FISev_inc*PAResolve #number of people with resolved severe PA
FIres_sevPA<-round(FIres_sevPA)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
FIYLD_PA_non_life <- FIlife_nonPA * (FIlifeexpect-AoO_PA) * DWnon #FIlifeexpect when age 0 = 81.575 mean
#Non-severe resolve #A
FIYLD_PA_non_res <- FIres_nonPA *(AoR_PA-AoO_PA)*DWnon
#Severe lifelong #C
FIYLD_PA_sev_life <- FIlife_sevPA * (FIlifeexpect-AoO_PA) * DWsev
#Severe resolve #D
FIYLD_PA_sev_res <- FIres_sevPA *(AoR_PA-AoO_PA) * DWsev

#Total YLD for PA in FI
FI_YLD_PA<-(FIYLD_PA_non_life + FIYLD_PA_non_res + FIYLD_PA_sev_life + FIYLD_PA_sev_res)
c(mean(FI_YLD_PA), quantile(FI_YLD_PA, probs = c(0.025, 0.975)))
c(mean(FI_YLD_PA/FIpop100), quantile(FI_YLD_PA/FIpop100, probs = c(0.025, 0.975)))
#             2.5%     97.5% 
#75.15123  42.55343 118.08150  #YLD per 100 000

#YLL is from mortality calculations

#Calculate DALY = YLD + YLL
FI_DALY_PA_FI <- mean(FI_YLD_PA)+mean(YLL_PA_FI)
FI_DALY_PA_lo_FI <- quantile(FI_YLD_PA, probs = c(0.025))+quantile(YLL_PA_FI, probs = c(0.025))
FI_DALY_PA_up_FI <- quantile(FI_YLD_PA, probs = c(0.975))+quantile(YLL_PA_FI, probs = c(0.975))
print(c(FI_DALY_PA_FI,FI_DALY_PA_lo_FI,FI_DALY_PA_up_FI))
#             2.5%    97.5% 
#4248.435 2401.440 6680.033          #Total DALY for PA in Finland

FI_DALY_PA <- mean(FI_YLD_PA/FIpop100)+mean(YLL_PA)
FI_DALY_PA_lo <- quantile(FI_YLD_PA/FIpop100, probs = c(0.025))+quantile(YLL_PA, probs = c(0.025))
FI_DALY_PA_up <- quantile(FI_YLD_PA/FIpop100, probs = c(0.975))+quantile(YLL_PA, probs = c(0.975))
print(c(FI_DALY_PA,FI_DALY_PA_lo,FI_DALY_PA_up))
#              2.5%     97.5% 
#75.34456  42.58873 118.46813  #Peanuts in Finland per 100 000

FI_DALYc_PA <- (mean(FI_YLD_PA)+mean(YLL_PA_FI))/mean(FIInc_PA)
FI_DALYc_PA_lo <- quantile(FI_YLD_PA/FIInc_PA, probs = c(0.025))+quantile(YLL_PA_FI/mean(FIInc_PA), probs = c(0.025))
FI_DALYc_PA_up <- quantile(FI_YLD_PA/FIInc_PA, probs = c(0.975))+quantile(YLL_PA_FI/mean(FIInc_PA), probs = c(0.975))
print(c(FI_DALYc_PA,FI_DALYc_PA_lo,FI_DALYc_PA_up))
#             2.5%    97.5% 
#3.212010 2.499935 3.986206          #Peanuts in Finland pr case

#Proportion of YLD in DALY per 100 000
propYLD_FI_PA<-(FI_YLD_PA/FIpop100)/FI_DALY_PA
mean(propYLD_FI_PA)*100



#################################################################
############ TREE NUT  in  F I N L A N D #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
FI_Dur_TN<-((1-TNResolve)*(FIlifeexpect-AoO_TN)+TNResolve*(AoR_TN-AoO_TN)) #Duration weighted by lifelong and resolve
FIInc_inc_TN<-TN_FI_prev/FI_Dur_TN #Incidence of TN in %
c(mean(FIInc_inc_TN*100),quantile(FIInc_inc_TN*100,probs = c(0.025,0.975)))
# Incidence number of people     #Multiply inc with target population
FIInc_TN<-FIpopulation*FIInc_inc_TN #Finnish population
FIInc_TN<- round(FIInc_TN) #Rounded number so we don't have 0.x people.
c(round(mean(FIInc_TN)),round(quantile(FIInc_TN, probs = c(0.025, 0.975))))    #Incidence in DK
c(round(mean(FIInc_TN/FIpop100)),round(quantile(FIInc_TN/FIpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
FINonsevTN_inc<-FIInc_TN*(1-TNSevere) #People with allergy has a non-severe form
FINonsevTN_inc <- round(FINonsevTN_inc) # #Rounded number so we don't have 0.x people

FIlife_nonTN<-FINonsevTN_inc*(1-TNResolve) #number of people with lifelong non-sev PA
FIlife_nonTN <-round(FIlife_nonTN) # #Rounded number so we don't have 0.x people
FIres_nonTN <-FINonsevTN_inc*TNResolve #number of people with resolved non-sev PA
FIres_nonTN <-round(FIres_nonTN) # #Rounded number so we don't have 0.x people

# Severe
FISev_incTN<-FIInc_TN*TNSevere #People with allergy has a severe form = anaphylaxis
FISev_incTN <-round(FISev_incTN)#Rounded number so we don't have 0.x people

FIlife_sevTN<-FISev_incTN*(1-TNResolve) #number of people with lifelong severe PA
FIlife_sevTN <-round(FIlife_sevTN)#Rounded number so we don't have 0.x people
FIres_sevTN<-FISev_incTN*TNResolve #number of people with resolved severe PA
FIres_sevTN<-round(FIres_sevTN)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
FIYLD_TN_non_life <- FIlife_nonTN * (FIlifeexpect-AoO_TN) * DWnon #FIlifeexpect when age 0 = 81.575 mean
#Non-severe resolve #A
FIYLD_TN_non_res <- FIres_nonTN *(AoR_TN-AoO_TN)*DWnon
#Severe lifelong #C
FIYLD_TN_sev_life <- FIlife_sevTN * (FIlifeexpect-AoO_TN) * DWsev
#Severe resolve #D
FIYLD_TN_sev_res <- FIres_sevTN *(AoR_TN-AoO_TN) * DWsev

#Total YLD for TN in FI
FI_YLD_TN<-(FIYLD_TN_non_life + FIYLD_TN_non_res + FIYLD_TN_sev_life + FIYLD_TN_sev_res)
c(mean(FI_YLD_TN), quantile(FI_YLD_TN, probs = c(0.025, 0.975)))
c(mean(FI_YLD_TN/FIpop100), quantile(FI_YLD_TN/FIpop100, probs = c(0.025, 0.975)))
#             2.5%    97.5% 
#30.00488 13.23017 53.53616    #YLD per 100 000

#YLL is calculated at mortality

#Calculate DALY = YLD + YLL
FI_DALY_TN_FI <- mean(FI_YLD_TN)+mean(YLL_TN_FI)
FI_DALY_TN_lo_FI <- quantile(FI_YLD_TN, probs = c(0.025))+quantile(YLL_TN_FI, probs = c(0.025))
FI_DALY_TN_up_FI <- quantile(FI_YLD_TN, probs = c(0.975))+quantile(YLL_TN_FI, probs = c(0.975))
print(c(FI_DALY_TN_FI,FI_DALY_TN_lo_FI,FI_DALY_TN_up_FI))
#               2.5%     97.5% 
#1697.7310  747.2119 3030.0310           #Total DALY for TN in Finland

FI_DALY_TN <- mean(FI_YLD_TN/FIpop100)+mean(YLL_TN)
FI_DALY_TN_lo <- quantile(FI_YLD_TN/FIpop100, probs = c(0.025))+quantile(YLL_TN, probs = c(0.025))
FI_DALY_TN_up <- quantile(FI_YLD_TN/FIpop100, probs = c(0.975))+quantile(YLL_TN, probs = c(0.975))
print(c(FI_DALY_TN,FI_DALY_TN_lo,FI_DALY_TN_up))
#             2.5%    97.5% 
#30.10869 13.25155 53.73658       #Nuts in Finland per 100 000

FI_DALYc_TN <- (mean(FI_YLD_TN)+mean(YLL_TN_FI))/mean(FIInc_TN)
FI_DALYc_TN_lo <- quantile(FI_YLD_TN/FIInc_TN, probs = c(0.025))+quantile(YLL_TN_FI/mean(FIInc_TN), probs = c(0.025))
FI_DALYc_TN_up <- quantile(FI_YLD_TN/FIInc_TN, probs = c(0.975))+quantile(YLL_TN_FI/mean(FIInc_TN), probs = c(0.975))
print(c(FI_DALYc_TN,FI_DALYc_TN_lo,FI_DALYc_TN_up))
#             2.5%    97.5% 
#5.096205 3.853592 6.529467     #Treenuts in Finland pr case

#Proportion of YLD in DALY per 100 000
propYLD_FI_TN<-(FI_YLD_TN/FIpop100)/FI_DALY_TN
mean(propYLD_FI_TN)*100


#################################################################
############ M I L K  in  F I N L A N D #########################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
FI_Dur_CM<-((1-CMResolve)*(FIlifeexpect-AoO_CM)+CMResolve*(AoR_CM-AoO_CM)) #Duration weighted by lifelong and resolve
FIInc_inc_CM<-CM_FI_prev/FI_Dur_CM #Incidence of CM in %
c(mean(FIInc_inc_CM*100),quantile(FIInc_inc_CM*100,probs = c(0.025,0.975))) #Result inc rate in real %
# Incidence number of people       #Multiply inc with target population
FIInc_CM<-FIpopulation*FIInc_inc_CM #Finnish population
FIInc_CM<- round(FIInc_CM) #Rounded number so we don't have 0.x people.
c(round(mean(FIInc_CM)),round(quantile(FIInc_CM, probs = c(0.025, 0.975))))    #Incidence in DK
c(round(mean(FIInc_CM/FIpop100)),round(quantile(FIInc_CM/FIpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
FINonsevCM_inc<-FIInc_CM*(1-CMSevere) #People with allergy has a non-severe form
FINonsevCM_inc <- round(FINonsevCM_inc) # #Rounded number so we don't have 0.x people

FIlife_nonCM<-FINonsevCM_inc*(1-CMResolve) #number of people with lifelong non-sev PA
FIlife_nonCM <-round(FIlife_nonCM) # #Rounded number so we don't have 0.x people
FIres_nonCM <-FINonsevCM_inc*CMResolve #number of people with resolved non-sev PA
FIres_nonCM <-round(FIres_nonCM) # #Rounded number so we don't have 0.x people

# Severe
FISev_incCM<-FIInc_CM*CMSevere #People with allergy has a severe form = anaphylaxis
FISev_incCM <-round(FISev_incCM)#Rounded number so we don't have 0.x people

FIlife_sevCM<-FISev_incCM*(1-CMResolve) #number of people with lifelong severe PA
FIlife_sevCM <-round(FIlife_sevCM)#Rounded number so we don't have 0.x people
FIres_sevCM<-FISev_incCM*CMResolve #number of people with resolved severe PA
FIres_sevCM<-round(FIres_sevCM)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
FIYLD_CM_non_life <- FIlife_nonCM * (FIlifeexpect-AoO_CM) * DWnon #FIlifeexpect when age 0 = 81.575 mean
#Non-severe resolve #A
FIYLD_CM_non_res <- FIres_nonCM *(AoR_CM-AoO_CM)*DWnon
#Severe lifelong #C
FIYLD_CM_sev_life <- FIlife_sevCM * (FIlifeexpect-AoO_CM) * DWsev
#Severe resolve #D
FIYLD_CM_sev_res <- FIres_sevCM *(AoR_CM-AoO_CM) * DWsev

#Total YLD for CM in FI
FI_YLD_CM<-(FIYLD_CM_non_life + FIYLD_CM_non_res + FIYLD_CM_sev_life + FIYLD_CM_sev_res)
c(mean(FI_YLD_CM), quantile(FI_YLD_CM, probs = c(0.025, 0.975)))
c(mean(FI_YLD_CM/FIpop100), quantile(FI_YLD_CM/FIpop100, probs = c(0.025, 0.975)))
#             2.5%    97.5% 
#25.38491 12.24226 44.14404   #YLD per 100 000

#YLL is found in mortality calculations

#Calculate DALY = YLD + YLL
FI_DALY_CM_FI <- mean(FI_YLD_CM)+mean(YLL_CM_FI)
FI_DALY_CM_lo_FI <- quantile(FI_YLD_CM, probs = c(0.025))+quantile(YLL_CM_FI, probs = c(0.025))
FI_DALY_CM_up_FI <- quantile(FI_YLD_CM, probs = c(0.975))+quantile(YLL_CM_FI, probs = c(0.975))
print(c(FI_DALY_CM_FI,FI_DALY_CM_lo_FI,FI_DALY_CM_up_FI))
#               2.5%     97.5% 
#1439.8276  691.4287 2507.7526           #Total DALY for TN in Finland

FI_DALY_CM <- mean(FI_YLD_CM/FIpop100)+mean(YLL_CM)
FI_DALY_CM_lo <- quantile(FI_YLD_CM/FIpop100, probs = c(0.025))+quantile(YLL_CM, probs = c(0.025))
FI_DALY_CM_up <- quantile(FI_YLD_CM/FIpop100, probs = c(0.975))+quantile(YLL_CM, probs = c(0.975))
print(c(FI_DALY_CM,FI_DALY_CM_lo,FI_DALY_CM_up))
#             2.5%    97.5% 
#25.53486 12.26225 44.47415       #Milk in Finland per 100 000

FI_DALYc_CM <- (mean(FI_YLD_CM)+mean(YLL_CM_FI))/mean(FIInc_CM)
FI_DALYc_CM_lo <- quantile(FI_YLD_CM/FIInc_CM, probs = c(0.025))+quantile(YLL_CM_FI/mean(FIInc_CM), probs = c(0.025))
FI_DALYc_CM_up <- quantile(FI_YLD_CM/FIInc_CM, probs = c(0.975))+quantile(YLL_CM_FI/mean(FIInc_CM), probs = c(0.975))
print(c(FI_DALYc_CM,FI_DALYc_CM_lo,FI_DALYc_CM_up))
#               2.5%     97.5% 
#1.1376155 0.7206441 1.7424036    #Milk in Finland pr case

#Proportion of YLD in DALY per 100 000
propYLD_FI_CM<-(FI_YLD_CM/FIpop100)/FI_DALY_CM
mean(propYLD_FI_CM)*100







#################################################################
############ P E A N U T  in  I C E L A N D #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
IC_Dur_PA<-((1-PAResolve)*(IClifeexpect-AoO_PA)+PAResolve*(AoR_PA-AoO_PA)) #Duration weighted by lifelong and resolve
ICInc_inc_PA<-PA_IC_prev/IC_Dur_PA #Incidence of PA in %
c(mean(ICInc_inc_PA*100),quantile(ICInc_inc_PA*100,probs = c(0.025,0.975)))
# Incidence number of people    #Multiply inc with target population
ICInc_PA<-ICpopulation*ICInc_inc_PA #Icelandic population
ICInc_PA<- round(ICInc_PA) #Rounded number so we don't have 0.x people.
c(round(mean(ICInc_PA)),round(quantile(ICInc_PA, probs = c(0.025, 0.975))))    #Incidence in DK
c(round(mean(ICInc_PA/ICpop100)),round(quantile(ICInc_PA/ICpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
ICNonsev_inc<-ICInc_PA*(1-PASevere) #People with allergy has a non-severe form
ICNonsev_inc <- round(ICNonsev_inc) # #Rounded number so we don't have 0.x people

IClife_nonPA<-ICNonsev_inc*(1-PAResolve) #number of people with lifelong non-sev PA
IClife_nonPA <-round(IClife_nonPA) # #Rounded number so we don't have 0.x people
ICres_nonPA <-ICNonsev_inc*PAResolve #number of people with resolved non-sev PA
ICres_nonPA <-round(ICres_nonPA) # #Rounded number so we don't have 0.x people

# Severe
ICSev_inc<-ICInc_PA*PASevere #People with allergy has a severe form = anaphylaxis
ICSev_inc <-round(ICSev_inc)#Rounded number so we don't have 0.x people

IClife_sevPA<-ICSev_inc*(1-PAResolve) #number of people with lifelong severe PA
IClife_sevPA <-round(IClife_sevPA)#Rounded number so we don't have 0.x people
ICres_sevPA<-ICSev_inc*PAResolve #number of people with resolved severe PA
ICres_sevPA<-round(ICres_sevPA)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
ICYLD_PA_non_life <- IClife_nonPA * (IClifeexpect-AoO_PA) * DWnon #IClifeexpect when age 0 = 82.5 mean
#c(mean(ICYLD_PA_non_life), quantile(ICYLD_PA_non_life, probs = c(0.025, 0.975)))
#Non-severe resolve #A
ICYLD_PA_non_res <- ICres_nonPA *(AoR_PA-AoO_PA)*DWnon
#Severe lifelong #C
ICYLD_PA_sev_life <- IClife_sevPA * (IClifeexpect-AoO_PA) * DWsev
#Severe resolve #D
ICYLD_PA_sev_res <- ICres_sevPA *(AoR_PA-AoO_PA) * DWsev

#Total YLD for PA in IC
IC_YLD_PA<-(ICYLD_PA_non_life + ICYLD_PA_non_res + ICYLD_PA_sev_life + ICYLD_PA_sev_res)
c(mean(IC_YLD_PA), quantile(IC_YLD_PA, probs = c(0.025, 0.975)))
c(mean(IC_YLD_PA/ICpop100), quantile(IC_YLD_PA/ICpop100, probs = c(0.025, 0.975)))
#             2.5%    97.5% 
#12.28542  4.11398 24.04911     #YLD per 100 000

#YLL is from mortality calculations

#Calculate DALY = YLD + YLL
IC_DALY_PA_IC <- mean(IC_YLD_PA)+mean(YLL_PA_IC)
IC_DALY_PA_lo_IC <- quantile(IC_YLD_PA, probs = c(0.025))+quantile(YLL_PA_IC, probs = c(0.025))
IC_DALY_PA_up_IC <- quantile(IC_YLD_PA, probs = c(0.975))+quantile(YLL_PA_IC, probs = c(0.975))
print(c(IC_DALY_PA_IC,IC_DALY_PA_lo_IC,IC_DALY_PA_up_IC))
#             2.5%    97.5% 
#48.59779 16.15911 95.16353           #Total DALY for PA in Iceland

IC_DALY_PA <- mean(IC_YLD_PA/ICpop100)+mean(YLL_PA)
IC_DALY_PA_lo <- quantile(IC_YLD_PA/ICpop100, probs = c(0.025))+quantile(YLL_PA, probs = c(0.025))
IC_DALY_PA_up <- quantile(IC_YLD_PA/ICpop100, probs = c(0.975))+quantile(YLL_PA, probs = c(0.975))
print(c(IC_DALY_PA,IC_DALY_PA_lo,IC_DALY_PA_up))
#               2.5%     97.5% 
#12.478761  4.149277 24.435742      #Peanut Iceland per 100 000

IC_DALYc_PA <- (mean(IC_YLD_PA)+mean(YLL_PA_IC))/mean(ICInc_PA)
IC_DALYc_PA_lo <- quantile(IC_YLD_PA/ICInc_PA, probs = c(0.025))+quantile(YLL_PA_IC/mean(ICInc_PA), probs = c(0.025))
IC_DALYc_PA_up <- quantile(IC_YLD_PA/ICInc_PA, probs = c(0.975))+quantile(YLL_PA_IC/mean(ICInc_PA), probs = c(0.975))
print(c(IC_DALYc_PA,IC_DALYc_PA_lo,IC_DALYc_PA_up))
#             2.5%    97.5% 
#3.267913 2.339784 4.282522    #Peanuts in Iceland pr case

#Proportion of YLD in DALY per 100 000
propYLD_IC_PA<-(IC_YLD_PA/ICpop100)/IC_DALY_PA
mean(propYLD_IC_PA)*100


################################################################
############ TREE NUT  in  I C E L A N D #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
IC_Dur_TN<-((1-TNResolve)*(IClifeexpect-AoO_TN)+TNResolve*(AoR_TN-AoO_TN)) #Duration weighted by lifelong and resolve
ICInc_inc_TN<-TN_IC_prev/IC_Dur_TN #Incidence of TN in %
c(mean(ICInc_inc_TN*100),quantile(ICInc_inc_TN*100,probs = c(0.025,0.975)))
# Incidence number of people     #Multiply inc with target population
ICInc_TN<-ICpopulation*ICInc_inc_TN #Icelandic population
ICInc_TN<- round(ICInc_TN) #Rounded number so we don't have 0.x people.
c(round(mean(ICInc_TN)),round(quantile(ICInc_TN, probs = c(0.025, 0.975))))    #Incidence in DK
c(round(mean(ICInc_TN/ICpop100)),round(quantile(ICInc_TN/ICpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
ICNonsevTN_inc<-ICInc_TN*(1-TNSevere) #People with allergy has a non-severe form
ICNonsevTN_inc <- round(ICNonsevTN_inc) # #Rounded number so we don't have 0.x people

IClife_nonTN<-ICNonsevTN_inc*(1-TNResolve) #number of people with lifelong non-sev PA
IClife_nonTN <-round(IClife_nonTN) # #Rounded number so we don't have 0.x people
ICres_nonTN <-ICNonsevTN_inc*TNResolve #number of people with resolved non-sev PA
ICres_nonTN <-round(ICres_nonTN) # #Rounded number so we don't have 0.x people

# Severe
ICSev_incTN<-ICInc_TN*TNSevere #People with allergy has a severe form = anaphylaxis
ICSev_incTN <-round(ICSev_incTN)#Rounded number so we don't have 0.x people

IClife_sevTN<-ICSev_incTN*(1-TNResolve) #number of people with lifelong severe PA
IClife_sevTN <-round(IClife_sevTN)#Rounded number so we don't have 0.x people
ICres_sevTN<-ICSev_incTN*TNResolve #number of people with resolved severe PA
ICres_sevTN<-round(ICres_sevTN)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
ICYLD_TN_non_life <- IClife_nonTN * (IClifeexpect-AoO_TN) * DWnon #IClifeexpect when age 0 = 82.5 mean
#Non-severe resolve #A
ICYLD_TN_non_res <- ICres_nonTN *(AoR_TN-AoO_TN)*DWnon
#Severe lifelong #C
ICYLD_TN_sev_life <- IClife_sevTN * (IClifeexpect-AoO_TN) * DWsev
#Severe resolve #D
ICYLD_TN_sev_res <- ICres_sevTN *(AoR_TN-AoO_TN) * DWsev

#Total YLD for TN in IC
IC_YLD_TN<-(ICYLD_TN_non_life + ICYLD_TN_non_res + ICYLD_TN_sev_life + ICYLD_TN_sev_res)
c(mean(IC_YLD_TN), quantile(IC_YLD_TN, probs = c(0.025, 0.975)))
c(mean(IC_YLD_TN/ICpop100), quantile(IC_YLD_TN/ICpop100, probs = c(0.025, 0.975)))
#               2.5%     97.5% 
#6.261605  2.269497 12.685892   #YLD per 100 000

#YLL is calculated at mortality

#Calculate DALY = YLD + YLL
IC_DALY_TN_IC <- mean(IC_YLD_TN)+mean(YLL_TN_IC)
IC_DALY_TN_lo_IC <- quantile(IC_YLD_TN, probs = c(0.025))+quantile(YLL_TN_IC, probs = c(0.025))
IC_DALY_TN_up_IC <- quantile(IC_YLD_TN, probs = c(0.975))+quantile(YLL_TN_IC, probs = c(0.975))
print(c(IC_DALY_TN_IC,IC_DALY_TN_lo_IC,IC_DALY_TN_up_IC))
#               2.5%     97.5% 
#24.789713  8.921702 50.184968          #Total DALY for TN in Iceland

IC_DALY_TN <- mean(IC_YLD_TN/ICpop100)+mean(YLL_TN)
IC_DALY_TN_lo <- quantile(IC_YLD_TN/ICpop100, probs = c(0.025))+quantile(YLL_TN, probs = c(0.025))
IC_DALY_TN_up <- quantile(IC_YLD_TN/ICpop100, probs = c(0.975))+quantile(YLL_TN, probs = c(0.975))
print(c(IC_DALY_TN,IC_DALY_TN_lo,IC_DALY_TN_up))
#               2.5%     97.5% 
#6.365411  2.290882 12.886312    #Nuts in Iceland per 100 000

IC_DALYc_TN <- (mean(IC_YLD_TN)+mean(YLL_TN_IC))/mean(ICInc_TN)
IC_DALYc_TN_lo <- quantile(IC_YLD_TN/ICInc_TN, probs = c(0.025))+quantile(YLL_TN_IC/mean(ICInc_TN), probs = c(0.025))
IC_DALYc_TN_up <- quantile(IC_YLD_TN/ICInc_TN, probs = c(0.975))+quantile(YLL_TN_IC/mean(ICInc_TN), probs = c(0.975))
print(c(IC_DALYc_TN,IC_DALYc_TN_lo,IC_DALYc_TN_up))
#             2.5%    97.5% 
#5.648017 3.889665 7.907275    #Treenuts in Iceland pr case


#Proportion of YLD in DALY per 100 000
propYLD_IC_TN<-(IC_YLD_TN/ICpop100)/IC_DALY_TN
mean(propYLD_IC_TN)*100



#################################################################
############ Worst case TREE NUT  in  I C E L A N D #####################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
IC_Dur_TNW<-((1-TNResolve)*(IClifeexpect-AoO_TN)+TNResolve*(AoR_TN-AoO_TN)) #Duration weighted by lifelong and resolve
ICInc_inc_TNW<-TNW_IC_prev/IC_Dur_TNW #Incidence of worst TN in %
c(mean(ICInc_inc_TNW*100),quantile(ICInc_inc_TNW*100,probs = c(0.025,0.975))) #Result inc rate real %
# Incidence number of people     #Multiply inc with target population
ICInc_TNW<-ICpopulation*ICInc_inc_TNW #Icelandic population
ICInc_TNW<- round(ICInc_TNW) #Rounded number so we don't have 0.x people.
c(round(mean(ICInc_TNW)),round(quantile(ICInc_TNW, probs = c(0.025, 0.975))))    #Incidence in DK
c(round(mean(ICInc_TNW/ICpop100)),round(quantile(ICInc_TNW/ICpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
ICNonsevTNW_inc<-ICInc_TNW*(1-TNSevere) #People with allergy has a non-severe form
ICNonsevTNW_inc <- round(ICNonsevTNW_inc) # #Rounded number so we don't have 0.x people

IClife_nonTNW<-ICNonsevTNW_inc*(1-TNResolve) #number of people with lifelong non-sev PA
IClife_nonTNW <-round(IClife_nonTNW) # #Rounded number so we don't have 0.x people
ICres_nonTNW <-ICNonsevTNW_inc*TNResolve #number of people with resolved non-sev PA
ICres_nonTNW <-round(ICres_nonTNW) # #Rounded number so we don't have 0.x people

# Severe
ICSev_incTNW<-ICInc_TNW*TNSevere #People with allergy has a severe form = anaphylaxis
ICSev_incTNW <-round(ICSev_incTNW)#Rounded number so we don't have 0.x people

IClife_sevTNW<-ICSev_incTNW*(1-TNResolve) #number of people with lifelong severe PA
IClife_sevTNW <-round(IClife_sevTNW)#Rounded number so we don't have 0.x people
ICres_sevTNW<-ICSev_incTNW*TNResolve #number of people with resolved severe PA
ICres_sevTNW<-round(ICres_sevTNW)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
ICYLD_TNW_non_life <- IClife_nonTNW * (IClifeexpect-AoO_TN) * DWnon #IClifeexpect when age 0 = 82.5 mean
#Non-severe resolve #A
ICYLD_TNW_non_res <- ICres_nonTNW *(AoR_TN-AoO_TN)*DWnon
#Severe lifelong #C
ICYLD_TNW_sev_life <- IClife_sevTNW * (IClifeexpect-AoO_TN) * DWsev
#Severe resolve #D
ICYLD_TNW_sev_res <- ICres_sevTNW *(AoR_TN-AoO_TN) * DWsev

#Total YLD for worst TN in IC
IC_YLD_TNW<-(ICYLD_TNW_non_life + ICYLD_TNW_non_res + ICYLD_TNW_sev_life + ICYLD_TNW_sev_res)
c(mean(IC_YLD_TNW), quantile(IC_YLD_TNW, probs = c(0.025, 0.975)))
c(mean(IC_YLD_TNW/ICpop100), quantile(IC_YLD_TNW/ICpop100, probs = c(0.025, 0.975)))
#               2.5%     97.5% 
#9.185017  2.639395 18.569719  #YLD per 100

#YLL is from mortality calculations

#Calculate DALY = YLD + YLL
IC_DALY_TNW_IC <- mean(IC_YLD_TNW)+mean(YLL_TN_IC)
IC_DALY_TNW_lo_IC <- quantile(IC_YLD_TNW, probs = c(0.025))+quantile(YLL_TN_IC, probs = c(0.025))
IC_DALY_TNW_up_IC <- quantile(IC_YLD_TNW, probs = c(0.975))+quantile(YLL_TN_IC, probs = c(0.975))
print(c(IC_DALY_TNW_IC,IC_DALY_TNW_lo_IC,IC_DALY_TNW_up_IC))
#             2.5%    97.5% 
#36.17477 10.36225 73.09918           #Total DALY for TNW in Iceland

IC_DALY_TNW <- mean(IC_YLD_TNW/ICpop100)+mean(YLL_TN)
IC_DALY_TNW_lo <- quantile(IC_YLD_TNW/ICpop100, probs = c(0.025))+quantile(YLL_TN, probs = c(0.025))
IC_DALY_TNW_up <- quantile(IC_YLD_TNW/ICpop100, probs = c(0.975))+quantile(YLL_TN, probs = c(0.975))
print(c(IC_DALY_TNW,IC_DALY_TNW_lo,IC_DALY_TNW_up))
#               2.5%     97.5% 
#9.288824  2.660780 18.770140     #Worst case nut Iceland per 100 000

IC_DALYc_TNW <- (mean(IC_YLD_TNW)+mean(YLL_TN_IC))/mean(ICInc_TNW)
IC_DALYc_TNW_lo <- quantile(IC_YLD_TNW/ICInc_TN, probs = c(0.025))+quantile(YLL_TN_IC/mean(ICInc_TNW), probs = c(0.025))
IC_DALYc_TNW_up <- quantile(IC_YLD_TNW/ICInc_TN, probs = c(0.975))+quantile(YLL_TN_IC/mean(ICInc_TNW), probs = c(0.975))
print(c(IC_DALYc_TNW,IC_DALYc_TNW_lo,IC_DALYc_TNW_up))
#              2.5%     97.5% 
#5.457212  1.786507 32.617733     #Worst case Treenuts in Denmark pr case


#Proportion of YLD in DALY per 100 000
propYLD_IC_TNW<-(IC_YLD_TNW/ICpop100)/IC_DALY_TNW
mean(propYLD_IC_TNW)*100




#################################################################
############ M I L K  in  I C E L A N D #########################

# Incidence
# Convert the prevalence to incidence       #Inc = prev / duration
# Create a total, weighted duration for lifelong and resolve
IC_Dur_CM<-((1-CMResolve)*(IClifeexpect-AoO_CM)+CMResolve*(AoR_CM-AoO_CM)) #Duration weighted by lifelong and resolve
ICInc_inc_CM<-CM_IC_prev/IC_Dur_CM #Incidence of CM in %
c(mean(ICInc_inc_CM*100),quantile(ICInc_inc_CM*100,probs = c(0.025,0.975))) #Result inc rate real %
# Incidence number of people       #Multiply inc with target population
ICInc_CM<-ICpopulation*ICInc_inc_CM #Icelandic population
ICInc_CM<- round(ICInc_CM) #Rounded number so we don't have 0.x people.
c(round(mean(ICInc_CM)),round(quantile(ICInc_CM, probs = c(0.025, 0.975))))    #Incidence in DK
c(round(mean(ICInc_CM/ICpop100)),round(quantile(ICInc_CM/ICpop100, probs = c(0.025, 0.975))))   #Incidence per 100 000

# Non-severe
ICNonsevCM_inc<-ICInc_CM*(1-CMSevere) #People with allergy has a non-severe form
ICNonsevCM_inc <- round(ICNonsevCM_inc) # #Rounded number so we don't have 0.x people

IClife_nonCM<-ICNonsevCM_inc*(1-CMResolve) #number of people with lifelong non-sev PA
IClife_nonCM <-round(IClife_nonCM) # #Rounded number so we don't have 0.x people
ICres_nonCM <-ICNonsevCM_inc*CMResolve #number of people with resolved non-sev PA
ICres_nonCM <-round(ICres_nonCM) # #Rounded number so we don't have 0.x people

# Severe
ICSev_incCM<-ICInc_CM*CMSevere #People with allergy has a severe form = anaphylaxis
ICSev_incCM <-round(ICSev_incCM)#Rounded number so we don't have 0.x people

IClife_sevCM<-ICSev_incCM*(1-CMResolve) #number of people with lifelong severe PA
IClife_sevCM <-round(IClife_sevCM)#Rounded number so we don't have 0.x people
ICres_sevCM<-ICSev_incCM*CMResolve #number of people with resolved severe PA
ICres_sevCM<-round(ICres_sevCM)#Rounded number so we don't have 0.x people


# DALY
# Calculate YLD = incidence * duration * DW
#Non-severe lifelong #B
ICYLD_CM_non_life <- IClife_nonCM * (IClifeexpect-AoO_CM) * DWnon #IClifeexpect when age 0 = 82.5 mean
#Non-severe resolve #A
ICYLD_CM_non_res <- ICres_nonCM *(AoR_CM-AoO_CM)*DWnon
#Severe lifelong #C
ICYLD_CM_sev_life <- IClife_sevCM * (IClifeexpect-AoO_CM) * DWsev
#Severe resolve #D
ICYLD_CM_sev_res <- ICres_sevCM *(AoR_CM-AoO_CM) * DWsev

#Total YLD for CM in IC
IC_YLD_CM<-(ICYLD_CM_non_life + ICYLD_CM_non_res + ICYLD_CM_sev_life + ICYLD_CM_sev_res)
c(mean(IC_YLD_CM), quantile(IC_YLD_CM, probs = c(0.025, 0.975)))
c(mean(IC_YLD_CM/ICpop100), quantile(IC_YLD_CM/ICpop100, probs = c(0.025, 0.975)))
#               2.5%     97.5% 
#10.855159  2.925719 23.058648    #YLD per 100 000

#YLL is found in mortality calculations

#Calculate DALY = YLD + YLL
IC_DALY_CM_IC <- mean(IC_YLD_CM)+mean(YLL_CM_IC)
IC_DALY_CM_lo_IC <- quantile(IC_YLD_CM, probs = c(0.025))+quantile(YLL_CM_IC, probs = c(0.025))
IC_DALY_CM_up_IC <- quantile(IC_YLD_CM, probs = c(0.975))+quantile(YLL_CM_IC, probs = c(0.975))
print(c(IC_DALY_CM_IC,IC_DALY_CM_lo_IC,IC_DALY_CM_up_IC))
#             2.5%    97.5% 
#42.85874 11.47189 91.08612            #Total DALY for CM in Iceland

IC_DALY_CM <- mean(IC_YLD_CM/ICpop100)+mean(YLL_CM)
IC_DALY_CM_lo <- quantile(IC_YLD_CM/ICpop100, probs = c(0.025))+quantile(YLL_CM, probs = c(0.025))
IC_DALY_CM_up <- quantile(IC_YLD_CM/ICpop100, probs = c(0.975))+quantile(YLL_CM, probs = c(0.975))
print(c(IC_DALY_CM,IC_DALY_CM_lo,IC_DALY_CM_up))
#               2.5%     97.5% 
#11.005110  2.945709 23.388759     #Milk in Iceland per 100 000

IC_DALYc_CM <- (mean(IC_YLD_CM)+mean(YLL_CM_IC))/mean(ICInc_CM)
IC_DALYc_CM_lo <- quantile(IC_YLD_CM/ICInc_CM, probs = c(0.025))+quantile(YLL_CM_IC/mean(ICInc_CM), probs = c(0.025))
IC_DALYc_CM_up <- quantile(IC_YLD_CM/ICInc_CM, probs = c(0.975))+quantile(YLL_CM_IC/mean(ICInc_CM), probs = c(0.975))
print(c(IC_DALYc_CM,IC_DALYc_CM_lo,IC_DALYc_CM_up))
#               2.5%     97.5% 
#1.1608102 0.6664463 1.8266494     #Milk in Iceland pr case


#Proportion of YLD in DALY per 100 000
propYLD_IC_CM<-(IC_YLD_CM/ICpop100)/IC_DALY_CM
mean(propYLD_IC_CM)*100







###################################
##################################
#Total DALY in each country

#TOTAL DALY in Denmark
SUM_DALY_DK<-(DK_YLD_PA+YLL_PA_DK)+(DK_YLD_TN+YLL_TN_DK)+(DK_YLD_CM+YLL_CM_DK)
c(mean(SUM_DALY_DK), quantile(SUM_DALY_DK, probs = c(0.025, 0.975)))
#With worst case
SUM_DALY_DKW<-(DK_YLD_PA+YLL_PA_DK)+(DK_YLD_TNW+YLL_TN_DK)+(DK_YLD_CM+YLL_CM_DK)
c(mean(SUM_DALY_DKW), quantile(SUM_DALY_DKW, probs = c(0.025, 0.975)))

#TOTAL DALY in Sweden
SUM_DALY_SE<-(SE_YLD_PA+YLL_PA_SE)+(SE_YLD_TN+YLL_TN_SE)+(SE_YLD_CM+YLL_CM_SE)
c(mean(SUM_DALY_SE), quantile(SUM_DALY_SE, probs = c(0.025, 0.975)))
#With worst case
SUM_DALY_SEW<-(SE_YLD_PA+YLL_PA_SE)+(SE_YLD_TNW+YLL_TN_SE)+(SE_YLD_CM+YLL_CM_SE)
c(mean(SUM_DALY_SEW), quantile(SUM_DALY_SEW, probs = c(0.025, 0.975)))

#TOTAL DALY in Norway
SUM_DALY_NO<-(NO_YLD_PA+YLL_PA_NO)+(NO_YLD_TN+YLL_TN_NO)+(NO_YLD_CM+YLL_CM_NO)
c(mean(SUM_DALY_NO), quantile(SUM_DALY_NO, probs = c(0.025, 0.975)))

#TOTAL DALY in Finland
SUM_DALY_FI<-(FI_YLD_PA+YLL_PA_FI)+(FI_YLD_TN+YLL_TN_FI)+(FI_YLD_CM+YLL_CM_FI)
c(mean(SUM_DALY_FI), quantile(SUM_DALY_FI, probs = c(0.025, 0.975)))

#TOTAL DALY in Iceland
SUM_DALY_IC<-(IC_YLD_PA+YLL_PA_IC)+(IC_YLD_TN+YLL_TN_IC)+(IC_YLD_CM+YLL_CM_IC)
c(mean(SUM_DALY_IC), quantile(SUM_DALY_IC, probs = c(0.025, 0.975)))
#With worst case
SUM_DALY_ICW<-(IC_YLD_PA+YLL_PA_IC)+(IC_YLD_TNW+YLL_TN_IC)+(IC_YLD_CM+YLL_CM_IC)
c(mean(SUM_DALY_ICW), quantile(SUM_DALY_ICW, probs = c(0.025, 0.975)))



###################################
##################################
#Total DALY for each food allergy

#Peanut
SUM_DALY_PA<-(DK_YLD_PA+YLL_PA_DK)+(SE_YLD_PA+YLL_PA_SE)+(NO_YLD_PA+YLL_PA_NO)+(FI_YLD_PA+YLL_PA_FI)+(IC_YLD_PA+YLL_PA_IC)
c(mean(SUM_DALY_PA), quantile(SUM_DALY_PA, probs = c(0.025, 0.975)))

#Tree nut
SUM_DALY_TN<-(DK_YLD_TN+YLL_TN_DK)+(SE_YLD_TN+YLL_TN_SE)+(NO_YLD_TN+YLL_TN_NO)+(FI_YLD_TN+YLL_TN_FI)+(IC_YLD_TN+YLL_TN_IC)
c(mean(SUM_DALY_TN), quantile(SUM_DALY_TN, probs = c(0.025, 0.975)))

#Worst case tree nut
SUM_DALY_TNW<-(DK_YLD_TNW+YLL_TN_DK)+(SE_YLD_TNW+YLL_TN_SE)+(NO_YLD_TN+YLL_TN_NO)+(FI_YLD_TN+YLL_TN_FI)+(IC_YLD_TNW+YLL_TN_IC)
c(mean(SUM_DALY_TNW), quantile(SUM_DALY_TNW, probs = c(0.025, 0.975)))

#Milk
SUM_DALY_CM<-(DK_YLD_CM+YLL_CM_DK)+(SE_YLD_CM+YLL_CM_SE)+(NO_YLD_CM+YLL_CM_NO)+(FI_YLD_CM+YLL_CM_FI)+(IC_YLD_CM+YLL_CM_IC)
c(mean(SUM_DALY_CM), quantile(SUM_DALY_CM, probs = c(0.025, 0.975)))


