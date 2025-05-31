##############################
###### D A T A  S E T S ######
##############################

#First part is studies for prevalence, then severity, and later resolve.



#################################
################################
########## PREVALENCE ###########

##############################
########## D E N M A R K ####

### T R E E N U T S #################
#Osterballe05
median(c(0.64, 3.31)) #=1.975%

#Osterballe09
median(c(0.032,0.432,1.056,0.08)) #=0.256%
#####
studies <- data.frame(
  year = c("Osterballe05","Osterballe09"),
  n = c(936,1272), 
  prevalence = c(NA,NA),  
  median = c(0.01975,0.00256),
  lower_CI = c(NA,NA),
  upper_CI = c(NA,NA)
)


#######################
### Worst case treenut ###  
#Sum of all types of nut #assumed no cross reaction
#Osterballe05
sum(c(0.64,3.31)) #= 3.95%
#Osterballe09
#(c(0.2,2.7,6.6,0.5)*0.16)
sum(c(0.032,0.432,1.056,0.08)) #=1.6%
####
studies <- data.frame(
  year = c("Osterballe05","Osterballe09"),
  n = c(936,1272), 
  prevalence = c(0.0395,0.016),  
  median = c(NA,NA),
  lower_CI = c(NA,NA),
  upper_CI = c(NA,NA)
)


### P E A N U T #################
studies <- data.frame(
  year = c("Osterballe05A","Osterballe05B","Osterballe05C","Osterballe05D","Osterballe09","Eller","Mortz"),
  n = c(111,486,301,936,1272,562,979), 
  prevalence = c(0,0.002,0,0.004,0.006,0.0071,0.005),  
  median = c(NA,NA,NA,NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,NA,0.002,NA,NA),
  upper_CI = c(NA,NA,NA,NA,0.014,NA,NA)
)


#### M I L K #################
studies <- data.frame(
  year = c("Osterballe05A","Osterballe05B","Osterballe05C","Osterballe05D","Osterballe09","Eller"),
  n = c(111,486,301,936,1272,562), 
  prevalence = c(0,0.006,0.003,0.003,0.001,0.0042),  
  median = c(NA,NA,NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,NA,0,NA),
  upper_CI = c(NA,NA,NA,NA,0.008,NA)
)



##############################
########## S W E D E N ####

### T R E E N U T S #################
#Rentzos
mean(c(1.484,0.224,1.176))  #=0.961%

#####
studies <- data.frame(
  year = c("Rentzos","Sterner","Johansson","BromsA","BromsB","BromsC","Strinnholm"),
  n = c(1042,195,1002,1119,1993,1774,2585), 
  prevalence = c(0.00961,0.0003,0.00175,0.00208,0.00224,0.00368,0.00576),  
  median = c(NA,NA,NA,NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,NA,NA,NA,NA),
  upper_CI = c(NA,NA,NA,NA,NA,NA,NA)
)

#######################
### Worst case treenut ###
#Rentzos
sum(c(1.484,0.224,1.176)) #=2.884%
####
studies <- data.frame(
  year = c("Rentzos","Sterner","Johansson","BromsA","BromsB","BromsC","Strinnholm"),
  n = c(1042,195,1002,1119,1993,1774,2585), 
  prevalence = c(0.02884,0.0003,0.00175,0.00208,0.00224,0.00368,0.00576),  
  median = c(NA,NA,NA,NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,NA,NA,NA,NA),
  upper_CI = c(NA,NA,NA,NA,NA,NA,NA)
)


### P E A N U T #################    OBS##Broms ABC er ikke delt op i antal i studiet
studies <- data.frame(
  year = c("Rentzos","Sterner","Ostblom","Johansson","BromsA","BromsB","BromsC","Strinnholm"),
  n = c(1042,195,2236,1002,1119,1993,1774,2585), 
  prevalence = c(0.0014,0.0005,0.0042,0.00115,0.00192,0.00304,0.00384,0.00512),  
  median = c(NA,NA,NA,NA,NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,NA,NA,NA,NA,NA),
  upper_CI = c(NA,NA,NA,NA,NA,NA,NA,NA)
)


### M I L K #################
studies <- data.frame(
  year = c("Rentzos","Sterner","Winberg","Ostblom","Johansson","BromsA","BromsB","BromsC","Strinnholm"),
  n = c(1042,195,2612,2236,1002,1119,1993,1774,2585), 
  prevalence = c(0,0,0.0015,0.00308,0.00035,0.00544,0.00704,0.00688,0.0144),  
  median = c(NA,NA,NA,NA,NA,NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,NA,NA,NA,NA,NA,NA),
  upper_CI = c(NA,NA,NA,NA,NA,NA,NA,NA,NA)
)


##############################
########## N O R W A Y ####

### T R E E N U T S #################
#There is only one study
studies <- data.frame(
  year = c("Johansson"),
  n = c(500), 
  prevalence = c(0.003),  
  median = c(NA),
  lower_CI = c(NA),
  upper_CI = c(NA)
)


### P E A N U T #################
studies <- data.frame(
  year = c("Johansson","Kvenshagen"),
  n = c(500,512), 
  prevalence = c(0.003,0.0028),  
  median = c(NA,NA),
  lower_CI = c(NA,NA),
  upper_CI = c(NA,NA)
)

### M I L K #################
studies <- data.frame(
  year = c("Johansson","Kvenshagen","EggesBo"),
  n = c(500,512,2721), 
  prevalence = c(0,0.0154,0.011),  
  median = c(NA,NA,NA),
  lower_CI = c(NA,NA,0.008),
  upper_CI = c(NA,NA,0.016)
)



##############################
########## F I N L A N D ####

### T R E E N U T S #################
studies <- data.frame(
  year = c("HertzenA","HertzenB","Purhonen","Jarvenpaa","KajosaariA","KajosaariB","KajosaariC","KajosaariD"),
  n = c(365,357,3308,1563,261,202,200,203), 
  prevalence = c(0.00882,0.01582,0.00056,0.00288,0.0032,0.0016,0.0032,0),  
  median = c(NA,NA,NA,NA,NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,0.00208,NA,NA,NA,NA),
  upper_CI = c(NA,NA,NA,0.00416,NA,NA,NA,NA)
)  

#NO worst case, as all values are for tree nut, only hertzen gives for Hazel, but hazel alone


### P E A N U T S #################    Kun et studie - men to aldre
studies <- data.frame(
  year = c("HertzenA","HertzenB"),
  n = c(365,357), 
  prevalence = c(0.01148,0.01414),  
  median = c(NA,NA),
  lower_CI = c(NA,NA),
  upper_CI = c(NA,NA)
)

### M I L K #################
studies <- data.frame(
  year = c("HertzenA","HertzenB","Pyrhonen09","IsolauriA","IsolauriB","IsolauriC","IsolauriD","Jarvenpaa","KajosaariA","KajosaariB","KajosaariC","KajosaariD"),
  n = c(365,357,3308,100,100,100,100,1563,261,202,200,203), 
  prevalence = c(0.00042,0.00392,0.01792,0.0045,0.0022,0.0005,0.00355,0.00208,0.0032,0.008,0.0032,0),  
  median = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,0.0021,0.0006,0.00002,0.0015,0.00145,NA,NA,NA,NA),
  upper_CI = c(NA,NA,NA,0.0082,0.0054,0.00275,0.007,0.0032,NA,NA,NA,NA)
)


##############################
########## I C E L A N D ####

### T R E E N U T S #################
#Lyons
mean(c(0.0196, 0)) #=0.0098%
LowCIMedian<-mean(c(0.0084, 0.1764)) #=0.0924%
UpCIMedian<-mean(c(0, 0.098))  #=0.049%
#Burney
mean(c(0.0635,0.0035))  #=0.0335%
#Grabenhenrich
mean(c(0.168,0.112))  #=0.14%

studies <- data.frame(
  year = c("Lyons","Burney","Grabenhenrich","Kristinsdottir"),
  n = c(2248,2114,1341,1341), 
  prevalence = c(0.000098,0.000335,0.0014,0.00014),  
  median = c(NA,NA,NA,NA),
  lower_CI = c(0.000924,NA,NA,NA),
  upper_CI = c(0.00049,NA,NA,NA)
)  


### Worst case
#Lyons
sum(c(0.0196,0)) #=0.0196%
#SD ud fra CI     #(upper - lower) / (2 * 1.96)
Hazel_SD <- (0.1764 - 0.0084) / (2 * 1.96)
Wal_SD <- (0.098 - 0) / (2 * 1.96)
#Samlet SD for summen hvis vi antager uafhængige fejl
sum_SD <- sqrt(Hazel_SD^2 + Wal_SD^2)
#95% konfidensinterval for summen
sum_CI_lower <- 0.0196 - 1.96 * sum_SD #= -0.0777%    #Changed to 0 in the data...
sum_CI_upper <- 0.0196 + 1.96 * sum_SD #= 0.1168%

#Burney
sum(c(0.0635,0.0035))  #=0.067%
#Grabenhenrich
sum(c(0.168,0.112))  #=0.28%

studies <- data.frame(
  year = c("Lyons","Burney","Grabenhenrich","Kristinsdottir"),
  n = c(2248,2114,1341,1341), 
  prevalence = c(0.000196,0.00067,0.0028,0.00014),  
  median = c(NA,NA,NA,NA),
  lower_CI = c(0,NA,NA,NA),
  upper_CI = c(0.001168,NA,NA,NA)
)  



### P E A N U T S #################
studies <- data.frame(
  year = c("Lyons","Burney","Grabenhenrich"),
  n = c(2248,2114,1341), 
  prevalence = c(0.001456,0.000225,0.00392),  
  median = c(NA,NA,NA),
  lower_CI = c(0.000168,NA,NA),
  upper_CI = c(0.004144,NA,NA)
)  

### M I L K #################        #Det sidste studie har to måder at måle på??
studies <- data.frame(
  year = c("Lyons","Burney","Grabenhenrich","KristinsdottirA","KristinsdottirB"),
  n = c(2248,2114,1341,1341,1341), 
  prevalence = c(0.001036,0.0006,0.01372,0.00098,0.00085),  
  median = c(NA,NA,NA,NA,NA),
  lower_CI = c(0.000056,NA,NA,NA,NA),
  upper_CI = c(0.003444,NA,NA,NA,NA)
)  






#######################################
#######################################
############ Severity #################

#Treenut        #50.11%  (95% CI: 32.78% - 67.42%)
#Treenut Worst  #57.83% (95% CI: 33.55% - 78.83%)
#Davoren
mean(c(74.1,66.7)) #70.4%
#Gur
mean(c(30,77.8)) #53.9%

studies <- data.frame(
  year = c("Davoren","Couch","Unsal","Gur"),
  n = c(213,109,613,112), 
  prevalence = c(0.704,0.28,0.477,0.539),  
  median = c(NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,NA),
  upper_CI = c(NA,NA,NA,NA)
) 


#Peanut      #29.69% (95% CI: 26.92% - 32.61%)
studies <- data.frame(
  year = c("Davoren","Deschildre","Couch"),
  n = c(213,669,109), 
  prevalence = c(0.305,0.3,0.26),  
  median = c(NA,NA,NA),
  lower_CI = c(NA,NA,NA),
  upper_CI = c(NA,NA,NA)
) 

#Milk    #17.98% (95% CI: 5.04% - 47.55%)
studies <- data.frame(
  year = c("Nowak","Aquilante","Unsal","Santos"),
  n = c(100,159,613,139), 
  prevalence = c(0.12,0.456,0.345,0.03),  
  median = c(NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,NA),
  upper_CI = c(NA,NA,NA,NA)
) 










########################################
########## R E S O L V E ############### Weight by total participants
#######################################

### Prevalence for resolve ###
### P E A N U T #################      #27.68% (95% CI: 18.36% - 39.43%)
studies <- data.frame(
  year = c("Peters","Hasan","Skolnick"),
  n = c(156,6,223), 
  prevalence = c(0.339,0.33,0.215),  
  median = c(NA,NA,NA),
  lower_CI = c(NA,NA,NA),
  upper_CI = c(NA,NA,NA)
)  

### M I L K #################          #75.18% (95% CI: 61.34% - 85.26%)
studies <- data.frame(
  year = c("Host","Skripak","Wood","Kaczmarski","Kubota","Schoemaker","Saarinen"),
  n = c(39,807,244,291,80,55,118), 
  prevalence = c(0.97,0.79,0.526,0.729,0.58,0.69,0.89),  
  median = c(NA,NA,NA,NA,NA,NA,NA),
  lower_CI = c(NA,NA,NA,NA,NA,NA,NA),
  upper_CI = c(NA,NA,NA,NA,NA,NA,NA)
)  


###############
### A G E #####
# Resolve, studies with several ages #

###### P E A N U T
#Peters     #mean = 5.332 years   #use this vaule to represent the study
total_par<-156 #total participants
max_res<-(0.339) #max resolve rate
age<-(c(4,6,10)) #years
prev<-(c(0.22,0.29-0.22,0.339-0.29)) #prevalence of those that resolve in each age group
prev_res<-(prev*total_par) #next
prev_res<-round(prev_res,digits=0) #number of people that resolve in each age group
total_age<-sum(age*prev_res)
weight_mean<-total_age/(total_par*max_res)
weight_mean<-round(weight_mean,digits =3)

#Hasan     #mean = 14.014 years   #use this vaule to represent the study
total_par<-6 #total participants
max_res<-(0.333) #max resolve rate
age<-(c(10,18)) #years
prev<-(c(0.17,0.333-0.17)) #prevalence of those that resolve in each age group
prev_res<-(prev*total_par) #next
prev_res<-round(prev_res,digits=0) #number of people that resolve in each age group
total_age<-sum(age*prev_res)
weight_mean<-total_age/(total_par*max_res)
weight_mean<-round(weight_mean,digits =3)



########################
#### M I L K
#Høst     mean = 2.379   #use this vaule to represent the study
total_par<-39 #total participants
max_res<-(0.97) #max resolve rate
age<-(c(1,2,3,5,10,15,26)) #years
prev<-(c(0.56,0.77-0.56,0.87-0.77,0.92-0.87,0.92-0.92,0.97-0.92,0.97-0.97)) #prevalence of those that resolve in each age group
prev_res<-(prev*total_par) #next
prev_res<-round(prev_res,digits=0) #number of people that resolve in each age group
total_age<-sum(age*prev_res)
weight_mean<-total_age/(total_par*max_res)
weight_mean<-round(weight_mean,digits =3)

#Skripak     mean = 9.681   #use this vaule to represent the study
total_par<-807 #total participants
max_res<-(0.79) #max resolve rate
age<-(c(4,8,12,16)) #years
prev<-(c(0.19,0.42-0.19,0.64-0.42,0.79-0.64)) #prevalence of those that resolve in each age group
prev_res<-(prev*total_par) #next
prev_res<-round(prev_res,digits=0) #number of people that resolve in each age group
total_age<-sum(age*prev_res)
weight_mean<-total_age/(total_par*max_res)
weight_mean<-round(weight_mean,digits =3)

#Kubota     #10.539 years   #use this vaule to represent the study
total_par<-80 #total participants 
max_res<-(0.58) #max resolve rate
age<-(c(9,12)) #years
prev<-(c(0.31,0.58-0.31)) #prevalence of those that resolve in each age group
prev_res<-(prev*total_par) #next
prev_res<-round(prev_res,digits=0) #number of people that resolve in each age group
total_age<-sum(age*prev_res)
weight_mean<-total_age/(total_par*max_res)
weight_mean<-round(weight_mean,digits =3)

#Saarinen       mean=3.348   #use this vaule to represent the study
total_par<-118 #total participants
max_res<-(0.89) #max resolve rate
age<-(c(1.6,2,5,8.6)) #years
prev<-(c(0.44,0.51-0.44,0.81-0.51,0.89-0.81)) #prevalence of those that resolve in each age group
prev_res<-(prev*total_par) #next
prev_res<-round(prev_res,digits=0) #number of people that resolve in each age group
total_age<-sum(age*prev_res)
weight_mean<-total_age/(total_par*max_res)
weight_mean<-round(weight_mean,digits =3)



