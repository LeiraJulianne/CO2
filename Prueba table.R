library(tidyverse)
library(seacarb)
library(performance)
#import BATS Bottle data from website
bats_bottle <- read_table2("http://batsftp.bios.edu/BATS/bottle/bats_bottle.txt", 
                           col_names = FALSE, skip = 60)
View(bats_bottle)

#assign column column names to BATS data from website
colnames(bats_bottle)=
  colnames(read_csv("http://batsftp.bios.edu/BATS/bottle/bats_bottle.txt", skip = 59))
View(bats_bottle)

#Varible names and units form BATS data
#yyyymmdd = Year Month Day   
#decy   = Decimal Year     
#time   = Time (hhmm)      
#latN   = Latitude (Deg N) 
#lonW   = Longitude (Deg W)
#Depth  = Depth (m)                  
#Temp   = Temperature ITS-90 (C)    
#CTD_S  = CTD Salinity (PSS-78)      
#Sal1   = Salinity-1 (PSS-78)        
#Sig-th = Sigma-Theta (kg/m^3)       
#O2(1)  = Oxygen-1 (umol/kg)          
#OxFixT = Oxygen Fix Temp (C)        
#Anom1  = Oxy Anomaly-1 (umol/kg)    
#CO2    = dissolved inorganic carbon (umol/kg)              
#Alk    = Alkalinity (uequiv)        
#NO3    = Nitrate+Nitrite-1 (umol/kg)
#NO2    = Nitrite-1 (umol/kg)        
#PO4    = Phosphate-1 (umol/kg)      
#Si     = Silicate-1 (umol/kg)       
#POC    = POC (ug/kg)                
#PON    = PON (ug/kg)                
#TOC    = TOC (umol/kg)                
#TN     = TN (umol/kg)  NOTE: Prior to BATS 121, DON is reported instead of TON
#Bact   = Bacteria enumeration (cells*10^8/kg)
#POP    = POP (umol/kg)
#TDP    = Total dissolved Phosphorus (nmol/kg)
#SRP    = Low-level phosphorus (nmol/kg)
#BSi    = Particulate biogenic silica (umol/kg)
#LSi    = Particulate lithogenic silica  (umol/kg)
#Pro    = Prochlorococcus (cells/ml)
#Syn    = Synechococcus (cells/ml)
#Piceu  = Picoeukaryotes (cells/ml)
#Naneu  = Nanoeukaryotes (cells/ml)   

?carb
#carb(flag, var1, var2, S=35, T=25, Patm=1, P=0, Pt=0, Sit=0,
#     k1k2="x", kf="x", ks="d", pHscale="T", b="u74", gas="potential", 
#     warn="y", eos="eos80", long=1.e20, lat=1.e20)
#

#we're missing pressure but we can calculate this from depth using TEOS-10 toolbox
bats_bottle= 
  bats_bottle %>% 
  mutate(sea_pressure=gsw_p_from_z(Depth*-1,latN))

#now we have all the variables we need to calculate carbonate chemistry!
#but we must be VERY Careful about our units and matching the correct variables

bats_co2=
  bats_bottle %>% 
  rename(DIC=CO2) %>% 
  filter(Depth<10) %>% 
  filter(Alk!=-999,DIC!=-999,Sal1!=-999,Temp!=-999,sea_pressure!=-999,
         PO41!=-999,Sal1!=-999,lonW!=-99,latN!=-999) %>%
  mutate(carb(flag=15, var1=Alk*10^-6, var2=DIC*10^-6, S=Sal1, T=Temp, Patm=1, P=sea_pressure/10, Pt=PO41*10^-6, Sit=Si1*10^-6,
              k1k2="l", kf="x", ks="d", pHscale="T", b="u74", gas="potential", 
              warn="y", eos="teos10", long=360-lonW, lat=latN))

tribble(
  ~DF, ~Sum Sq, ~Mean Sq,~F value
  #--|--|----|--|----|--|----|--|--
  "I(decy - 2000)",  1, 88066,88066,83.947
  "Residuals", 385, 403893, 1049
  "Pr(>F)", 
  
)

Analysis of Variance Table for pCo2   
| Df |Sum Sq| Mean Sq| F value|
  |:----:||:----:||:----:||:------:|
  |I(decy - 2000)|   1|    88066 |  88066| 83.947|
  Residuals      |385 | 403893   |   1049| 
  
  | Pr(>F)| 
  |I(decy - 2000)| < 2.2e-16|
  