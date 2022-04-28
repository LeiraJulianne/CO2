#####Bermuda Atlantic Time Series-- ocean acidification##
#1) Is surface ocean pCO2 increasing?
#2) Is surface ocean seawater pH decreasing?
#3) Is surface ocean omega decreasing?
#Import data set

#Load library

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

view(bats_co2)

#1) Is surface ocean pCO2 increasing?
bats_co2 %>% 
  ggplot()+
  geom_point(mapping = aes(x=decy, y=pCO2))+
  geom_smooth(mapping = aes(x=decy, y=pCO2), method="lm")+
  ylab(" pCO2") +
  xlab("Years")+
  coord_cartesian(xlim = c(1991,2019))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor= element_blank(),
        panel.background = element_blank(), #removes annoying gray area
        axis.line= element_line(colour = "black"), #adds lines for the axes
        text = element_text(size = 20))


#How to cuantify if its increasing? statistics 
#First try
#pco2_model= with(bats_co2, lm(pCO2~decy)
#anova(pco2_model)
#summary(pco2_model)
#check_model(pco2_model)
#
##############Anova %>% p-value=<0.05 hay there's significance difference. 

pco2_model= with(bats_co2,lm(pCO2~I(decy-2000)))
anova(pco2_model)
summary(pco2_model)
check_model(pco2_model)

#p-value=2.2e-16 there's significance difference. 

################
#2) Is surface ocean seawater pH decreasing? Include full citation in Figure 1 caption. Be sure to discuss oxygen minium zones and uptake of oxygen by respiration! For example, the Gulf of Mexico or the Eastern Tropical Pacific have very low oxygen because it is consumed by the respiration of organic matter. Also be sure to explain all of your figures thoroughly. If you are not discussing the details of the figure, it should be removed. "The current determination method, measures DO in water content according to the diffusion rate of molecular oxygen. " comes up with an almost direct match on google search. This is plagiarism. Any copied text in the final paper will result in a failed term paper assignment.

#OJO 1991 DATA
#PLOT
bats_co2 %>% 
  ggplot()+
  geom_point(mapping = aes(x=decy, y=pH))+
  geom_smooth(mapping = aes(x=decy, y=pH), method="lm")+
    ylab(" pH") +
    xlab("Years")+
  coord_cartesian(xlim = c(1991,2019))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor= element_blank(),
          panel.background = element_blank(), #removes annoying gray area
          axis.line= element_line(colour = "black"), #adds lines for the axes
          text = element_text(size = 20))

#How to cuantify if its increasing? statistics 
pH_model= with(bats_co2, lm(pH~I(decy-2000)))
anova(pH_model)
summary(pH_model)
check_model(pH_model)
#p-value = 1.461e-15 there's significance difference.

#lm(pCO2~I(decy~2000)))

#3) Is surface ocean omega decreasing?
#for omega, this is the "seawater saturation state with respect to (aragonite/calcite)" W_Ar   W_Ca

#ARAGONITE
bats_co2 %>% 
  ggplot()+
  geom_point(mapping = aes(x=decy, y=OmegaAragonite))+
  geom_smooth(mapping = aes(x=decy, y=OmegaAragonite), method="lm") +
  ylab(" pH") +
  xlab("Omega Aragonite")+
  coord_cartesian(xlim = c(1991,2019))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor= element_blank(),
        panel.background = element_blank(), #removes annoying gray area
        axis.line= element_line(colour = "black"), #adds lines for the axes
        text = element_text(size = 20))

#How to cuantify if its increasing? statistics 
OmeAragonite_model= with(bats_co2, lm(OmegaAragonite~decy))
anova(OmeAragonite_model)
summary(OmeAragonite_model)
check_model(OmeAragonite_model)
#p-value= 1.353e-08

#Calcite
bats_co2 %>% 
  ggplot()+
  geom_point(mapping = aes(x=decy, y=OmegaCalcite))+
  geom_smooth(mapping = aes(x=decy, y=OmegaCalcite, method="lm")) +
  ylab(" Omega Calcite") +
  xlab("Years")+
  coord_cartesian(xlim = c(1991,2019))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor= element_blank(),
        panel.background = element_blank(), #removes annoying gray area
        axis.line= element_line(colour = "black"), #adds lines for the axes
        text = element_text(size = 20))

#How to cuantify if its increasing? statistics 
OmeCalcite_model= with(bats_co2, lm(OmegaCalcite~decy))
anova(OmeCalcite_model)
summary(OmeCalcite_model)
check_model(OmeCalcite_model)
#p-value= 9.619e-12


##################
#paper
#Acceleration of ocean warming, salinification, deoxygenation and acidification in the surface subtropical North Atlantic Ocean


##########
#ggplot()+ geom_point(mapping = aes(x=sigma_theta, y= Depth,group= Season, color=Season), size=1,alpha=0.5)+
#  facet_wrap(facets = "Season") SEPARATE PLOTS+
#  scale_x_continuous(position= "top")+
#  scale_y_reverse() +
#  ylab("Depth (m)") +
#  xlab("Density(kg/m^3)")+
#  theme(panel.grid.major = element_blank(), 
#        panel.grid.minor= element_blank(),
#        panel.background = element_blank(), #removes annoying gray area
#        axis.line= element_line(colour = "black"), #adds lines for the axes
#        text = element_text(size = 20))