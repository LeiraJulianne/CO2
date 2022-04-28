#####Bermuda Atlantic Time Series-- ocean acidification##
#1) Is surface ocean pCO2 increasing?
#2) Is surface ocean seawater pH decreasing?
#3) Is surface ocean omega decreasing?
#Import data set

#Load library

library(tidyverse)
library(seacarb)
library(performance)
library(naniar) #na results
library(ggcorrplot)
library(devtools)
library(devtools)
#install.packages("ggfortify")
library(ggfortify)

#install.packages("vgv/ggbiplot")
#library(xTools)
#library(ggbiplot)
##import BATS Bottle data from website
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

#MULTIDATA ANALYSIS

#Create a month and year column with the BATS data
bats_co2$Year=as.numeric(substr(bats_co2$yyyymmdd,1,4))
bats_co2$Month=as.numeric(substr(bats_co2$yyyymmdd,5,6))

#lets look at our variables
colnames(bats_co2)

#we want to asses corelation between key parameter so lets filter this out for things we want to explore

bats_co2_sub= 
  bats_co2 %>% 
    select(decy,Month, Year, Depth,Temp, Sal1,pCO2,'Sig-th', 'O2(1)','NO31', 'NO21', 'PO41', TN, TOC) %>% 
  replace_with_na_all(condition=~.x==-999)

view(bats_co2_sub)

#recall last weej we want to predict annual pCO2 increases
bats_co2 %>% 
  ggplot(mapping =aes(x=decy, y=pCO2))+
  geom_point()+
  geom_line()+
  geom_smooth(method="lm")
  
  #what if we really want to predict pco2?
  
  #Since we have unequal lengths of ata, we will need to find the days where everything is represented for our modeling approach.
  
  data2=bats_co2_sub[complete.cases(bats_co2_sub),]
  view(data2)

######pCPO2 ~YEAR; we could figure pco2 with temp, depth,salinity,Density,pH,O2,TOC,NO3
  
  #
  
  moddel.aic.forward= 
    step(lm(pCO2~1, data=data2), direction = "forward", trace=1,
         scope=~decy+Month+Year+Depth+Temp+Sal1+`Sig-th`+ `O2(1)`+`NO31` +`NO21`+ `PO41`+TN+ TOC)
  
 # our best AIC model is:
  
pco2.model= lm(pCO2 ~ Temp + Year + Month + NO21 + Sal1 , data=data2) #pCO2 ~ Temp + Year + Month + NO21 + Sal1 + `o2(NOT HELPING)`
summary(pco2.model)
anova(pco2.model)

#I actually wanted to predict Hydrostation pCO2?
m1=lm(pCO2 ~Temp, data=data2)
m2=lm(pCO2 ~decy,data=data2)
m3=lm(pCO2~ Temp + decy, data=data2)
m4=lm(pCO2 ~ Temp*decy,data=data2)
AIC(m1,m2,m3,m4)
BIC(m1,m2,m3,m4)
anova(m1,m3) #Notice de difference 
summary(m1)
summary(m2)
summary(m3)
summary(m4)
#HIGER pCO2, low o2 through photosynthesis more oxy more nutrients

#How do we actually make predictions and plot them? 
bats.co2.predict= cbind(data2,predict(pco2.model, interval = "confidence", level=0.95))
#adds fit,lwr and upr to our data
view(bats.co2.predict)

#plot our model fit relative toour pco2 data
bats.co2.predict %>% 
  ggplot()+
  geom_point(aes(decy,pCO2))+ 
  geom_line(aes(decy,fit))+
  geom_ribbon(aes(x=decy,ymin=lwr,ymax=upr),alpha=0.4,fill="purple") +
  theme_classic() 

#compute correlation matrix
correlation_maxtrix= round(cor(data2),1) #create a correlation matrix 
ggcorrplot(correlation_maxtrix, method = "square") #plot correlation matrix
coorp.mat=round(cor_pmat(data2),2) #create a correlation matrix p-value

#PCA or principal components analysis.
pc=prcomp(data2,center=TRUE,scale. = TRUE) #SCALE= make equal scale ex. 500ppm vs picomol/kg 
print(pc)
summary(pc) #Proportion of Variance 0.3468 PC1 PC PRINT SD= +

#How does biochemistry vary between season? 
#fist make a season column
data2$season=if_else(data2$Month==12| data2$Month==1|data2$Month==2|data2$Month==3, "winter", 
                     if_else(data2$Month==7|data2$Month==8|data2$Month==9, "summer",
                             if_else(data2$Month==4|data2$Month==5|data2$Month==6, "spring","fall")))
view(data2)

autoplot(pc,data=data2,colour="season", loadings=TRUE, loadings.label=TRUE,frame=TRUE, frame.type='norm')

#Homework
#Multivariate lab
#1) Predict pCO2 for Hydrosration s and compare to BATS pCO2 data (1988-2020) so we want to reconstruct pCO2 from 1956-1988
#2) What parameter are avalibale the entire timeseries at hydrostation s?
#3) of those parameters, how to construct the best model using BATS data for prediction pco2?
#4 construct the model using BATS data
#5) apply the model to Hydrostation S to predict pCO2 (+- uncertainties)
#6)plot hydrostation s pco2 predictions and BATS real pco2 data on the same plot
#7) What is the error of predicted vs real pco2 at Hydrostation s?
#HINT: you will need to interpolate onto the same timescale? you can use approx function= (x,y,xout,method=linear)

#x= decy for BATS, y=pCO2 for BATS, xout=decy for Hydrostation S.