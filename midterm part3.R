library(useful)
set.seed(50)


#clean the data risk factors and access to care

riskData <- read.csv("RISKFACTORSANDACCESSTOCARE.csv",stringsAsFactors = F)


# subset only certain columns
risk_dat <- subset(riskData, select = c(CHSI_County_Name,CHSI_State_Abbr, 
                                        No_Exercise, Smoker,Obesity, 
                                        High_Blood_Pres, Diabetes))
# change names
renamerisk_dat <- c("County.name","State.name","No.exercise","Smoking","Obesity","High.Blood","Diabetes")
names(risk_dat)<- renamerisk_dat

#Check the total number of missing valuese for each parameters.
sum(risk_dat$No.exercise < 0)
sum(risk_dat$Smoking <0)
sum(risk_dat$Obesity <0)
sum(risk_dat$High.Blood<0)
sum(risk_dat$Diabetes<0)

#Set the missing values in different columns into the mean of that column.

noex.m <- mean(risk_dat$No.exercise[risk_dat$No.exercise >= 0])
for(i in 1:length(risk_dat$No.exercise))
{
  if(risk_dat$No.exercise[i] < 0){
    risk_dat$No.exercise[i] <- noex.m
  }
}

smo.m <- mean(risk_dat$Smoking[risk_dat$Smoking >= 0])
for(i in 1:length(risk_dat$Smoking))
{
  if(risk_dat$Smoking[i] < 0){
    risk_dat$Smoking[i] <- smo.m
  }
}

obe.m <- mean(risk_dat$Obesity[risk_dat$Obesity >=0])
for(i in 1:length(risk_dat$Obesity))
{
  if(risk_dat$Obesity[i] < 0){
    risk_dat$Obesity <-obe.m
  }
}

hblood.m <- mean(risk_dat$High.Blood[risk_dat$High.Blood >=0])
for(i in 1:length(risk_dat$High.Blood))
{
  if(risk_dat$High.Blood[i] < 0){
    risk_dat$High.Blood[i] <-hblood.m
  }
}

dia.m <- mean(risk_dat$Diabetes[risk_dat$Diabetes >= 0])
for(i in 1:length(risk_dat$Diabetes))
{
  if(risk_dat$Diabetes[i] < 0){
    risk_dat$Diabetes[i] <- dia.m
  }
}

View(risk_dat)

#Check the index for Worcester MA

w <- which(risk_dat$County.name == "Worcester" & risk_dat$State.name =="MA")
w


#############
#clean the data measure of birth and death

measureData <- read.csv("MEASURESOFBIRTHANDDEATH.csv",stringsAsFactors = F)

# subset only certain columns
measure_dat <- subset(measureData, select = c(CHSI_County_Name,CHSI_State_Abbr, 
                                              Brst_Cancer, Col_Cancer, CHD, 
                                              Lung_Cancer, Stroke))

# change names
renamemeasure_dat <- c("County.name","State.name","Breast.cancer","Colon.cancer",
                       "Coronary Heart Disease","Lung.cancer","Stroke")

names(measure_dat)<- renamemeasure_dat

#Check the total number of missing valuese for each parameters
sum(measure_dat$Breast.cancer < 0)
sum(measure_dat$Colon.cancer <0)
sum(measure_dat$`Coronary Heart Disease` <0)
sum(measure_dat$Lung.cancer<0)
sum(measure_dat$Stroke<0)


#Set the missing values in different columns into the mean of that column.
breast.m <- mean(measure_dat$Breast.cancer[measure_dat$Breast.cancer >= 0])
for(i in 1:length(measure_dat$Breast.cancer))
{
  if(measure_dat$Breast.cancer[i] < 0){
    measure_dat$Breast.cancer[i] <- breast.m
  }
}

col.m <- mean(measure_dat$Colon.cancer[measure_dat$Colon.cancer >= 0])
for(i in 1:length(measure_dat$Colon.cancer))
{
  if(measure_dat$Colon.cancer[i] < 0){
    measure_dat$Colon.cancer[i] <- col.m
  }
}

chd.m <- mean(measure_dat$`Coronary Heart Disease`[measure_dat$`Coronary Heart Disease` >=0])
for(i in 1:length(measure_dat$`Coronary Heart Disease`))
{
  if(measure_dat$`Coronary Heart Disease`[i] < 0){
    measure_dat$`Coronary Heart Disease`[i] <-chd.m
  }
}

lung.m <- mean(measure_dat$Lung.cancer[measure_dat$Lung.cancer >= 0])
for(i in 1:length(measure_dat$Lung.cancer))
{
  if(measure_dat$Lung.cancer[i] < 0){
    measure_dat$Lung.cancer[i] <-lung.m
  }
}

stroke.m <- mean(measure_dat$Stroke[measure_dat$Stroke >= 0])
for(i in 1:length(measure_dat$Stroke))
{
  if(measure_dat$Stroke[i] < 0){
    measure_dat$Stroke[i] <- stroke.m
  }
}

View(measure_dat)



#Check the index for Wocester MA

w <- which(measure_dat$County.name == "Worcester" & measure_dat$State.name =="MA")
w

#check the most appropiate amount of clusters for the data:
measure.kn <- FitKMeans(measure_dat[, 3:7], max.clusters = 10 , nstart = 1,iter.max=25)
measure.kn
risk.kn <- FitKMeans(measure_dat[, 3:7], max.clusters = 10 , nstart = 1,iter.max=25)
risk.kn 

#Do the cluster for measure of birth and death and set total number of clusters as 5 
measure.km <- kmeans(x = risk_dat[,3:7], centers = 5)
measure.km
plot(measure.km, data = risk_dat)

#Do the cluster for measure of birth and death and set total number of clusters as 6
risk.km <- kmeans(x = measure_dat[,3:7], centers = 6)
risk.km
plot(risk.km, data = measure_dat)

