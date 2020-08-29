#This R code was created by Ed Ryan on 27th August 2020
#It builds three multinomial logistic regression models which will be used as part of a larger
#model that will simulate a simple cricket game.  For simplicity we use the same set of values 
#for the input covariates (Batting average, Powerplay and SpinBowler) to determine the possible 
#outcome for each of the 120 (20 overs) balls that we simulate.  

#Remove all current objects stored in R environment:
rm(list = ls())

#install and load any R packages that are needed:
#install.packages("nnet") #uncomment this for the first time you run R code.
library(nnet)

#Set the current working directory (i.e. where the R code and dataset is stored):
setwd("C:/Work/Rcourse/Session4")

# Read in data and look at the first few rows of it
cdata.IN <- read.csv("Cricket_data_v2_Durham_home_matches_training.csv")
head(cdata.IN)


####################################################################
#BUILD SUBMODEL 1: PREDICTING 0 RUNS, 1-6 RUNs, OR A WICKET.       #
####################################################################

#Create an index and find the row numbers of those that have NAs in the Batting Average column:
N=dim(cdata.IN)[1]
Ind=c(1:N)
Ind.noNA=Ind[is.na(cdata.IN$BattingAverage)==FALSE]
inputs_m1=cdata.IN[Ind.noNA,1:9]

#Process the outputs that we only get 3 possible outcomes:
outputs_m1=cdata.IN[Ind.noNA,10:16]
outcome1=as.vector(1*outputs_m1[,1])                   #outcome1 = 0 runs
outcome2=as.vector(2*apply(outputs_m1[,2:6],1,sum))  #outcome2 = 1,2,3,4 or 6 runs
outcome3=as.vector(3*outputs_m1[,7])                 #outcome3 = wicket
outcome_m1=outcome1+outcome2+outcome3

#For clarity we'll put the four columns of data into a new data frame which we'll call cdata:
cdata_m1=as.data.frame(cbind(inputs_m1$PowerPlay,inputs_m1$SpinBowler,inputs_m1$BattingAverage,outcome_m1))
names(cdata_m1)=c("PowerPlay","SpinBowler","BattingAverage","Outcome")
names(cdata_m1)

#Format categorical variables
PowerPlay=factor(cdata_m1$PowerPlay)
SpinBowler=factor(cdata_m1$SpinBowler)
Outcome=factor(cdata_m1$Outcome)

#Train the logistic regression model:
model1 <- multinom(Outcome ~ PowerPlay + SpinBowler + BattingAverage, data=cdata_m1)

#Check out the results to the model:
s1=summary(model1)


####################################################################################################
#BUILD SUBMODEL 2: WHERE THERE IS AT LEAST 1 RUN, PREDICT WHETHER IT'S 1-3 RUNS, 4 RUNS OR 6 RUNS. #
####################################################################################################

#Create an index and find the row numbers of those that record 1-6 runs
#(recall that 'outputs_m1' was calculated at the start of the R code for submodel 1)
N1=dim(outputs_m1)[1]
Ind1=c(1:N1)
names(outputs_m1)
Ind1.onlyruns=Ind1[(outputs_m1$runs_0==0) & (outputs_m1$Wicket==0)]
inputs_m2=inputs_m1[Ind1.onlyruns,]

#Process the outputs that we only get 3 possible outcomes:
names(outputs_m1)                                    
outputs_m2=outputs_m1[Ind1.onlyruns,2:6]             #outputs_m1 consists of 7 columns but only need columns 2-6.
names(outputs_m2)
outcome1=as.vector(1*apply(outputs_m2[,1:3],1,sum))  #outcome1 = 1,2 or 3 runs. check with names(outputs_m2).
outcome2=as.vector(2*outputs_m2[,4])                 #outcome2 = 4 runs (boundary and touches ground beforehand)
outcome3=as.vector(3*outputs_m2[,5])                 #outcome3 = 6 runs (boundary without touching ground beforehand)
outcome_m2=outcome1+outcome2+outcome3

#Put the four columns of data into a new data frame which we'll call cdata_m2:
cdata_m2=as.data.frame(cbind(inputs_m2$PowerPlay,inputs_m2$SpinBowler,inputs_m2$BattingAverage,outcome_m2))
names(cdata_m2)=c("PowerPlay","SpinBowler","BattingAverage","Outcome")
names(cdata_m2)

#Format categorical variables
PowerPlay=factor(cdata_m2$PowerPlay)
SpinBowler=factor(cdata_m2$SpinBowler)
Outcome=factor(cdata_m2$Outcome)

#Train the logistic regression model:
model2 <- multinom(Outcome ~ PowerPlay + SpinBowler + BattingAverage, data=cdata_m2)

#Check out the results to the model:
s2=summary(model2)


############################################################################################
#BUILD SUBMODEL 3: WHERE THERE are 1-3 RUNS, PREDICT WHETHER IT'S 1 RUN, 2 RUNS OR 3 RUNS. #
############################################################################################

#Create an index and find the row numbers of those that record 1-3 runs
#(recall that 'outputs_m2' was calculated at the start of the R code for submodel 2)
N2=dim(outputs_m2)[1]
Ind2=c(1:N2)
names(outputs_m1)
Ind2.onlyruns=Ind1[(outputs_m2$runs_4==0) & (outputs_m2$runs_6==0)]
inputs_m3=inputs_m2[Ind2.onlyruns,]

#Process the outputs that we only get 3 possible outcomes:
names(outputs_m2)                                    
outputs_m2=outputs_m1[Ind2.onlyruns,2:6]             #outputs_m1 consists of 5 columns but only need columns 1-3
names(outputs_m2)
outcome1=as.vector(1*outputs_m2[,1])                 #outcome1 = 1 runs
outcome2=as.vector(2*outputs_m2[,2])                 #outcome2 = 2 runs
outcome3=as.vector(3*outputs_m2[,3])                 #outcome3 = 3 runs
outcome_m3=outcome1+outcome2+outcome3

#Put the four columns of data into a new data frame which we'll call cdata_m3:
cdata_m3=as.data.frame(cbind(inputs_m3$PowerPlay,inputs_m3$SpinBowler,inputs_m3$BattingAverage,outcome_m3))
names(cdata_m3)=c("PowerPlay","SpinBowler","BattingAverage","Outcome")
names(cdata_m3)

#Format categorical variables
PowerPlay=factor(cdata_m3$PowerPlay)
SpinBowler=factor(cdata_m3$SpinBowler)
Outcome=factor(cdata_m3$Outcome)

#Train the logistic regression model:
model3 <- multinom(Outcome ~ PowerPlay + SpinBowler + BattingAverage, data=cdata_m3)

#Check out the results to the model:
s3=summary(model3)



###########################################################################
#SAVE ALL THE MODEL PARAMETERS (STORED IN s1, s2 AND s3) AS AN RData file #
###########################################################################

save(s1, s2, s3, file = "CricketModel_parameters.RData")
