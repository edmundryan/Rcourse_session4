#This R code was created by Ed Ryan on 27th August 2020
#It simulates a simple cricket game where the outcomes are either "0 runs", "1 run", "2 runs",
#"3 runs", "4 runs", "6 runs" or "Wicket".  We take the parameters (coefficients) of the three
#multinomail logistic regression models calculated from Rcourse_Session4_ParameterEstimation.R
#in order to compute the probabilities of each of these seven possible outcomes.  

#Remove all current objects stored in R environment:
rm(list = ls())

#Specify the number of overs and the number of overs where a spin bowler is used in the innings for match 4:
num.overs=20
num.overs.spin=11

#Set the current working directory (i.e. where the R code and dataset is stored):
setwd("C:/Work/Rcourse/Session4")

#Load the model parameters and read in the csv file that stores the batsman's batting averages:
load("CricketModel_parameters.RData")
batting.averages.IN <- read.csv("Inputs_match4_battingaverages.csv")
ba.all=batting.averages.IN$BattingAverage

#Create the other input variables needed to run the model.  These consist of Powerplay (yes for first 6 overs,  
#no for the remaining 14 overs) and SpinBowler
PowerPlay=c(rep(1,6),rep(0,14))
prob.spin=num.overs.spin/num.overs
SpinBowler=rbinom(num.overs,1,prob.spin)  #we use a bernoulli distribution (which is a binomial distn with n=1)

#Create a function that evaluates a multinomial logistic regression function for a set of inputs and parameters
#and returns the probabilities of the three possible outcomes:
calc.probs = function(coef,PowerPlay,SpinBowler,BattingAverage){
  n2=coef[1,1]+(coef[1,2]*PowerPlay) + (coef[1,3]*SpinBowler) + (coef[1,4]*BattingAverage) 
  n3=coef[2,1]+(coef[2,2]*PowerPlay) + (coef[2,3]*SpinBowler) + (coef[2,4]*BattingAverage)
  
  #We now need to find p1=Pr(Outcome==1), p2=Pr(Outcome==2) and p3=Pr(Outcome==3) such that:
  #log(p2/p1) = n2 and 
  #log(p3/p1) = n3
  #This can be done using the inverse logit formulation given by (see slide 9 of presentation):
  p2=exp(n2)/(1+exp(n2)+exp(n3))
  p3=exp(n3)/(1+exp(n2)+exp(n3))
  
  #Then we calculate p1 from the fact that p1+p2+p3=1:
  p1=1-p2-p3
  
  return(c(p1,p2,p3))
}

#Create empty list to store the number of runs:
num.runs=rep(NA,num.overs*6)
num.wickets=rep(0,num.overs*6)

#initialise a number of things:
wicket.count=0          #This keep track of the number of wickets (i.e. the number of batsman that are out)
batsman.curr=1          #batsman.curr is an ID for which of current batsman is at the strikers end (either 1 or 2) 
batsman.count=2         #Keeping track of how many batsman are batting or have batted
ba.batsman1=ba.all[1]   #ba.batsman1 is the 'batting average' for batsman 1.
ba.batsman2=ba.all[2]   #ba.batsman2 is the 'batting average' for batsman 2.
ba.batsman.curr=ba.batsman1  #ba.batsman.curr changes depending on which batsman is at the strikers end.

#Run the model:
for (over in 1:num.overs){        #over = over number
  for (ball in 1:6){              #ball = ball in over
    ball.num=(over-1)*6 + ball    #ball.num = overall ball number in innings

    if (wicket.count<10){         #This if statement is here in case all batsman get out before end of innings.
      
      #Calculate the probabilities of the three possible outcomes from submodel 1 (0 runs, 1-6 runs or a wicket):
      probs=calc.probs(s1$coefficients,PowerPlay[over],SpinBowler[over],ba.batsman.curr)
    
      #We generate a random number and use this along with probs[1], probs[2] and probs[3] to determine 
      #which of the three outcome to choose:
      r=runif(1)
      if (r<=probs[1]) {                      #0 runs
        num.runs[ball.num]=0
        need.model2=0
      } else if ( r<=(probs[1]+probs[2])) {   #1-6 runs
        need.model2=1
      } else {                                #wicket
        num.runs[ball.num]=0
        num.wickets[ball.num]=1
        need.model2=0
        wicket.count=wicket.count+1
        batsman.count=batsman.count+1
        if(batsman.curr==1){
          ba.batsman1=ba.all[batsman.count]
          ba.batsman.curr=ba.batsman1
        } else if(batsman.curr==2){
          ba.batsman2=ba.all[batsman.count]
          ba.batsman.curr=ba.batsman2
        }
      }
      
      #If the '1-6 runs' outcome was chosen then we need to run submodel 2 (i.e. need.model2=1)
      #to work out whether it is '1-3 runs', '4 runs' or '6 runs'
      if (need.model2==1){
        #Calculate the probabilities of the three possible outcomes from submodel 2 (1-3 runs, 4 runs or 6 runs):
        probs=calc.probs(s2$coefficients,PowerPlay[over],SpinBowler[over],ba.batsman.curr)
      
        #We generate a random number and use this along with probs[1], probs[2] and probs[3] to determine 
        #which of the three outcome to choose:  
        r=runif(1)
        if (r<=probs[1]) {                      #1-3 runs
          need.model3=1
        } else if ( r<=(probs[1]+probs[2])) {   #4 runs
          num.runs[ball.num]=4
          need.model3=0
        } else {                                #6 runs
          num.runs[ball.num]=6
          need.model3=0
        }
      } else {
        need.model3=0
      }
    
      #Finally, if the '1-3 runs' outcome was chosen above then we need to run submodel 3 (i.e. need.model3=1)
      #to work out whether it is '1 run', '2 runs' or '3 runs'
      if (need.model3==1){
        #Calculate the probabilities of the three possible outcomes from submodel 3 (1 run, 2 runs or 3 runs):
        probs=calc.probs(s3$coefficients,PowerPlay[over],SpinBowler[over],ba.batsman.curr)
      
        #We generate a random number and use this along with probs[1], probs[2] and probs[3] to determine 
        #which of the three outcome to choose:
        r=runif(1)
        if (r<=probs[1]) {                      #1 run
          num.runs[ball.num]=1
        } else if ( r<=(probs[1]+probs[2])) {   #2 runs
          num.runs[ball.num]=2
        } else {                                #3 runs
          num.runs[ball.num]=3
        }
      }
    
      #If the number of runs is odd (1 or 3) then the other batsman comes into strike, hence the values for
      #batsman.curr and ba.batsman.curr change: 
      if ((num.runs[ball]==1) || (num.runs[ball]==3)){  #the || symbol means 'or'.
        if(batsman.curr==1){
          batsman.curr=2
          ba.batsman.curr=ba.batsman2
        } else if(batsman.curr==2){
          batsman.curr=1
          ba.batsman.curr=ba.batsman1
        }
      }
    } else {
      num.runs[ball.num]=0
    }
  }
}

num.runs
sum(num.runs)
num.wickets
wicket.count

#131 runs that were actually scored.
