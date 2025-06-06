######################################################################################### ##
##                                                                                        ##
## This file contains the R code that we used to analyze data for:                        ##
## Measuring Distinct Social Skills via Multiple Speed Assessments -                      ##
## A Behavior-Focused Personnel Selection Approach                                        ##
## Simon M. Breil, Boris Forthmann, & Mitja D. Back                                       ##
##                                                                                        ##
##               This code is licensed under a CC-BY4.0 license.                          ##
##             (see https://creativecommons.org/licenses/by/4.0/)                         ##
##                                                                                        ##
######################################################################################### ##

#Packages need to be installed prior to first loading.
#This can be done by the following code:
#install.packages("name_of_package")

### load packages
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(lme4)
library(psych)
library(mice)
library(lavaan)
library(semTools)
library(future)
library(brms)


#For the following reliability and variance analyses we need the q-multiplier which scales the contribution of variance attributable to assessor/rater main effects.
#we use the following function based on 
#Putka, D. J., Le, H., McCloy, R. A., & Diaz, T. (2008). Ill-structured measurement designs in organizational research: Implications for estimating interrater reliability. Journal of Applied Psychology, 93, 959-981. doi: 0.1037/0021-9010.93.5.959
#The function for the q-multiplier was applied to Putka et al.'s data provided in Tables of their article and in their online supplemental material. All of their findings were replicated with the function here

### function for q-multiplier used to rescale variance
q.multiplier <- function(x,  k.hat = harmonic.mean(rowSums(x))){     require(psych)    
  dat <- x    
  den <- nrow(dat)*(nrow(dat)-1)    
  ciip <- c()    
  kikip <- c()    
  for(i in 1:nrow(dat)){        
    for(k in (1:nrow(dat))[-i]){            
      ciip <- c(ciip,sum(colSums(dat[c(i,k),])==2))            
      kikip <- c(kikip,sum(dat[i,])*sum(dat[k,]))        
    }    
  }    
  q.multiplier <- 1/k.hat - sum(ciip/kikip)/den    
  res <- list(q.multiplier=q.multiplier,k.hat=k.hat)    
  return(res) }


#Load preprocessed data available at OSF page
load("Data.RData")

#Variables
#Assessee: Anonymized assessee ID number
#Rating: Rating on the respective skill dimension 0 to 5
#Rater: Assessor: Anonymized assessor ID number
#Exercise: Name of exercise: 6 different exercises
#Dimension: Name of dimension: 3 different dimensions
#Sample: Name of sample: 3 different samples
#HumanZahn: Type of major assessees applied for: Human = human medicine, Zahn = dentistry
#SampleMajor: Combination of sample and major: 6 different subsamples

#Analyses####

#Method####
#Age and Gender of participants is not included to keep anonymization. 
#This also applies to the results presented in Online Supplement 1 (correlations between age, gender, and skill ratings)

#describe(Data_wide$Age)
#vars   n  mean   sd median trimmed  mad min max range skew kurtosis   se
#1      589 18.86 1.43 19   18.64 1.48  16  28    12 2.47     9.24 0.06
#table(Data_wide$Sex)
#1(male)   2 (female) 
#183      406 

#Table 2####
#N, M, SD, and correlations of skill dimension ratings
Data_Table2 <- select(Data,Assessee, Rating, Exercise,SampleMajor) #select relevant variables
Data_Table2 <-aggregate(Data_Table2$Rating, by=list(Data_Table2$Assessee,Data_Table2$Exercise,Data_Table2$SampleMajor),FUN=mean, na.rm=TRUE) #aggregate ratings across assessors
Data_Table2a <- dcast(Data_Table2, Group.1 ~ Group.2) #create wide dataset: 1 column per exercise
Data_Table2a <- select(Data_Table2a, Persuasion, Unreasonable, Crisis, "Bad news",Presentation,Mistake) #select relevant variables
describe(Data_Table2a) #get N, M, and SD
corr.test(Data_Table2a, use ="pairwise",adjust = "none") #get correlations

#Calculate correlations in parentheses (standardized within subsamples and exercises)
Data_Table2b <- ddply(Data_Table2, c("Group.2","Group.3"), transform, Rating_z = scale(x)) #standardize ratings per subsample and per exercise
Data_Table2b <- dcast(Data_Table2b, Group.1 ~ Group.2) #create wide dataset: 1 column per exercise
Data_Table2b <- select(Data_Table2b, Persuasion, Unreasonable, Crisis, "Bad news",Presentation,Mistake) #select relevant variables
describe(Data_Table2b) #check if standardization worked
corr.test(Data_Table2b, use ="pairwise",adjust = "none") #get correlations


#Test the difference between correlations of the same skills vs. different skills based on multiple imputation
names(Data_Table2a)[4]<-"Bad_news"
m <- 'Persuasion ~~ v1*Persuasion + s1*Unreasonable + d1*Crisis + d2*Bad_news + d3*Presentation + d4*Mistake
      Unreasonable ~~ v2*Unreasonable + d5*Crisis + d6*Bad_news + d7*Presentation + d8*Mistake
      Crisis ~~ v3*Crisis + s2*Bad_news + d9*Presentation + d10*Mistake
      Bad_news ~~ v4*Bad_news + d11*Presentation + d12*Mistake
      Presentation ~~ v5*Presentation + s3*Mistake
      Mistake ~~ v6*Mistake
      # difference between average same
      # and average different exercises (w.r.t. the dimensions tested)
      s := (fisherz(s1/sqrt(v1*v2)) + fisherz(s2/sqrt(v3*v4)) + fisherz(s3/sqrt(v5*v6)))/3
      d := (fisherz(d1/sqrt(v1*v3)) + fisherz(d2/sqrt(v1*v4)) + fisherz(d3/sqrt(v1*v5)) + fisherz(d4/sqrt(v1*v6)) + fisherz(d5/sqrt(v2*v3)) + fisherz(d6/sqrt(v2*v4)) + fisherz(d7/sqrt(v2*v5)) + fisherz(d8/sqrt(v2*v6)) + fisherz(d9/sqrt(v3*v5)) + fisherz(d10/sqrt(v3*v6)) + fisherz(d11/sqrt(v4*v5)) + fisherz(d12/sqrt(v4*v6)))/12
      diff := s - d
      # s, d, and difference in correlation metric
      sr := fisherz2r(s)
      dr := fisherz2r(d)
      diffr := fisherz2r(diff)
      '
#Estimate
est <- sem.mi(m, Data_Table2a, m = 40, miPackage = "mice", seed = 2020) #Estimate correlations based on multiple imputation
#Show results
summary(est, standardized = T, ci = TRUE) #Please note that the specific results might vary (to a very small degree) depending on the version of the "mice" package. Here we used version 3.13.0.

#Calculate G(q,k) (reliability) for the different exercises #Here we calculated separate q-multipliers for each exercises as the Assessee-Rater matrix were slightly different depending on the included assessees
#Persuasion Exercise
Subset <- Data[Data$Exercise=="Persuasion",] #select relevant ratings
rr.mat.ha.bez <- table(Subset$Assessee,Subset$Rater)>0 #create ### ratee-rater matrix
(q.1 <- q.multiplier(rr.mat.ha.bez,1)) #assign q-multiplier 0.9722347
(q.k <- q.multiplier(rr.mat.ha.bez)) # assign q-multiplier  0.4722347 
Model <- lmer(Rating ~ 1 + (1|Assessee) + (1|Rater)+ (1|SampleMajor),Subset) #here we also specified variance related to subsamples which should be excluded from the reliability calculations (i.e., neither reliable nor unreliable variance)
Variances <- as.data.frame(VarCorr(Model)) #extract variances
vcomp.c <-subset(Variances, Variances$grp=="Assessee")$vcov
vcomp.j <-subset(Variances, Variances$grp=="Rater")$vcov
vcomp.e <-subset(Variances, Variances$grp=="Residual")$vcov
vcomp.c/(vcomp.c+vcomp.j*q.1[[1]]+vcomp.e) #G(q,1) .61  
vcomp.c/(vcomp.c+vcomp.j*q.k[[1]]+vcomp.e/q.k[[2]]) #G(q,2) .76

#Unreasonable Exercise
Subset <- Data[Data$Exercise=="Unreasonable",] #select relevant ratings
rr.mat.ha.bez <- table(Subset$Assessee,Subset$Rater)>0 #create ### ratee-rater matrix
(q.1 <- q.multiplier(rr.mat.ha.bez,1)) #assign q-multiplier 0.9166544
(q.k <- q.multiplier(rr.mat.ha.bez)) # assign q-multiplier  0.4166544
Model <- lmer(Rating ~ 1 + (1|Assessee) + (1|Rater)+ (1|SampleMajor),Subset) #here we also specified variance related to subsamples which should be excluded from the reliability calculations (i.e., neither reliable nor unreliable variance)
Variances <- as.data.frame(VarCorr(Model)) #extract variances
vcomp.c <-subset(Variances, Variances$grp=="Assessee")$vcov
vcomp.j <-subset(Variances, Variances$grp=="Rater")$vcov
vcomp.e <-subset(Variances, Variances$grp=="Residual")$vcov
vcomp.c/(vcomp.c+vcomp.j*q.1[[1]]+vcomp.e) #G(q,1) .61
vcomp.c/(vcomp.c+vcomp.j*q.k[[1]]+vcomp.e/q.k[[2]]) #G(q,2) .76

#Crisis Exercise
Subset <- Data[Data$Exercise=="Crisis",] #select relevant ratings
rr.mat.ha.bez <- table(Subset$Assessee,Subset$Rater)>0 #create ### ratee-rater matrix
(q.1 <- q.multiplier(rr.mat.ha.bez,1)) #assign q-multiplier 0.9722347
(q.k <- q.multiplier(rr.mat.ha.bez)) # assign q-multiplier  0.4722347 
Model <- lmer(Rating ~ 1 + (1|Assessee) + (1|Rater)+ (1|SampleMajor),Subset) #here we also specified variance related to subsamples which should be excluded from the reliability calculations (i.e., neither reliable nor unreliable variance)
Variances <- as.data.frame(VarCorr(Model)) #extract variances
vcomp.c <-subset(Variances, Variances$grp=="Assessee")$vcov
vcomp.j <-subset(Variances, Variances$grp=="Rater")$vcov
vcomp.e <-subset(Variances, Variances$grp=="Residual")$vcov
vcomp.c/(vcomp.c+vcomp.j*q.1[[1]]+vcomp.e) #G(q,1) 58
vcomp.c/(vcomp.c+vcomp.j*q.k[[1]]+vcomp.e/q.k[[2]]) #G(q,2) .74

#Bad news Exercise
Subset <- Data[Data$Exercise=="Bad news",] #select relevant ratings
rr.mat.ha.bez <- table(Subset$Assessee,Subset$Rater)>0 #create ### ratee-rater matrix
(q.1 <- q.multiplier(rr.mat.ha.bez,1)) #assign q-multiplier 0.9688018
(q.k <- q.multiplier(rr.mat.ha.bez)) # assign q-multiplier 0.4688018
Model <- lmer(Rating ~ 1 + (1|Assessee) + (1|Rater)+ (1|SampleMajor),Subset) #here we also specified variance related to subsamples which should be excluded from the reliability calculations (i.e., neither reliable nor unreliable variance)
Variances <- as.data.frame(VarCorr(Model)) #extract variances
vcomp.c <-subset(Variances, Variances$grp=="Assessee")$vcov
vcomp.j <-subset(Variances, Variances$grp=="Rater")$vcov
vcomp.e <-subset(Variances, Variances$grp=="Residual")$vcov
vcomp.c/(vcomp.c+vcomp.j*q.1[[1]]+vcomp.e) #G(q,1) .38
vcomp.c/(vcomp.c+vcomp.j*q.k[[1]]+vcomp.e/q.k[[2]]) #G(q,2) .55

#Presentation Exercise
Subset <- Data[Data$Exercise=="Presentation",] #select relevant ratings
rr.mat.ha.bez <- table(Subset$Assessee,Subset$Rater)>0 #create ### ratee-rater matrix
(q.1 <- q.multiplier(rr.mat.ha.bez,1)) #assign q-multiplier 0.9582815
(q.k <- q.multiplier(rr.mat.ha.bez)) # assign q-multiplier 0.4582815
Model <- lmer(Rating ~ 1 + (1|Assessee) + (1|Rater)+ (1|SampleMajor),Subset) #here we also specified variance related to subsamples which should be excluded from the reliability calculations (i.e., neither reliable nor unreliable variance)
Variances <- as.data.frame(VarCorr(Model)) #extract variances
vcomp.c <-subset(Variances, Variances$grp=="Assessee")$vcov
vcomp.j <-subset(Variances, Variances$grp=="Rater")$vcov
vcomp.e <-subset(Variances, Variances$grp=="Residual")$vcov
vcomp.c/(vcomp.c+vcomp.j*q.1[[1]]+vcomp.e) #G(q,1) .54
vcomp.c/(vcomp.c+vcomp.j*q.k[[1]]+vcomp.e/q.k[[2]]) #G(q,2) .70

#Mistake Exercise
Subset <- Data[Data$Exercise=="Mistake",] #select relevant ratings
rr.mat.ha.bez <- table(Subset$Assessee,Subset$Rater)>0 #create ### ratee-rater matrix
(q.1 <- q.multiplier(rr.mat.ha.bez,1)) #assign q-multiplier 0.950621
(q.k <- q.multiplier(rr.mat.ha.bez)) # assign q-multiplier 0.450621
Model <- lmer(Rating ~ 1 + (1|Assessee) + (1|Rater)+ (1|SampleMajor),Subset) #here we also specified variance related to subsamples which should be excluded from the reliability calculations (i.e., neither reliable nor unreliable variance)
Variances <- as.data.frame(VarCorr(Model)) #extract variances
vcomp.c <-subset(Variances, Variances$grp=="Assessee")$vcov
vcomp.j <-subset(Variances, Variances$grp=="Rater")$vcov
vcomp.e <-subset(Variances, Variances$grp=="Residual")$vcov
vcomp.c/(vcomp.c+vcomp.j*q.1[[1]]+vcomp.e) #G(q,1) .42
vcomp.c/(vcomp.c+vcomp.j*q.k[[1]]+vcomp.e/q.k[[2]]) #G(q,2) .59

#Table 3####

#The variance of the dependent variable was rather small (compared to the scaling implied by the default priors in brms for variance components estimates (i.e., half-t distribution with a scaling of 10)). 
#Hence, we z-standardized the dependent variable and multiplied by 10 to prevent technical issues such as divergent transitions.
#For the fixed effects we used the default priors of brms (i.e., improper flat priors), which influence potential results as little as possible.

Data$Rating.10 <- scale(Data$Rating)*10

FullModel <- brm(Rating.10 ~ 1 +
                   (1|Assessee:SampleMajor) + #variance related to assessee differences
                   (1|Assessee:Dimension:SampleMajor) + #variance related to assessee differences depending on the skill dimensions
                   (1|Assessee:Dimension:Exercise:SampleMajor) + #variance related to assessee differences in exercises (nested in dimensions)
                   # unreliable components  
                   (1 | Rater:Dimension:Exercise:SampleMajor) + #systematic differences (e.g. leniency) of raters nested in exercises nested in dimensions
                   # unrelated
                   (1 | Dimension) + #skill dimension main effects
                   (1 | Dimension:Exercise)+ #exercise main effects (nested in dimensions)
                   (1 | SampleMajor) + #sample main effects
                   (1 | Dimension:SampleMajor) +  #dimension main effects depending on the sample
                   (1 | Dimension:Exercise:SampleMajor), #exercise main effects (nested in dimensions) depending on the sample
                 Data,family = gaussian,iter=10000,warmup = 5000,thin = 10, cores=getOption("mc.cores",4L),control = list(adapt_delta = 0.99999,max_treedepth = 15))

summary(FullModel)

rr.mat <- table(Data$Assessee,Data$Rater)>0 #create rater q-multiplier for the overall variance attribution. Here, we used the complete ratee-rater matrix to calculate the q-multiplier (calculating the amount of overlap among the assessors rating each candidate across all exercises and samples)
q.multiplier(rr.mat,1) #q-multiplier overall 1 Rater: 0.9937837 used later to rescale rater variance
q.multiplier(rr.mat,2) #q-multiplier overall 2 Rater: 0.4937837 used later to rescale rater variance

#extract relevant variance components
vcomp.c <- (VarCorr(FullModel)$'Assessee:SampleMajor'$sd^2)[c(1)]
vcomp.d <- (VarCorr(FullModel)$'Assessee:Dimension:SampleMajor'$sd^2)[c(1)]
vcomp.e <- (VarCorr(FullModel)$'Assessee:Dimension:Exercise:SampleMajor'$sd^2)[c(1)]
vcomp.j <- (VarCorr(FullModel)$'Rater:Dimension:Exercise:SampleMajor'$sd^2)[c(1)]
vcomp.jq  <- (VarCorr(FullModel)$'Rater:Dimension:Exercise:SampleMajor'$sd^2)[c(1)]* 0.9937837  #For (unreliable) between-person variance we rescaled the assessor variance by using the q-multiplier
vcomp.r <- (VarCorr(FullModel)$'residual__'$sd^2)[c(1)]
vcomp.di <- (VarCorr(FullModel)$'Dimension'$sd^2)[c(1)]
vcomp.ex <- (VarCorr(FullModel)$'Dimension:Exercise'$sd^2)[c(1)]
vcomp.s <- (VarCorr(FullModel)$'SampleMajor'$sd^2)[c(1)]
vcomp.dis <- (VarCorr(FullModel)$'Dimension:SampleMajor'$sd^2)[c(1)]
vcomp.exs <- (VarCorr(FullModel)$'Dimension:Exercise:SampleMajor'$sd^2)[c(1)]

OverallVar <- vcomp.c + vcomp.d + vcomp.e + vcomp.j + vcomp.r + vcomp.di + vcomp.ex + vcomp.s + vcomp.dis + vcomp.exs #Calculate overall Variance
OverallBetweenPerson <- vcomp.c + vcomp.d + vcomp.e + vcomp.jq + vcomp.r #Calculate between-person variance. Here we used the rescaled assessor/rater variance (jq instead of j)
OverallReliable <- vcomp.c + vcomp.d + vcomp.e #Calculate reliable between-person variance
OverallReliableAggregated <- vcomp.c + vcomp.d + vcomp.e/2 ##Calculate reliable between-person variance on a dimension level. Here we divided the assessee-exercise variance by the number of exercises that assessed a given dimension (i.e., 2)

#Total variance in % (for Online Supplement 3)
vcomp.c/OverallVar*100
vcomp.d/OverallVar*100
vcomp.e/OverallVar*100
vcomp.j/OverallVar*100
vcomp.r/OverallVar*100
vcomp.di/OverallVar*100
vcomp.ex/OverallVar*100
vcomp.s/OverallVar*100
vcomp.dis/OverallVar*100
vcomp.exs/OverallVar*100

#Between-assessee variance in %
vcomp.c/OverallBetweenPerson*100
vcomp.d/OverallBetweenPerson*100
vcomp.e/OverallBetweenPerson*100
vcomp.jq/OverallBetweenPerson*100 #Here we used the rescaled rater variance (jq instead of j)
vcomp.r/OverallBetweenPerson*100

#Reliable variance in %
vcomp.c/OverallReliable*100
vcomp.d/OverallReliable*100
vcomp.e/OverallReliable*100
OverallReliable/OverallBetweenPerson #G(q,1)
OverallReliable/(OverallReliable+vcomp.j*0.4937837+vcomp.r/2) #G(q,2) #here we divided residual variance by the number of raters/assessors per exercise/rating

#Reliable variance on a dimension level (aggregated across exercises) in %
vcomp.c/OverallReliableAggregated*100
vcomp.d/OverallReliableAggregated*100
(vcomp.e/2)/OverallReliableAggregated*100
OverallReliableAggregated/(vcomp.j*0.9937837/2+vcomp.r/2+OverallReliableAggregated) #G(q,1) #as assessors were nested within exercises we divided assessor variance and error variance by the number of exercises that assessed a given dimension (i.e., 2)
OverallReliableAggregated/(vcomp.j*0.4937837/2+vcomp.r/2/2+OverallReliableAggregated) #G(q,2) #here we divided residual variance by a) the number of raters/assessors per exercise/rating (i.e., 2) and b) by the number of exercises that assessed a given dimension (i.e., 2)

#Figure 3#### Calculate amount of reliable variance depending on the number of exercises. For this exercise variance is divided by a (theoretical) number of exercises.
#not aggregated
vcomp.c/(vcomp.c + vcomp.d + vcomp.e)*100
vcomp.d/(vcomp.c + vcomp.d + vcomp.e)*100
(vcomp.e)/(vcomp.c + vcomp.d + vcomp.e)*100

#aggregated across 2 exercises
vcomp.c/(vcomp.c + vcomp.d + vcomp.e/2)*100
vcomp.d/(vcomp.c + vcomp.d + vcomp.e/2)*100
(vcomp.e/2)/(vcomp.c + vcomp.d + vcomp.e/2)*100

#aggregated across 3 exercises
vcomp.c/(vcomp.c + vcomp.d + vcomp.e/3)*100
vcomp.d/(vcomp.c + vcomp.d + vcomp.e/3)*100
(vcomp.e/3)/(vcomp.c + vcomp.d + vcomp.e/3)*100

#aggregated across 4 exercises
vcomp.c/(vcomp.c + vcomp.d + vcomp.e/4)*100
vcomp.d/(vcomp.c + vcomp.d + vcomp.e/4)*100
(vcomp.e/4)/(vcomp.c + vcomp.d + vcomp.e/4)*100

#aggregated across 5 exercises
vcomp.c/(vcomp.c + vcomp.d + vcomp.e/5)*100
vcomp.d/(vcomp.c + vcomp.d + vcomp.e/5)*100
(vcomp.e/5)/(vcomp.c + vcomp.d + vcomp.e/5)*100


###Online Supplement####

#Online Supplement 1 concerned age and gender of participants. To keep anonymization this data is not provided here.

#Online Supplement 2
#Calculate separate models per sample, one model with strict exclusion criteria, and one model based on imputed missings

#Sample 1
Sample1 <- subset(Data, Data$Sample =="WS18") #Only Sample 1
Sample1$Rating.10 <- scale(Sample1$Rating)*10
Sample1Model <- brm(Rating.10 ~ 1 +
                      (1|Assessee) + #variance related to assessee differences
                      (1|Assessee:Dimension) + #variance related to assessee differences depending on the skill dimensions
                      (1|Assessee:Dimension:Exercise) + #variance related to assessee differences in exercises (nested in dimensions)
                      # unreliable components  
                      (1 | Rater:Dimension:Exercise) + #systematic differences (e.g. leniency) of raters nested in exercises nested in dimensions
                      # unrelated
                      (1 | Dimension) + 
                      (1 | Dimension:Exercise) ,Sample1,family = gaussian,iter=10000,warmup = 5000,thin = 10, cores=getOption("mc.cores",4L),control = list(adapt_delta = 0.9999999,max_treedepth = 15)) 
summary(Sample1Model)

rr.mat <- table(Sample1$Assessee,Sample1$Rater)>0 #create ### ratee-rater matrix
(q.1.ha.bez <- q.multiplier(rr.mat,1)) #assign q-multiplier 0.9791636
(q.k.ha.bez <- q.multiplier(rr.mat,2)) # assign q-multiplier 0.4791636

vcomp.c <- (VarCorr(Sample1Model)$'Assessee'$sd^2)[c(1)]
vcomp.d <- (VarCorr(Sample1Model)$'Assessee:Dimension'$sd^2)[c(1)]
vcomp.e <- (VarCorr(Sample1Model)$'Assessee:Dimension:Exercise'$sd^2)[c(1)]
vcomp.j <- (VarCorr(Sample1Model)$'Rater:Dimension:Exercise'$sd^2)[c(1)]
vcomp.jq  <- (VarCorr(Sample1Model)$'Rater:Dimension:Exercise'$sd^2)[c(1)]* 0.9791636
vcomp.r <- (VarCorr(Sample1Model)$'residual__'$sd^2)[c(1)]

OverallBetweenPerson <- vcomp.c + vcomp.d + vcomp.e + vcomp.jq + vcomp.r
OverallReliable <- vcomp.c + vcomp.d + vcomp.e

vcomp.c/OverallReliable*100 #Reliable assessee variance in %
vcomp.d/OverallReliable*100 #Reliable assessee-dimension variance in %
vcomp.e/OverallReliable*100 #Reliable assessee-exercise variance in %
OverallReliable/OverallBetweenPerson #G(q,1)
OverallReliable/(OverallReliable+vcomp.j*0.4791636+vcomp.r/2) #G(q,2)

OverallReliableAggregated2 <- vcomp.c + vcomp.d + vcomp.e/2
vcomp.d/OverallReliableAggregated2*100 #Reliable assesseee-dimension variance on an aggregated level in %


#Sample 2
Sample2 <- subset(Data, Data$Sample =="SS19") #Only Sample 2
Sample2$Rating.10 <- scale(Sample2$Rating)*10
Sample2Model <- brm(Rating.10 ~ 1 +
                      (1|Assessee) + #variance related to assessee differences
                      (1|Assessee:Dimension) + #variance related to assessee differences depending on the skill dimensions
                      (1|Assessee:Dimension:Exercise) + #variance related to assessee differences in exercises (nested in dimensions)
                      # unreliable components  
                      (1 | Rater:Dimension:Exercise) + #systematic differences (e.g. leniency) of raters nested in exercises nested in dimensions
                      # unrelated
                      (1 | Dimension) + 
                      (1 | Dimension:Exercise) ,Sample2,family = gaussian,iter=10000,warmup = 5000,thin = 10, cores=getOption("mc.cores",4L),control = list(adapt_delta = 0.9999999,max_treedepth = 15)) 

summary(Sample2Model)
rr.mat <- table(Sample2$Assessee,Sample2$Rater)>0 #create ### ratee-rater matrix
(q.1.ha.bez <- q.multiplier(rr.mat,1)) #assign q-multiplier 0.9825696
(q.k.ha.bez <- q.multiplier(rr.mat,2)) # assign q-multiplier 0.4825696

vcomp.c <- (VarCorr(Sample2Model)$'Assessee'$sd^2)[c(1)]
vcomp.d <- (VarCorr(Sample2Model)$'Assessee:Dimension'$sd^2)[c(1)]
vcomp.e <- (VarCorr(Sample2Model)$'Assessee:Dimension:Exercise'$sd^2)[c(1)]
vcomp.j <- (VarCorr(Sample2Model)$'Rater:Dimension:Exercise'$sd^2)[c(1)]
vcomp.jq  <- (VarCorr(Sample2Model)$'Rater:Dimension:Exercise'$sd^2)[c(1)]* 0.9825696
vcomp.r <- (VarCorr(Sample2Model)$'residual__'$sd^2)[c(1)]

OverallBetweenPerson <- vcomp.c + vcomp.d + vcomp.e + vcomp.jq + vcomp.r
OverallReliable <- vcomp.c + vcomp.d + vcomp.e
OverallReliableAggregated <- vcomp.c + vcomp.d + vcomp.e/2

vcomp.c/OverallReliable*100 #Reliable assessee variance in %
vcomp.d/OverallReliable*100 #Reliable assessee-dimension variance in %
vcomp.e/OverallReliable*100 #Reliable assessee-exercise variance in %
OverallReliable/OverallBetweenPerson #G(q,1)
OverallReliable/(OverallReliable+vcomp.j*0.4825696+vcomp.r/2) #G(q,2)

OverallReliableAggregated2 <- vcomp.c + vcomp.d + vcomp.e/2
vcomp.d/OverallReliableAggregated2*100 #Reliable assesseee-dimension variance on an aggregated level in %


#Sample 3
Sample3 <- subset(Data, Data$Sample =="WS19") #Only Sample 3
Sample3$Rating.10 <- scale(Sample3$Rating)*10
Sample3Model <- brm(Rating.10 ~ 1 +
                      (1|Assessee) + #variance related to assessee differences
                      (1|Assessee:Dimension) + #variance related to assessee differences depending on the skill dimensions
                      (1|Assessee:Dimension:Exercise) + #variance related to assessee differences in exercises (nested in dimensions)
                      # unreliable components  
                      (1 | Rater:Dimension:Exercise) + #systematic differences (e.g. leniency) of raters nested in exercises nested in dimensions
                      # unrelated
                      (1 | Dimension) + 
                      (1 | Dimension:Exercise) ,Sample3,family = gaussian,iter=10000,warmup = 5000,thin = 10, cores=getOption("mc.cores",4L),control = list(adapt_delta = 0.99999999,max_treedepth = 15)) 

summary(Sample3Model)

rr.mat <- table(Sample3$Assessee,Sample3$Rater)>0 #create ### ratee-rater matrix
(q.1.ha.bez <- q.multiplier(rr.mat,1)) #assign q-multiplier 0.9823561
(q.k.ha.bez <- q.multiplier(rr.mat,2)) # assign q-multiplier 0.4823561

vcomp.c <- (VarCorr(Sample3Model)$'Assessee'$sd^2)[c(1)]
vcomp.d <- (VarCorr(Sample3Model)$'Assessee:Dimension'$sd^2)[c(1)]
vcomp.e <- (VarCorr(Sample3Model)$'Assessee:Dimension:Exercise'$sd^2)[c(1)]
vcomp.j <- (VarCorr(Sample3Model)$'Rater:Dimension:Exercise'$sd^2)[c(1)]
vcomp.jq  <- (VarCorr(Sample3Model)$'Rater:Dimension:Exercise'$sd^2)[c(1)]* 0.9823561
vcomp.r <- (VarCorr(Sample3Model)$'residual__'$sd^2)[c(1)]

OverallBetweenPerson <- vcomp.c + vcomp.d + vcomp.e + vcomp.jq + vcomp.r
OverallReliable <- vcomp.c + vcomp.d + vcomp.e
OverallReliableAggregated <- vcomp.c + vcomp.d + vcomp.e/2

vcomp.c/OverallReliable*100 #Reliable assessee variance in %
vcomp.d/OverallReliable*100 #Reliable assessee-dimension variance in %
vcomp.e/OverallReliable*100 #Reliable assessee-exercise variance in %
OverallReliable/OverallBetweenPerson #G(q,1)
OverallReliable/(OverallReliable+vcomp.j*0.4823561+vcomp.r/2) #G(q,2)

OverallReliableAggregated2 <- vcomp.c + vcomp.d + vcomp.e/2
vcomp.d/OverallReliableAggregated2*100 #Reliable assesseee-dimension variance on an aggregated level in %


#strict exclusion criteria
Control <- subset(Data, Data$SampleMajor !="WS19_Zahn" & Data$SampleMajor !="SS19_Zahn") #do not use dentistry sample 2 and 3 (because these individuals did not have at least two different skills assessed via at least two exercises)
Control <- subset(Control, Control$Sample =="WS18" | Control$Dimension =="Stability"| Control$Dimension =="Warmth") #and do not use persuasion in sample 2 and 3 (because this skill was only assessed via one exercise)
Control$Rating.10 <- scale(Control$Rating)*10

ControlModel <- brm(Rating.10 ~ 1 +
                   (1|Assessee:SampleMajor) + #variance related to assessee differences
                   (1|Assessee:Dimension:SampleMajor) + #variance related to assessee differences depending on the skill dimensions
                   (1|Assessee:Dimension:Exercise:SampleMajor) + #variance related to assessee differences in exercises (nested in dimensions)
                   # unreliable components  
                   (1 | Rater:Dimension:Exercise:SampleMajor) + #systematic differences (e.g. leniency) of raters nested in exercises nested in dimensions
                   # unrelated
                   (1 | Dimension) + 
                   (1 | Dimension:Exercise)+
                   (1 | SampleMajor) + 
                   (1 | Dimension:SampleMajor) + 
                   (1 | Dimension:Exercise:SampleMajor),Control,family = gaussian,iter=10000,warmup = 5000,thin = 10, cores=getOption("mc.cores",4L),control = list(adapt_delta = 0.99999999,max_treedepth = 15)) 

summary(ControlModel)

rr.mat <- table(Control$Assessee,Control$Rater)>0 #create ### ratee-rater matrix
(q.1.ha.bez <- q.multiplier(rr.mat,1)) #assign q-multiplier 0.9911936
(q.k.ha.bez <- q.multiplier(rr.mat,2)) # assign q-multiplier 0.4911936

vcomp.c <- (VarCorr(ControlModel)$'Assessee:SampleMajor'$sd^2)[c(1)]
vcomp.d <- (VarCorr(ControlModel)$'Assessee:Dimension:SampleMajor'$sd^2)[c(1)]
vcomp.e <- (VarCorr(ControlModel)$'Assessee:Dimension:Exercise:SampleMajor'$sd^2)[c(1)]
vcomp.j <- (VarCorr(ControlModel)$'Rater:Dimension:Exercise:SampleMajor'$sd^2)[c(1)]
vcomp.jq  <- (VarCorr(ControlModel)$'Rater:Dimension:Exercise:SampleMajor'$sd^2)[c(1)]* 0.9911936
vcomp.r <- (VarCorr(ControlModel)$'residual__'$sd^2)[c(1)]

OverallBetweenPerson <- vcomp.c + vcomp.d + vcomp.e + vcomp.jq + vcomp.r
OverallReliable <- vcomp.c + vcomp.d + vcomp.e
OverallReliableAggregated <- vcomp.c + vcomp.d + vcomp.e/2

vcomp.c/OverallReliable*100 #Reliable assessee variance in %
vcomp.d/OverallReliable*100 #Reliable assessee-dimension variance in %
vcomp.e/OverallReliable*100 #Reliable assessee-exercise variance in %
OverallReliable/OverallBetweenPerson #G(q,1)
OverallReliable/(OverallReliable+vcomp.j*0.4911936+vcomp.r/2) #G(q,2)


#based on multiple imputation (missing data pattern is MAR)
#please note that "mice" version 3.9.0 was used and results might vary slightly depending on the version

Data_red <- as.data.frame(Data)
Data_red$Rater_pos <- NA
for(i in unique(Data_red$Assessee)){
  for(j in unique(Data_red$Exercise)){
    for(k in unique(Data_red$Dimension)){
      if(nrow(Data_red[Data_red$Assessee==i & Data_red$Exercise==j & Data_red$Dimension==k,])==2){
        Data_red[Data_red$Assessee==i & Data_red$Exercise==j & Data_red$Dimension==k,"Rater_pos"] <- (1:2)[order(substr(Data_red[Data_red$Assessee==i & Data_red$Exercise==j & Data_red$Dimension==k,"Rater"],7,9))]
      }
    }
  }
}
Data_red_t <- Data_red[,names(Data_red)[c(1,4,5,9,2)]]
# into wide format
library(tidyr)
Data_red_wide <- pivot_wider(Data_red_t,
                             names_from = 2:4,
                             values_from = 5)
names(Data_red_wide) <- gsub(" ",".",names(Data_red_wide))
# missings per variable
apply(Data_red_wide,2,function(x)sum(is.na(x)))
# distribution of raters across exercises
apply(table(Data$Rater[Data$SampleMajor=="WS18_Human"],Data$Exercise[Data$SampleMajor=="WS18_Human"]),2,function(x)unique(x[x>0])/sum(unique(x[x>0])))
apply(table(Data$Rater[Data$SampleMajor=="WS18_Zahn"],Data$Exercise[Data$SampleMajor=="WS18_Zahn"]),2,function(x)unique(x[x>0])/sum(unique(x[x>0])))
apply(table(Data$Rater[Data$SampleMajor=="SS19_Human"],Data$Exercise[Data$SampleMajor=="SS19_Human"]),2,function(x)unique(x[x>0])/sum(unique(x[x>0])))
apply(table(Data$Rater[Data$SampleMajor=="SS19_Zahn"],Data$Exercise[Data$SampleMajor=="SS19_Zahn"]),2,function(x)unique(x[x>0])/sum(unique(x[x>0])))
apply(table(Data$Rater[Data$SampleMajor=="WS19_Human"],Data$Exercise[Data$SampleMajor=="WS19_Human"]),2,function(x)unique(x[x>0])/sum(unique(x[x>0])))
apply(table(Data$Rater[Data$SampleMajor=="WS19_Zahn"],Data$Exercise[Data$SampleMajor=="WS19_Zahn"]),2,function(x)unique(x[x>0])/sum(unique(x[x>0])))
### use the following number of phantom rater teams in imputation
### (uniformly distributed!
#WS18_Human: Mistake: 4 Teams
#WS18_Human: Presentation: 4 Teams  
#WS18_Zahn: Mistake: 2 Teams
#WS18_Zahn: Presentation: 2 Teams   
#SS19_Human: Unreasonable: 4 Teams
#SS19_Zahn: Unreasonable : 2 Teams  
#SS19_Zahn: Bad News : 2 Teams    
#WS19_Human: Unreasonable: 4 Teams
#WS19_Zahn: Unreasonable : 2 Teams  
#WS19_Zahn: Mistake : 2 Teams
#
### impute data

imp <- mice(Data_red_wide,m = 40)
### check densities of original 
### and imputed data
densityplot(imp)
### check convergence of imputations
plot(imp)
### imputations into long format
long <- complete(imp, "long", include = F)
### reshape into full long format
longlong <- pivot_longer(long, 4:15)
### recover exercise
longlong$Exercise <- sapply(longlong$name,function(x)unlist(strsplit(x,"_"))[1])
### recover dimension
longlong$Dimension <- sapply(longlong$name,function(x)unlist(strsplit(x,"_"))[2])
### recover rater_pos
longlong$Rater_pos <- sapply(longlong$name,function(x)unlist(strsplit(x,"_"))[3])
### merge with other information
agg_info <- aggregate(cbind(Sample,HumanZahn,SampleMajor) ~ Assessee, Data, unique)
longlong <- merge(longlong, agg_info,by = "Assessee",all.x=T)
### recover name Bad news for merging
longlong$Exercise <- gsub("\\."," ",longlong$Exercise)
### get rater ids back
longlong <- merge(longlong, Data_red[,c(1,3:5,9)],by = c("Assessee","Exercise","Dimension","Rater_pos"),all.x = T)
### create phantom rater list
#
### WS18_Human
longlong[longlong$SampleMajor=="WS18_Human"&longlong$Exercise=="Mistake",][order(longlong$.imp[longlong$SampleMajor=="WS18_Human"&longlong$Exercise=="Mistake"],
                                                                                 longlong$Assessee[longlong$SampleMajor=="WS18_Human"&longlong$Exercise=="Mistake"],longlong$Rater_pos[longlong$SampleMajor=="WS18_Human"&longlong$Exercise=="Mistake"]),"Rater"] <- paste0(c(rep(paste(sample(letters,size = 8),collapse = ""),74),
                                                                                                                                                                                                                                                                              rep(paste(sample(letters,size = 8),collapse = ""),74),
                                                                                                                                                                                                                                                                              rep(paste(sample(letters,size = 8),collapse = ""),74),
                                                                                                                                                                                                                                                                              rep(paste(sample(letters,size = 8),collapse = ""),76)),rep(1:2,length = 298))
longlong[longlong$SampleMajor=="WS18_Human"&longlong$Exercise=="Presentation",][order(longlong$.imp[longlong$SampleMajor=="WS18_Human"&longlong$Exercise=="Presentation"],
                                                                                      longlong$Assessee[longlong$SampleMajor=="WS18_Human"&longlong$Exercise=="Presentation"],longlong$Rater_pos[longlong$SampleMajor=="WS18_Human"&longlong$Exercise=="Presentation"]),"Rater"] <- paste0(c(rep(paste(sample(letters,size = 8),collapse = ""),74),
                                                                                                                                                                                                                                                                                             rep(paste(sample(letters,size = 8),collapse = ""),74),
                                                                                                                                                                                                                                                                                             rep(paste(sample(letters,size = 8),collapse = ""),74),
                                                                                                                                                                                                                                                                                             rep(paste(sample(letters,size = 8),collapse = ""),76)),rep(1:2,length = 298))


### WS18_Zahn
longlong[longlong$SampleMajor=="WS18_Zahn"&longlong$Exercise=="Mistake",][order(longlong$.imp[longlong$SampleMajor=="WS18_Zahn"&longlong$Exercise=="Mistake"],
                                                                                longlong$Assessee[longlong$SampleMajor=="WS18_Zahn"&longlong$Exercise=="Mistake"],longlong$Rater_pos[longlong$SampleMajor=="WS18_Zahn"&longlong$Exercise=="Mistake"]),"Rater"] <- paste0(c(rep(paste(sample(letters,size = 8),collapse = ""),53),
                                                                                                                                                                                                                                                                           rep(paste(sample(letters,size = 8),collapse = ""),53)),rep(1:2,length = 106))
longlong[longlong$SampleMajor=="WS18_Zahn"&longlong$Exercise=="Presentation",][order(longlong$.imp[longlong$SampleMajor=="WS18_Zahn"&longlong$Exercise=="Presentation"],
                                                                                     longlong$Assessee[longlong$SampleMajor=="WS18_Zahn"&longlong$Exercise=="Presentation"],longlong$Rater_pos[longlong$SampleMajor=="WS18_Zahn"&longlong$Exercise=="Presentation"]),"Rater"] <- paste0(c(rep(paste(sample(letters,size = 8),collapse = ""),53),
                                                                                                                                                                                                                                                                                          rep(paste(sample(letters,size = 8),collapse = ""),53)),rep(1:2,length = 106))
### SS19_Human
longlong[longlong$SampleMajor=="SS19_Human"&longlong$Exercise=="Unreasonable",][order(longlong$.imp[longlong$SampleMajor=="SS19_Human"&longlong$Exercise=="Unreasonable"],
                                                                                      longlong$Assessee[longlong$SampleMajor=="SS19_Human"&longlong$Exercise=="Unreasonable"],longlong$Rater_pos[longlong$SampleMajor=="SS19_Human"&longlong$Exercise=="Unreasonable"]),"Rater"] <- paste0(c(rep(paste(sample(letters,size = 8),collapse = ""),68),
                                                                                                                                                                                                                                                                                             rep(paste(sample(letters,size = 8),collapse = ""),68),
                                                                                                                                                                                                                                                                                             rep(paste(sample(letters,size = 8),collapse = ""),68),
                                                                                                                                                                                                                                                                                             rep(paste(sample(letters,size = 8),collapse = ""),70)),rep(1:2,length = 274))
### SS19_Zahn
longlong[longlong$SampleMajor=="SS19_Zahn"&longlong$Exercise=="Bad news",][order(longlong$.imp[longlong$SampleMajor=="SS19_Zahn"&longlong$Exercise=="Bad news"],
                                                                                 longlong$Assessee[longlong$SampleMajor=="SS19_Zahn"&longlong$Exercise=="Bad news"],longlong$Rater_pos[longlong$SampleMajor=="SS19_Zahn"&longlong$Exercise=="Bad news"]),"Rater"] <- paste0(c(rep(paste(sample(letters,size = 8),collapse = ""),54),
                                                                                                                                                                                                                                                                              rep(paste(sample(letters,size = 8),collapse = ""),54)),rep(1:2,length = 108))
longlong[longlong$SampleMajor=="SS19_Zahn"&longlong$Exercise=="Unreasonable",][order(longlong$.imp[longlong$SampleMajor=="SS19_Zahn"&longlong$Exercise=="Unreasonable"],
                                                                                     longlong$Assessee[longlong$SampleMajor=="SS19_Zahn"&longlong$Exercise=="Unreasonable"],longlong$Rater_pos[longlong$SampleMajor=="SS19_Zahn"&longlong$Exercise=="Unreasonable"]),"Rater"] <- paste0(c(rep(paste(sample(letters,size = 8),collapse = ""),54),
                                                                                                                                                                                                                                                                                          rep(paste(sample(letters,size = 8),collapse = ""),54)),rep(1:2,length = 108))
### WS19_Human
longlong[longlong$SampleMajor=="WS19_Human"&longlong$Exercise=="Unreasonable",][order(longlong$.imp[longlong$SampleMajor=="WS19_Human"&longlong$Exercise=="Unreasonable"],
                                                                                      longlong$Assessee[longlong$SampleMajor=="WS19_Human"&longlong$Exercise=="Unreasonable"],longlong$Rater_pos[longlong$SampleMajor=="WS19_Human"&longlong$Exercise=="Unreasonable"]),"Rater"] <- paste0(c(rep(paste(sample(letters,size = 8),collapse = ""),75),
                                                                                                                                                                                                                                                                                             rep(paste(sample(letters,size = 8),collapse = ""),75),
                                                                                                                                                                                                                                                                                             rep(paste(sample(letters,size = 8),collapse = ""),75),
                                                                                                                                                                                                                                                                                             rep(paste(sample(letters,size = 8),collapse = ""),75)),rep(1:2,length = 300))
### WS19_Zahn
longlong[longlong$SampleMajor=="WS19_Zahn"&longlong$Exercise=="Mistake",][order(longlong$.imp[longlong$SampleMajor=="WS19_Zahn"&longlong$Exercise=="Mistake"],
                                                                                longlong$Assessee[longlong$SampleMajor=="WS19_Zahn"&longlong$Exercise=="Mistake"],longlong$Rater_pos[longlong$SampleMajor=="WS19_Zahn"&longlong$Exercise=="Mistake"]),"Rater"] <- paste0(c(rep(paste(sample(letters,size = 8),collapse = ""),23),
                                                                                                                                                                                                                                                                           rep(paste(sample(letters,size = 8),collapse = ""),23)),rep(1:2,length = 46))
longlong[longlong$SampleMajor=="WS19_Zahn"&longlong$Exercise=="Unreasonable",][order(longlong$.imp[longlong$SampleMajor=="WS19_Zahn"&longlong$Exercise=="Unreasonable"],
                                                                                     longlong$Assessee[longlong$SampleMajor=="WS19_Zahn"&longlong$Exercise=="Unreasonable"],longlong$Rater_pos[longlong$SampleMajor=="WS19_Zahn"&longlong$Exercise=="Unreasonable"]),"Rater"] <- paste0(c(rep(paste(sample(letters,size = 8),collapse = ""),23),
                                                                                                                                                                                                                                                                                          rep(paste(sample(letters,size = 8),collapse = ""),23)),rep(1:2,length = 46))
### check number of raters for each station
rowSums(table(longlong$Exercise[longlong$.imp==1],
              longlong$Rater[longlong$.imp==1]),
        longlong$SampleMajor[longlong$.imp==1])/2
colSums(table(longlong$Exercise[longlong$.imp==1],
              longlong$Rater[longlong$.imp==1]),
        longlong$SampleMajor[longlong$.imp==1])/2
### rename dependent variable
names(longlong)[8] <- "Rating"
### create data list
dat_list <- vector(mode = "list", 40)
for(i in 1:40){
  dat_list[[i]] <- longlong[longlong$.imp==i,]
}


### run bayesian model
plan(multiprocess)
Imputed <- brm_multiple(I(scale(Rating)*10) ~ 1 +
                          (1|Assessee:SampleMajor) + #variance related to assessee differences
                          (1|Assessee:Dimension:SampleMajor) + #variance related to assessee differences depending on the skill dimensions
                          (1|Assessee:Dimension:Exercise:SampleMajor) + #variance related to assessee differences in exercises (nested in dimensions)
                          # unreliable components  
                          (1 | Rater:Dimension:Exercise:SampleMajor) + #Systematic differences (e.g. leniency) of raters nested in exercises nested in dimensions
                          # unrelated
                          (1 | Dimension) + 
                          (1 | Dimension:Exercise)+
                          (1 | SampleMajor) + 
                          (1 | Dimension:SampleMajor) + 
                          (1 | Dimension:Exercise:SampleMajor),dat_list,family = gaussian,iter=10000,warmup = 5000,thin = 10, control = list(adapt_delta = 0.999999999,max_treedepth = 15))


#assign q-multiplier 0.9937837 here we used the same q-multiplier as for the main results
#assign q-multiplier 0.4937837 here we used the same q-multiplier as for the main results

vcomp.c <- (VarCorr(Imputed)$'Assessee:SampleMajor'$sd^2)[c(1)]
vcomp.d <- (VarCorr(Imputed)$'Assessee:Dimension:SampleMajor'$sd^2)[c(1)]
vcomp.e <- (VarCorr(Imputed)$'Assessee:Dimension:Exercise:SampleMajor'$sd^2)[c(1)]
vcomp.j <- (VarCorr(Imputed)$'Rater:Dimension:Exercise:SampleMajor'$sd^2)[c(1)]
vcomp.jq  <- (VarCorr(Imputed)$'Rater:Dimension:Exercise:SampleMajor'$sd^2)[c(1)]* 0.9937837
vcomp.r <- (VarCorr(Imputed)$'residual__'$sd^2)[c(1)]

OverallBetweenPerson <- vcomp.c + vcomp.d + vcomp.e + vcomp.jq + vcomp.r
OverallReliable <- vcomp.c + vcomp.d + vcomp.e
OverallReliableAggregated <- vcomp.c + vcomp.d + vcomp.e/2

vcomp.c/OverallReliable*100 #Reliable assessee variance in %
vcomp.d/OverallReliable*100 #Reliable assessee-dimension variance in %
vcomp.e/OverallReliable*100 #Reliable assessee-exercise variance in %
OverallReliable/OverallBetweenPerson #G(q,1)
OverallReliable/(OverallReliable+vcomp.j*0.4937837+vcomp.r/2) #G(q,2)

#please note that the combined model may issue false positive convergence warnings, as the MCMC chains corresponding to different datasets may not necessarily overlap, even if each of the original models did converge. To find out whether each of the original models converged, investigate fit$rhats, where fit denotes the output of brm_multiple."
range(unlist(Imputed$rhats))
#rhats are ok

#there were some divergent transitions, test whether this impacted the results
### get posterior samples
ps <- posterior_samples(Imputed, add_chain = T)

###
quantile(ps[,grep("sd_Assessee:SampleMajor",names(ps))],
         p = c(.025,.975))

quantile(ps[,grep("sd_Assessee:Dimension:SampleMajor",names(ps))],
         p = c(.025,.975))

quantile(ps[,grep("sd_Assessee:Dimension:Exercise:SampleMajor",names(ps))],
         p = c(.025,.975))
### matches summary of Imputed!

### which chains are affected by divergent transitions?
np <- nuts_params(Imputed)
### get chains for which divergent transitions were found
chains_dt <- np[np$Parameter=="divergent__"&np$Value==1,"Chain"]

### posterior samples without divergent transitions
ps_no_dt <- ps[!ps$chain %in% chains_dt,]

### recheck credible interval without divergent transitions
quantile(ps_no_dt[,grep("sd_Assessee:SampleMajor",names(ps_no_dt))],
         p = c(.025,.975))

quantile(ps_no_dt[,grep("sd_Assessee:Dimension:SampleMajor",names(ps_no_dt))],
         p = c(.025,.975))

quantile(ps_no_dt[,grep("sd_Assessee:Dimension:Exercise:SampleMajor",names(ps_no_dt))],
         p = c(.025,.975))

### there is hardly any difference observable, so the divergent transitions did not meaningfully impact the results
