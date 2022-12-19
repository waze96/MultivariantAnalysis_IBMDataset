library(MASS)
set.seed(2)
#load data
load("./Data/df_preprocessed_IBM_Employees.Rdata")
str(df)

dcon<-df[!sapply(df, is.factor)]
dcon$Attrition<-df$Attrition
dcon<-dcon[,c(which(colnames(dcon)=="Attrition"),which(colnames(dcon)!="Attrition"))]

#keep only numeric and moving attrition to first column to make things easier
df2 <- dcon
str(df2)

df2<-subset(df2, select = c(Attrition,Age,MonthlyIncome,NumCompaniesWorked,
                            TotalWorkingYears,YearsAtCompany,SalaryHikePerYear,
                            YearsInCurrentRole,YearsSinceLastPromotion,AverageYearsCompany))

dcon1<-subset(dcon, select = -c(HourlyRate,DailyRate,MonthlyRate))

## SPLIT
llwork <- sample(1:nrow(df2),round(0.8*nrow(df2),dig=0))
dfwork <- df2[llwork,]
dftest <- df2[-llwork,]

dftest1 <- dftest
dfwork1<-dfwork

dfwork2 <- dcon1[llwork,]
dftest2 <- dcon1[-llwork,]

dftest3 <- dftest2
dfwork3 <- dfwork2

#subset
#LDA
attrition.lda <- lda(Attrition~Age+MonthlyIncome+NumCompaniesWorked+TotalWorkingYears+
                       YearsAtCompany*SalaryHikePerYear+YearsInCurrentRole+
                       YearsSinceLastPromotion+AverageYearsCompany, dfwork)

attrition.lda1 <- lda(Attrition~Age+MonthlyIncome+NumCompaniesWorked+TotalWorkingYears+
                        YearsAtCompany+SalaryHikePerYear+YearsInCurrentRole+
                        YearsSinceLastPromotion+AverageYearsCompany, dfwork1)

attrition.lda2 <- lda(Attrition~Age+MonthlyIncome+NumCompaniesWorked+
                        PercentSalaryHike+TotalWorkingYears+TrainingTimesLastYear+YearsAtCompany+
                        YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager+
                        SalaryHikePerYear+AverageYearsCompany, dfwork2)

attrition.lda3 <- lda(Attrition~Age*MonthlyIncome*NumCompaniesWorked*
                        PercentSalaryHike*TotalWorkingYears*TrainingTimesLastYear*YearsAtCompany*
                      YearsInCurrentRole*YearsSinceLastPromotion*YearsWithCurrManager*
                        SalaryHikePerYear*AverageYearsCompany, dfwork3)

#coefficients of the LDA
attrition.lda$scaling
attrition.lda1$scaling
attrition.lda2$scaling
attrition.lda3$scaling

# Predictions
attrition.lda.valuesw  <- predict(attrition.lda, dfwork)
attrition.lda.values1w <- predict(attrition.lda1, dfwork1)
attrition.lda.values2w <- predict(attrition.lda2, dfwork2)
attrition.lda.values3w <- predict(attrition.lda3, dfwork3)

attrition.lda.values  <- predict(attrition.lda, dftest)
attrition.lda.values1 <- predict(attrition.lda1, dftest1)
attrition.lda.values2 <- predict(attrition.lda2, dftest2)
attrition.lda.values3 <- predict(attrition.lda3, dftest3)


dfwork[,11]<- attrition.lda.valuesw$x[,1]
dfwork1[,11]<- attrition.lda.values1w$x[,1]
dfwork2[,15]<- attrition.lda.values2w$x[,1]
dfwork3[,15]<- attrition.lda.values3w$x[,1]

dftest[,11]<- attrition.lda.values$x[,1]
dftest1[,11]<- attrition.lda.values1$x[,1]
dftest2[,15]<- attrition.lda.values2$x[,1]
dftest3[,15]<- attrition.lda.values3$x[,1]

names(dfwork)[11]<-"LDA1"
names(dfwork1)[11]<-"LDA1"
names(dfwork2)[15]<-"LDA1"
names(dfwork3)[15]<-"LDA1"

names(dftest)[11]<-"LDA1"
names(dftest1)[11]<-"LDA1"
names(dftest2)[15]<-"LDA1"
names(dftest3)[15]<-"LDA1"

calcWithinGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the standard deviation for group i:
    sdi <- sd(levelidata)
    numi <- (levelilength - 1)*(sdi * sdi)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the within-groups variance
  Vw <- numtotal / (denomtotal - numlevels)
  return(Vw)
}


groupStandardise <- function(variables, groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the group-standardised version of each variable
  for (i in 1:numvariables) {
    variablei <- variables[i]
    variablei_name <- variablenames[i]
    variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    variablei_mean <- mean(as.matrix(variablei))  
    variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
    data_length <- nrow(variablei)
    if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
    variables_new[`variablei_name`] <- variablei_new
  }
  return(variables_new)
}


#normalizing features
groupstandardisedconcentrations <- groupStandardise(dfwork[2:10], dfwork[1]) 

calcBetweenGroupsVariance <- function(variable,groupvariable){
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the overall grand mean:
  grandmean <- mean(as.matrix(variable) )         
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels) {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the mean and standard deviation for group i:
    meani <- mean( as.matrix(levelidata) )
    sdi <- sd(levelidata)
    numi <- levelilength * ((meani - grandmean)^2)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the between-groups variance
  Vb <- numtotal / (numlevels - 1)
  Vb <- Vb[[1]]
  return(Vb)
}

calcSeparations <- function(variables,groupvariable){
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the separation for each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablename <- variablenames[i]
    Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    Vb <- calcBetweenGroupsVariance(variablei, groupvariable)
    sep <- Vb/Vw
    print(paste("variable",variablename,"Vw=",Vw,"Vb=",Vb,"separation=",sep))
  }
}

# Separation between the LDA axis (ratio of within & between distance)
calcSeparations(attrition.lda.values$x,dftest[1])
calcSeparations(attrition.lda.values1$x,dftest1[1])
calcSeparations(attrition.lda.values2$x,dftest2[1])
calcSeparations(attrition.lda.values3$x,dftest3[1])

#total separation
par(mfrow=c(1,1))
hist(attrition.lda.values$x[,1])
hist(attrition.lda.values1$x[,1])
hist(attrition.lda.values2$x[,1])
hist(attrition.lda.values3$x[,1])

#plot  of LDA axis and observations 
plot(attrition.lda.values$x[,1]) # make a scatterplot
plot(attrition.lda.values1$x[,1]) # make a scatterplot
plot(attrition.lda.values2$x[,1]) # make a scatterplot
plot(attrition.lda.values3$x[,1]) # make a scatterplot

attrition.lda$scaling[,1]
attrition.lda1$scaling[,1]
attrition.lda2$scaling[,1]
attrition.lda3$scaling[,1]

# Use the rules to classification task
par(mfrow=c(1,2))
ldahist(data = dftest[,11], g=dftest$Attrition)
ldahist(data = dftest1[,11], g=dftest1$Attrition)
ldahist(data = dftest2[,15], g=dftest2$Attrition)
ldahist(data = dftest3[,15], g=dftest3$Attrition)

n<-dim(dftest)[1]
for(i in 1:n){
  if(dfwork[i,11]<0) dfwork[i,12]<-'No'
  else dfwork[i,12]<-'Yes'
  if(dfwork1[i,11]<0) dfwork1[i,12]<-'No'
  else dfwork1[i,12]<-'Yes'
  if(dfwork2[i,15]<0) dfwork2[i,16]<-'No'
  else dfwork2[i,16]<-'Yes'
  if(dfwork3[i,15]<0) dfwork3[i,16]<-'No'
  else dfwork3[i,16]<-'Yes'
  
  if(dftest[i,11]<0) dftest[i,12]<-'No'
  else dftest[i,12]<-'Yes'
  if(dftest1[i,11]<0) dftest1[i,12]<-'No'
  else dftest1[i,12]<-'Yes'
  if(dftest2[i,15]<0) dftest2[i,16]<-'No'
  else dftest2[i,16]<-'Yes'
  if(dftest3[i,15]<0) dftest3[i,16]<-'No'
  else dftest3[i,16]<-'Yes'
}


#confusion matrix
table(dftest[,1])
MCw<-table(dfwork[,1], dfwork[,12]); MCw
MC1w<-table(dfwork1[,1], dfwork1[,12]); MC1w
MC2w<-table(dfwork2[,1], dfwork2[,16]); MC2w
MC3w<-table(dfwork3[,1], dfwork3[,16]); MC3w

MC<-table(dftest[,1], dftest[,12]); MC
MC1<-table(dftest1[,1], dftest1[,12]); MC1
MC2<-table(dftest2[,1], dftest2[,16]); MC2
MC3<-table(dftest3[,1], dftest3[,16]); MC3


#accuracy
accuracyw<-sum(diag(MCw))/dim(dfwork)[1]; accuracyw
accuracy1w<-sum(diag(MC1w))/dim(dfwork1)[1]; accuracy1w
accuracy2w<-sum(diag(MC2w))/dim(dfwork2)[1]; accuracy2w
accuracy3w<-sum(diag(MC3w))/dim(dfwork3)[1]; accuracy3w

accuracy<-sum(diag(MC))/dim(dftest)[1]; accuracy
accuracy1<-sum(diag(MC1))/dim(dftest1)[1]; accuracy1
accuracy2<-sum(diag(MC2))/dim(dftest2)[1]; accuracy2
accuracy3<-sum(diag(MC3))/dim(dftest3)[1]; accuracy3

#recallPositive
recallw<-diag(MCw)[2]/colSums(MCw)[2]; recallw
recall1w<-diag(MC1w)[2]/colSums(MC1w)[2]; recall1w
recall2w<-diag(MC2w)[2]/colSums(MC2w)[2]; recall2w
recall3w<-diag(MC3w)[2]/colSums(MC3w)[2]; recall3w

recall<-diag(MC)[2]/colSums(MC)[2]; recall
recall1<-diag(MC1)[2]/colSums(MC1)[2]; recall1
recall2<-diag(MC2)[2]/colSums(MC2)[2]; recall2
recall3<-diag(MC3)[2]/colSums(MC3)[2]; recall3


#compute missclassification rate
MR<-1-accuracy
MR

plot(attrition.lda, col=as.numeric(dftest$Attrition)) # assign color code based on factor code

# Looking for middle points around medians to use them for classification task


printMeanAndSdByGroup <- function(variables,groupvariable)
{
  # find the names of the variables
  variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
  # within each group, find the mean of each variable
  groupvariable <- groupvariable[,1] # ensures groupvariable is not a list
  means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
  names(means) <- variablenames
  print(paste("Means:"))
  print(means)
  # within each group, find the standard deviation of each variable:
  sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
  names(sds) <- variablenames
  print(paste("Standard deviations:"))
  print(sds)
  # within each group, find the number of samples:
  samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
  names(samplesizes) <- variablenames
  print(paste("Sample sizes:"))
  print(samplesizes)
}

#Medians(Statistics) of the Discriminant functions by groups
printMeanAndSdByGroup(attrition.lda.values$x,df2[1])

plot(df2[,2],df2[,3], col="red") #we can choose better attributes to plot
plot(df2[,5],df2[,7], col="red")

