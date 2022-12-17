library(MASS)

#load data
setwd("~/GitHub/MultivariantAnalysis_IBMDataset/")
load("./Data/df_preprocessed_IBM_Employees.Rdata") 
str(df)

#transform to factor
df$Attrition=as.factor(df$Attrition)

#keep only numeric and moving attrition to first column to make things easier
df2 <- df[,c(2,1,4,6,13,19,20,21,24,29,30,32,33,34,35)]
str(df2)

#LDA
attrition.lda <- lda(df$Attrition ~ df$Age + df$DailyRate + df$DistanceFromHome + df$HourlyRate + 
                       df$MonthlyIncome + df$MonthlyRate + df$NumCompaniesWorked + df$PercentSalaryHike + 
                       df$TotalWorkingYears + df$TrainingTimesLastYear + df$YearsAtCompany + 
                       df$YearsInCurrentRole + df$YearsSinceLastPromotion + df$YearsWithCurrManager)
attrition.lda

#coefficients of the LDA
attrition.lda$scaling

names(df2)
attrition.lda.values <- predict(attrition.lda, df2[2:15])

df2[,16]<- attrition.lda.values$x[,1]
attrition.lda.values$x[,1]
names(df2)[16]<-"LDA1"

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
  for (i in 1:numvariables)
  {
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
groupstandardisedconcentrations <- groupStandardise(df2[2:15], df2[1]) 

calcBetweenGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the overall grand mean:
  grandmean <- mean(as.matrix(variable) )         
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
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

calcSeparations <- function(variables,groupvariable)
{
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
calcSeparations(attrition.lda.values$x,df2[1])

#total separation

hist(attrition.lda.values$x[,1])

ldahist(data = attrition.lda.values$x[,1], g=df2$Attrition)

#plot  of LDA axis and observations 
plot(attrition.lda.values$x[,1]) # make a scatterplot

plot(df2[,16])
text(attrition.lda.values$x[,1],df2$Attrition,cex=0.7,pos=4,col="red")

plot(df2[,16], type="n")
text(attrition.lda.values$x[,1],df2$Attrition,cex=0.7,pos=4,col="red")

attrition.lda$scaling[,1]

# Use the rules to classification task
par(mfrow=c(1,2))
ldahist(data = df2[,16], g=df2$Attrition)

n<-dim(df2)[1]
for(i in 1:n){ #this i am not sure about
  if(df2[i,16]<0){df2[i,17]<-1
  }else{
    df2[i,17]<-2
  }
}


#confusion matrix
table(df2[,1])
MC<-table(df2[,1], df2[,17])
MC


#accuracy
accuracy<-sum(diag(MC))/dim(df2)[1]
accuracy

#compute missclassification rate
MR<-1-accuracy
MR

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
