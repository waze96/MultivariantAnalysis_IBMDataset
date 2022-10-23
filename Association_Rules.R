#load packages
requiredPackages <- c("arules", "htmlwidgets", "arulesViz", "plotly", "ggplot2")
packages.check <- lapply(requiredPackages, FUN = function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Load data
PROJECT_PATH="~/Documents/UPC/1Q/MultivariateAnalysis_MVVA/Labs/MultivariantAnalysis_IBMDataset"

setwd(PROJECT_PATH)
load("./Data/df_preprocessed_IBM_Employees.Rdata") 

#remove irrelevant attributes
df <- subset(df, select = -c(Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager, MultivariateOutlier, SalaryHikePerYear, AverageYearsCompany, AgeToJubilation, DifferentManagers))

# Transform data into a transactional dataset
attrition_trx = as(df, "transactions")
str(attrition_trx)

itemFrequencyPlot(attrition_trx, topN=10, cex.names = 1) #top 10 frequent items

# We make a graph to determine the support level to keep for our analysis
confidenceLevels = seq(from=0.95, to=0.8, by=-0.01)

rules_sup20 = NULL
rules_sup25 = NULL
rules_sup30 = NULL
rules_sup35 = NULL
rules_sup40 = NULL
rules_sup45 = NULL
rules_sup50 = NULL

for (i in 1:length(confidenceLevels)) {
  rules_attrition = apriori(attrition_trx, parameter = list(supp = 0.20, conf=confidenceLevels[i]))
  rules_red = is.redundant(rules_attrition)
  rules_sup20[i] = length(rules_attrition[!rules_red])
}
for (i in 1:length(confidenceLevels)) {
  rules_attrition = apriori(attrition_trx, parameter = list(supp = 0.25, conf=confidenceLevels[i]))
  rules_red = is.redundant(rules_attrition)
  rules_sup25[i] = length(rules_attrition[!rules_red])
}
for (i in 1:length(confidenceLevels)) {
  rules_attrition = apriori(attrition_trx, parameter = list(supp = 0.30, conf=confidenceLevels[i]))
  rules_red = is.redundant(rules_attrition)
  rules_sup30[i] = length(rules_attrition[!rules_red])
}
for (i in 1:length(confidenceLevels)) {
  rules_attrition = apriori(attrition_trx, parameter = list(supp = 0.35, conf=confidenceLevels[i]))
  rules_red = is.redundant(rules_attrition)
  rules_sup35[i] = length(rules_attrition[!rules_red])
}
for (i in 1:length(confidenceLevels)) {
  rules_attrition = apriori(attrition_trx, parameter = list(supp = 0.40, conf=confidenceLevels[i]))
  rules_red = is.redundant(rules_attrition)
  rules_sup40[i] = length(rules_attrition[!rules_red])
}
for (i in 1:length(confidenceLevels)) {
  rules_attrition = apriori(attrition_trx, parameter = list(supp = 0.45, conf=confidenceLevels[i]))
  rules_red = is.redundant(rules_attrition)
  rules_sup45[i] = length(rules_attrition[!rules_red])
}
for (i in 1:length(confidenceLevels)) {
  rules_attrition = apriori(attrition_trx, parameter = list(supp = 0.50, conf=confidenceLevels[i]))
  rules_red = is.redundant(rules_attrition)
  rules_sup50[i] = length(rules_attrition[!rules_red])
}

nb_rules = data.frame(rules_sup20, rules_sup25 ,rules_sup30, rules_sup35, rules_sup40, rules_sup45, rules_sup50, confidenceLevels)
ggplot(data=nb_rules, aes(x=confidenceLevels)) +
  #geom_line(aes(y=rules_sup20, colour="Support level of 20%")) + 
  #geom_point(aes(y=rules_sup20,colour="Support level of 20%")) +
  geom_line(aes(y=rules_sup25, colour="Support level of 25%")) + 
  geom_point(aes(y=rules_sup25,colour="Support level of 25%")) +
  geom_line(aes(y=rules_sup30, colour="Support level of 30%")) + 
  geom_point(aes(y=rules_sup30,colour="Support level of 30%")) +
  geom_line(aes(y=rules_sup35, colour="Support level of 35%")) + 
  geom_point(aes(y=rules_sup35,colour="Support level of 35%")) +
  geom_line(aes(y=rules_sup40, colour="Support level of 40%")) + 
  geom_point(aes(y=rules_sup40,colour="Support level of 40%")) +
  geom_line(aes(y=rules_sup45, colour="Support level of 45%")) + 
  geom_point(aes(y=rules_sup45,colour="Support level of 45%")) +
  #geom_line(aes(y=rules_sup50, colour="Support level of 50%")) + 
  #geom_point(aes(y=rules_sup50,colour="Support level of 50%")) +
  # Polishing the graph
  theme_bw() + ylab("") + geom_hline(yintercept=20) + geom_vline(xintercept = 0.85) +
  ggtitle("Number of extracted rules with Apriori after deletion of redudant rules")

# The rules with the parameter choosen
rules_attrition = apriori(attrition_trx, parameter = list(supp = 0.35, conf = 0.85))
rules_red = is.redundant(rules_attrition)
rules = rules_attrition[!rules_red]

# For using both algo, we have to obtain the same result
itemsets <- eclat(attrition_trx, parameter = list(supp = 0.35))
rules_with_eclat <- ruleInduction(itemsets, attrition_trx, confidence = 0.85)
rules_red = is.redundant(rules_with_eclat)
rules_with_eclat = rules_with_eclat[!rules_red]

summary(rules)
inspect(sort(rules, by="confidence"))
inspect(sort(rules_with_eclat, by="confidence")) # we obtain the same result as expected

# Plot rules as a graph
plot(rules,
     method = "graph",
     engine = "htmlwidget")

# Plot rules as scatterplot
plot(rules,
     measure = c("confidence", "lift"),
     shading = "confidence",
     jitter = 1,
     engine = "html")

# Interactive matrix-based plot
plot(rules, method = "matrix",
     shading ="confidence",
     engine = "html"
)

# Grouped matrix plot of rules
plot(rules, 
     method = "grouped",
     measure = "lift",
     shading = "confidence")

# Parallel coordinate plots with confidence as color coding
plot(rules, 
     method = "paracoord", 
     shading = "confidence")

# Extract rules with attrition on the right side
attrition_rules_rhs = apriori(attrition_trx, 
                              parameter = list(supp = 0.1,
                                               conf = 0.5), 
                              appearance = list(default = "lhs",
                                                rhs = "Attrition=Yes")) 

# Inspect the rules
inspect(attrition_rules_rhs) #0 rules obtained even after lowering support and confidence level
summary(attrition_rules_rhs)

# Extract rules with attrition on the left side
attrition_rules_lhs = apriori(attrition_trx, 
                              parameter = list(supp = 0.1,
                                               conf = 0.5), 
                              appearance = list(
                                default = "rhs",
                                lhs = "Attrition=Yes")) 

# Inspect the rules
inspect(attrition_rules_lhs) #12 rules obtained with low confidence level
summary(attrition_rules_lhs)
