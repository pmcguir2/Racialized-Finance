# In this tutorial, we’ll guide you through some simple analysis of the Federal 
# Reserve Bank’s 2019 Survey of Consumer Finances (SCF) in order to assess the 
# presence of racial bias in consumer lending algorithms, as well as the American
# economic context more broadly. To proceed through this tutorial, simply run 
# each line of code in sequential order. These comments will help guide you 
# through the data and methods along the way.

# First, we’ll begin by installing and loading all of the packages we will need 
# for this analysis:

install.packages('haven')
install.packages('Hmisc')
install.packages('ggplot2')
install.packages('SciViews')
install.packages('dplyr')
install.packages('stats')
library(stats)
library(dplyr)
library(SciViews)
library(ggplot2)
library(Hmisc)
library(haven)

# Next, we will download the data file necessary for our analysis from the Fed’s
# website. It comes as a compressed Stata .dta file, but we can unzip it and 
# read it into R for our analysis.

download.file("https://www.federalreserve.gov/econres/files/scf2019s.zip", "p19i6.zip" )

unzip('p19i6.zip', 'p19i6.dta')

data_2019 <- read_dta('p19i6.dta')

# Now that we have our data, we can get a sense of how it is structured:

dim.data.frame(data_2019)

# As we can see, we have 28,885 unique observations (respondents) of 5,333 
# variables. That’s quite a few variables! This is an extremely intensive
# survey which asks respondents about all types of debts, assets, financial 
# practices, savings habits, etc. - hence the 5,333 variables! In order to 
# understand what they all mean, we can look to the Fed’s codebook, which 
# breaks down the meaning of each variable (and the values it can take): 
# https://www.federalreserve.gov/econres/files/codebk2019.txt

# Looking at the codebook, we can identify key variables about demographic 
# information, such as race, which will be key to our analysis. We can begin to
# use these variables to assess the racial and ethnic makeup of our sample.

# Variable x6809 shows respondent race. Let’s see what its distribution looks like.

Hmisc::describe(data_2019$x6809)

# From this result, we can see that our sample is approximately 72% white, 
# 13% Black, and 10% other. Not considering Hispanic/Latino status, this is 
# representative of the US as a whole.

# Variable x7004 shows whether the respondent identifies as Hispanic/Latino:

Hmisc::describe(data_2019$x7004)

# We can see that approximately 12% of our sample is Hispanic/Latino. This again
# is representative of the US population.

# In our analysis, we focus on racial biases particularly against Black, 
# non-Latino individuals. For this reason, we construct a new variable 
# indicating whether someone in our sample is in this category.

data_2019$blacknonlatino <- 0

for (i in 1: nrow(data_2019)){
  if (data_2019$x6809[i]== 2 & data_2019$x7004[i] == 5){
    data_2019$blacknonlatino[i] = 1}
}

Hmisc::describe(data_2019$blacknonlatino)

# 12.62% of our sample is Black and non-Latino. This is going to be a subset of 
# our sample that we focus on in our analysis.

# Because we want to investigate the presence of racial bias in consumer lending 
# practices, it’s important to consider what goes into consumer determinations 
# of “creditworthiness.” As would be expected, key determinants of 
# creditworthiness include income and assets. We’ll begin by exploring these two.

# First, we'll take a look at annual income from salary/wages, captured in 
# variable x5702. To get a sense of the distribution of this variable, we can 
# create a histogram:

ggplot(data_2019, aes(x5702)) +
  geom_histogram(bins=100)

# Clearly, this distribution is extremely skewed; due to a small amount of 
# extremely high earners, it is nearly impossible to see what’s going on in the 
# bulk of the distribution.

# To see just how skewed it is, let's compare the mean income in our dataset:

mean(data_2019$x5702)

# To our median income: 

median(data_2019$x5702)

# That's a $90,000 difference! In addition to revealing remarkable income 
# inequality, that skew is going to make statistical analysis more difficult 
# than if it were more normally distributed. 

# Therefore, we can take the natural log of income to produce a more normal 
# distribution:

data_2019$lnincome <- ln(data_2019$x5702)


# We can re-generate a histogram to show this fact:
ggplot(data_2019, aes(lnincome)) +
  geom_histogram(bins=100)

# Importantly, however, these variables now no longer reveal just dollar figures
# of income - they reflect log levels of income, which represent percent 
# differences.

# Again, the SCF contains quite a few variables, many which measure different 
# sources of income. The Fed identifies 12 variables which, when combined, 
# produce a respondent's full annual income.These include, among others, 
# investment income, property income, government benefits, insurance payments, 
# annuities, retirement income, and more.

# For our analysis, we can combine each of those into a measure of annual income:

data_2019$totalincome <- (data_2019$x5702 + data_2019$x5704+data_2019$x5706 + data_2019$x5708 + data_2019$x5710+data_2019$x5712+data_2019$x5714+data_2019$x5716+data_2019$x5718+data_2019$x5720+data_2019$x5722+data_2019$x5724)

# Like in the previous steps, we can also take a log of this variable to produce
# a more normal distribution:

data_2019$lntotalincome <- ln(data_2019$totalincome)

# Now, let’s turn from income to wealth, another important factor in determining 
# creditworthiness about which the SCF collects a great deal of information. 
# Whereas income shows the funds coming into a household annually, wealth shows 
# the household's net worth - their assets subtracted by their debts. 
# First, let’s take a look at assets.

# Though the Fed doesn’t offer an official group of variables which can be 
# combined to get total assets, we identified 24 variables which measure 
# different sources of wealth. These include home equity, car equity, money in 
# checking and savings accounts, investments, and more. We can combine these 
# variables to produce a rough estimate of a respondent’s assets:


data_2019$roughassets <- (data_2019$x3506 + data_2019$x3510 + data_2019$x3514 + data_2019$x3518 + data_2019$x3522 + data_2019$x3526 + data_2019$x6551 + data_2019$x6559 + data_2019$x6567 + data_2019$x6552 + data_2019$x6560 + data_2019$x6568 + data_2019$x3730 + data_2019$x3736 + data_2019$x3742 + data_2019$x3748 + data_2019$x3754 + data_2019$x3760 + data_2019$x3728 + data_2019$x6580 + data_2019$x4005 + data_2019$x4022 + data_2019$x4026 + data_2019$x4030)

# Similarly, we can combine a number of different variables to get a rough 
# estimate of a respondent’s total debts. We identified 11 variables which 
# measure total amounts owed on certain sources of debt. We combine them here:

data_2019$roughdebts <- (data_2019$x1108 + data_2019$x1119 + data_2019$x1130 + data_2019$x1136 + data_2019$x1219 + data_2019$x1339 + data_2019$x2006 + data_2019$x2424 + data_2019$x7179 + data_2019$x7183 + data_2019$x6437)

# To get a rough estimate of a respondent’s net worth, we can subtract their 
# debts from their assets:

data_2019$roughNW <- (data_2019$roughassets - data_2019$roughdebts)

# To get a sense of how this data is distributed, let’s look at the sample 
# median and mean net worth:

mean(data_2019$roughNW)
median(data_2019$roughNW)


# There is an even more significant rightward skew in the wealth distribution 
# than in income. Therefore, we will take another log of this variable to 
# produce a more normal distribution for subsequent analysis.

data_2019$lnroughNW <- ln(data_2019$roughNW)
# Having looked at assets and debts, we can turn to other determinants of credit
# worthiness. We focus particularly on missed payments, foreclosures, and 
# bankruptcy. Each of these negatively impacts a respondent’s credit. 
# Fortunately, the SCF has valuable data on these as well.

# Variable x6772 shows whether a respondent has ever declared bankruptcy, 
# variable x3031 if they have ever faced foreclosure, and 3004 whether they make
# on time payments. We can clean up these data to produce dummy variables which
# show either a 1 (yes) or 0 (no) to simplify our analysis.

data_2019$bankruptcy = 0

for (i in 1: nrow(data_2019)){
  if (data_2019$x6772[i]== 1){
    data_2019$bankruptcy[i] = 1}
}

data_2019$foreclosure = 0 

for (i in 1: nrow(data_2019)){
  if (data_2019$x3031[i]== 1){
    data_2019$foreclosure[i] = 1}
}

data_2019$ontimepayments = 0 
for (i in 1: nrow(data_2019)){
  if (data_2019$x3004[i]== 1){
    data_2019$ontimepayments[i] = 1}
}

# Now, we can take a look at the distribution of each variable:

Hmisc::describe(data_2019$bankruptcy)
Hmisc::describe(data_2019$foreclosure)
Hmisc::describe(data_2019$ontimepayments)


# These variables, along with respondent’s wealth and income, are considered for 
# credit worthiness. However, in order to compare actual determinations of credit
# worthiness by consumer creditors, we need data on loan rejections. Fortunately,
# the SCF includes this data as well.

# Variable x407 refers to whether or not the respondent was turned down for a 
# credit application in the past year. The responses include:

# 1.    Turned down
# 3.    Not given as much as they wanted
# 5.    Neither

# To make our analysis simpler, we can combine variables 1 and 3 to create a 
# single measure of loan rejection. This is an imperfect solution, but will make
# our analysis simpler and will still convey a general outcome of people not 
# obtaining the credit they need.

data_2019$rejected <- 0
for (i in 1: nrow(data_2019)){
  if ((data_2019$x407[i]== 1) | (data_2019$x407[i]==3)){
    data_2019$rejected[i] = 1}
}

# Now, we can assess how many people in our sample were rejected for loans in 
# the past year.

Hmisc::describe(data_2019$rejected)

# Approximately 10.92% of respondents didn’t get the credit they needed. With 
# this baseline understanding in mind, we can now consider how these rejections
# differ across racial lines. First, however, we should consider how the 
# determinants of creditworthiness differ across racial lines.

# We can start to investigate racial differences between our subset of interest 
# - Black non-Latino respondents - and the sample as a whole by constructing a 
# subsample and comparing it to the main sample:

data_blacknonlatino <- filter(data_2019, blacknonlatino == 1)

# Now that we have two distinct samples, we can compare their incomes, net 
# worth, and likelihood of having a foreclosure, bankruptcy, and on time 
# payments.

Hmisc::describe(data_blacknonlatino$totalincome)

Hmisc::describe(data_2019$totalincome)

# As we can see, Black respondents have about half of the median total income 
# of the sample mean.

Hmisc::describe(data_blacknonlatino$roughNW)

Hmisc::describe(data_2019$roughNW)

# This disparity is even wider for net worth; Black respondents have one tenth 
# of the wealth of the sample mean.

Hmisc::describe(data_blacknonlatino$bankruptcy)

Hmisc::describe(data_2019$bankruptcy)

# Black respondents are 2 percentage points more likely to have declared 
# bankruptcy than the sample mean.

Hmisc::describe(data_blacknonlatino$ontimepayments)

Hmisc::describe(data_2019$ontimepayments)

# They are also 11 percentage points more likely to be behind on payments.

Hmisc::describe(data_blacknonlatino$foreclosure)

Hmisc::describe(data_2019$foreclosure)

# Notably, Black respondents are less likely to have had a foreclosure, likely 
# because they are less likely to own their home.

Hmisc::describe(data_blacknonlatino$rejected)

Hmisc::describe(data_2019$rejected)

# Finally, Black respondents are about 6 percentage points more likely to be 
# rejected for a loan compared to the sample as a whole.

# Without going any further in our analysis, these findings are critically 
# important; they reveal dramatic disparities in wealth, income, opportunity, 
# and financial security for Black Americans. These disparities, themselves 
# created by historical and contemporary discrimination and exploitation in 
# employment, education, banking, housing, health, and many other spaces, 
# inevitably are translated to loan rejections through creditworthiness 
# algorithms. These findings reveal biases and structural racism embedded and 
# encoded throughout our society, and without a race-conscious approach to 
# technology or broader systems, an “objective” algorithm will reproduce these 
# inequities. 

# Yet, as we are trying to highlight whether additional bias exists beyond this 
# reproducing effect, we proceed with a statistical method in which we can assess
# the extent to which race predicts the likelihood of a loan rejection, 
# controlling for all of the variables we have outlined which determine 
# creditworthiness.

# To do so, we use a regression model, through which we can measure the effect 
# of a change in an independent variable on the outcome of a dependent variable. 
# Specifically, we will use a logistic regression model, which allows us to 
# measure the impact of independent variables on the outcome of a “binomial” 
# dependent variable (i.e. it can take one of two values: rejected or not).

# Before we can perform this regression, however, we need to clean and prepare 
# the data.

# First, we will get all the variables we need together in a new, smaller 
# dataframe:

data_2019_subset <- select(data_2019, rejected, blacknonlatino, roughassets, roughdebts, lntotalincome, bankruptcy, ontimepayments, foreclosure)

# We then clean the data, removing any infinite values:

data_2019_subset <- data_2019_subset[!is.infinite(rowSums(data_2019_subset)),]

# Or any missing values:

data_2019_ready <- data_2019_subset[complete.cases(data_2019_subset), ]  

# Now, our data is ready to go. We will regress our dependent variable (whether 
# or not a respondent was rejected in the past year) on our independent variables 
# (which include our main variable of interest - whether the respondent is Black
# - as well as a number of control variables, listed below). 


reg <- glm(rejected ~ blacknonlatino + roughassets + roughdebts + lntotalincome + bankruptcy + ontimepayments + foreclosure, data = data_2019_ready, family = binomial)

# Let’s take a look at the results:

summary(reg)

# There’s a lot of information here, but the key result in our analysis is the
# columns for the row “blacknonlatino.” This is our coefficient of interest, 
# which measures the impact of a one unit change in the variable blacknonlatino
# on the dependent variable. The estimate for this coefficient, 3.689e-01, shows
# that, holding all else constant, Black respondents were e^0.3689 - 1 = 45% more 
# likely to be rejected for a loan. This means that if a Black respondent had the
# same income, assets, and credit history as a non-Black respondent, they would 
# be 45% more likely to be rejected. This is a highly significant result. As you
# can see, it is significant beyond the 0.001% level of significance, meaning 
# that there is a less than 0.001% chance that the coefficient is equal to zero.

# Importantly, there is a great deal of information missing from our analysis. 
# It’s quite possible that there are additional, omitted variables which could 
# both be correlated with being Black and with a loan rejection that explain this
# remarkably significant impact of race in determining creditworthiness (e.g. 
# access to technology, proximity to a bank, institutional trust, or the like).
# However, based on this analysis, there is significant reason to be concerned 
# that the algorithms used to predict creditworthiness are biased against Black 
# individuals. 

# These findings have important implications for the intersection of race,
# technology, and surveillance. For one, the presence of biases in algorithms
# such as those used for consumer credit - both in the reproduction of systemic
# inequality and the introduction of additional racial bias - can have highly
# damaging effects on communities of color and other marginalized groups. These
# algorithms are increasingly important with the rise of big data, surveillance
# capitalism, and social media. While audits like ours are helpful in revealing 
# inequities that may exist, they reveal a fundamental issue of accountability. 
# Most algorithms, like those used in consumer credit, are proprietary; we can
# investigate the outcomes that occur from them, but we can't see exactly what 
# they are doing or how they work. Thus, holding them accountable is remarkably 
# difficult. 

# Furthermore, our analysis reveals an important facet of surveillance:
# marginalized communities are generally more surveilled than those in power.
# This may seem more obvious in the case of police surveillance, for instance, 
# than in this case of credit. Yet, as discussed previously, the determinants 
# of creditworthiness include such factors as foreclosure, eviction, late
# payments, and the like. For wealthier, white individuals, their finances are 
# hardly surveilled in this way. A poorer individual of color, however, is more
# likely - due to systemic inequities - to rent their home from an eviction-
# prone landlord, take loans from sub-prime or payday lenders, and so on. This 
# levies credit surveillance disproportionately on poorer communities of color,
# disproportionately producing negative measurements of credit worthiness for
# these groups. This construction of "creditworthiness" can be reconstructed in 
# a race-conscious, justice-oriented manner. Through our analysis here, we urge
# such a reconstruction.

# However, our analysis in this tutorial is just a starting place for future 
# insights. Whereas we focus on specific variables which comprise 
# creditworthiness, a specific racial group, a specific year, and so on, we 
# encourage you to alter our choices and see what comes from it. Investigate
# biases against other communities. Investigate whether those biases are changing
# over time. Investigate whether certain determinants of creditworthiness are 
# more or less important than others. Through repeated, accessible research like
# this, we can begin to democratize data, peer into the "black box" of
# proprietary algorithms, reveal biases in "objective" algorithms,
# and hold those in power accountable.

# This concludes our tutorial, but we hope it doesn't conclude your
# investigation. Please feel free to contact us at pmcguir2@nd.edu or 
# pzajakow@nd.edu for further information.

