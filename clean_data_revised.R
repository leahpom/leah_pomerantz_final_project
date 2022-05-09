# Library statements
library(tidyverse)
library(ggplot2)
library(magrittr)
library(modelsummary)
library(stargazer)
library(psych)
library(sandwich)
library(lmtest)

# set working directory to use the function - idk what's going on here and it's too late to figure out with the function
#setwd("/Users/leahpomerantz/Desktop/Spring\ 2022/Data\ Science/Project/dsProject/dsProject.R")

# source the function I created to load in and execute the code
source("dsProject.R")
NLSY79_total <- open_data() # takes both values that the function produces
NLSY79_data <- NLSY79_total$data # something here is going wrong in my function, because it's staying as a list
NLSY79_colnames <- NLSY79_total$cat_labels # this tracks our column names, which we can use later
# the plan with the column names is to first filter our data, but use this as a guide to easily reference, i.e., make
# it the first row and then just exclude the first row from our data analysis

# check that things are looking right
head(NLSY79_data) # this shows the data

# filter down to just women
df_1 <- NLSY79_data %>% dplyr::filter(SAMPLE_SEX_1979 == 2)

#************ working with the children's data ***************
# from here, need to get the children in to get their year of birth

# source the function for the children's data

# NOTE: HAD TO MANUALLY SET WD HERE 

source("data_YA.R")
NLSY79_YA_total <- open_data_YA() # takes both values that the function produces
NLSY79_YA_data <- NLSY79_YA_total$data # something here is going wrong in my function, because it's staying as a list
NLSY79_YA_colnames <- NLSY79_YA_total$cat_labels # this tracks our column names, which we can use later
# the plan with the column names is to first filter our data, but use this as a guide to easily reference, i.e., make
# it the first row and then just exclude the first row from our data analysis

# check that things look good 
head(NLSY79_YA_data)

min(NLSY79_YA_data$CYRB_XRND) # the youngest child was born in 1970

# create a college/no college variable for the kids

# first, modify the data set because there's an xrnd variable in the middle of the hgc variables
loop_data <- NLSY79_YA_data[,-8]

# NOTE: have to first only go through 2012, because they changed the coding 2014-2016

ya_college <- as.vector(c(rep(NA,nrow(loop_data)))) # create a vector of NAs of the same length as #observations

# NOTE: checked this loop on the first ten rows and was able to verify the results

for (i in 1:nrow(loop_data)){ # loop through the rows
  for (j in 6:10) { # loop through each column - we now have observation [i,j]
    if (is.na(loop_data[i,j]) == FALSE){ # works up to here
      if(loop_data[i,j] >= 13){
        ya_college[i] <- 1 # store a 1 the moment we have some college recorded
        break # stop looking once we have some college
      }
      else{
        ya_college[i] <- 0 # this way we know there isn't an NA but they didn't complete college
      }
      if(is.na(ya_college[i]) == FALSE){
        if(ya_college[i] == 1){
          break # stop looking through the columns once we have some college
        }
      }
    }
  }
}

# count how many people never recorded their education level
num_no_col <- 0 # throwaway variable for number of no observed majors
for(i in 1:length(ya_college)){
  if(is.na(ya_college[i]) == TRUE){
    num_no_col <- num_no_col + 1
  }
} 

num_no_col # 3696 is the number of NAs we're still left with

# need to create another loop to deal with the 2014, 2016, 2018 and see what happens

for (i in 1:nrow(loop_data)){ # tested on the first 6 rows to make sure it only did what it was supposed to do
  for (j in 11:13) { # loop through each column - we now have observation [i,j]
    if (is.na(loop_data[i,j]) == FALSE){ # works up to here
      if(loop_data[i,j] >= 6){ # greater than or equal to 6 is our "some college" for these columns
        ya_college[i] <- 1 # store a 1 the moment we have some college recorded
        break # stop looking once we have some college
      }
      else{
        if(is.na(ya_college[i]) == TRUE){ # this way, we are only overwriting an NA, not a previous 1
          ya_college[i] <- 0 # this way we know there isn't an NA but they didn't complete college
        }
      }
      if(is.na(ya_college[i]) == FALSE){
        if(ya_college[i] == 1){
          break # stop looking through the columns once we have some college
        }
      }
    }
  }
}

# recount how many people never recorded their education level
num_no_col <- 0 # throwaway variable for number of no observed majors
for(i in 1:length(ya_college)){
  if(is.na(ya_college[i]) == TRUE){
    num_no_col <- num_no_col + 1
  }
} 

num_no_col # we are down to 3046 people who never recorded their college level

# bind the ya_college to the data frame
NLSY79_YA_mod <- cbind(NLSY79_YA_data, ya_college)
head(NLSY79_YA_mod)

# ********* INCOME DATA **********

# NOTE: there are 11545 children and 6283 women. I can choose to either look at just the eldest children, or I 
# can try and look at all the children and merge only the relevant columns - maternal education and maternal income

# to do the income: create 18 columns - income at birth, income at 1 year, etc. 
# Use the birth year of the child, link to the mother's ID, and then fill in with the appropriate variable name
# or column number based on the birth year

# loop to create the empty vectors - note that income_yr_0 is the birth year
for(i in 0:18){ 
  assign(paste("income_yr_", i, sep = ""), as.vector(c(rep(NA,nrow(NLSY79_YA_mod)))))
}

income_df <- as.data.frame(cbind(income_yr_0, income_yr_1, income_yr_2, income_yr_3, income_yr_4, income_yr_5, income_yr_6, 
                                 income_yr_7, income_yr_8, income_yr_9, income_yr_10, income_yr_11, income_yr_12, income_yr_13,
                                 income_yr_14, income_yr_15, income_yr_16, income_yr_17, income_yr_18))

# loop logic
# 1) go through number of rows of the children
# 2) go through the number of rows of the mothers - using NLSY79_data because it's easier to match the kids this way
# 3) if the mother ID columns match, then take the birth year for this observation i
# 4) loop from 0:18 to fill in the columns for each income year
# (a) the structure of the loop would be:
#   income_yr_0[i] <- TNFI_HHI_TRUNC_*birth year*[i]
#   income_yr_1[i] <- TNFI_HHI_TRUNC_*birth year + 1*[i]

# modify column names to make the loop simpler - not working, probably need to simplify

# *********** PICK UP HERE *************
# I was definitely onto something with the loop below, but I need to standardize the names
# I need to modify the direct thing below to standardize the names
# and then modify the loop so that it pulls from the right names 
# and then calculate the average using the mean with na.omit and that should create a new variable that will
# be an average across ages
# this is something to mention in the final paper about issues I have had

moth_rename <- df_1

moth_rename <- dplyr::rename(moth_rename, income_1979 = TNFI_HHI_TRUNC_1979, income_1980 = TNFI_HHI_TRUNC_1980,
                             income_1981 = TNFI_HHI_TRUNC_1981, income_1982 = TNFI_HHI_TRUNC_1982,
                             income_1983 = TNFI_HHI_TRUNC_1983, income_1984 = TNFI_HHI_TRUNC_1984,
                             income_1985 = TNFI_HHI_TRUNC_1985, income_1986 = TNFI_HHI_TRUNC_1986,
                             income_1987 = TNFI_CENSUS_TRUNC_1987, income_1988 = TNFI_TRUNC_1988,
                             income_1989 = TNFI_TRUNC_1989, income_1990 = TNFI_TRUNC_1990,
                             income_1991 = TNFI_TRUNC_1991, income_1992 = TNFI_TRUNC_1992,
                             income_1993 = TNFI_TRUNC_1993, income_1994 = TNFI_TRUNC_1994,
                             income_1996 = TNFI_TRUNC_1996, income_1998 = TNFI_TRUNC_1998,
                             income_2000 = TNFI_TRUNC_2000, income_2002 = TNFI_TRUNC_2002,
                             income_2004 = TNFI_TRUNC_2004, income_2006 = TNFI_TRUNC_2006,
                             income_2008 = TNFI_TRUNC_2008, income_2010 = TNFI_TRUNC_2010,
                             income_2012 = TNFI_TRUNC_2012, income_2014 = TNFI_TRUNC_2014,
                             income_2016 = TNFI_TRUNC_2016, income_2018 = TNFI_TRUNC_2018)
# add in odd years as NA for below use
for (oddy in seq(1995,2017,2)) {
  moth_rename %<>% mutate("income_{oddy}" := NA_real_)
}

# merge mother's income into child data
NLSY79_YA_mod %<>% left_join(moth_rename %>% select(CASEID_1979,starts_with("income")), by=c("MPUBID_XRND" = "CASEID_1979"))

# now loop over birth years and set up the income at each age (initalize to NA)
# found it on StackOverflow here: https://stackoverflow.com/questions/26003574/use-dynamic-name-for-new-column-variable-in-dplyr
for (a in 0:18) {
  NLSY79_YA_mod %<>% mutate("faminc_age_{a}" := NA_real_)
}
# now fill in with base::replace and also use some dplyr/rlang magic
# especially this SO answer: https://stackoverflow.com/a/49311813/1311724
for (byr in min(NLSY79_YA_mod$CYRB_XRND):max(NLSY79_YA_mod$CYRB_XRND)) {
  max.first.age <- max(1979-byr,0) # some kids were born before the first year mother's income was tracked
  min.last.age <- min(2018-byr,18) # some kids were born close to the last year mother's income was tracked
  for (a in max.first.age:min.last.age) {
    tempdest <- paste0("faminc_age_",a)
    tempsrc  <- paste0("income_",byr+a)
    NLSY79_YA_mod %<>% mutate("faminc_age_{a}" := replace(!!rlang::sym(tempdest),CYRB_XRND==byr,!!rlang::sym(tempsrc)))
  }
}

# create the average across ages 0-18
temp <- rowMeans(NLSY79_YA_mod %>% select(starts_with("faminc_age_")), na.rm=T)
NLSY79_YA_mod %<>% mutate(avg.income.pre.college = temp)


# **************** RESULTS TIME BABY ***********************

# create a new data frame for the regression
reg_YA <- NLSY79_YA_mod[,-c(16:74)]

# we now have the YA stuff, but we need a variable with the highest grade ever completed for the mothers
# for this, we can do another left join
reg_YA %<>% left_join(df_1 %>% select(CASEID_1979,HGC_EVER_XRND), by=c("MPUBID_XRND" = "CASEID_1979"))

head(reg_YA) # looks good, but let's rename the HFC_EVER_XRND variable 

reg_YA <- rename(reg_YA, mother_highest_grade = HGC_EVER_XRND)

# we still have a tone of YA's to remove from the data 
# first, let's check if there are any shared characteristics of the NAs vs not for ya_college

YA_college_NA <- filter(reg_YA, is.na(ya_college) == TRUE) # data frame where it's only NAs in the college observation

YA_college_av <- filter(reg_YA, is.na(ya_college) == FALSE) # data frame where it's all the not NAs in the college

# create a summary of the two - most interested in the mother's education, income, and race, but will do all 
sum_NA <- as.data.frame(describe(YA_college_NA[,-c(6:14)], fast = TRUE)) # variables 6:14 were just used for the composite
sum_data <- as.data.frame(describe(YA_college_av[,-c(6:14)], fast = TRUE))

datasummary_df(sum_NA,output="markdown") %>% print # combine into a table to use in the markdown file
datasummary_df(sum_data, output = "markdown") %>% print

# do a nice little visualization
as.factor(YA_college_av$mother_highest_grade)
as.factor(YA_college_NA$mother_highest_grade)

college_data <- ggplot(YA_college_av, aes(x=mother_highest_grade)) + 
  geom_histogram(color="black", fill="cyan")
college_data

no_data <- ggplot(YA_college_NA, aes(x=mother_highest_grade)) + 
  geom_histogram(color="black", fill="purple")
no_data

# a little less educated and quite a bit poorer in the no college observations - INCLUDE THIS IN PAPER 

reg_YA <- reg_YA[,-c(6:14)] # 6:14 were what we used to create the composite for child education level

# check for NA's
summary(reg_YA)

# we have NA's that we need to get rid of in the sex category, ya_college, average income, and mother's highest grade
# note on the NA's: The only ones I'm actually worried about are the ya_college, because it's the only really significant
# ones 

reg_YA <- na.omit(reg_YA)

# also need to modify gender to be 0 and 1 instead of 1 and 2
reg_YA %<>% mutate(CSEX_XRND = replace(CSEX_XRND,CSEX_XRND==1,0))
reg_YA %<>% mutate(CSEX_XRND = replace(CSEX_XRND,CSEX_XRND==2,1))

# need to make sure that race and mother's highest grade are treated as binary/categorical
reg_YA %<>% mutate(mother_highest_grade = as.factor(mother_highest_grade))
reg_YA %<>% mutate(CRACE_XRND = as.factor(CRACE_XRND))

# last transformation: need to make the income variable a log
sum(reg_YA$avg.income.pre.college == 0) # 4 variables have a zero - this means we need to get rid of it for the log
reg_YA <- subset(reg_YA, avg.income.pre.college != 0)
reg_YA <- mutate(reg_YA, log_avg_income = log(avg.income.pre.college))
summary(reg_YA)

#**************** TIME TO RUN THE REGRESSION ********************

# linear probability model
lpm_YA <- reg_YA
as.numeric(lpm_YA$ya_college)
lpm_YA <- within(lpm_YA, mother_highest_grade <- relevel(mother_highest_grade, ref = 12))
reg <- lm(ya_college ~ mother_highest_grade + CRACE_XRND + log_avg_income,lpm_YA)
summary(reg)

reg_rob <- coeftest(reg, vcov = vcovHC(reg, type="HC1"))
reg_rob

# get nicer output for use in the paper
modelsummary(list("Non-Robust Standard Errors" = reg, "Robust Standard Errors" = reg_rob), 
             sestimate = c("estimate", "{estimate}{stars}","{estimate} ({std.error})"), 
             stars = TRUE, output="markdown") %>% print

# NOTE: REMEMBER TO GET ROBUST ERRORS

# some VERY interesting results here, will discuss in the paper

# logistic regression model
logit_YA <- reg_YA %>% mutate(mother_highest_grade = as.factor(mother_highest_grade))
logit_YA <- within(logit_YA, mother_highest_grade <- relevel(mother_highest_grade, ref = 12))
mylogit <- glm(ya_college ~ mother_highest_grade + CRACE_XRND + log_avg_income, 
               data = logit_YA, family = "binomial")
summary(mylogit)

modelsummary(list("Logistic Regression" = mylogit), 
             sestimate = c("estimate", "{estimate}{stars}","{estimate} ({std.error})"), 
             stars = TRUE, output="markdown") %>% print

# run the regressions again using bin categories for the mother's education level
# levels: 1 = elementary school (0-5), 2 = middle school (6-8), 3 = high school graduate (9-12), 4 = college (>12)
lpm_YA_new <- lpm_YA
lpm_YA_new$moth_educ_cat <- as.factor(ifelse(lpm_YA_new$mother_highest_grade %in% c(0:5), '1',
                            ifelse(lpm_YA_new$mother_highest_grade %in% c(6:8), '2', 
                            ifelse(lpm_YA_new$mother_highest_grade %in% c(9:12), '3', 
                            ifelse(lpm_YA_new$mother_highest_grade %in% c(13:20), '4', '5')))))

# LPM
lpm_YA_new <- within(lpm_YA_new, moth_educ_cat <- relevel(moth_educ_cat, ref = 3)) # relevel
lpm_mod <- lm(ya_college ~ moth_educ_cat + CRACE_XRND + log_avg_income,lpm_YA_new) # regression

lpm_mod_rob <- coeftest(lpm_mod, vcov = vcovHC(lpm_mod, type="HC1"))

# get nicer output for use in the paper
modelsummary(list("Non-Robust Standard Errors" = lpm_mod, "Robust Standard Errors" = lpm_mod_rob), 
             sestimate = c("estimate", "{estimate}{stars}","{estimate} ({std.error})"), 
             stars = TRUE, output="markdown") %>% print

# logit model 
logit_YA_new <- lpm_YA_new
mylogit_mod <- glm(ya_college ~ moth_educ_cat + CRACE_XRND + log_avg_income, 
               data = logit_YA_new, family = "binomial")
summary(mylogit_mod)

modelsummary(list("Logistic Regression" = mylogit_mod), 
             sestimate = c("estimate", "{estimate}{stars}","{estimate} ({std.error})"), 
             stars = TRUE, output="markdown") %>% print

# ************************************************************
# creating a function to move my cleaned data to my MA thesis
# ************************************************************

open_YA_edlev <- function(){
  mylist <- list(YA_college_av = YA_college_av, YA_college_NA = YA_college_NA, ds_reg_data = lpm_YA)
  return(mylist)
}





