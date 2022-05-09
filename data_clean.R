# Library statements
library(tidyverse)
library(ggplot2)

# set working directory to use the function - idk what's going on here and it's too late to figure out with the function
setwd("/Users/leahpomerantz/Desktop/Spring\ 2022/Data\ Science/Project/dsProject/dsProject.R")

# source the function I created to load in and execute the code
source("/Users/leahpomerantz/Desktop/Spring\ 2022/Data\ Science/Project/dsProject/dsProject.R")
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

source("/Users/leahpomerantz/Desktop/Spring\ 2022/Data\ Science/Project/data_YA/data_YA.R")
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
      if(loop_data[i,j] == 13){
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

#********* NOTE: I CAN'T FIGURE OUT WHAT'S GOING WRONG HERE - MUST BE MY LOGIC??? SEE IF KIRA CAN HELP*********

for (i in 1:nrow(NLSY79_YA_mod)) { # pull the child's observation
  birth_yr <- NLSY79_YA_mod$CYRB_XRND[i] # assign the birth year to a variable
  for(j in 1:nrow(moth_rename)){ # pull the mother's observations
    yr_match <- birth_yr # assign it to a variable that can be used in a loop
    if(NLSY79_YA_mod$MPUBID_XRND[i] == moth_rename$CASEID_1979[j]){ # compare to find the mothers of the children
      for (k in 0:18) {
        var_name <- paste("income_", yr_match, sept = "") # create the variable name to go through the income
        for(l in 1:ncol(moth_rename)){
          if(var_name == colnames(moth_rename[l])){
            income_df[i,k+1] <- moth_rename[j,l]
          }
        }
        yr_match <- yr_match + 1
      }
      #yr_match <- yr_match + 1
    }
  }
}

# 
# #for(i in 1:nrow(NLYS79_YA_mod)){ # get observation that we'll pull birth year from
# for(i in 1:2){
#   for(j in 1:nrow(moth_rename)){ # get mother's observation to compare
#     if(NLSY79_YA_mod$MPUBID_XRND[i] == moth_rename$CASEID_1979[j]){ # i's and j's should match properly i think
#       birth_yr <- NLSY79_YA_mod$CYRB_XRND[i] # assign the birth year to a variable
#       yr_match <- birth_yr # this fixed one problem
#       for (k in 0:18) {
#         # print(yr_match) # this for statement is where things are going wrong
#         var_name <- paste("income_", yr_match, sep = "") # create the variable name
#         print(var_name) #this is working
#         for(l in 1:ncol(moth_rename)){
#           # seems to be where we're going wrong
#           #print(colnames(df_1[l]))
#           if(var_name == colnames(moth_rename[l])){ # check for the match seems to be where we're going wrong
#             # NLSY79_data[i,l] is now the family income for child i in year l, and we will continue to do this until
#             # we fill in for each of the variables
#             #print(colnames(NLSY79_data[l]))
#             income_df[i,k] <- moth_rename[i,l] # i should be right to keep the rows correctly matched
#           }
#         }
#         yr_match <- yr_match + 1 # could do this or birth_yr + i, doesn't really matter
#       }
#     }
#     
#   }
# }

# still full of NA's, probably something wrong with my conditional statement

# eventually going to nee dto make a decision about how to do the income and justify it in the paper
# because there will probably be several NAs for each year
# can probably assume the conditions for filling in the average




