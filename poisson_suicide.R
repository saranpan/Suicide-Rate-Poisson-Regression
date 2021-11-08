
#  title: "Suicide rate using Poisson regression "
#############################################################################

                          ##  Section 0. SETUP LIBRARIES ##
library(tidyverse)

library(MASS)
#library(equatiomatic)
library(rsq)
library(broom)
library(clipr)
library(magrittr)
library(car)
library(fastDummies)
library(bestglm)
library(leaps)
library(plm)

#############################################################################

                          ## Section 1. Analyze the Data ##
  
#Import data

setwd("C:/Users/Wallik/Desktop/data")
data <- read.csv('project/who_suicide_statistics.csv')
#attach(data)

#Check missing value
print('Total missing value : ',sum(is.na(data)))

na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

print('missing value of each column')
print(na_count)

# (Default) Dropping the missing value
data = na.omit(data)

#EDA
summary(data)

# filter ASEAN country 
unique(data$country)
# check out : 
# https://colab.research.google.com/drive/15WQV4AFobdVStcL_8HAGOOwz-Bu7Cs1S?usp=sharing

target <- c('Armenia',
            'Azerbaijan',
            'Bahrain',
            'Brunei Darussalam',
            'Cyprus',
            'Georgia',
            'Hong Kong SAR',
            'Iran (Islamic Rep of)',
            'Israel',
            'Japan',
            'Kazakhstan',
            'Kuwait',
            'Kyrgyzstan',
            'Macau',
            'Maldives',
            'Mongolia',
            'Oman',
            'Philippines',
            'Qatar',
            'Republic of Korea',
            'Singapore',
            'Sri Lanka',
            'Thailand',
            'Turkey',
            'Turkmenistan',
            'United Arab Emirates',
            'Uzbekistan')

# df = Asia country data
df = filter(data, data$country %in% target)

summary(data$year)
summary(df)

min(data$year)

# Plot: numeric variables
data_num <- data[ , c(2,5,6)]
plot(data_num , col="#69b3a2",main="World Level")

df_num <- df[ , c(2,5,6)]
plot(df_num , col="#69b3a2",main="Asian Level")


#Plot the suicide count against country, cumulated by age
p<-ggplot(data=df, aes(x=country, y=suicides_no,fill=age)) +
  geom_bar(stat="identity") 

p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Plot the suicide count against country, cumulated by sex
p<-ggplot(data=df, aes(x=sex, y=suicides_no,fill=sex)) +
  geom_bar(stat="identity") 

p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Plot the suicide count against sex
p<-ggplot(data=df, aes(x=sex, y=suicides_no,fill=sex)) +
  geom_bar(stat="identity") 

p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Plot the suicide count against population colored by country
p<-ggplot(data=df, aes(x=population, y=suicides_no, color=country)) +
  geom_point() 


p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Plot the suicide count against time 


sum_count = aggregate(df$suicides_no, by=list(year=df$year), FUN=sum)
names(sum_count)[names(sum_count) == 'x'] <- 'suicides_no'

p<-ggplot(data=sum_count, aes(x=year, y=suicides_no)) +
  geom_line()+
  geom_point(stat="identity")

p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ggtitle("        Suicide count per year")

#Plot the suicide count against sex 


sum_count = aggregate(df$population, by=list(sex=df$sex), FUN=sum)
names(sum_count)[names(sum_count) == 'x'] <- 'population'

p<-ggplot(data=sum_count, aes(x=sex, y=population)) +
   geom_bar(stat="identity") 

p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# suspect the 2016 why there is a low number in suicide count


freq_year = data.frame(table(df$year))
names(freq_year)[1] <- "year";names(freq_year)[2] <- "frequency"
freq_year

p<-ggplot(data=freq_year, aes(x=year, y=frequency)) +
  geom_bar(stat="identity") 

p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

###################################################################################
                           ## Section 2. Process the data  ##

# Work on the main data (= Asian)
attach(df)

# Pre-processing Categorical variable to Dummy variable form

# So let's create the processed dataset (data_prep)

#Cat preprocess
cat = c('country','sex','age')

df_prep = dummy_cols(df, select_columns = cat, remove_first_dummy = TRUE)
df_prep = df_prep[ , !(names(df_prep) %in% cat), drop=TRUE]

# (avoid overflow) time stamp the year by subtract the oldest time of dataset (which is 1979)
# Time dictionary: {1979 : 0 , 1980 : 1, ...., 2016 : 37}
df_prep$year = df_prep$year - min(df_prep$year)

#Relocate the label *(suicides_no) to the last column
df_prep <- df_prep[, c(which(colnames(df_prep) != "suicides_no"), which(colnames(df_prep) == "suicides_no"))]

#suspend this model, since glm doesn't valid
#offset_full_model <- glm(suicides_no~dummy_country+year+dummy_sex+age , 
#                family = poisson(link = log),offset=population)

detach(df)
attach(df_prep)

# Since using offset command make the GLM function unable to find the coefficient, 
#so we treat population as feature then

###################################################################################
                            ## Section 3. Create (full) model  ##

# Since using offset command make the GLM function unable to find the coefficient, 
#so we treat population as feature then


full_model <- glm(suicides_no ~ ., data=df_prep ,
                  family = poisson(link = log))


summary(full_model)

plot_log_coeffs <- function(poisson_model) {
  coeffs <- coefficients(poisson_model)
  mp <- barplot(coeffs, col="#3F97D0", xaxt='n', main="Poisson Regression Coefficients",ylab = 'log(suicide count)')
  lablist <- names(coeffs)
  text(mp, par("usr")[3], labels = lablist, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6,)
}


# Create function for plotting the coefficient
plot_coeffs <- function(poisson_model,var = 'none') {
  coeffs <- coefficients(poisson_model)
  if(var == 'num'){
    coeffs <- coefficients(poisson_model)[2:3]
    coef_type = (exp(coeffs)-1 ) * 100
    main_title = 'The predicted mean change of suicide count (%) when increase by 1 unit' 
    str = 0;par=3}
  else if(var == 'country'){
    coeffs <- coefficients(poisson_model)[4:29] 
    coef_type = (exp(coeffs)-1) * 100
    
    # Baseline of country: Armenia
    main_title = 'The predicted mean of suicide count for each country changed from predicted mean of Armenia (%)'
    str = 90;par=4}
  else if(var == 'sex'){
    coeffs <- coefficients(poisson_model)[30] 
    coef_type = (exp(coeffs)-1) * 100
    
    # Baseline of sex: female
    main_title = 'The predicted mean of suicide count for Male changed from predicted mean of Female (%)'
    str = 0;par=3}
  else if(var == 'age'){
    coeffs <- coefficients(poisson_model)[31:35] 
    coef_type = (exp(coeffs)-1) * 100
    
    # Baseline of age: 5-14 years old
    main_title = 'The predicted mean of suicide count for each age range changed from predicted mean of 5-14 years (%)'
    str = 0;par=3}
    
  mp <- barplot(coef_type, col="#3F97D0", xaxt='n', main=main_title, cex.main=0.6 ,
                ylab = 'the change of suicide count (%)'
                #,ylim = c(-100,0)
                )
  
  lablist <- names(coeffs)
  
  text(mp, par("usr")[3] , labels = lablist, 
       srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
  
  #top coefficient of the bar
  text(mp , par("usr")[par] , labels = formatC(coef_type, format = "e", digits = 2), 
       srt = str ,adj = c(0.1,0.1), cex=0.6 , pos=3)
  
  }



#plot no_log
plot_coeffs(full_model,var='num')
plot_coeffs(full_model,var='country')
plot_coeffs(full_model,var='sex')
plot_coeffs(full_model,var='age')

#plot log
plot_log_coeffs(full_model)


full_model




##############################-Optional zone-#####################################

# Write the Latex equation for the model
# extract_eq(full_model,use_coefs = TRUE) 

#Write the function to return the equation automatically
model_equation <- function(model, ...) {
  format_args <- list(...)
  
  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}

model_equation(full_model, digits = 2)


# Purposely, to copy clipboard the text from the summary of the first model into Google Sheet
full_model %>%
  tidy() %>%
  write_clip()
# Now, you can paste to the google sheet

##############################---------------#####################################

                   # Since some feature might not be useful
                   # So Section 3 will show you how can we drop the useless feature

                   # and some features need to be interacted together or to itself
                   # So, Section 4 will show you the progress of Feature Engineering

###################################################################################

                              ## Section 4. Model selection ##

#since P-value seems to not work, so we use AIC

## 3.1 Backward stepwise (AIC)

step(full_model,direction = 'backward')
# it suggests remove nothing give the lowest AIC, (405319)
#winner:
full_model


## 3.2. All Subset Selection (AIC)

# It's look like the bestglm does not support the dataset with 
#more than 15 columns, so we use just Backward stepwise 
bestglm(df_prep , family = poisson, IC = "AIC", TopModels = 2,
               method = "exhaustive")

#######################################################
## 4. Creative feature engineer (Interaction term : population) ##

# We collect the result of each model in this table table_df
test_df <- data.frame(
  modify = c ("+ 1 none"), 
  AIC = c(AIC(full_model)),
  stringsAsFactors = FALSE)


#4.1 ) Population ^ 2 only
test_model <- glm(data= df_prep , suicides_no ~ . + I(population^2)  , 
                              family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+ 2nd order population polynomial",AIC(test_model))

#Wow, AIC seem to be lower by 405300-370410 = 34890
#This might suggest the higher population, the suicide count seems to be more (as it correlated)

#4.2 ) Population ^ 3 only
test_model <- glm(data= df_prep , suicides_no ~ . + I(population^3)  , 
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+ population^3",AIC(test_model))

# Duh, the AIC increases by 377905 - 370410 = 7495

#4.3 ) Population ^ 2 + Population ^ 3 
test_model <- glm(data= df_prep ,suicides_no ~ . + I(population^2) + I(population^3)  , 
                              family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+ 3rd order population polynomial",AIC(test_model))

#Wow, AIC seem to be lower by 370410-353710 = 16700
#4.4 ) Population ^ 2 + Population ^ 3 + Population ^ 4
test_model <- glm(data= df_prep ,suicides_no ~ . + I(population^2) + I(population^3) + I(population^4)  , 
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+ 4th order population polynomial",AIC(test_model))

summary(test_model)

#Wow, AIC seem to be lower by 353710-310097 = 43613

#4.5 ) Population ^ 3 + Population ^4
test_model <- glm(data= df_prep ,suicides_no ~ . + I(population^3) + I(population^4)  , 
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+ population^3+ population^4",AIC(test_model))

#4.6 ) + Population ^ 5
test_model <- glm(data= df_prep ,suicides_no ~ . + I(population^2) + I(population^3) + I(population^4) + I(population^5) , 
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+ 5th order population polynomial",AIC(test_model))

#Wow, AIC seem to be lower by 310097-291783 = 18314

#4.7 ) + Population ^ 6
test_model <- glm(data= df_prep ,suicides_no ~ . + I(population^2) + I(population^3) + I(population^4) + I(population^5)+I(population^6) , 
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+ 6th order population polynomial",AIC(test_model))
#Wow, AIC seem to be lower by 291783-278026 = 13757

#4.8 ) + Population ^ 7
test_model <- glm(data= df_prep ,suicides_no ~ . + I(population^2) + I(population^3) + I(population^4) + I(population^5)+I(population^6)+I(population^7) , 
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+ 7th order population polynomial",AIC(test_model))

#Wow, AIC seem to be lower by 278026-265751 = 12275

# The AIC seems to be lower and lower every time

# THIS SEEMS TO BE A BAD SIGN OF OVERFITTING ; 

test_df = test_df[-c(3, 6), ]  
row.names(test_df) <- NULL

# and the AIC is gradually decreased which is not great for higher degree of freedom
# So I decided to stop population at ^4


# Let plot
p<-ggplot(data=test_df, aes(x=modify, y=AIC)) +
  geom_bar(stat="identity") 

p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Winner : 4th order p.
full_model <- glm(data= df_prep ,suicides_no ~ . + I(population^2) + I(population^3) + I(population^4)  , 
                  family = poisson(link = log))

summary(full_model)


full_model %>%
  tidy() %>%
  write_clip()

#since the changes is the highest : 353710-310097 = 43613

# so we keep this as the feature for the next

########################################################
## 5. Creative feature engineer (Interaction term : year) ##

# Preprocessing dummy variable
dummy_country = factor(df$country)
dummy_sex = factor(df$sex)
dummy_age = factor(df$age, order = TRUE, levels =c('5-14 years','15-24 years', '25-34 years', '35-54 years','55-74 years', '75+ years'))

df$year = df$year - 1979

#report dataset
test_df <- data.frame(
  modify = c ("+ 1 none"), 
  AIC = c(AIC(full_model)),
  stringsAsFactors = FALSE)

# 5.1) year * population
test_model <- glm(data= df_prep ,suicides_no ~ . + I(population^2) + I(population^3) + I(population^4) +
                    + I(year * population), 
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+year * population",AIC(test_model))

# Duh, AIC increases 310174-310097=77


#df2 = dummy_cols(df,select_columns = c('country','sex','age'))
#df2

# 5.2) year * sex
test_model <- glm(data = df , suicides_no ~ year + population + dummy_country + dummy_sex + dummy_age + 
                    I(population^2)+ I(population^3)+ I(population^4) +
                    year * dummy_sex, 
                  
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+year * sex",AIC(test_model))

# 5.3) year * country
test_model <- glm(data = df , suicides_no ~ year + population + dummy_country + dummy_sex + dummy_age + 
                    I(population^2)+ I(population^3)+ I(population^4) +
                    year * dummy_country, 
                  
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+year * country",AIC(test_model))

# 5.2 + 5.3
test_model <- glm(data = df , suicides_no ~ year + population + dummy_country + dummy_sex + dummy_age + 
                    I(population^2)+ I(population^3)+ I(population^4) +
                    year * dummy_country + year * dummy_sex, 
                  
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+year * country + year * sex ",AIC(test_model))

# 5.2 * 5.3
test_model <- glm(data = df , suicides_no ~ year + population + dummy_country + dummy_sex + dummy_age + 
                    I(population^2)+ I(population^3)+ I(population^4) +
                    year * dummy_country*dummy_sex, 
                  
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+year * sex * country",AIC(test_model))


############################################
# 5.4) country * age
test_model <- glm(data = df , suicides_no ~ year + population + dummy_country + dummy_sex + dummy_age + 
                    I(population^2)+ I(population^3)+ I(population^4) +
                    country * dummy_age , 
                  
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+country * age",AIC(test_model))

# 5.2 * 5.3 + 5.4

test_model <- glm(data = df , suicides_no ~ year + population + dummy_country + dummy_sex + dummy_age + 
                    I(population^2)+ I(population^3)+ I(population^4) +
                    country * dummy_age + year*dummy_country*dummy_sex , 
                  
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+year * sex * country +country * age",AIC(test_model))

############################################
# 5.5) year * age

test_model <- glm(data = df , suicides_no ~ year + population + dummy_country + dummy_sex + dummy_age + 
                    I(population^2)+ I(population^3)+ I(population^4) +
                    year*age , 
                  
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+year * age",AIC(test_model))

# 5.2 * 5.3 + 5.4 + 5.5

test_model <- glm(data = df , suicides_no ~ year + population + dummy_country + dummy_sex + dummy_age + 
                    I(population^2)+ I(population^3)+ I(population^4) +
                    country * dummy_age + year*dummy_country*dummy_sex + year*dummy_age , 
                  
                  family = poisson(link = log))

test_df[nrow(test_df) + 1,] = c("+year * sex * country +country * age + year * age",AIC(test_model))
############################################

p<-ggplot(data=test_df, aes(x=modify, y=AIC)) +
  geom_bar(stat="identity") 

p+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

final_model = test_model

options(max.print=1000000)

final = summary(final_model)

final

model_equation(final_model, digits = 5)


final_model %>%
  tidy() %>%
  write_clip()

############################################

predict_set <- data.frame(
  country = c ("dummy_countryThailand"),
  year = c(1979+2),
  sex = c ("dummy_sexmale"),
  age = c ("dummy_age.Q"),
  population = c(250000))#,
  #stringsAsFactors = FALSE)

predict(final_model, predict_set)

is.factor(factor(df$country))
