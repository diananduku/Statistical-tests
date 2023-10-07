##importing the dataset
data= read.csv("C:/Users/USER/OneDrive/Data Science books/datasets/titanic dataset/tested.csv")
View(data)

##dropping the passenger ID, cabin, ticket and names column since they are not useful
df=data[,-c(1,4,9,11)]
View(df)

#data exploration

##checking for duplicated values
duplicates=(data[duplicated(data)])
sum_duplicates=sum(duplicates)
sum_duplicates

#no duplicate values

#descriprive statistics
summary(df)

##data cleaning
##checking for missing values
missing_values <- colSums(is.na(df))
missing_values

##missing values are only in age and fare category

#replace fare with mean
mean_fare = mean(df$Fare, na.rm = TRUE)
df$Fare[is.na(df$Fare)] = mean_fare

##replace age with the mean age for each class

head(df)

# Calculate the mean age for each Pclass using tapply()
mean_age_by_pclass =tapply(df$Age, df$Pclass, FUN = mean, na.rm = TRUE)

##replacing th emissing values
df$Age = ifelse(is.na(df$Age), mean_age_by_pclass[as.character(df$Pclass)], df$Age)

##recheckingg missing values
missing_values = colSums(is.na(df))
missing_values

##no missing values

##correlation matrix for the numeric variables
##selecting numeric variables
numeric_variables=sapply(df, is.numeric)
numeric_data=df[, numeric_variables]
correlation_matrix=cor(numeric_data)
correlation_matrix

##there is no strong correlation between the variables
##there is a moderately strong relationship between class and gender given class' age mean was used to fill the missing NA values for age

##generating frequency plots for each variable in the dataset
library(ggplot2)

# frewuencies for the variables

gender_frequencies=table(df$Sex)
gender_frequencies
barplot(gender_frequencies)

class_frequencies=table(df$Pclass)
class_frequencies
barplot(class_frequencies)


survived_frequencies=table(df$Survived)
survived_frequencies
barplot(survived_frequencies)


embarked_frequencies=table(df$Embarked)
embarked_frequencies


#place age into categories and fing the frequencies
age_categories <- cut(df$Age, 
                      breaks = c(0, 18, 30, 50, max(df$Age, na.rm = TRUE)),
                      labels = c("0-18", "19-30", "31-50", "51+"),
                      include.lowest = TRUE,
                      right = FALSE)

age_frequencies=table(age_categories)
age_frequencies
barplot(age_frequencies)

##place fare into categories and find frequencies
fare_categories <- cut(df$Fare,
                       breaks = c(0, 10, 30, 50, 100, max(df$Fare, na.rm = TRUE)),
                       labels = c("0-10", "11-30", "31-50", "51-100", "101+"),
                       include.lowest = TRUE,
                       right = FALSE)

fare_frequencies=table(fare_categories)
fare_frequencies
barplot(fare_frequencies)

boxplot(df$Fare)
#there is one outlier: one passenger paid a significantly higher amount of fare

##HYPOTHESIS TESTING
#HYPOTHESIS 1
#Ho:There is no statistical and significant difference in the ages between survivors vs non-survivors
#H1:The mean age of survivors is less than the mean age for non-survivors 

##encoding the sex into binary
# Convert "Sex" to binary (0 for male, 1 for female)
df$Sex <- ifelse(df$Sex == "male", 0, 1)
survivors=filter(df, df$Survived==1)
head(survivors)
non_survivors=filter(df,df$Survived==0)
head(non_survivors)

##performing t-test
t_test_age<- t.test(survivors$Age, non_survivors$Age, 
                    alternative='less',
                    paired=FALSE)
t_test_age

##Interpretation:The p-value is 0.541, which is larger than 0.05. Therefore, we fail to reject the null hypothesis
##Conclusion: There is no statistical and significant difference in the ages of survivors vs non survivors.
##The differences are due to random variation

#HYPOTHESIS 2
#Ho:There is no association between survival rate  and passenger class
#H1: There is a statistical and significant association between survival rate and passenger class
contingency_table=table(df$Survived, df$Pclass)
contingency_table

#chi-square test
chisq.test(contingency_table)

##Interpretation: Since the p-value  is 0.03, which is below 0.05, we have sufficient evidence to reject the null hypothesis
#Therefore we conclude that there is a significant association between passenger class and survival rate

##Determining which passenger class had a higher survival rate
survival_percentages = with(df, prop.table(table(Pclass, Survived), margin = 1) * 100)
survival_percentages

##Conclusion: Passengers in 1st class had a higher survival rate. Survival in 2nd and 3rd class was similar


##HYPOTHESIS 3
#Ho: There is no association between sex and survival rate
#H1: There is a statistical and significant association between sex and survival rate

contingency_table_sex=table(df$Sex,df$Survived)
contingency_table_sex

#performing chi-square test
chisq.test(contingency_table_sex)

#Interpretation: The p-value is 2.462e-08 which is significantly smaller than 0.05. We therefore reject the null hypothesis
#Therefore, we can conclude that there is a statistical and significant association between survival rate and sex

##Determining which sex had a higher survival rate
sex_survival_percentages = with(df, prop.table(table(Sex, Survived)) * 100)
sex_survival_percentages

##male =0, female =1
#63% of males did not survive, while 36% of females did not survive

##Therefore, we conclude that the biggest indicator for survival rate was sex, followed by passenger class. 
#Age did not have an influence on survival rate