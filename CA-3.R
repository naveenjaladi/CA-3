# As the code contains special character of euro symbol the file is saved with UTF-8 Enconding
# While running the code click file and choose reopen with enconding
# Dataset ppr-2019-dublin.csv

# Reading the dataset into the dataframe
price_property <- read.csv("ppr-2019-dublin.csv")

# Displaying Number of rows in the data frame
nrow(price_property)
# Structure of data frame
str(price_property)
# First ten rows of data frame
head(price_property, 10)

# Getting the column names of data frame
colnames(price_property)
# Assigning New column names into variable
col_names <- c("Date Of Sale", "Address", "Postal Code", "County", "Price", "Not Full Market Price",
               "VAT Exclusive", "Description Of Property", "Property Size Description")
# Assigning new column names to the data frame
colnames(price_property) <- col_names
# Checking the column names
colnames(price_property)
str(price_property)

# In order to clean the data change the structure of data frame from factors to the required form 
# like characters or numeric 
# Changing the structure required columns into characters
price_property$Address <- as.character(price_property$Address)
price_property$`Postal Code` <- as.character(price_property$`Postal Code`)
price_property$County <- as.character(price_property$County)
price_property$Price <- as.character(price_property$Price)
price_property$`Not Full Market Price` <- as.character(price_property$`Not Full Market Price`)
price_property$`VAT Exclusive` <- as.character(price_property$`VAT Exclusive`)
price_property$`Description Of Property` <- as.character(price_property$`Description Of Property`)
price_property$`Property Size Description` <- as.character(price_property$`Property Size Description`)
str(price_property)

# Stringr library for removing unwanted strings from the column values
library(stringr)
# The price column contains the price in the form of €40,000 
# Intially the euro and , strings are removed from the price column values and then converted into numeric
# If they are not removed then value totally cahnges as it considers both euro and , symbol
# Using str_remove_all will remove the specified character from the column values
price_property$Price <- str_remove_all(price_property$Price, "€")
price_property$Price <- str_remove_all(price_property$Price, ",")
# Converting Price column into numeric 
price_property$Price <- as.numeric(price_property$Price)
# structure after converting columns
str(price_property)

# As we are dealing with the postal code and price columns check if the data is ambiguous or not
# Plotting the postal code column 
barplot(prop.table(table(price_property$`Postal Code`)))
# From the plot it can be interpreted that a column contains the data as Dublin 6w 
# So remove w from the data
# Using string_remove_all to remove the unwanted characters from the postal code
price_property$`Postal Code` <- str_remove_all(price_property$`Postal Code`, "w")
barplot(prop.table(table(price_property$`Postal Code`)))

# No of Null rows before dealing with missing data
# To display the rows with all null data
data.frame(sapply(price_property, function(x)sum(length(which(is.na(x))))))    
nrow(price_property[!complete.cases(price_property),])


# Replacing all missing values with NA 
# The missing values are only in the columns of postal code which contains where the property locates
# As it cannot be reaplaced it is replaced with NA
# Another column is Property size description only 9 rows contains the data and reamining data is null 
# So replace null values in Property size description with NA
price_property[price_property == ""] <- NA


# Plotting missing values
library(VIM)
missing_values <- aggr(price_property, prop= FALSE, numbers = TRUE)
summary(missing_values)

# To display the rows with all null data
# Displaying number of NA rows alongside of each column
data.frame(sapply(price_property, function(x)sum(length(which(is.na(x)))))) 
# Displaying the number of incomplete rows of data
nrow(price_property[!complete.cases(price_property),])

# Selecting with data where postal code definetly contains data into new data frame
price_code <- subset(price_property[, ], !is.na(price_property$`Postal Code`))
# structure of new data frame
str(price_code)

#  Displaying number of NA rows alongside of each column
data.frame(sapply(price_code, function(x)sum(length(which(is.na(x)))))) 
# Displaying the number of incomplete rows of data
nrow(price_code[!complete.cases(price_code),])

# As the price depends on the columns Not full market price and Vat exclusive
# Consider the data that best describe the price
# As the price varies based on the two columns but in the dataset we are provided with character data
# No numeric data is provided so we can't change the price column data based on these two columns
# So select the rows of data which will include the total price
# total price contains Not full market price as No and Vat Exclusive as NO
# select all the rows of data which results total price using subset
# Selecting the rows with Not Full Market Price having Yes into Price_code_not_full_market
price_code_not_full_market <- subset(price_code[,], price_code$`Not Full Market Price` == "Yes")
# Selecting the rows with Not Full Market Price having No into Price_code_not_full_market
# Price_code_full_market is used for further analysis
price_code_full_market <- subset(price_code[,], price_code$`Not Full Market Price` == "No")

# Selecting the rows with Vat Exlcusive Price having No into Price_code_not_final
# The price_code _final perfectly defines the total price of each property entry
price_code_final <- subset(price_code_full_market[,], price_code_full_market$`VAT Exclusive` == "No")

# Plotting Price and postal code using ggplot
library(ggplot2)
ggplot(price_code_final, aes(x = Price, y = `Postal Code`)) + geom_boxplot()

# For further statistical analysis a random sample of 5000 is selected
# random sampling of 5000 varibales has to be done because 
# the statistics tests can only be tested for 5000 samples
random_price <- price_code_final[sample(1:nrow(price_code_final), 5000, replace = FALSE), ]
str(random_price)
#  Displaying number of NA rows alongside of each column
data.frame(sapply(random_price, function(x)sum(length(which(is.na(x)))))) 


# Ploting normality Graph 
# qqnorm(random_price$Price, random_price$`Postal Code`)
# If we use above qqnorm code it will result an error as it contains a numeric data and categorical data
# So using aov fucntion will results list of 13 values with coefficient, residuals, terms, model etc..
# We use residuals from the aov function 
# Residual is the value difference between a entered value and the mean of all values for that group
# A residual is positive when the corresponding value is greater than the sample mean
# and is negative when the value is less than the sample mean
# Using aov fucntion between price and postcode
anova1 <- aov(random_price$Price ~ random_price$`Postal Code`)
# summary of anova1
summary(anova1)


# Normality distribution graph
qqnorm(residuals(anova1), main = "Price Plot");qqline(residuals(anova1), col = 2)
# Normality distribution test using shapiro test
shapiro.test(residuals(anova1))
# The test has resulted a p value of 2.2e-16 which is very less than 0.05 
# So the population is not normal distributed
# We can also apply shapiro test for each individual value p value for each postal code value 
# i.e it will results w and p values for each postal code Dublin 1, Dublin 9, Dublin 8, .....
with(random_price, tapply(Price, `Postal Code`, shapiro.test))

# Hypothesis testing
# By using the residuals of anavo hypothesis testing is carried out
# We have taken aov function for price and postal code 
# so they are independent variables and not normally distributed so Mann-whitney test is used
# Here the NULL Hypothesis is that : The price varies with the postal code showing a relation.
wilcox.test(residuals(anova1))
# From the results it can be infered that 
# The p value is 2.2e-16 which is very small p value
# So null hypothesis is rejected and alternative hypothesis is followed

# We can also apply wilcox test for each individual value p value for each postal code value 
# i.e it will results w and p values for each postal code Dublin 1, Dublin 9, Dublin 8, .....
with(random_price, tapply(Price, `Postal Code`, wilcox.test))


# Power test for sample size determination
# Loading pwr library
library(pwr)
# giving the test name and conventional effect size 
# This is done to determine the effective size for power test
size <- cohen.ES(test = "p", size = "large")
#size1 <- ES.h(random_price$Price, random_price$`Postal Code`)
size

# Computing the power of test with effective size as from conventional effective size 
# Giving the power model is 0.85 any power greater than 0.8 is good value
# Type I Error or sig.level is rejecting the null hypothesis in favor of a false alternative hypothesis
# Which as aslo known as alpha
# power of test is 1 minus the Type II error probability
# Type II Error is failing to reject a false null hypothesis in favor of a true alternative hypothesis
# Which is also known as beta
# The below code gives the power of the model with sample size of each group having 100 observations
samplesize <- pwr.t.test(d=size$effect.size ,n=NULL,
                         sig.level = 0.05, power = 0.85, 
                         alternative = "two.sided")
samplesize
# Plotting the power of test
plot(samplesize)