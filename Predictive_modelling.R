# In my assessment i have dependent variables as primary education as its is extracted from the proverty dataset
# Research question for this dataset is to find relationship between crime rates and poverty of the country.
# Felt hard to get total number of people who under the poverty line from poverty dataset instead of it the primary eduction 
# count of the total people are extracted from the poverty dataset.

# So as per the above statement i considered crime_counts as a independent variable and primary eduction as a dependent variable.

# Relationship between these variables are tested using the hypothesis statistical method and resulted in alternative 
# hypothesis is true.

# In the dataframe there are only on dependent variable are avaliable so predictive model selection is starting with the 
# simple linear regression.

# Dataframe dervied from CA Assessment.
Predictive_build_df <- Crime_Analytics_df
Predictive_build_df

# Showing the struture of the dataframe and found the both dependent and independent variables are quantatitive values.
str(Predictive_build_df)

# Top 6 rows are listed from the dataframe.
head(Predictive_build_df)

#Struture of the datframe and it shows crime_counts has the data-type int and primary_education_count has the data-type numeric
str(Predictive_build_df)

#Chnaging the Crime Counts from integer to numeric data type.
Predictive_build_df$Crime_Counts <- as.numeric(Predictive_build_df$Crime_Counts)

#Removing the scientific notation
options(scipen=999)

# Lm function was used to build the linear model with formula of Primary_ED_Counts(dependent variable) ~ Crime_Counts(independent variable).
Crime_linear_model <- lm(Primary_ED_Counts ~ Crime_Counts, data = Predictive_build_df) 
Crime_linear_model

# The results says the intercept of -3466.0412 and beta coefficient of 0.5497 for dependent variable primary_eduction.

# In order to reterive this information in graphical representation the plot() has been implemented.
plot(Predictive_build_df$Crime_Counts,Predictive_build_df$Primary_ED_Counts,
     xlab="Crime_Counts",
     ylab="Primary_Education_count",
     main="Regression line")

# The Fit line was drawn in order to check the correlation between two variables.
abline(Crime_linear_model)

#Summaring the linear regression model.
summary(Crime_linear_model)

# Lm function was used to build the polynomial model with formula of Crime_Counts(dependent variables) ~ Primary_ED_counts(independent variable) 
Crime_poly_model <- lm(Primary_ED_Counts ~ Crime_Counts + I(Crime_Counts ^ 2), data = Predictive_build_df)
Crime_poly_model

# In order to reterive this information in graphical representation the plot() has been implemented.
plot(Predictive_build_df$Crime_Counts,Predictive_build_df$Primary_ED_Counts,
     xlab="Crime_Counts",
     ylab="Primary_ED_Counts",
     main="Polynomial regression line with 2nd order")

# The Fit line was drawn in order to check the correlation between two variables.
abline(Crime_poly_model)

#Summaring the linear regression model.
summary(Crime_poly_model)


# Checking linearity using scatter plots.
scatter.smooth(x = Predictive_build_df$Primary_ED_Counts,
               y = Predictive_build_df$Crime_Counts,
               main = "Primary_counts   ~ Crime_Counts",
               xlab = "Primary_Counts",
               ylab = "Crime_Counts")

# Checking correlation between the two variables.
cor(Predictive_build_df$Crime_Counts, Predictive_build_df$Primary_ED_Counts)
cor

# Checking with the outliers.
opar <- par(no.readonly = TRUE)

# Splitting the frame into two columns
par(mfrow = c(1,2))

# Data frame to be used in boxplot were attached
attach(Predictive_build_df)

# The box plot was drawn in order to get the outlier rows from the dataframe.
boxplot(Crime_Counts,
        main = "Crime_Counts",
        sub = paste("Outliner rows: ",
                    boxplot.stats(Crime_Counts)$out))

# The box plot was drawn in order to get the outlier rows from the dataframe.
boxplot(Primary_ED_Counts,
        main="Primary_education_counts",
        sub = paste("Outliner rows: ",
                    boxplot.stats(Primary_ED_Counts)$out))

# detaching the dataframe.
detach(Predictive_build_df)
par <- opar

#Removing outliers for better model performance will reduce the number of rows in dataframe so the model was continued with the outliers.

#In order to use the density function the package called e1071 was installed in R environment.
install.packages("e1071")
library(e1071)

# Splitting the frame into two columns
par(mfrow  = c(1,2))

#density plot for crime_counts was builted in order to evaluate the skewness of the crime_count data.
plot(density(Predictive_build_df$Crime_Counts), main = "Density Plot: Crime_counts",
     ylab = "Frequency",
     sub = paste("Skewness:",
                 round(e1071::skewness(Predictive_build_df$Crime_Counts),2)))

# polygon function was used in order to check the normality distribution and adavance level of graphical representation.
polygon(density(Predictive_build_df$Crime_Counts), col = "red")

#density plot for Primary_Education_count was builted in order to evaluate the skewness of the Primary_Education_count data.
plot(density(Predictive_build_df$Primary_ED_Counts), main = "Density Plot: Primary_Education_count",
     ylab = "Frequency",
     sub = paste("Skewness:",
                 round(e1071::skewness(Predictive_build_df$Primary_ED_Counts),2)))

# polygon function was used in order to check the normality distribution and adavance level of graphical representation.
polygon(density(Predictive_build_df$Primary_ED_Counts), col = "red")

# The skewness of the data for crime_counts is 4.43 and skewness for the primary_education_count is 3.67 which indicaties
# Builting the model using this data variables will lead in poor prediction accuracy.

#The testing and training data was splitted with the ratio of 70:30 with the seeding value of 1.
set.seed(1)
Predictive_build_df_row_data <- nrow(Predictive_build_df)
test_samples <- sample(1:Predictive_build_df_row_data, size = round(0.7 * Predictive_build_df_row_data), replace = FALSE)

# the test andf train samples.
train_data <- Predictive_build_df[test_samples, ]
test_data <- Predictive_build_df[-test_samples, ]

#Validating the model by using the linear regression model.
train_lm <- lm(Primary_ED_Counts ~ Crime_counts, data = train_data)
train_lm

#Summary information of the  linear regression model.
summary(train_lm)

#AIC(Akaike Information Criterion) the value of AIC is high and resulting in poor accuracy rate.
AIC(train_lm)

#BIC(Bayesian Information Criterion) the value of BIC is high and resulting in poor accuracy rate.
BIC(train_lm)

# Forecasting the predictive models using the different methods.
predicted_CrimeCounts <- predict(train_lm, test_data)

#actual and predicative value accurrancy are evaluated in order to denote the both values are moving 
#in the same direction.
actuals_predictions <- data.frame(cbind(actuals = test_data$Crime_Counts, predicted = predicted_CrimeCounts))
head(actuals_predictions)

#Correlation accuracy between the actual and predictive values.The correlation accurray shows 0.98 precentage relationship between the 
#crime and primary counts but still the model is falling under various evaluation so the model was considered as poor predictive accuracy model.
corr_accuracy <- cor(actuals_predictions)
corr_accuracy

# Min-max accuracy was implemented to find the 1 nearest better prediction accurray value. The value of Min-max accuracy was 0.65
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy

# Mape accuracy shows 0.38 this value is slightly high to consider and resulting the model has poor prediction accurarcy.
mape <- mean(abs((actuals_predictions$predicted - actuals_predictions$actuals)) / actuals_predictions$actuals)
mape