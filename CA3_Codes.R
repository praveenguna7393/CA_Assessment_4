
#Copying crime dataset in working directory
#loading the crime dataset in dataframe

Crime_df <- read.csv("IRELAND_CRIME_GARDA_DIVISION_wise_2003-2019.csv", header = TRUE, check.names = FALSE)
Crime_df

#Check.names argument was turned into false in order to prevent the x in front of the column names.

head(Crime_df)

#Checking the type of the data
class(Crime_df)

#struture of the dataframe
str(Crime_df)

#Dealing with the missing data in dataframe.
no_records <- Crime_df[!complete.cases(Crime_df),]
no_records
nrow(no_records)

#As per the result from complete.cases function there is no missing values in crime datasets.

library(mice)
md.pattern(Crime_df)

#By using the mice libaray the dataset was again observed as completely observed data.

#The objective of the analytics is to find the relationship between the poverty and crime rates in ireland.
# In the poverty datasets i have records only for the years 2006, 2011 and 2016.
# So other years of crime data or not need for the further analytics, so in crime dataframe 2006, 2011 and 2016
# records are needed and other quarter years are removed.

Crime_df <- Crime_df[grep("((2003|2004|2005|2007|2008|2009|2010|2012|2013|2014|2015|2017|2018|2019)Q[0-9])"
                          ,names(Crime_df),invert = TRUE)]
Crime_df

# Crime counts are measured in quarter wise of the year summing the quarters into whole year.

CR_2006 <- Crime_df$`2006Q1` + Crime_df$`2006Q2`+Crime_df$`2006Q3`+Crime_df$`2006Q4`  
CR_2006

CR_2011 <- Crime_df$`2011Q1`+ Crime_df$`2011Q2`+ Crime_df$`2011Q3`+ Crime_df$`2011Q4`
CR_2011

CR_2016 <- Crime_df$`2016Q1`+ Crime_df$`2016Q2`+ Crime_df$`2016Q3`+ Crime_df$`2016Q4`
CR_2016

Crime_df <- Crime_df[grep("((2006|2011|2016)Q[0-9])"
                          ,names(Crime_df),invert = TRUE)]
Crime_df

Crime_df <- data.frame(Crime_df,CR_2006,CR_2011,CR_2016)
Crime_df

names(Crime_df)

#Columns like region, Offence.code, offence and type of offence are not need for my analytics
#So i am droping these columns from my crime datasets.

Crime_df <- subset(Crime_df, select = c("GARDA.DIVISION","CR_2006","CR_2011","CR_2016"))
Crime_df

# Renaming the Column name GARDA.DIVISION into COUNTY.

names(Crime_df)[names(Crime_df) == "GARDA.DIVISION"] <- "COUNTY"
Crime_df

#Use unquie function to get the unique county values in column
df <- unique(Crime_df$COUNTY)
df
# I need Countywise crime counts for these i am going to replace the county values into distinct value.
# Before we replace county value we need to change the type column from factor to character.

library(plyr)                                       
Crime_df$COUNTY <- revalue(Crime_df$COUNTY,c("CORK CITY" = "CORK","CORK NORTH" = "CORK","CORK WEST" = "CORK",
                                             "D.M.R. SOUTH CENTRAL" = "DUBLIN","D.M.R. NORTH CENTRAL" = "DUBLIN",
                                             "D.M.R. NORTHERN" = "DUBLIN","D.M.R. SOUTHERN" = "DUBLIN",
                                             "D.M.R. EASTERN" = "DUBLIN","D.M.R. WESTERN" = "DUBLIN"))


library(reshape2)
Crime_df <- melt(Crime_df, id = c("COUNTY"))
Crime_df[order(Crime_df$COUNTY),]
class(Crime_df)

colnames(Crime_df)[2] <- "Year"
colnames(Crime_df)[3] <- "Crime_Counts"

Crime_df$Year <- revalue(Crime_df$Year,c("CR_2006" = "2006","CR_2011" = "2011","CR_2016" = "2016")) 

head(Crime_df)

library(sqldf)
Final_Crime_df <- sqldf('SELECT COUNTY, Year, SUM(Crime_Counts) AS Crime_Counts 
                        FROM Crime_df GROUP BY COUNTY, Year')
Final_Crime_df

y<-strsplit(as.character( Final_Crime_df[,1])  , "/", fixed=TRUE)
check = data.frame(COUNTY= unlist(y), Year= rep(Final_Crime_df[,2], sapply(y, length)),Crime_Counts= rep(Final_Crime_df[,3], sapply(y, length)))
check

Final_Crime_df <- check
Final_Crime_df


# Population living below Nationl_poverty_LINE.
Poverty_df <- read.csv("National_Poverty_Line.csv", header = TRUE)
Poverty_df

head(Poverty_df)

#Checking the type of the data
class(Poverty_df)

#struture of the dataframe bhbhjhj nkjh mnkjk mnjhj 
str(Poverty_df)

names(Poverty_df)

colnames(Poverty_df)

Poverty_population <- subset(Poverty_df, select = c("COUNTY","TOTPOP06","TOTPOP11","TOTPOP16","EDLOW_06",
                                                    "EDLOW_11","EDLOW_16","UNEMPM06","UNEMPM11","UNEMPM16",
                                                    "UNEMPF06","UNEMPF11","UNEMPF16"))
Poverty_population

no_records <- Poverty_population[!complete.cases(Poverty_population),]
no_records
nrow(no_records)

Poverty_population <- Poverty_population[!apply(is.na(Poverty_population) | Poverty_population == "", 1, all),]

# Changing the colname which make sense.
colnames(Poverty_population) <- c("COUNTY","Total_Population_2006","Total_Population_2011","Total_Population_2016",
                                  "Primary_Education_2006","Primary_Education_2011","Primary_Education_2016",
                                  "UnEmployement_Male_2006","UnEmployement_Male_2011","UnEmployement_Male_2016",
                                  "UnEmployement_Female_2006","UnEmployement_Female_2011","UnEmployement_Female_2016")

colnames(Poverty_population)

#Sum the umemployee rate of female and male

unEmployement_2006 <- Poverty_population$UnEmployement_Male_2006 + Poverty_population$UnEmployement_Female_2006
unEmployement_2011 <- Poverty_population$UnEmployement_Male_2011 + Poverty_population$UnEmployement_Female_2011
unEmployement_2016 <- Poverty_population$UnEmployement_Male_2016 + Poverty_population$UnEmployement_Female_2016

#Dropping the Unemployement columns of male and female
Poverty_population[, c("UnEmployement_Male_2006","UnEmployement_Male_2011","UnEmployement_Male_2016",
                       "UnEmployement_Female_2006","UnEmployement_Female_2011","UnEmployement_Female_2016")] <- list(NULL)

#Add the new unEmployement columns in dataframe

Poverty_population <- data.frame(Poverty_population,unEmployement_2006,unEmployement_2011,unEmployement_2016)
colnames(Poverty_population)

Poverty_population

#using the population column going to get the total number people under primary_eduction and unEmployement

PrimaryED_Counts_2006 <- Poverty_population$Total_Population_2006 * Poverty_population$Primary_Education_2006 / 100
PrimaryED_Counts_2011 <- Poverty_population$Total_Population_2011 * Poverty_population$Primary_Education_2011 / 100
PrimaryED_Counts_2016 <- Poverty_population$Total_Population_2016 * Poverty_population$Primary_Education_2016 / 100
unEmployee_Counts_2006 <- Poverty_population$Total_Population_2006 * Poverty_population$unEmployement_2006 / 100
unEmployee_Counts_2011 <- Poverty_population$Total_Population_2011 * Poverty_population$unEmployement_2011 / 100
unEmployee_Counts_2016 <- Poverty_population$Total_Population_2016 * Poverty_population$unEmployement_2016 / 100

#Dropping the column which are in proportion.
Poverty_population[, c("Primary_Education_2006","Primary_Education_2011","Primary_Education_2016",
                       "unEmployement_2006","unEmployement_2011","unEmployement_2016")] <- list(NULL)

Poverty_population <- data.frame(Poverty_population, PrimaryED_Counts_2006,PrimaryED_Counts_2011,PrimaryED_Counts_2016,
                                 unEmployee_Counts_2006,unEmployee_Counts_2011,unEmployee_Counts_2016)
colnames(Poverty_population)

Poverty_population

#Round the column values

library(dplyr)
Poverty_population <- Poverty_population %>% mutate_at(vars(PrimaryED_Counts_2006,PrimaryED_Counts_2011,PrimaryED_Counts_2016,
                                                            unEmployee_Counts_2006,unEmployee_Counts_2011,unEmployee_Counts_2016),
                                                       funs(round(., 0)))

Final_Proverty_PrimaryEducation_df <- subset(Poverty_population, select = c("COUNTY","PrimaryED_Counts_2006","PrimaryED_Counts_2011",
                                                                            "PrimaryED_Counts_2016"))

Final_Proverty_UnEmployement_df <- subset(Poverty_population, select = c("COUNTY","unEmployee_Counts_2006","unEmployee_Counts_2011",
                                                                         "unEmployee_Counts_2016"))

#Add year column in the poverty dataset inorder to merge the crime datasets.
library(reshape2)
check_df <- melt(Final_Proverty_PrimaryEducation_df, id = c("COUNTY"))
check_df[order(check_df$COUNTY),]
class(check_df)

colnames(check_df)[2] <- "Year"
colnames(check_df)[3] <- "Primary_Education_Counts"

#Replacing the values
check_df$Year <- revalue(check_df$Year,c("PrimaryED_Counts_2006" = "2006","PrimaryED_Counts_2011" = "2011","PrimaryED_Counts_2016" = "2016")) 
check_df

Final_Poverty_primaryEd_df <- check_df
Final_Poverty_primaryEd_df

na.omit(Final_Poverty_primaryEd_df)

library(sqldf)
Final_Poverty_primaryEd_df <- sqldf('SELECT COUNTY, Year, SUM(Primary_Education_Counts) AS Primary_ED_Counts 
                        FROM Final_Poverty_primaryEd_df GROUP BY COUNTY, Year')
Final_Poverty_primaryEd_df

library(reshape2)
check_df1 <- melt(Final_Proverty_UnEmployement_df, id = c("COUNTY"))
check_df1[order(check_df$COUNTY),]
class(check_df1)

colnames(check_df1)[2] <- "Year"
colnames(check_df1)[3] <- "UnEmployeement_Counts"

#Replacing the values
check_df1$Year <- revalue(check_df1$Year,c("unEmployee_Counts_2006" = "2006","unEmployee_Counts_2011" = "2011","unEmployee_Counts_2016" = "2016")) 
check_df1

Final_Poverty_unEmployee_df <- check_df1
Final_Poverty_unEmployee_df

na.omit(Final_Poverty_unEmployee_df)

library(sqldf)
Final_Poverty_unEmployee_df <- sqldf('SELECT COUNTY, Year, SUM(UnEmployeement_Counts) AS UnEmployee_Counts 
                        FROM Final_Poverty_unEmployee_df GROUP BY COUNTY, Year')
Final_Poverty_unEmployee_df

#Merge Datatsets
library(plyr)
test1 <- ddply(merge(Final_Crime_df,Final_Poverty_primaryEd_df, all.x = TRUE),
               .(COUNTY, Year))

library(plyr)
test2 <- ddply(merge(Final_Crime_df,Final_Poverty_unEmployee_df, all.x = TRUE),
               .(COUNTY, Year))

Final_df <- ddply(merge(test1,test2, all.x = TRUE),
                  .(COUNTY, Year))
Final_df

head(Final_df)
temp_df <- Final_df
is.numeric(temp_df$COUNTY)
is.numeric(temp_df$Year)
is.numeric(temp_df$Crime_Counts)
is.numeric(temp_df$PrimaryED_Counts)
is.numeric(temp_df$UnEmployee_Counts)


temp_df$COUNTY <- as.numeric(Final_df$COUNTY)
temp_df$Year <- as.numeric(Final_df$Year)
temp_df$Crime_Counts <- as.numeric(Final_df$Crime_Counts)
str(temp_df)

install.packages("corrplot")
opar <- par(no.readonly = TRUE)
library(corrplot)
corrplot(corr = cor(temp_df),tl.col = "Black",tl.cex = 0.90)

pairs(temp_df)
head(temp_df)

#My County variable is categorical variable.
str(Final_df)

#Implementing statistical method into the datasets.
# Final dataframe was created by merging the National_population_line and crime_datasets
Crime_Analytics_df <- Final_df
Final_df

library(lattice)


#histogram was used in order to check the whether the varibles in dataframe
#are normally distributed or not.
histogram(~Primary_ED_Counts | Year, data = Final_df)


#QQ-Plot was used in order to find the normality distribution by using the reference line year 2006.
with(Crime_Analytics_df,
     qqplot(Crime_Analytics_df$Crime_Counts, 
            Crime_Analytics_df$Primary_ED_Counts,
            main = "Primary_ED_Counts vs Crime_Counts",
            xlab = "Crime_Counts",
            ylab = "Primary_ED_Counts"))

Crime_linear_model <- lm(Crime_Analytics_df$Crime_Counts ~ Crime_Analytics_df$Primary_ED_Counts, data = Predictive_build_df) 
Crime_linear_model

plot(Crime_Analytics_df$Crime_Counts,Crime_Analytics_df$Primary_ED_Counts,
     xlab="crime_counts",
     ylab="primary_counts",
     main="Regression line")

# To remove the 
abline(Crime_linear_model)

summary(Crime_linear_model)

#The qq refernece line was drawn with the year '2006' for crime_counts.
with(Crime_Analytics_df, {
  qqnorm(Crime_Analytics_df$Crime_Counts[Crime_Analytics_df$Year == "2006"],
         main = "2006")
  qqline(Crime_Analytics_df$Crime_Counts[Crime_Analytics_df$Year == "2006"])
})

#In order to find the p-value to find the variables are normally distributed or not the function called shapiro was used.
normality_test <- shapiro.test(Crime_Analytics_df$Crime_Counts)
normality_test$p.value


#In order to find the p-value to find the variables are normally distributed or not the function called shapiro was used.
normality_test <- shapiro.test(Crime_Analytics_df$Primary_ED_Counts)
normality_test$p.value

# In order to find the relationship between the crime_counts and primary_eduction_counts the correlation test was 
# by using the spearmen method.

correlation_test = cor.test(Final_df$Crime_Counts, Final_df$Primary_ED_Counts, method = 'spearman', exact = F)
correlation_test

install.packages('pwr')
library(pwr)
effective_size <- cohen.ES(test="r",size="large")
effective_size

# Power calculation analysis.
size_sample <- pwr.r.test(r=effective_size$effect.size, sig.level = 0.05, power = 0.9, alternative = "two.sided")
size_sample

# Hypothesis statstical method has been implemented in order to find the correlation between the two variables using spearman 
# function'
hypo <- cor.test(x=Crime_Analytics_df$Crime_Counts, y=Crime_Analytics_df$Primary_ED_Counts, method = 'spearman', exact = F)
hypo

# removing unemployement count variables from the dataframe.
Crime_Analytics_df = subset(Crime_Analytics_df, select = -c(UnEmployee_Counts) )
Crime_Analytics_df

# Correlation between Crime_counts and primary eduction.
corr = cor.test(Predictive_build_df$Crime_Counts, Final_df$Primary_ED_Counts, method = 'spearman', exact = F)
corr






