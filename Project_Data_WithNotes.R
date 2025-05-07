# Load necessary libraries
library(dplyr)   # For data manipulation
library(ggplot2) # For plotting

# List all files in the directory (useful for troubleshooting)
all_files <- list.files(recursive = TRUE, full.names = TRUE)

# Load dataset 1
data1 <- read.csv("imdb_top_1000.csv")
summary(data1)  # Summarize dataset 1 to understand its structure

# Load dataset 2
data2 <- read.csv("movies_updated.csv")
# Load dataset 3
data3 <- read.csv("movies_data.csv")

# Standardize column names
data1 <- data1 %>%
  rename(
    Series_Title = Series_Title,   # already correct
    Released_Year = Released_Year,
    Genre = Genre,
    Runtime = Runtime,
    IMDB_Rating = IMDB_Rating,
    Director = Director,
    No_of_Votes = No_of_Votes,
    Gross = Gross,
    Star1 = Star1,
    Star2 = Star2,
    Star3 = Star3,
    Star4 = Star4
  )

data2 <- data2 %>%
  rename(
    Series_Title = name,   #data2 names
    Released_Year = year,
    Genre = genre,
    Runtime = runtime..,
    IMDB_Rating = score,
    Director = director,
    No_of_Votes = votes,
    Gross = gross,
    Star1 = star,
    Star2 = ,
    Star3 = ,
    Star4 = 
  )

data3 <- data3 %>%
  mutate(Gross = Earnings)

# Standardize column names in data3, we saw that Gross was different than the actual
#Gross value, causing an issue with negative "Gross" values

data3 <- select(data3, -Gross)

data3 <- data3 %>%
  rename(
    Series_Title = Movie,   # already correct
    Released_Year = Release.year,
    Genre = Genre,
    Runtime = Running.time,
    IMDB_Rating = IMDb.score,
    Director = Director,
    Gross = Box.Office,
    Star1 = Actor.1,
    Star2 = Actor.2,
    Star3 = Actor.3,
    Star4 = 
  )

#5.4.2025, need to remove Gross since that is actually not the revenue, but BoxOffice - Budget

# Merge data1 and data2 on Series_Title
merged_12 <- full_join(data1, data2, by = "Series_Title")

# Then merge the result with data3
merged_all <- full_join(merged_12, data3, by = "Series_Title")

merged_all <- merged_all %>%
  mutate(
    Released_Year = coalesce(
      if ("Released_Year.x" %in% names(.)) as.integer(Released_Year.x) else NA_integer_,
      if ("Released_Year.y" %in% names(.)) as.integer(Released_Year.y) else NA_integer_,
      if ("Released_Year"   %in% names(.)) as.integer(Released_Year)   else NA_integer_
    ),
    Genre = coalesce(
      if ("Genre.x" %in% names(.)) as.character(Genre.x) else NA_character_,
      if ("Genre.y" %in% names(.)) as.character(Genre.y) else NA_character_,
      if ("Genre"   %in% names(.)) as.character(Genre)   else NA_character_
    ),
    Runtime = coalesce(
      if ("Runtime.x" %in% names(.)) as.character(Runtime.x) else NA_character_,
      if ("Runtime.y" %in% names(.)) as.character(Runtime.y) else NA_character_,
      if ("Runtime"   %in% names(.)) as.character(Runtime)   else NA_character_
    ),
    IMDB_Rating = coalesce(
      if ("IMDB_Rating.x" %in% names(.)) as.numeric(IMDB_Rating.x) else NA_real_,
      if ("IMDB_Rating.y" %in% names(.)) as.numeric(IMDB_Rating.y) else NA_real_,
      if ("IMDB_Rating"   %in% names(.)) as.numeric(IMDB_Rating)   else NA_real_
    ),
    Director = coalesce(
      if ("Director.x" %in% names(.)) as.character(Director.x) else NA_character_,
      if ("Director.y" %in% names(.)) as.character(Director.y) else NA_character_,
      if ("Director"   %in% names(.)) as.character(Director)   else NA_character_
    ),
    Gross = coalesce(
      if ("Gross.x" %in% names(.)) as.numeric(Gross.x) else NA_real_,
      if ("Gross.y" %in% names(.)) as.numeric(Gross.y) else NA_real_,
      if ("Gross"   %in% names(.)) as.numeric(Gross)   else NA_real_
    ),
    Star1 = coalesce(
      if ("Star1.x" %in% names(.)) as.character(Star1.x) else NA_character_,
      if ("Star1.y" %in% names(.)) as.character(Star1.y) else NA_character_,
      if ("Star1"   %in% names(.)) as.character(Star1)   else NA_character_
    ),
    Star2 = coalesce(
      if ("Star2.x" %in% names(.)) as.character(Star2.x) else NA_character_,
      if ("Star2.y" %in% names(.)) as.character(Star2.y) else NA_character_,
      if ("Star2"   %in% names(.)) as.character(Star2)   else NA_character_
    ),
    Star3 = coalesce(
      if ("Star3.x" %in% names(.)) as.character(Star3.x) else NA_character_,
      if ("Star3.y" %in% names(.)) as.character(Star3.y) else NA_character_,
      if ("Star3"   %in% names(.)) as.character(Star3)   else NA_character_
    ),
    Star4 = coalesce(
      if ("Star4.x" %in% names(.)) as.character(Star4.x) else NA_character_,
      if ("Star4.y" %in% names(.)) as.character(Star4.y) else NA_character_,
      if ("Star4"   %in% names(.)) as.character(Star4)   else NA_character_
    )
  ) %>%
  select(
    Series_Title, Released_Year, Genre, Runtime, IMDB_Rating, Director,
    Gross, Star1, Star2, Star3, Star4
  )


# Identify duplicates and remove all rows with duplicated Series_Title
merged_all_no_duplicates <- merged_all %>%
  filter(!Series_Title %in% merged_all$Series_Title[duplicated(merged_all$Series_Title) | duplicated(merged_all$Series_Title, fromLast = TRUE)])
any(duplicated(merged_all_no_duplicates$Series_Title))  # Should return FALSE

Main_df <- merged_all_no_duplicates

#THE ABOVE IS THE DATASET.

#Question: Does the year affect the gross
library(ggplot2)

# Plot Gross vs Released_Year (Inflation), easier to break records as time goes on?
ggplot(merged_all_no_duplicates, aes(x = Released_Year, y = Gross)) +
  geom_point() +  # Add scatter plot
  labs(
    title = "Gross by Year",
    x = "Year",
    y = "Gross Funds ($)"
  ) +
  theme_minimal()  # Clean theme
#Bryan Test Commmit
#5.4.2025

#Code Chunk 6 For most of this to help
#Regression
#Begin by partitioning your original dataset for the regression task into three sets: a 
#training partition and two holdout partitions.  One holdout partition will be for model 
#selection (the validation partition).  One partition will be used to report the final model 
#out-of-sample performance estimate (the testing partition).  I would recommend saving 
#these three partitions using the write.table() or write.csv() commands (or making sure 
#you’re all using the same random seed to generate the partitions) so that all group 
#members start with the same training partition and same holdout partitions so everyone 
#is making “apples to apples” comparisons. 

#Update Runtime to make it an integer
merged_all_no_duplicates$Runtime <- as.numeric(gsub(" min", "", merged_all_no_duplicates$Runtime))

#Our dataset for now is: "merged_all_no_duplicates" 

#FRACTION OF DATA TO BE USED AS IN-SAMPLE TRAINING DATA
p1<-.7 #70% FOR TRAINING / Validation, planning for 40% to train, 30% to validate
p2<- 30/70 #this is the 30% of the the total data

#NUMBER OF OBSERVATIONS IN DATAFRAME
obs_count<-dim(merged_all_no_duplicates)[1]

#OF OBSERVATIONS IN THE TRAINING DATA (IN-SAMPLE DATA)
#floor() ROUNDS DOWN TO THE NEAREST WHOLE NUMBER
training_size <- floor(p1 * obs_count)
training_size
#SET THE RANDOM SEED FOR REPRODUCIBILITY
set.seed(12345)
#RANDOMLY SHUFFLES THE ROW NUMBERS OF ORIGINAL DATASET
train_ind <- sample(obs_count, size = training_size)
Training <- merged_all_no_duplicates[train_ind, ] #PULLS RANDOM ROWS FOR TRAINING 70% which we need to break again
Testing_Partition <- merged_all_no_duplicates[-train_ind, ] #This is our Testing Partition


#We will break the other into validation from the "Training"
obs_count2<-dim(Training)[1] 
validation_size <- floor(p2 * obs_count2) #30% of the 70% split before
set.seed(12345) #set seed
validation_ind <- sample(obs_count2, size = validation_size) #Same as above for creating the index
Validation_Partition <- Training[validation_ind,] #PULLS RANDOM ROWS FOR Validation 70% which we need to break again
Training_Partition <- Training[-validation_ind,] #Remainder go to Training

#Check to see if it adds up
obs_count == nrow(Validation_Partition) + nrow(Training_Partition) + nrow(Testing_Partition)
#We now have: Training Partition 40%, Validation Partition 30%, and a Testing Partion 30%

#Creation of a simple Regression model, y = mx + b
M1 <- lm(Gross~IMDB_Rating + Released_Year + Runtime,Training_Partition)
summary(M1)
#Multiple R-squared:  0.1112

PRED_1_IN <- predict(M1, Training_Partition) 
PRED_1_OUT <- predict(M1, Testing_Partition) 

Training_Partition$PRED_1_IN_Num <- unname(PRED_1_IN)  #fix an issue where we had named numerics, causing NA for Pred_IN
Training_Partition$Gross_Num <- (Training_Partition$Gross) 

# Strip names before computing error
Testing_Partition$PRED_1_OUT_Num <- unname(PRED_1_OUT)  #fix an issue where we had class = named numerics, causing NA for Pred_IN
Testing_Partition$Gross_Num <- (Testing_Partition$Gross) #fix an issue where we had class = named numerics, causing NA for Pred_IN

#Checking class and lengths
class(Testing_Partition$PRED_1_OUT_Num)
class(Testing_Partition$Gross_Num)
length(Testing_Partition$PRED_1_OUT_Num)
length(Training_Partition$PRED_1_IN_Num)

#We need to remove the NAs otherwise the RMSE comes out as nothing, we fill in the Gross_Nums here away
#The complete.cases() function in R is used to identify rows in a data frame, matrix, or vector that do not contain
#any missing values (NA). This function returns a logical vector indicating which cases are
#complete.
#IN SAMPLE CLEANING - Trainign DF, along with RMSE Calc
valid_rows <- complete.cases(Training_Partition$PRED_1_IN_Num, Training_Partition$Gross_Num)
RMSE_1_In <- sqrt(mean((Training_Partition$PRED_1_IN_Num[valid_rows] - Training_Partition$Gross_Num[valid_rows])^2))

#OUT SAMPLE CLEANING - Testing DF, along with RMSE Calc
valid_rows <- complete.cases(Testing_Partition$PRED_1_OUT_Num, Testing_Partition$Gross_Num)
RMSE_1_Out <- sqrt(mean((Testing_Partition$PRED_1_OUT_Num[valid_rows] - Testing_Partition$Gross_Num[valid_rows])^2))

plot(Training_Partition$IMDB_Rating, Training_Partition$Gross,
     xlab = "IMDB Rating", ylab = "Gross", main = "Gross vs IMDB Rating")
abline(M1, col = "red")

#We see RMSEs between the two, RMSE for out of sample is actually smaller than the In Sample, this is likely due to
#James Cameron movies like Titanic and Avatar, huge outliers in our dataframes
#---------------------------------------------------------------------------------

#log transform data

# Now create the log variable
Training_Partition$Log_Gross <- log(Training_Partition$Gross)
Testing_Partition$Log_Gross <- log(Testing_Partition$Gross)

# Now this will work
M1_log <- lm(Log_Gross ~ IMDB_Rating + Released_Year + Runtime, data = Training_Partition)
summary(M1_log)
# Note: R² is low, model likely underfit, Multiple R-squared:  0.09304

#SQRT transform data
Training_Partition$SQRT_Gross <- (Training_Partition$Gross)^.5
Testing_Partition$SQRT_Gross <- (Testing_Partition$Gross)^.5

M1_SQRT <- lm(SQRT_Gross ~ IMDB_Rating + Released_Year + Runtime, data = Training_Partition)
summary(M1_SQRT)
#Multiple R-squared:  0.1249

#Decide on a set of metrics with your group that everyone will use when benchmarking the
#models for the regression task for both in-sample and out-of-sample performance.
#Test against other data

# Filter partitions to only rows where Gross > 0
Training_Partition <- Training_Partition[!is.na(Training_Partition$Gross) & Training_Partition$Gross > 0, ]
Testing_Partition <- Testing_Partition[!is.na(Testing_Partition$Gross) & Testing_Partition$Gross > 0, ]

# Log-transform the Gross column
Training_Partition$Log_Gross <- log(Training_Partition$Gross)
Testing_Partition$Log_Gross <- log(Testing_Partition$Gross)

# Run linear regression with log(Gross) as the outcome
M1_log <- lm(Log_Gross ~ IMDB_Rating + Released_Year + Runtime, data = Training_Partition)
summary(M1_log) 

# Plot the relationship between IMDB Rating and Log Gross
plot(Training_Partition$IMDB_Rating, Training_Partition$Log_Gross,
     xlab = "IMDB Rating", ylab = "Log Gross", main = "Log Gross vs IMDB Rating")
abline(M1_log, col = "red")

# Try square root transformation of Gross
Training_Partition$SQRT_Gross <- sqrt(Training_Partition$Gross)
Testing_Partition$SQRT_Gross <- sqrt(Testing_Partition$Gross)

# Regression using sqrt(Gross)
M1_SQRT <- lm(SQRT_Gross ~ IMDB_Rating + Released_Year + Runtime, data = Training_Partition)
summary(M1_SQRT)

# Generate predictions SQRT
PRED_1_IN_SQRT <- predict(M1_SQRT, Training_Partition)
PRED_1_OUT_SQRT <- predict(M1_SQRT, Testing_Partition)

# Clean vectors for RMSE calculation (strip names, handle NA)
# This changes "Named Num Class" to "num"
PRED_1_IN_vec_SQRT <- unname(PRED_1_IN_SQRT)
Gross_IN_vec_SQRT <- unname(Training_Partition$SQRT_Gross)
PRED_1_OUT_vec_SQRT <- unname(PRED_1_OUT_SQRT)
Gross_OUT_vec_SQRT <- unname(Testing_Partition$SQRT_Gross)

#this removes all na cases, we cannot compute NA datapoints
valid_idx_in <- complete.cases(PRED_1_IN_vec_SQRT,Gross_IN_vec_SQRT)
valid_idx_out <- complete.cases(PRED_1_OUT_vec_SQRT, Gross_OUT_vec_SQRT)

# In-sample RMSE for the SQRT data
RMSE_1_IN_SQRT <- sqrt(mean((PRED_1_IN_vec_SQRT[valid_idx_in]  - Gross_IN_vec_SQRT[valid_idx_in] )^2))
RMSE_1_IN_SQRT
#6194.202

# Out-of-sample RMSE for the SQRT data
RMSE_1_OUT_SQRT <- sqrt(mean((PRED_1_OUT_vec_SQRT[valid_idx_out] - Gross_OUT_vec_SQRT[valid_idx_out])^2))
RMSE_1_OUT_SQRT
#5826.217

