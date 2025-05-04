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

# Standardize column names in data3
data3 <- data3 %>%
  rename(
    Series_Title = Movie,   # already correct
    Released_Year = Release.year,
    Genre = Genre,
    Runtime = Running.time,
    IMDB_Rating = IMDb.score,
    Director = Director,
    Gross = Gross,
    Star1 = Actor.1,
    Star2 = Actor.2,
    Star3 = Actor.3,
    Star4 = 
  )

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

#Regression
#Begin by partitioning your original dataset for the regression task into three sets: a 
#training partition and two holdout partitions.  One holdout partition will be for model 
#selection (the validation partition).  One partition will be used to report the final model 
#out-of-sample performance estimate (the testing partition).  I would recommend saving 
#these three partitions using the write.table() or write.csv() commands (or making sure 
#you’re all using the same random seed to generate the partitions) so that all group 
#members start with the same training partition and same holdout partitions so everyone 
#is making “apples to apples” comparisons. 


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
M1 <- lm(Gross~IMDB_Rating + Released_Year,Training_Partition)
summary(M1)
PRED_1_IN <- predict(M1, Training_Partition) 
PRED_1_OUT <- predict(M1, Testing_Partition) 

# Strip names before computing error
PRED_1_OUT_vec <- unname(PRED_1_OUT)  #fix an issue where we had named numerics, causing NA for Pred_IN
Gross_OUT_vec <- unname(Testing_Partition$Gross) #fix an issue where we had named numerics, causing NA for Pred_IN

valid_idx <- complete.cases(PRED_1_OUT_vec, Gross_OUT_vec) #fix an issue where RMSE was coming as NA since we had "NA" values
RMSE_1_OUT <- sqrt(mean((PRED_1_OUT_vec[valid_idx] - Gross_OUT_vec[valid_idx])^2))
RMSE_1_OUT

# Now do the RMSE
RMSE_1_OUT <- sqrt(mean((PRED_1_OUT_vec - Gross_OUT_vec)^2))
RMSE_1_OUT
summary(M1)

#Plot what we have.
plot(Training_Partition$IMDB_Rating, Training_Partition$Gross,
     xlab = "IMDB Rating", ylab = "Gross", main = "Gross vs IMDB Rating")
abline(M1, col = "red")

#Based off the summary of "M1" we see a R^2 of 0.068 which represents around 6.8% of the gross
#Data may be showing such a large issue since we have a very very large Gross from Avatar.
#We can try to visualize this as a percentage of change instead via log transform

# Filter Training and Testing to only rows where Gross > 0
Training_Partition <- Training_Partition[!is.na(Training_Partition$Gross) & Training_Partition$Gross > 0, ]
Testing_Partition <- Testing_Partition[!is.na(Testing_Partition$Gross) & Testing_Partition$Gross > 0, ]

# Now create the log variable
Training_Partition$Log_Gross <- log(Training_Partition$Gross)
Testing_Partition$Log_Gross <- log(Testing_Partition$Gross)

# Now this will work
M1_log <- lm(Log_Gross ~ IMDB_Rating + Released_Year, data = Training_Partition)
summary(M1_log)

#Our data is stil wild, log transformation helped with reducing our R^2, only showing 7.6% now
#We will need to include more information to see what decides the gross amount.

