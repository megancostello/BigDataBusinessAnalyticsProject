# Load necessary libraries
library(dplyr)   # For data manipulation
library(ggplot2) # For plotting
library(mgcv) #FOR gam()

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
#data1 <- data1 %>%
 # rename(
  #  Series_Title = Series_Title,   # already correct
   # Released_Year = Released_Year,
    #Genre = Genre,
    #Runtime = Runtime,
    #IMDB_Rating = IMDB_Rating,
    #Director = Director,
    #No_of_Votes = No_of_Votes,
    #Gross = Gross,
    #Star1 = Star1,
    #Star2 = Star2,
    #Star3 = Star3,
    #Star4 = Star4
  #)


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
    Star4 = ,
    budget = budget
  )

data2$Runtime <- as.numeric(gsub("[^0-9]", "", data2$Runtime))
data2$Runtime <- data2$Runtime/10

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
    Star4 = ,
    budget = Budget
  )

#5.4.2025, need to remove Gross since that is actually not the revenue, but BoxOffice - Budget

# Merge data2 and data3 on Series_Title
merged_23 <- full_join(data2, data3, by = "Series_Title")

# Then merge the result with data3
#merged_all <- full_join(merged_12, data3, by = "Series_Title")

merged_all <- merged_23 %>%
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
    ),
    budget = coalesce(
      if ("budget.x" %in% names(.)) as.numeric(budget.x) else NA_real_,
      if ("budget.y" %in% names(.)) as.numeric(budget.y) else NA_real_,
      if ("budget"   %in% names(.)) as.numeric(budget)   else NA_real_
    )
  ) %>%
  select(
    Series_Title, Released_Year, Genre, Runtime, IMDB_Rating, Director,
    Gross, Star1, Star2, Star3, Star4, budget
  )


# Identify duplicates and remove all rows with duplicated Series_Title
merged_all_no_duplicates <- merged_all %>%
  filter(!Series_Title %in% merged_all$Series_Title[duplicated(merged_all$Series_Title) | duplicated(merged_all$Series_Title, fromLast = TRUE)])
any(duplicated(merged_all_no_duplicates$Series_Title))  # Should return FALSE

Main_df <- merged_all_no_duplicates

sum(is.na(Main_df$budget))          # Count of NA in budget
sum(!is.finite(Main_df$budget))     # Count of Inf, -Inf, or NaN in budget
sum(is.na(Main_df$Gross))           # Count of NA in Gross
sum(!is.finite(Main_df$Gross))      # Count of Inf, -Inf, or NaN in Gross

#below removes all NA values and Gross Values to make future calcs easier
Main_df <- merged_all_no_duplicates[!is.na(Main_df$budget) & !is.na(Main_df$Gross), ]

sum(is.na(Main_df$budget))          # Count of NA in budget
sum(!is.finite(Main_df$budget))     # Count of Inf, -Inf, or NaN in budget
sum(is.na(Main_df$Gross))           # Count of NA in Gross
sum(!is.finite(Main_df$Gross))      # Count of Inf, -Inf, or NaN in Gross

#add variables for Model Checking
Main_df$budget2 <- Main_df$budget^2
Main_df$budget3 <- Main_df$budget^3
Main_df$budget4 <- Main_df$budget^.5

  
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
Main_df$Runtime <- as.numeric(gsub(" min", "", Main_df$Runtime))

#Our dataset for now is: "merged_all_no_duplicates" 

#FRACTION OF DATA TO BE USED AS IN-SAMPLE TRAINING DATA
p1<-.7 #70% FOR TRAINING / Validation, planning for 40% to train, 30% to validate
p2<- 30/70 #this is the 30% of the the total data

#NUMBER OF OBSERVATIONS IN DATAFRAME
obs_count<-dim(Main_df)[1]

#OF OBSERVATIONS IN THE TRAINING DATA (IN-SAMPLE DATA)
#floor() ROUNDS DOWN TO THE NEAREST WHOLE NUMBER
training_size <- floor(p1 * obs_count)
training_size
#SET THE RANDOM SEED FOR REPRODUCIBILITY
set.seed(12345)
#RANDOMLY SHUFFLES THE ROW NUMBERS OF ORIGINAL DATASET
train_ind <- sample(obs_count, size = training_size)
Training <- Main_df[train_ind, ] #PULLS RANDOM ROWS FOR TRAINING 70% which we need to break again
Testing_Partition <- Main_df[-train_ind, ] #This is our Testing Partition


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

M1 <- lm(Gross~budget,Training_Partition)
summary(M1)
#Multiple R-squared:  0.5368

M2 <- lm(Gross~budget + budget2,Training_Partition)
summary(M2)
#Multiple R-squared:  0.5334

M3 <- lm(Gross~budget + budget2 + budget3,Training_Partition)
summary(M3)
#Multiple R-squared:  0.4415

M4 <- lm(Gross~budget4,Training_Partition)
summary(M4)
#Multiple R-squared:  0.3863

#MODEL 6: Y=s(x) SPLINE MODEL
M5 <- gam(Gross ~ s(budget), data = Training_Partition, family = 'gaussian')
summary(M5) #generates summary diagnostic output
#R-sq.(adj) =   0.58

#These are named nums, which cause issues
Training_Partition$PRED_1_IN <- unname(predict(M1, Training_Partition))
Testing_Partition$PRED_1_OUT <- unname(predict(M1, Testing_Partition))
RMSE_1_In <- sqrt(mean((Training_Partition$PRED_1_IN - Training_Partition$Gross)^2))
RMSE_1_In
RMSE_1_Out <- sqrt(mean((Testing_Partition$PRED_1_OUT - Testing_Partition$Gross)^2))
RMSE_1_Out

Training_Partition$PRED_2_IN <- unname(predict(M2, Training_Partition))
Testing_Partition$PRED_2_OUT <- unname(predict(M2, Testing_Partition))
RMSE_2_In <- sqrt(mean((Training_Partition$PRED_2_IN - Training_Partition$Gross)^2))
RMSE_2_In
RMSE_2_Out <- sqrt(mean((Testing_Partition$PRED_2_OUT - Testing_Partition$Gross)^2))
RMSE_2_Out

Training_Partition$PRED_3_IN <- unname(predict(M3, Training_Partition))
Testing_Partition$PRED_3_OUT <- unname(predict(M3, Testing_Partition))
RMSE_3_In <- sqrt(mean((Training_Partition$PRED_3_IN - Training_Partition$Gross)^2))
RMSE_3_In
RMSE_3_Out <- sqrt(mean((Testing_Partition$PRED_3_OUT - Testing_Partition$Gross)^2))
RMSE_3_Out

Training_Partition$PRED_4_IN <- unname(predict(M4, Training_Partition))
Testing_Partition$PRED_4_OUT <- unname(predict(M4, Testing_Partition))
RMSE_4_In <- sqrt(mean((Training_Partition$PRED_4_IN - Training_Partition$Gross)^2))
RMSE_4_In
RMSE_4_Out <- sqrt(mean((Testing_Partition$PRED_4_OUT - Testing_Partition$Gross)^2))
RMSE_4_Out

Training_Partition$PRED_5_IN <- unname(predict(M5, Training_Partition))
Testing_Partition$PRED_5_OUT <- unname(predict(M5, Testing_Partition))
RMSE_5_In <- sqrt(mean((Training_Partition$PRED_5_IN - Training_Partition$Gross)^2))
RMSE_5_In
RMSE_5_Out <- sqrt(mean((Testing_Partition$PRED_5_OUT - Testing_Partition$Gross)^2))
RMSE_5_Out

TABLE_VAL <- as.table(matrix(c(RMSE_1_In, RMSE_2_In, RMSE_3_In, RMSE_4_In, RMSE_5_In, RMSE_1_Out, RMSE_2_Out, RMSE_3_Out, RMSE_4_Out, RMSE_5_Out), ncol=5, byrow=TRUE))
colnames(TABLE_VAL) <- c('LINEAR', 'QUADRATIC', 'CUBIC', 'Square Root', 'SPLINE')
rownames(TABLE_VAL) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_VAL #REPORT OUT-OF-SAMPLE ERRORS FOR ALL HYPOTHESIS
#The best in sample appears to be Spline, but best out of sample is Quadratic

#use the validation sample to conduct a "Non-contaminated" R^2.

Validation_Partition$PRED_2_Out <- unname(predict(M2,Validation_Partition))
RMSE_2_Validate <- sqrt(mean((Validation_Partition$PRED_2_Out - Validation_Partition$Gross)^2))
RMSE_2_Validate


#Creation of a simple Regression model, y = mx + b
M1 <- lm(Gross~IMDB_Rating + Released_Year + Runtime + budget + budget2,Training_Partition)
summary(M1)
#Multiple R-squared:  0.5595

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
M1_log <- lm(Log_Gross ~ IMDB_Rating + Released_Year + Runtime + budget, data = Training_Partition)
summary(M1_log)
# Multiple R-squared: 0.3496

#SQRT transform data
Training_Partition$SQRT_Gross <- (Training_Partition$Gross)^.5
Testing_Partition$SQRT_Gross <- (Testing_Partition$Gross)^.5

M1_SQRT <- lm(SQRT_Gross ~ IMDB_Rating + Released_Year + Runtime + budget, data = Training_Partition)
summary(M1_SQRT)
#Multiple R-squared:  0.6137

Training_Partition$Cube_RT_Gross <- (Training_Partition$Gross)^(1/3)
Testing_Partition$Cube_RT_Gross <- (Testing_Partition$Gross)^(1/3)

M1_Cube_RT <- lm(Cube_RT_Gross ~ IMDB_Rating + Released_Year + Runtime + budget, data = Training_Partition)
summary(M1_Cube_RT)


#Decide on a set of metrics with your group that everyone will use when benchmarking the
#models for the regression task for both in-sample and out-of-sample performance.
#Test against other data

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
#3606.337

# Out-of-sample RMSE for the SQRT data
RMSE_1_OUT_SQRT <- sqrt(mean((PRED_1_OUT_vec_SQRT[valid_idx_out] - Gross_OUT_vec_SQRT[valid_idx_out])^2))
RMSE_1_OUT_SQRT
#3613.986

#Standard
PRED_1_IN <- predict(M1, Training_Partition)
PRED_1_OUT <- predict(M1, Testing_Partition) 

PRED_1_IN_vec <- unname(PRED_1_IN)
Gross_IN_vec <- unname(Training_Partition$Gross)
PRED_1_OUT_vec <- unname(PRED_1_OUT)
Gross_OUT_vec <- unname(Testing_Partition$Gross)

RMSE_1_IN <- sqrt(mean((PRED_1_IN_vec[valid_idx_in]  - Gross_IN_vec[valid_idx_in] )^2))
RMSE_1_IN
#102909686 something is wrong here

RMSE_1_OUT <- sqrt(mean((PRED_1_OUT_vec[valid_idx_out] - Gross_OUT_vec[valid_idx_out])^2))
RMSE_1_OUT
#91350964 something is wrong here
