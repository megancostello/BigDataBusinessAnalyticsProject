
# Load necessary libraries
#install.packages("glmnet") #THIS IS NEEDED TO RUN glmnet(), needed for regularization
library(dplyr)   # For data manipulation
library(ggplot2) # For plotting
library(mgcv) #FOR gam()
library(tidyverse)


# List all files in the directory (useful for troubleshooting)
all_files <- list.files(recursive = TRUE, full.names = TRUE)

# Load dataset 1
data1 <- read.csv("imdb_top_1000.csv")
summary(data1)  # Summarize dataset 1 to understand its structure

# Load dataset 2
data2 <- read.csv("movies_updated.csv")
# Load dataset 3
data3 <- read.csv("movies_data.csv")

#REDCATED DATASET 1 since its missing values is preventing useful interpretation of data.
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

#This is removing the "Min" and spacing in the Runtime in the data
data2$Runtime <- as.numeric(gsub("[^0-9]", "", data2$Runtime))
#The minutes were larger by a factor of 10 than they should have been
data2$Runtime <- data2$Runtime/10


# Standardize column names in data3, we saw that Gross was different than the actual
#Gross value, causing an issue with negative "Gross" values
#This Gross Value is incorrect, this is representing Budget - Box.Office

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


#MAIN DATAFRAME WILL BE Main_df for clarity
Main_df <- merged_all_no_duplicates


#Check to see if values are usable in budget

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
Main_df$budgetLog <- log(Main_df$budget)

#clean up budgetLog
Main_df$budgetLog[Main_df$budgetLog == -Inf] <- 0

# view top three genres
unique(Main_df$Genre)
Main_df %>% count(Genre)

# add dummy categorical vars for top three genres (plus horror)
Main_df$Action[Main_df$Genre=='Action']<-1 #if it's action, code the action variable 1
Main_df$Action[Main_df$Genre!='Action']<-0 #if it's not action, code the action variable 0

Main_df$Drama[Main_df$Genre=='Drama']<-1 #if it's Drama, code the Drama variable 1
Main_df$Drama[Main_df$Genre!='Drama']<-0 #if it's not Drama, code the Drama variable 0

Main_df$Comedy[Main_df$Genre=='Comedy']<-1 #if it's Comedy, code the Comedy variable 1
Main_df$Comedy[Main_df$Genre!='Comedy']<-0 #if it's not Comedy, code the Comedy variable 0

Main_df$Horror[Main_df$Genre=='Horror']<-1 #if it's Horror, code the Horror variable 1
Main_df$Horror[Main_df$Genre!='Horror']<-0 #if it's not Horror, code the Horror variable 0

#Update Runtime to make it an integer
Main_df$Runtime <- as.numeric(gsub(" min", "", Main_df$Runtime))
Main_df <- Main_df[, c("Gross", setdiff(names(Main_df), "Gross"))]

# ====== DATAFRAME Creation END ================================
  
#THE ABOVE IS THE DATASET.

# ====== DATA PARTITIONING ================================

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

#Check to see if it adds up, should return TRUE
obs_count == nrow(Validation_Partition) + nrow(Training_Partition) + nrow(Testing_Partition)
#We now have: Training Partition 40%, Validation Partition 30%, and a Testing Partion 30%

# ====== DATA PARTITIONING END================================


#BIVARIATE LINEAR MODEL - SIMPLE
# ====== 4a linear model ================================
M1 <- lm(Gross~budget,Training_Partition)
summary(M1)

# ====== 4b linear model - Transformations ================================
#QUADRATIC REGRESSION#
M2 <- lm(Gross~budget + budget2,Training_Partition)
summary(M2)

#CUBIC REGRESSION#
M3 <- lm(Gross~budget + budget2 + budget3,Training_Partition)
summary(M3)

#SQRT REGRESSION#
M4 <- lm(Gross~budget4,Training_Partition)
summary(M4)

# ====== 4c linear model ================================

#simplified dataframe in attempt to do regularization on multivariate
library(glmnet) #needed for regularization function glmnet(), install library above
training <- Training_Partition[, c("Gross", "budget", "budget2")] #REGULARIZE BIVARIATE JUST BUDGET VS GROSS#
holdout <- Testing_Partition[, c("Gross", "budget", "budget2")] #REGULARIZE BIVARIATE JUST BUDGET VS GROSS#
col_of_ones <- rep(1, dim(training)[1]) #Column of ones to include the intercept when we multiply matrix
X <- as.matrix(cbind(col_of_ones, training[-1])) #bind the col of ones to the training dataset, except gross
y <- training[,1] #matrix for "Gross"
X_mat <- as.matrix(X) 
ridge_model <- glmnet(X_mat, y, alpha = 0)
ridge_model
cv_ridge <- cv.glmnet(X_mat, y, alpha = 0)
# Get the lambda that gives minimum mean cross-validated error
best_lambda <- cv_ridge$lambda.min
cat("Best lambda:", best_lambda, "\n")
# If you want a slightly more regularized model (1 SE rule)
lambda_1se <- cv_ridge$lambda.1se
cat("1-SE lambda:", lambda_1se, "\n")
ridge_model <- glmnet(X_mat, y, alpha = 0, lambda = best_lambda)
ridge_model
# E_IN using training data
pred_train <- predict(ridge_model, newx = X_mat)
E_IN_BIVARIATE <- sqrt(mean((y - pred_train)^2))
E_IN_BIVARIATE
# Prepare the holdout (test) set for prediction
X_test <- as.matrix(cbind(rep(1, nrow(holdout)), holdout[, -1]))  # Add intercept term for test set
y_test <- holdout[, 1]  # True values for the test set
# E_OUT using test data (out-of-sample error)
pred_test <- predict(ridge_model, newx = X_test)
E_OUT_BIVARIATE <- sqrt(mean((y_test - pred_test)^2))  # RMSE for test data
cat("E_OUT (RMSE for test data):", E_OUT_BIVARIATE, "\n")

# ====== 4c linear model end================================

# ====== 4d General Addiditve Structure ===================================

#MODEL 5: Y=s(x) SPLINE MODEL
M5 <- gam(Gross ~ s(budget), data = Training_Partition, family = 'gaussian')
summary(M5) #generates summary diagnostic output

# ====== 4e PLOT the Above ===================================
x_grid <- seq(min(Training_Partition$budget), max(Training_Partition$budget), length.out = 300)
#x_grid <- seq(0,80,.1) #CREATES GRID OF X-AXIS VALUES
#Training Datapoints plotted
plot(Training_Partition$Gross ~ Training_Partition$budget, col='blue')

predictions_1 <- predict(M1, data.frame(budget = x_grid))
predictions_2 <- predict(M2, data.frame(budget = x_grid, budget2 = x_grid^2))
predictions_3 <- predict(M3, data.frame(budget = x_grid, budget2 = x_grid^2, budget3 = x_grid^3))
predictions_5 <- predict(M5, data.frame(budget = x_grid), type = 'response')

lines(x_grid, predictions_1, col='blue', lwd=3) #PLOTS M1
lines(x_grid, predictions_2, col='lightgreen', lwd=3) #PLOTS M2
lines(x_grid, predictions_3, col='green', lwd=3) #PLOTS M3
lines(x_grid, predictions_5, col='yellow', lwd=3) #PLOTS M5 CV RIDGE on Bivariate

#Test Datapoints plotted
points(Testing_Partition$Gross ~ Testing_Partition$budget, col='red', pch=3, cex=.5)

#These are named nums, which cause issues, use unname() function to fix
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


#================================================================================
#simplified dataframe in attempt to do regularization on multivariate
training <- Training_Partition[, c("Gross", "budget2", "budget", "IMDB_Rating")]
holdout <- Testing_Partition[, c("Gross", "budget2", "budget", "IMDB_Rating")]
col_of_ones <- rep(1, dim(training)[1])
X <- as.matrix(cbind(col_of_ones, training[-1]))
y <- training[,1]
X_mat <- as.matrix(X)
ridge_model <- glmnet(X_mat, y, alpha = 0)
ridge_model

# Assuming X_mat is your matrix of predictors and y is your response variable
# Perform cross-validated ridge regression (alpha = 0 for ridge)
cv_ridge <- cv.glmnet(X_mat, y, alpha = 0)
# Plot the cross-validation curve
plot(cv_ridge)

#Plot RMSE rather than MSE
# Get mean cross-validated errors (MSE)
mse_values <- cv_ridge$cvm
# Convert MSE to RMSE
rmse_values <- sqrt(mse_values)
# Get log(lambda) values
log_lambda <- log(cv_ridge$lambda)
# Plot RMSE vs log(lambda)
plot(log_lambda, rmse_values, type = "b", pch = 20, col = "blue",
     xlab = "log(Lambda)", ylab = "RMSE",
     main = "Cross-Validated RMSE vs log(Lambda)")
abline(v = log(cv_ridge$lambda.min), col = "red", lty = 2)  # Best lambda

# Get the lambda that gives minimum mean cross-validated error
best_lambda <- cv_ridge$lambda.min
cat("Best lambda:", best_lambda, "\n")
# If you want a slightly more regularized model (1 SE rule)
lambda_1se <- cv_ridge$lambda.1se
cat("1-SE lambda:", lambda_1se, "\n")
ridge_model <- glmnet(X_mat, y, alpha = 0, lambda = best_lambda)
ridge_model
# E_IN using training data
pred_train <- predict(ridge_model, newx = X_mat)
E_IN <- sqrt(mean((y - pred_train)^2))
E_IN
# Prepare the holdout (test) set for prediction
X_test <- as.matrix(cbind(rep(1, nrow(holdout)), holdout[, -1]))  # Add intercept term for test set
y_test <- holdout[, 1]  # True values for the test set
# E_OUT using test data (out-of-sample error)
pred_test <- predict(ridge_model, newx = X_test)
E_OUT <- sqrt(mean((y_test - pred_test)^2))  # RMSE for test data
cat("E_OUT (RMSE for test data):", E_OUT, "\n")
TABLE_VAL <- as.table(matrix(c(RMSE_1_In, E_IN_BIVARIATE, RMSE_2_In, RMSE_3_In, RMSE_4_In, RMSE_5_In,E_IN, RMSE_1_Out, E_OUT_BIVARIATE, RMSE_2_Out, RMSE_3_Out, RMSE_4_Out, RMSE_5_Out, E_OUT), ncol=7, byrow=TRUE))
colnames(TABLE_VAL) <- c('Bivariate LINEAR','Bi LINEAR Reg', 'QUADRATIC', 'CUBIC', 'Square Root', 'SPLINE',"Multi Ridge-Reg")
rownames(TABLE_VAL) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_VAL #REPORT OUT-OF-SAMPLE ERRORS FOR ALL HYPOTHESIS

#The best in sample appears to be Spline, but best out of sample is Quadratic

#use the validation sample to conduct a "Non-contaminated" R^2.

Validation_Partition$PRED_2_Out <- unname(predict(M2,Validation_Partition))
RMSE_2_Validate <- sqrt(mean((Validation_Partition$PRED_2_Out - Validation_Partition$Gross)^2))
RMSE_2_Validate

#############################
#    5. MULTIVARIATE        #
#############################

# ====== 5a linear model ================================
MT0 <- lm(Gross~IMDB_Rating + budget + Action + Comedy + Drama + Horror,Training_Partition)
summary(MT0)
#Multiple R-squared:  0.559

PRED_0_IN <- predict(MT0, Training_Partition) 
PRED_0_OUT <- predict(MT0, Testing_Partition) 

Training_Partition$PRED_0_IN_Num <- unname(PRED_0_IN)  #fix an issue where we had named numerics, causing NA for Pred_IN
Training_Partition$Gross_Num <- (Training_Partition$Gross) 

# Strip names before computing error
Testing_Partition$PRED_0_OUT_Num <- unname(PRED_0_OUT)  #fix an issue where we had class = named numerics, causing NA for Pred_IN
Testing_Partition$Gross_Num <- (Testing_Partition$Gross) #fix an issue where we had class = named numerics, causing NA for Pred_IN

# calculate RMSE
RMSE_MT0_In <- sqrt(mean((Training_Partition$PRED_0_IN_Num - Training_Partition$Gross_Num)^2))
RMSE_MT0_In
RMSE_MT0_Out <- sqrt(mean((Testing_Partition$PRED_0_OUT_Num - Testing_Partition$Gross_Num)^2))
RMSE_MT0_Out

# ====== 5b linear model apply regularization ================================

#REGULARIZATION RIDGE

training <- Training_Partition[, c("Gross", "Comedy", "Drama", "Action", "Horror", "budget", "IMDB_Rating")]
holdout <- Testing_Partition[, c("Gross", "Comedy", "Drama", "Action", "Horror", "budget", "IMDB_Rating")]

col_of_ones <- rep(1, dim(training)[1])

X <- as.matrix(cbind(col_of_ones, training[-1]))

y <- training[,1]

library(glmnet)
X_mat <- as.matrix(X)
ridge_model <- glmnet(X_mat, y, alpha = 0)
ridge_model

# Assuming X_mat is your matrix of predictors and y is your response variable

# Perform cross-validated ridge regression (alpha = 0 for ridge)
cv_ridge <- cv.glmnet(X_mat, y, alpha = 0)

# Plot the cross-validation curve
plot(cv_ridge)

# Get the lambda that gives minimum mean cross-validated error
best_lambda <- cv_ridge$lambda.min
cat("Best lambda:", best_lambda, "\n")

# If you want a slightly more regularized model (1 SE rule)
lambda_1se <- cv_ridge$lambda.1se
cat("1-SE lambda:", lambda_1se, "\n")

ridge_model <- glmnet(X_mat, y, alpha = 0, lambda = best_lambda)
ridge_model

# E_IN using training data
pred_train <- predict(ridge_model, newx = X_mat)
ridge_E_IN <- sqrt(mean((y - pred_train)^2))
ridge_E_IN

# Prepare the holdout (test) set for prediction
X_test <- as.matrix(cbind(rep(1, nrow(holdout)), holdout[, -1]))  # Add intercept term for test set
y_test <- holdout[, 1]  # True values for the test set

# E_OUT using test data (out-of-sample error)
pred_test <- predict(ridge_model, newx = X_test)
ridge_E_OUT <- sqrt(mean((y_test - pred_test)^2))  # RMSE for test data

# ====== 5c model includes transformations budget2 ================================
#Creation of a simple Regression model, y = mx + b
MT1 <- lm(Gross~IMDB_Rating + budget + budget2 + budget3 + Action + Drama + Horror, Training_Partition)
summary(MT1)
#Multiple R-squared:  0.5693

PRED_1_IN <- predict(MT1, Training_Partition) 
PRED_1_OUT <- predict(MT1, Testing_Partition) 

Training_Partition$PRED_1_IN_Num <- unname(PRED_1_IN)  #fix an issue where we had named numerics, causing NA for Pred_IN
Training_Partition$Gross_Num <- (Training_Partition$Gross) 

# Strip names before computing error
Testing_Partition$PRED_1_OUT_Num <- unname(PRED_1_OUT)  #fix an issue where we had class = named numerics, causing NA for Pred_IN
Testing_Partition$Gross_Num <- (Testing_Partition$Gross) #fix an issue where we had class = named numerics, causing NA for Pred_IN

# calculate RMSE
RMSE_MT1_In <- sqrt(mean((Training_Partition$PRED_1_IN_Num - Training_Partition$Gross_Num)^2))
RMSE_MT1_In
RMSE_MT1_Out <- sqrt(mean((Testing_Partition$PRED_1_OUT_Num - Testing_Partition$Gross_Num)^2))
RMSE_MT1_Out



# ====== 5d SVM ==========================================================
library(e1071) #SVM LIBRARY

svm_training <- Training_Partition[, c("Gross", "IMDB_Rating", "budget", "budget2")]
svm_testing <- Testing_Partition[, c("Gross", "IMDB_Rating", "budget", "budget2")]
# svm_training$Gross <- log(svm_training$Gross)
# svm_training$budget <- log(svm_training$budget)
# svm_training$budget2 <- log(svm_training$budget2)
# 
# svm_testing$Gross <- log(svm_testing$Gross)
# svm_testing$budget <- log(svm_testing$budget)
# svm_testing$budget2 <- log(svm_testing$budget2)

#BUILD SVM CLASSIFIER
  # hard to get it to run without hitting max iterations, decreased cost and set SCALE to TRUE, kernel = linear
SVM_Model<- svm(Gross~IMDB_Rating + budget + budget2, 
                data = svm_training, 
                type = "eps-regression", #set to "eps-regression" for numeric prediction
                kernel = "linear",
                cost=0.1,                   #REGULARIZATION PARAMETER
                gamma = 1/(ncol(svm_training)-1), #DEFAULT KERNEL PARAMETER
                coef0 = 0,                    #DEFAULT KERNEL PARAMETER
                degree=2,                     #POLYNOMIAL KERNEL PARAMETER
                scale = TRUE)                #RESCALE DATA? (SET TO TRUE TO NORMALIZE)

print(SVM_Model) #DIAGNOSTIC SUMMARY

#REPORT IN AND OUT-OF-SAMPLE ERRORS (1-ACCURACY)
(E_IN_PRETUNE<-1-mean(unname(predict(SVM_Model, svm_training))==Training_Partition$Gross))
(E_OUT_PRETUNE<-1-mean(unname(predict(SVM_Model, svm_testing))==Testing_Partition$Gross))

#TUNING THE SVM BY CROSS-VALIDATION
tune_control<-tune.control(cross=10) #SET K-FOLD CV PARAMETERS
set.seed(12)
TUNE <- tune.svm(x = svm_training[,-1],
                 y = svm_training[,1],
                 type = "eps-regression",
                 kernel = "radial",
                 tunecontrol=tune_control,
                 cost=c(.01, .1, 1, 10, 100, 1000), #REGULARIZATION PARAMETER
                 gamma = 1/(ncol(svm_training)-1), #KERNEL PARAMETER
                 coef0 = 0,           #KERNEL PARAMETER
                 degree = 2)          #POLYNOMIAL KERNEL PARAMETER

print(TUNE) #OPTIMAL TUNING PARAMETERS FROM VALIDATION PROCEDURE

SVM_Model_tuned<- svm(Gross~IMDB_Rating + budget + budget2, 
                data = svm_training, 
                type = "eps-regression", #set to "eps-regression" for numeric prediction
                kernel = "radial",
                cost=10,                   #REGULARIZATION PARAMETER
                gamma = 0.333, #DEFAULT KERNEL PARAMETER
                coef0 = 0,                    #DEFAULT KERNEL PARAMETER
                degree=2,                     #POLYNOMIAL KERNEL PARAMETER
                scale = TRUE)                #RESCALE DATA? (SET TO TRUE TO NORMALIZE)

print(SVM_Model_tuned) #DIAGNOSTIC SUMMARY

#REPORT IN AND OUT-OF-SAMPLE ERRORS (1-ACCURACY)
(E_IN_TUNED<-1-mean(unname(predict(SVM_Model_tuned, svm_training))==Training_Partition$Gross))
(E_OUT_TUNED<-1-mean(unname(predict(SVM_Model_tuned, svm_testing))==Testing_Partition$Gross))

#GENERATE PREDICTIONS 
preds_svm_in <- predict(SVM_Model_tuned, svm_training)%>%
  bind_cols(svm_training)
preds_svm_out <- predict(SVM_Model_tuned, svm_testing)%>%
  bind_cols(svm_testing)

# Function to compute RMSE manually
rmse_manual <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

rmse_svm_in <- rmse_manual(preds_svm_in$Gross, preds_svm_in$...1)
rmse_svm_out <- rmse_manual(preds_svm_out$Gross, preds_svm_out$...1)

# ====== 5e regression tree ==========================================================

library(tidymodels) 
library(rpart.plot)
#SPECIFYING THE regression TREE MODEL

reg_spec <- decision_tree(min_n = 20 , #minimum number of observations for split
                            tree_depth = 30, #max tree depth
                            cost_complexity = 0.01)  %>% #regularization parameter
  set_engine("rpart") %>%
  set_mode("regression")
print(reg_spec)

#ESTIMATING THE MODEL 
reg_fmla <- Gross ~ .
reg_tree <- reg_spec %>%
  fit(formula = reg_fmla, data = svm_training)
print(reg_tree)

#VISUALIZING THE TREE MODEL:
reg_tree$fit %>%
  rpart.plot(type = 2,roundint = FALSE)

plotcp(reg_tree$fit)

#GENERATE PREDICTIONS AND COMBINE WITH TEST SET
pred_reg <- predict(reg_tree, new_data = svm_testing) %>%
  bind_cols(svm_testing)

#OUT-OF-SAMPLE ERROR ESTIMATES FROM yardstick OR ModelMetrics PACKAGE
rmse(pred_reg, estimate=.pred, truth=Gross)

# TUNING TREE ------------------

#BLANK TREE SPECIFICATION FOR TUNING
tree_spec <- decision_tree(min_n = tune(),
                           tree_depth = tune(),
                           cost_complexity= tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

#CREATING A TUNING PARAMETER GRID
tree_grid <- grid_regular(parameters(tree_spec), levels = 3)

set.seed(123) #SET SEED FOR REPRODUCIBILITY WITH CROSS-VALIDATION
tune_results <- tune_grid(tree_spec,
                          reg_fmla, #MODEL FORMULA
                          resamples = vfold_cv(svm_training, v=3), #RESAMPLES / FOLDS
                          grid = tree_grid, #GRID
                          metrics = metric_set(rmse, rsq)) #BENCHMARK METRIC

#RETRIEVE OPTIMAL PARAMETERS FROM CROSS-VALIDATION
best_params <- select_best(tune_results)
best_params

# NEW TUNED TREE ---------------------

reg_spec_tune <- decision_tree(min_n = 40 , #minimum number of observations for split
                          tree_depth = 8, #max tree depth
                          cost_complexity = 0.0000000001)  %>% #regularization parameter
  set_engine("rpart") %>%
  set_mode("regression")
print(reg_spec_tune)

#ESTIMATING THE MODEL 
reg_fmla_tune <- Gross ~ .
reg_tree_tune <- reg_spec_tune %>%
  fit(formula = reg_fmla_tune, data = svm_training)
print(reg_tree_tune)

#VISUALIZING THE TREE MODEL:
reg_tree_tune$fit %>%
  rpart.plot(type = 2,roundint = FALSE)

plotcp(reg_tree_tune$fit)

#GENERATE PREDICTIONS AND COMBINE WITH TEST SET
pred_reg_in_tune <- predict(reg_tree_tune, new_data = svm_training) %>%
  bind_cols(svm_training)

#GENERATE PREDICTIONS AND COMBINE WITH TEST SET
pred_reg_out_tune <- predict(reg_tree_tune, new_data = svm_testing) %>%
  bind_cols(svm_testing)

#OUT-OF-SAMPLE ERROR ESTIMATES FROM yardstick OR ModelMetrics PACKAGE
rmse_tree_in <- rmse(pred_reg_in_tune, estimate=.pred, truth=Gross)
rmse_tree_out <- rmse(pred_reg_out_tune, estimate=.pred, truth=Gross)

# ====== 5f tree-based ensemble model ==========================================================
library(baguette) #FOR BAGGED TREES
library(caret)

spec_bagged <- bag_tree(min_n = 20 , #minimum number of observations for split
                        tree_depth = 30, #max tree depth
                        cost_complexity = 0.01, #regularization parameter
                        class_cost = NULL)  %>% #for output class imbalance adjustment (binary data only)
  set_mode("regression") %>% #can set to regression for numeric prediction
  set_engine("rpart", times=100) #times = # OF ENSEMBLE MEMBERS IN FOREST
spec_bagged

#FITTING THE MODEL
set.seed(123)
#MODEL DESCRIPTION:
fmla <- Gross ~.

bagged_forest <- spec_bagged %>%
  fit(formula = fmla, data = svm_training)
print(bagged_forest)

#GENERATE IN-SAMPLE PREDICTIONS ON THE TRAIN SET AND COMBINE WITH TRAIN DATA
pred_class_bf_in <- predict(bagged_forest, new_data = svm_training, type="numeric") %>%
  bind_cols(svm_training) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA

#GENERATE OUT-OF-SAMPLE PREDICTIONS ON THE TEST SET AND COMBINE WITH TEST DATA
pred_class_bf_out <- predict(bagged_forest, new_data = svm_testing, type="numeric") %>%
  bind_cols(svm_testing) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA

# TUNING BAGGED TREE ------------------

#BLANK TREE SPECIFICATION FOR TUNING

bag_model <- bag_tree(min_n = tune() , #minimum number of observations for split
                        tree_depth = tune(), #max tree depth
                        cost_complexity = tune(), #regularization parameter
                        class_cost = NULL)  %>% #for output class imbalance adjustment (binary data only)
  set_mode("regression") %>% #can set to regression for numeric prediction
  set_engine("rpart", times=100) #times = # OF ENSEMBLE MEMBERS IN FOREST
bag_model

workflow_spec <- workflow() %>%
  add_model(bag_model) %>%
  add_recipe(recipe(Gross ~ ., data = svm_training))

folds <- vfold_cv(svm_training, v = 5)

# Grid of hyperparameters
grid_vals <- grid_regular(
  min_n(range = c(2, 10)),
  tree_depth(range = c(1, 10)),
  cost_complexity(range = c(0, 1)),
  levels = 4)

tuned_results <- tune_grid(
  workflow_spec,
  resamples = folds,
  grid = grid_vals,
  metrics = metric_set(rmse)
)

best_params <- select_best(tuned_results)
best_params

spec_bagged_tune <- bag_tree(min_n = 7 , #minimum number of observations for split
                        tree_depth = 4, #max tree depth
                        cost_complexity = 4.64, #regularization parameter
                        class_cost = NULL)  %>% #for output class imbalance adjustment (binary data only)
  set_mode("regression") %>% #can set to regression for numeric prediction
  set_engine("rpart", times=100) #times = # OF ENSEMBLE MEMBERS IN FOREST
spec_bagged_tune

bagged_forest_tune <- spec_bagged_tune %>%
  fit(formula = fmla, data = svm_training)
print(bagged_forest_tune)

#GENERATE IN-SAMPLE PREDICTIONS ON THE TRAIN SET AND COMBINE WITH TRAIN DATA
pred_class_bftune_in <- predict(bagged_forest_tune, new_data = svm_training, type="numeric") %>%
  bind_cols(svm_training) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA

#GENERATE OUT-OF-SAMPLE PREDICTIONS ON THE TEST SET AND COMBINE WITH TEST DATA
pred_class_bftune_out <- predict(bagged_forest_tune, new_data = svm_testing, type="numeric") %>%
  bind_cols(svm_testing) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA

rmse_bftune_in <- rmse(pred_class_bftune_in, truth = Gross, estimate = .pred)
rmse_bftune_out <- rmse(pred_class_bftune_out, truth = Gross, estimate = .pred)

# ====== 5g RMSE table ==========================================================

TABLE_MULTIVAR_RMSE <- as.table(matrix(c(RMSE_MT0_In, RMSE_MT1_In, ridge_E_IN, rmse_svm_in, rmse_tree_in$.estimate, rmse_bftune_in$.estimate, RMSE_MT0_Out, RMSE_MT1_Out, ridge_E_OUT, rmse_svm_out, rmse_tree_out$.estimate, rmse_bftune_out$.estimate), ncol=6, byrow=TRUE))
colnames(TABLE_MULTIVAR_RMSE) <- c('LINEAR', 'RIDGE', 'NONLINEAR', 'SVM', 'TREE', 'BAGGED TREE')
rownames(TABLE_MULTIVAR_RMSE) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_MULTIVAR_RMSE #REPORT OUT-OF-SAMPLE ERRORS FOR ALL HYPOTHESIS


# ====== 8 binary classification ================================

# ====== 9 multi class prediction tools ================================

# ====== 9a SVM ==========================================================
library(e1071) #SVM LIBRARY

svm_training_class <- Training_Partition[, c("Genre", "Gross", "IMDB_Rating", "budget", "budget2")]
svm_testing_class <- Testing_Partition[, c("Genre", "Gross", "IMDB_Rating", "budget", "budget2")]

# Convert the genre variable to a factor
svm_training_class$Genre <- as.factor(svm_training_class$Genre)
svm_testing_class$Genre <- as.factor(svm_testing_class$Genre)

# Check the factor levels
levels(svm_training_class$Genre)
levels(svm_testing_class$Genre)

# Convert the factor genre into numeric
# svm_training_class$Genre <- as.numeric(svm_training_class$Genre)
# svm_testing_class$Genre <- as.numeric(svm_testing_class$Genre)

# svm_training$Gross <- log(svm_training$Gross)
# svm_training$budget <- log(svm_training$budget)
# svm_training$budget2 <- log(svm_training$budget2)
# 
# svm_testing$Gross <- log(svm_testing$Gross)
# svm_testing$budget <- log(svm_testing$budget)
# svm_testing$budget2 <- log(svm_testing$budget2)

#BUILD SVM CLASSIFIER
# hard to get it to run without hitting max iterations, decreased cost and set SCALE to TRUE, kernel = linear
SVM_Model_class <- svm(Genre~.,
                data = svm_training_class, 
                type = "C-classification", #set to "eps-regression" for numeric prediction
                kernel = "radial",
                cost=0.1,                   #REGULARIZATION PARAMETER
                gamma = 1/(ncol(svm_training_class)-1), #DEFAULT KERNEL PARAMETER
                coef0 = 0,                    #DEFAULT KERNEL PARAMETER
                degree=2,                     #POLYNOMIAL KERNEL PARAMETER
                scale = FALSE)                #RESCALE DATA? (SET TO TRUE TO NORMALIZE)

print(SVM_Model_class) #DIAGNOSTIC SUMMARY

#REPORT IN AND OUT-OF-SAMPLE ERRORS (1-ACCURACY)
(E_IN_CLASS_PRETUNE<-1-mean(unname(predict(SVM_Model_class, svm_training_class))==svm_training_class$Genre))
(E_OUT_CLASS_PRETUNE<-1-mean(unname(predict(SVM_Model_class, svm_testing_class))==svm_testing_class$Genre))

#TUNING THE SVM BY CROSS-VALIDATION
tune_control<-tune.control(cross=10) #SET K-FOLD CV PARAMETERS
set.seed(123)
TUNE <- tune.svm(x = svm_training_class[,-1],
                 y = svm_training_class[,1],
                 type = "C-classification",
                 kernel = "radial",
                 tunecontrol=tune_control,
                 cost=c(.01, .1, 1, 10, 100, 1000), #REGULARIZATION PARAMETER
                 gamma = 1/(ncol(svm_training_class)-1), #KERNEL PARAMETER
                 coef0 = 0,           #KERNEL PARAMETER
                 degree = 2)          #POLYNOMIAL KERNEL PARAMETER

print(TUNE) #OPTIMAL TUNING PARAMETERS FROM VALIDATION PROCEDURE

SVM_Model_class_tuned<- svm(Genre~., 
                      data = svm_training_class, 
                      type = "C-classification", #set to "eps-regression" for numeric prediction
                      kernel = "radial",
                      cost=1,                   #REGULARIZATION PARAMETER
                      gamma = 0.25, #DEFAULT KERNEL PARAMETER
                      coef0 = 0,                    #DEFAULT KERNEL PARAMETER
                      degree=2,                     #POLYNOMIAL KERNEL PARAMETER
                      scale = FALSE)                #RESCALE DATA? (SET TO TRUE TO NORMALIZE)

print(SVM_Model_class_tuned) #DIAGNOSTIC SUMMARY

#REPORT IN AND OUT-OF-SAMPLE ERRORS (1-ACCURACY)
(E_IN_CLASS_TUNED<-1-mean(unname(predict(SVM_Model_class_tuned, svm_training_class))==svm_training_class$Genre))
(E_OUT_CLASS_TUNED<-1-mean(unname(predict(SVM_Model_class_tuned, svm_testing_class))==svm_testing_class$Genre))

in_class_svm_tuned_accuracy <- mean(unname(predict(SVM_Model_class_tuned, svm_training_class))==svm_training_class$Genre)
out_class_svm_tuned_accuracy <- mean(unname(predict(SVM_Model_class_tuned, svm_testing_class))==svm_testing_class$Genre)


#GENERATE PREDICTIONS 
preds_svm_class_in <- predict(SVM_Model_class_tuned, svm_training_class)%>%
  bind_cols(svm_training_class)
preds_svm_class_out <- predict(SVM_Model_class_tuned, svm_testing_class)%>%
  bind_cols(svm_testing_class)

# Function to compute RMSE manually
rmse_manual <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

rmse_svm_class_in <- rmse_manual(preds_svm_class_in$Genre, preds_svm_class_in$...1)
rmse_svm_class_out <- rmse_manual(preds_svm_class_out$Genre, preds_svm_class_out$...1)

# ====== 9b regression tree ==========================================================

library(tidymodels) 
library(rpart.plot)
#SPECIFYING THE regression TREE MODEL

reg_spec_class <- decision_tree(min_n = 20 , #minimum number of observations for split
                          tree_depth = 30, #max tree depth
                          cost_complexity = 0.01)  %>% #regularization parameter
  set_engine("rpart") %>%
  set_mode("classification")
print(reg_spec_class)

#ESTIMATING THE MODEL 
class_fmla <- Genre ~ .
class_tree <- reg_spec %>%
  fit(formula = class_fmla, data = svm_training_class)
print(class_tree)

#VISUALIZING THE TREE MODEL:
class_tree$fit %>%
  rpart.plot(type = 3,roundint = FALSE)

plotcp(class_tree$fit)

#GENERATE PREDICTIONS AND COMBINE WITH TEST SET
pred_class_reg <- predict(class_tree, new_data = svm_testing_class) %>%
  bind_cols(svm_testing_class)

pred_class_reg_numeric <- pred_class_reg
pred_class_reg_numeric$Genre <- as.numeric(pred_class_reg_numeric$Genre)
pred_class_reg_numeric$.pred_class <- as.numeric(pred_class_reg_numeric$.pred_class)


#OUT-OF-SAMPLE ERROR ESTIMATES FROM yardstick OR ModelMetrics PACKAGE
tree_class_rmse_out <- rmse(pred_class_reg_numeric, estimate=.pred_class, truth=Genre)

#GENERATE OUT-OF-SAMPLE CONFUSION MATRIX AND DIAGNOSTICS
confusion <- table(pred_class_reg$.pred_class, pred_class_reg$Genre)
confusionMatrix(confusion) #FROM CARET PACKAGE

# TUNING TREE ------------------

#BLANK TREE SPECIFICATION FOR TUNING
tree_spec_class <- decision_tree(min_n = tune(),
                           tree_depth = tune(),
                           cost_complexity= tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

#CREATING A TUNING PARAMETER GRID
tree_grid_class <- grid_regular(parameters(tree_spec_class), levels = 3)

set.seed(123) #SET SEED FOR REPRODUCIBILITY WITH CROSS-VALIDATION
tune_class_results <- tune_grid(tree_spec_class,
                          class_fmla, #MODEL FORMULA
                          resamples = vfold_cv(svm_training_class, v=3), #RESAMPLES / FOLDS
                          grid = tree_grid_class, #GRID
                          metrics = metric_set(accuracy)) #BENCHMARK METRIC

#RETRIEVE OPTIMAL PARAMETERS FROM CROSS-VALIDATION
best_params <- select_best(tune_class_results)
best_params

# NEW TUNED TREE ---------------------

class_spec_tune <- decision_tree(min_n = 1 , #minimum number of observations for split
                               tree_depth = 2, #max tree depth
                               cost_complexity = 0.0000000001)  %>% #regularization parameter
  set_engine("rpart") %>%
  set_mode("classification")
print(class_spec_tune)

#ESTIMATING THE MODEL 
class_fmla_tune <- Genre ~ .
class_tree_tune <- class_spec_tune %>%
  fit(formula = class_fmla_tune, data = svm_training_class)
print(class_tree_tune)

#VISUALIZING THE TREE MODEL:
class_tree_tune$fit %>%
  rpart.plot(type = 2,roundint = FALSE)

plotcp(class_tree_tune$fit)

#GENERATE PREDICTIONS AND COMBINE WITH TEST SET
pred_class_in_tune <- predict(class_tree_tune, new_data = svm_training_class) %>%
  bind_cols(svm_training_class)

#GENERATE PREDICTIONS AND COMBINE WITH TEST SET
pred_class_out_tune <- predict(class_tree_tune, new_data = svm_testing_class) %>%
  bind_cols(svm_testing_class)

#OUT-OF-SAMPLE ERROR ESTIMATES FROM yardstick OR ModelMetrics PACKAGE
rmse_tree_in <- rmse(pred_class_in_tune, estimate=.pred_class, truth=Genre)
rmse_tree_out <- rmse(pred_class_out_tune, estimate=.pred_class, truth=Genre)

#GENERATE OUT-OF-SAMPLE CONFUSION MATRIX AND DIAGNOSTICS
confusion_class_tuned <- table(pred_class_out_tune$.pred_class, pred_class_out_tune$Genre)
confusionMatrix(confusion_class_tuned) #FROM CARET PACKAGE

# # ====== 9c tree-based ensemble model ==========================================================

# library(baguette) #FOR BAGGED TREES
# library(caret)
# 
# spec_bagged_class <- bag_tree(min_n = 20 , #minimum number of observations for split
#                         tree_depth = 30, #max tree depth
#                         cost_complexity = 0.01, #regularization parameter
#                         class_cost = NULL)  %>% #for output class imbalance adjustment (binary data only)
#   set_mode("classification") %>% #can set to regression for numeric prediction
#   set_engine("rpart", times=100) #times = # OF ENSEMBLE MEMBERS IN FOREST
# spec_bagged_class
# 
# #FITTING THE MODEL
# set.seed(123)
# #MODEL DESCRIPTION:
# fmla_class <- Genre ~.
# 
# bagged_forest_class <- spec_bagged_class %>%
#   fit(formula = fmla_class, data = svm_training_class)
# print(bagged_forest_class)
# 
# # svm_training_class_numeric <- svm_training_class
# # svm_testing_class_numeric <- svm_testing_class
# # 
# # svm_training_class_numeric$Genre <- as.numeric(svm_training_class_numeric$Genre)
# # svm_testing_class_numeric$Genre <- as.numeric(svm_testing_class_numeric$Genre)
# 
# 
# #GENERATE IN-SAMPLE PREDICTIONS ON THE TRAIN SET AND COMBINE WITH TRAIN DATA
# pred_class_bf_in_class <- predict(bagged_forest_class, new_data = svm_training_class, type="class") %>%
#   bind_cols(svm_training_class) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA
# 
# #GENERATE OUT-OF-SAMPLE PREDICTIONS ON THE TEST SET AND COMBINE WITH TEST DATA
# pred_class_bf_out_class <- predict(bagged_forest_class, new_data = svm_testing_class, type="class") %>%
#   bind_cols(svm_testing_class) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA
# 
# #GENERATE IN-SAMPLE CONFUSION MATRIX AND DIAGNOSTICS
# confusion_bag_out <- table(pred_class_bf_out_class$.pred_class, pred_class_bf_out_class$Genre)
# confusionMatrix(confusion) #FROM CARET PACKAGE
# 
# #GENERATE IN-SAMPLE CONFUSION MATRIX AND DIAGNOSTICS
# confusion_bag_in <- table(pred_class_bf_in_class$.pred_class, pred_class_bf_in_class$Genre)
# confusionMatrix(confusion) #FROM CARET PACKAGE
# 
# # TUNING BAGGED TREE ------------------
# 
# #BLANK TREE SPECIFICATION FOR TUNING
# 
# bag_model_class <- bag_tree(min_n = tune() , #minimum number of observations for split
#                       tree_depth = tune(), #max tree depth
#                       cost_complexity = tune(), #regularization parameter
#                       class_cost = NULL)  %>% #for output class imbalance adjustment (binary data only)
#   set_mode("classification") %>% #can set to regression for numeric prediction
#   set_engine("rpart", times = 50)  # Number of bagged trees
# bag_model_class
# 
# workflow_spec <- workflow() %>%
#   add_model(bag_model_class) %>%
#   add_recipe(recipe(Genre ~ ., data = svm_training_class))
# 
# svm_training_class$Genre <- as.factor(svm_training_class$Genre)
# 
# folds <- vfold_cv(svm_training_class, v = 3, strata = Genre)
# 
# # Grid of hyperparameters
# grid_vals <- grid_regular(
#   min_n(range = c(2, 10)),
#   tree_depth(range = c(1, 10)),
#   cost_complexity(range = c(0, 1)),
#   levels = 4)
# 
# library(dplyr)
# 
# folds %>%
#   mutate(class_counts = map(splits, ~table(analysis(.x)$Genre))) %>%
#   pull(class_counts)
# 
# tuned_results <- tune_grid(
#   workflow_spec,
#   resamples = folds,
#   grid = grid_vals,
#   metrics = metric_set(accuracy)
# )
# 
# best_params <- select_best(tuned_results)
# best_params
# 
# spec_bagged_tune <- bag_tree(min_n = 7 , #minimum number of observations for split
#                              tree_depth = 4, #max tree depth
#                              cost_complexity = 4.64, #regularization parameter
#                              class_cost = NULL)  %>% #for output class imbalance adjustment (binary data only)
#   set_mode("classification") %>% #can set to regression for numeric prediction
#   set_engine("rpart", times=100) #times = # OF ENSEMBLE MEMBERS IN FOREST
# spec_bagged_tune
# 
# bagged_forest_tune <- spec_bagged_tune %>%
#   fit(formula = fmla, data = svm_training_class)
# print(bagged_forest_tune)
# 
# #GENERATE IN-SAMPLE PREDICTIONS ON THE TRAIN SET AND COMBINE WITH TRAIN DATA
# pred_class_bftune_in <- predict(bagged_forest_tune, new_data = svm_training_class, type="factor") %>%
#   bind_cols(svm_training_class) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA
# 
# #GENERATE OUT-OF-SAMPLE PREDICTIONS ON THE TEST SET AND COMBINE WITH TEST DATA
# pred_class_bftune_out <- predict(bagged_forest_tune, new_data = svm_testing_class, type="factor") %>%
#   bind_cols(svm_testing_class) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA
# 
# rmse_bftune_in <- rmse(pred_class_bftune_in, truth = Genre, estimate = .pred)
# rmse_bftune_out <- rmse(pred_class_bftune_out, truth = Genre, estimate = .pred)

# ===== Random Forest bc tuning the bagged tree is not working :( =========

library(ranger)
library(vip)
library(baguette)
library(tidymodels)
library(caret)

spec_rf <- rand_forest(min_n = 20 , #minimum number of observations for split
                       trees = 100, #of ensemble members (trees in forest)
                       mtry = 2)  %>% #number of variables to consider at each split
  set_mode("classification") %>% #can set to regression for numeric prediction
  set_engine("ranger") #alternative engine / package: randomForest
spec_rf

fmla <- Genre ~.

#FITTING THE RF MODEL
set.seed(123) #NEED TO SET SEED WHEN FITTING OR BOOTSTRAPPED SAMPLES WILL CHANGE
random_forest <- spec_rf %>%
  fit(formula = fmla, data = svm_training_class) #%>%
print(random_forest)

#RANKING VARIABLE IMPORTANCE (CAN BE DONE WITH OTHER MODELS AS WELL)
set.seed(123) #NEED TO SET SEED WHEN FITTING OR BOOTSTRAPPED SAMPLES WILL CHANGE
rand_forest(min_n = 20 , #minimum number of observations for split
            trees = 100, #of ensemble members (trees in forest)
            mtry = 2)  %>% #number of variables to consider at each split
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(fmla, data = svm_training_class) %>%
  vip() #FROM VIP PACKAGE - ONLY WORKS ON RANGER FIT DIRECTLY

# TUNING RANDOM FOREST

rf_recipe <- recipe(Genre ~ ., data = svm_training_class)

rf_model <- rand_forest(
  mtry = tune(),           # number of variables randomly sampled at each split
  min_n = tune(),          # minimal node size
  trees = 500              # number of trees to build
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

rf_wf <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_model)

set.seed(123)
folds <- vfold_cv(svm_training_class, v = 5, strata = Genre)

# How many predictors do you have?
n_predictors <- ncol(svm_training_class) - 1  # subtract 1 for the response

grid_vals <- grid_random(
  mtry(range = c(1, n_predictors)),
  min_n(range = c(2, 20)),
  size = 20
)

set.seed(234)
tuned_results <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = grid_vals,
  metrics = metric_set(accuracy)
)

best_params <- select_best(tuned_results)
best_params

spec_rf_tune <- rand_forest(min_n = 18 , #minimum number of observations for split
                       trees = 100, #of ensemble members (trees in forest)
                       mtry = 2)  %>% #number of variables to consider at each split
  set_mode("classification") %>% #can set to regression for numeric prediction
  set_engine("ranger") #alternative engine / package: randomForest
spec_rf_tune

fmla <- Genre ~.

#FITTING THE RF MODEL
set.seed(123) #NEED TO SET SEED WHEN FITTING OR BOOTSTRAPPED SAMPLES WILL CHANGE
random_forest_tune <- spec_rf_tune %>%
  fit(formula = fmla, data = svm_training_class) #%>%
print(random_forest_tune)

#RANKING VARIABLE IMPORTANCE (CAN BE DONE WITH OTHER MODELS AS WELL)
set.seed(123) #NEED TO SET SEED WHEN FITTING OR BOOTSTRAPPED SAMPLES WILL CHANGE
rand_forest(min_n = 18 , #minimum number of observations for split
            trees = 100, #of ensemble members (trees in forest)
            mtry = 2)  %>% #number of variables to consider at each split
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(fmla, data = svm_training_class) %>%
  vip() #FROM VIP PACKAGE - ONLY WORKS ON RANGER FIT DIRECTLY

#GENERATE IN-SAMPLE PREDICTIONS ON THE TRAIN SET AND COMBINE WITH TRAIN DATA
pred_class_rf_tune_in <- predict(random_forest_tune, new_data = svm_training_class, type="class") %>%
  bind_cols(svm_training_class) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA

#GENERATE IN-SAMPLE CONFUSION MATRIX AND DIAGNOSTICS
confusion <- table(pred_class_rf_tune_in$.pred_class, pred_class_rf_tune_in$Genre)
confusionMatrix(confusion) #FROM CARET PACKAGE

#GENERATE OUT-OF-SAMPLE PREDICTIONS ON THE TEST SET AND COMBINE WITH TEST DATA
pred_class_rf_tune_out <- predict(random_forest_tune, new_data = svm_testing_class, type="class") %>%
  bind_cols(svm_testing_class) #ADD CLASS PREDICTIONS DIRECTLY TO TEST DATA

#GENERATE OUT-OF-SAMPLE CONFUSION MATRIX AND DIAGNOSTICS
confusion <- table(pred_class_rf_tune_out$.pred_class, pred_class_rf_tune_out$Genre)
confusionMatrix(confusion) #FROM CARET PACKAGE

# ====== 9d validation partition ==========================================================
















# 
# #SET UP GRID OF REGULARIZATION PARAMETER (LAMBDA) VALUES
# lambda <- seq(0, 2,.001)
# 
# #INITIALIZE EMPTY MATRIX TO STORE ESTIMATED MODEL COEFFICIENTS FOR EACH LAMBDA
# BETA_RIDGE <- matrix(NA, nrow = dim(t(X)%*%X)[1], ncol=length(lambda))
# 
# #INITIALIZE EMPTY MATRICES FOR STORING PREDICTION AND ERRORS
# PRED_IN <- matrix(NA, nrow = dim(Training_Partition)[1], ncol=length(lambda))
# PRED_OUT <- matrix(NA, nrow = dim(Testing_Partition)[1], ncol=length(lambda))
# E_IN <- matrix(NA, nrow = 1, ncol=length(lambda))
# E_OUT <- matrix(NA, nrow = 1, ncol=length(lambda))
# 
# for (i in 1:length(lambda)){
# 
#   #COMPUTE PSEUDOINVERSE SOLUTION
#   BETA_RIDGE[,i] <- solve(t(X)%*%X+lambda[i]*diag(dim(t(X)%*%X)[1]))%*%t(X)%*%y
# 
#   #COMPUTE PREDICTIONS IN AND OUT-OF-SAMPLE
#   PRED_IN[,i] <- X%*%BETA_RIDGE[,i]
#   PRED_OUT[,i] <- X_holdout%*%BETA_RIDGE[,i]
# 
#   #COMPUTE PREDICTION ERRORS (MSE) IN AND OUT-OF-SAMPLE
#   E_IN[i] <- sqrt(mean((y-PRED_IN[,i])^2))
#   E_OUT[i] <- sqrt(mean((y_holdout-PRED_OUT[,i])^2))
# }
# 
# #STORE ERRORS VS. LAMBDAS IN SEPARATE DATAFRAMES
# df_IN <- data.frame(cbind(Error=as.numeric(E_IN), Lambda=lambda))
# df_OUT <- data.frame(cbind(Error=as.numeric(E_OUT), Lambda=lambda))
# 
# ggplot(df_IN, aes(y=Error, x=Lambda)) +
#   geom_line(color='blue') +
#   geom_line(data=df_OUT, color='red') +
#   ggtitle("E_IN & E_OUT VS. REGULARIZATION PARAMETER (LAMBDA)") +
#   theme(plot.title = element_text(hjust = .5))
# 
# #REPORT MINIMUM E_OUT ESTIMATE FROM BEST REGULARIZED MODEL
# (min(df_OUT$Error))
# 
# #RECOVER OPTIMAL LAMBDA
# (Opt_Lambda <- df_OUT$Lambda[which.min(df_OUT$Error)])
# 
# #REPLOT WITH MINIMUM ERROR IDENTIFIED
# ggplot(df_OUT, aes(y=Error, x=Lambda)) +
#   geom_line(color='red') +
#   geom_vline(xintercept=Opt_Lambda, color='red', lty=2) +
#   ggtitle("E_OUT VS. REGULARIZATION PARAMETER (LAMBDA)") +
#   theme(plot.title = element_text(hjust = .5))
# 
# 
# #
# # #Checking class and lengths
# # class(Testing_Partition$PRED_1_OUT_Num)
# # class(Testing_Partition$Gross_Num)
# # length(Testing_Partition$PRED_1_OUT_Num)
# # length(Training_Partition$PRED_1_IN_Num)
# #
# # #We need to remove the NAs otherwise the RMSE comes out as nothing, we fill in the Gross_Nums here away
# # #The complete.cases() function in R is used to identify rows in a data frame, matrix, or vector that do not contain
# # #any missing values (NA). This function returns a logical vector indicating which cases are
# # #complete.
# # #IN SAMPLE CLEANING - Trainign DF, along with RMSE Calc
# # valid_rows <- complete.cases(Training_Partition$PRED_1_IN_Num, Training_Partition$Gross_Num)
# # RMSE_1_In <- sqrt(mean((Training_Partition$PRED_1_IN_Num[valid_rows] - Training_Partition$Gross_Num[valid_rows])^2))
# # RMSE_1_In
# #
# # #OUT SAMPLE CLEANING - Testing DF, along with RMSE Calc
# # valid_rows <- complete.cases(Testing_Partition$PRED_1_OUT_Num, Testing_Partition$Gross_Num)
# # RMSE_1_Out <- sqrt(mean((Testing_Partition$PRED_1_OUT_Num[valid_rows] - Testing_Partition$Gross_Num[valid_rows])^2))
# # RMSE_1_Out
# #
# # #We see RMSEs between the two, RMSE for out of sample is actually smaller than the In Sample, this is likely due to
# # #James Cameron movies like Titanic and Avatar, huge outliers in our dataframes
# # #---------------------------------------------------------------------------------
# #
# # #log transform data
# #
# # # Now create the log variable
# # Training_Partition$Log_Gross <- log(Training_Partition$Gross)
# # Testing_Partition$Log_Gross <- log(Testing_Partition$Gross)
# #
# # # Now this will work
# # M1_log <- lm(Log_Gross ~ IMDB_Rating + Released_Year + Runtime + budget, data = Training_Partition)
# # summary(M1_log)
# # # Multiple R-squared: 0.3496
# #
# # #SQRT transform data
# # Training_Partition$SQRT_Gross <- (Training_Partition$Gross)^.5
# # Testing_Partition$SQRT_Gross <- (Testing_Partition$Gross)^.5
# #
# # M1_SQRT <- lm(SQRT_Gross ~ IMDB_Rating + Released_Year + Runtime + budget, data = Training_Partition)
# # summary(M1_SQRT)
# # #Multiple R-squared:  0.6137
# #
# # Training_Partition$Cube_RT_Gross <- (Training_Partition$Gross)^(1/3)
# # Testing_Partition$Cube_RT_Gross <- (Testing_Partition$Gross)^(1/3)
# #
# # M1_Cube_RT <- lm(Cube_RT_Gross ~ IMDB_Rating + Released_Year + Runtime + budget, data = Training_Partition)
# # summary(M1_Cube_RT)
# #
# #
# # #Decide on a set of metrics with your group that everyone will use when benchmarking the
# # #models for the regression task for both in-sample and out-of-sample performance.
# # #Test against other data
# #
# # # Generate predictions SQRT
# # PRED_1_IN_SQRT <- predict(M1_SQRT, Training_Partition)
# # PRED_1_OUT_SQRT <- predict(M1_SQRT, Testing_Partition)
# #
# # # Clean vectors for RMSE calculation (strip names, handle NA)
# # # This changes "Named Num Class" to "num"
# # PRED_1_IN_vec_SQRT <- unname(PRED_1_IN_SQRT)
# # Gross_IN_vec_SQRT <- unname(Training_Partition$SQRT_Gross)
# # PRED_1_OUT_vec_SQRT <- unname(PRED_1_OUT_SQRT)
# # Gross_OUT_vec_SQRT <- unname(Testing_Partition$SQRT_Gross)
# #
# # #this removes all na cases, we cannot compute NA datapoints
# # valid_idx_in <- complete.cases(PRED_1_IN_vec_SQRT,Gross_IN_vec_SQRT)
# # valid_idx_out <- complete.cases(PRED_1_OUT_vec_SQRT, Gross_OUT_vec_SQRT)
# #
# # # In-sample RMSE for the SQRT data
# # RMSE_1_IN_SQRT <- sqrt(mean((PRED_1_IN_vec_SQRT[valid_idx_in]  - Gross_IN_vec_SQRT[valid_idx_in] )^2))
# # RMSE_1_IN_SQRT
# # #3606.337
# #
# # # Out-of-sample RMSE for the SQRT data
# # RMSE_1_OUT_SQRT <- sqrt(mean((PRED_1_OUT_vec_SQRT[valid_idx_out] - Gross_OUT_vec_SQRT[valid_idx_out])^2))
# # RMSE_1_OUT_SQRT
# # #3613.986
# #
# # #Standard
# # PRED_1_IN <- predict(M1, Training_Partition)
# # PRED_1_OUT <- predict(M1, Testing_Partition)
# #
# # PRED_1_IN_vec <- unname(PRED_1_IN)
# # Gross_IN_vec <- unname(Training_Partition$Gross)
# # PRED_1_OUT_vec <- unname(PRED_1_OUT)
# # Gross_OUT_vec <- unname(Testing_Partition$Gross)
# #
# # RMSE_1_IN <- sqrt(mean((PRED_1_IN_vec[valid_idx_in]  - Gross_IN_vec[valid_idx_in] )^2))
# # RMSE_1_IN
# # #102909686 something is wrong here
# #
# # RMSE_1_OUT <- sqrt(mean((PRED_1_OUT_vec[valid_idx_out] - Gross_OUT_vec[valid_idx_out])^2))
# # RMSE_1_OUT
# # #91350964 something is wrong here


#############################
#IMPLEMENTING REGULARIZATION#
#############################

# #INITIALIZE EMPTY MATRICES FOR STORING PREDICTION AND ERRORS
# PRED_IN <- matrix(NA, nrow = dim(Training_Partition)[1], ncol=length(lambda))
# PRED_OUT <- matrix(NA, nrow = dim(Testing_Partition)[1], ncol=length(lambda))
# E_IN <- matrix(NA, nrow = 1, ncol=length(lambda))
# E_OUT <- matrix(NA, nrow = 1, ncol=length(lambda))
# 
# for (i in 1:length(lambda)){
#   
#   #COMPUTE PSEUDOINVERSE SOLUTION
#   BETA_RIDGE[,i] <- solve(t(X)%*%X+lambda[i]*diag(dim(t(X)%*%X)[1]))%*%t(X)%*%y
#   
#   #COMPUTE PREDICTIONS IN AND OUT-OF-SAMPLE
#   PRED_IN[,i] <- X%*%BETA_RIDGE[,i]
#   PRED_OUT[,i] <- X_holdout%*%BETA_RIDGE[,i]
#   
#   #COMPUTE PREDICTION ERRORS (MSE) IN AND OUT-OF-SAMPLE
#   E_IN[i] <- sqrt(mean((y-PRED_IN[,i])^2))
#   E_OUT[i] <- sqrt(mean((y_holdout-PRED_OUT[,i])^2))
# }
# 
# #STORE ERRORS VS. LAMBDAS IN SEPARATE DATAFRAMES
# df_IN <- data.frame(cbind(Error=as.numeric(E_IN), Lambda=lambda))
# df_OUT <- data.frame(cbind(Error=as.numeric(E_OUT), Lambda=lambda))
# 
# ggplot(df_IN, aes(y=Error, x=Lambda)) +
#   geom_line(color='blue') +
#   geom_line(data=df_OUT, color='red') +
#   ggtitle("E_IN & E_OUT VS. REGULARIZATION PARAMETER (LAMBDA)") +
#   theme(plot.title = element_text(hjust = .5))
# 
# #REPORT MINIMUM E_OUT ESTIMATE FROM BEST REGULARIZED MODEL
# (min(df_OUT$Error))
# 
# #RECOVER OPTIMAL LAMBDA
# (Opt_Lambda <- df_OUT$Lambda[which.min(df_OUT$Error)])
