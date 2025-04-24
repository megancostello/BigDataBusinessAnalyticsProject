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



