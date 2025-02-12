

copilot_texts <- list.files("/Users/nicholasrgonzalez/Documents/GenAI-Behavior/corpora/copilot_docs/Manifesto", pattern = "\\.txt$", full.names = TRUE)

View(copilot_texts)

####

# Load necessary libraries
library(quanteda)

## Step 1: Creating copilot_df Corpus

# Read text files and store them in a data frame
copilot_texts <- list.files("/Users/nicholasrgonzalez/Documents/GenAI-Behavior/corpora/copilot_docs/Manifesto", 
                            pattern = "\\.txt$", full.names = TRUE)

copilot_df <- data.frame(
  file = basename(copilot_texts), 
  content = sapply(copilot_texts, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")),  
  stringsAsFactors = FALSE
)

head(copilot_df)  

## Step 2: Cleaning copilot_df

# Rename 'content' column to 'manifestos'
copilot_df$manifestos <- copilot_df$content

# Initialize document variables
copilot_df$race <- NA       
copilot_df$gender <- NA      
copilot_df$income <- NA      
copilot_df$urban <- NA       

# Remove unnecessary columns
copilot_df <- copilot_df[, -c(1, 2)]  

# Assign demographic metadata
race <- c("White", "Black or African American", "Hispanic or Latino", "Asian",  
          "White", "American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander",  
          "Black or African American", "White", "Hispanic or Latino")

gender <- c("Female", "Male", "Female", "Male", "Non-Binary", "Female",  
            "Male", "Non-Binary", "Female", "Male")

urban <- c("Urban", "Urban", "Urban", "Urban", "Rural", "Rural", "Urban", "Rural", "Urban", "Urban")

income <- c("Middle", "Low", "Low", "High", "Middle", "Low", "Middle", "Low", "High", "Middle")

# Add demographic data to copilot_df
copilot_df$race <- race
copilot_df$gender <- gender
copilot_df$income <- income
copilot_df$urban <- urban

## Step 3: Convert Data Frame to Corpus

copilot_df$manifestos <- as.character(copilot_df$manifestos)

copilot_corpus <- corpus(copilot_df, text_field = "manifestos")

# Assign document variables (metadata)
docvars(copilot_corpus) <- copilot_df[, c("race", "gender", "urban", "income")]

## Step 4: Tokenization and Cleaning

# Tokenize text
copilot_tokens <- tokens(copilot_corpus)

# Remove punctuation, digits, and URLs
copilot_tokens_clean <- tokens_remove(copilot_tokens, pattern = "[[:punct:]]|[[:digit:]]|http\\S+")

# Convert tokens to lowercase
copilot_tokens_clean <- tokens_tolower(copilot_tokens_clean)

# Remove stopwords
copilot_tokens_clean <- tokens_remove(copilot_tokens_clean, pattern = stopwords("en"))

## Step 5: Save the Cleaned Corpus

final_copilot_corpus <- copilot_tokens_clean

save(final_copilot_corpus, file = "nick_final_copilot_corpus.RData")