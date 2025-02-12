gpt_texts <- list.files("/Users/nicholasrgonzalez/Documents/GenAI-Behavior/corpora/gpt_docs/Manifesto", pattern = "\\.txt$", full.names = TRUE)



# Load necessary libraries
library(quanteda)

## Step 1: Creating gpt_df Corpus

# Read text files and store them in a data frame
gpt_df <- data.frame(
  file = basename(gpt_texts), 
  content = sapply(gpt_texts, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")),  
  stringsAsFactors = FALSE
)

head(gpt_df)  

## Step 2: Cleaning gpt_df

# Rename 'content' column to 'manifestos'
gpt_df$manifestos <- gpt_df$content

# Initialize document variables
gpt_df$race <- NA       
gpt_df$gender <- NA      
gpt_df$income <- NA      
gpt_df$urban <- NA       

# Remove unnecessary columns
gpt_df <- gpt_df[, -c(1, 2)]  

# Assign demographic metadata
race <- c("White", "Black or African American", "Hispanic or Latino", "Asian",  
          "White", "American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander",  
          "Black or African American", "White", "Hispanic or Latino")

gender <- c("Female", "Male", "Female", "Male", "Non-Binary", "Female",  
            "Male", "Non-Binary", "Female", "Male")

urban <- c("Urban", "Urban", "Urban", "Urban", "Rural", "Rural", "Urban", "Rural", "Urban", "Urban")

income <- c("Middle", "Low", "Low", "High", "Middle", "Low", "Middle", "Low", "High", "Middle")

# Add demographic data to gpt_df
gpt_df$race <- race
gpt_df$gender <- gender
gpt_df$income <- income
gpt_df$urban <- urban

## Step 3: Convert Data Frame to Corpus

gpt_df$manifestos <- as.character(gpt_df$manifestos)

gpt_corpus <- corpus(gpt_df$manifestos)

# Assign document variables (metadata)
docvars(gpt_corpus) <- gpt_df[, c("race", "gender", "urban", "income")]

## Step 4: Tokenization and Cleaning

# Tokenize text
gpt_tokens <- tokens(gpt_corpus)

# Remove punctuation, digits, and URLs
gpt_tokens_clean <- tokens_remove(gpt_tokens, pattern = "[[:punct:]]|[[:digit:]]|http\\S+")

# Convert tokens to lowercase
gpt_tokens_clean <- tokens_tolower(gpt_tokens_clean)

# Remove stopwords
gpt_tokens_clean <- tokens_remove(gpt_tokens_clean, pattern = stopwords("en"))

## Step 5: Save the Cleaned Corpus

final_gpt_corpus <- gpt_tokens_clean

save(final_gpt_corpus, file = "nick_final_gpt_corpus.RData")