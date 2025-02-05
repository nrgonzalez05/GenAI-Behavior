## creating claude corpus


claude_tf_mani <- list.files("/Users/nicholasrgonzalez/Documents/GenAI-Behavior/corpora/claude_docs/claude_manifestos", pattern = "\\.txt$", full.names = TRUE)

claude_mani_df <- data.frame(
  file = basename(claude_tf_mani),
  content = sapply(claude_tf_mani, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")), 
  stringsAsFactors = FALSE
)

head(claude_mani_df) 

## now cleaning df

claude_mani_df$manifestos <- claude_mani_df$content

claude_mani_df$race <- NA       
claude_mani_df$gender <- NA      
claude_mani_df$income <- NA      
claude_mani_df$urban <- NA       

claude_mani_df <- claude_mani_df[, -c(1, 2)]  

race <- c("White", "Black or African American", "Hispanic or Latino", "Asian", 
          "White", "American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander", 
          "Black or African American", "White", "Hispanic or Latino")

gender <- c("Female", "Male", "Female", "Male", "Non-Binary", "Female", 
            "Male", "Non-Binary", "Female", "Male")

urban <- c("Urban", "Urban", "Urban", "Urban", "Rural", "Rural", "Urban", "Rural", "Urban", "Urban")

income <- c("Middle", "Low", "Low", "High", "Middle", "Low", "Middle", "Low", "High", "Middle")


claude_mani_df$race <- race
claude_mani_df$gender <- gender
claude_mani_df$income <- income
claude_mani_df$urban <- urban

library(quanteda)

claude_mani_df$manifestos <- as.character(claude_mani_df$manifestos)

claude_mani_corpus <- corpus(claude_mani_df$manifestos)

docvars(claude_mani_corpus) <- claude_mani_df[, c("race", "gender", "urban", "income")]


claude_mani_tokens <- tokens(claude_mani_corpus)


claude_mani_tokens_clean <- tokens_remove(claude_mani_tokens, pattern = "[[:punct:]]|[[:digit:]]|http\\S+")

# Convert tokens to lowercase
claude_mani_tokens_clean <- tokens_tolower(claude_mani_tokens_clean)

claude_mani_tokens_clean <- tokens_remove(claude_mani_tokens_clean, pattern = stopwords("en"))

final_claude_corpus <- claude_mani_tokens_clean

save(final_claude_corpus, file = "final_claude_corpus.RData")

