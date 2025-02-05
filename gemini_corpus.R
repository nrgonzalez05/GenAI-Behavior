## creating claude corpus


gemini_mani_tf <- list.files("/Users/nicholasrgonzalez/Documents/GenAI-Behavior/corpora/gemini_docs/gemini_manifestos", pattern = "\\.txt$", full.names = TRUE)

gemini_df <- data.frame(
  file = basename(gemini_mani_tf),
  content = sapply(gemini_mani_tf, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")), 
  stringsAsFactors = FALSE
)

head(gemini_df) 

## now cleaning df

gemini_df$manifestos <- gemini_df$content

gemini_df$race <- NA       
gemini_df$gender <- NA      
gemini_df$income <- NA      
gemini_df$urban <- NA       


race <- c("White", "Black or African American", "Hispanic or Latino", "Asian", 
          "White", "American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander", 
          "Black or African American", "White", "Hispanic or Latino")

gender <- c("Female", "Male", "Female", "Male", "Non-Binary", "Female", 
            "Male", "Non-Binary", "Female", "Male")

urban <- c("Urban", "Urban", "Urban", "Urban", "Rural", "Rural", "Urban", "Rural", "Urban", "Urban")

income <- c("Middle", "Low", "Low", "High", "Middle", "Low", "Middle", "Low", "High", "Middle")


gemini_df$race <- race
gemini_df$gender <- gender
gemini_df$income <- income
gemini_df$urban <- urban

library(quanteda)

gemini_df$manifestos <- as.character(gemini_df$manifestos)


gemini_mani_corpus <- corpus(gemini_df$manifestos)

docvars(gemini_mani_corpus) <- gemini_df[, c("race", "gender", "urban", "income")]

gemini_tokens <- tokens(gemini_mani_corpus)

gemini_tokens <- tokens_remove(gemini_tokens, pattern = "[[:punct:]]|[[:digit:]]|http\\S+")

gemini_tokens_clean <- tokens_tolower(gemini_tokens)

gemini_clean <- tokens_remove(gemini_tokens_clean, pattern = stopwords("en"))

final_gemini_corpus <- gemini_clean

save(final_gemini_corpus, file = "final_gemini_corpus.RData")

