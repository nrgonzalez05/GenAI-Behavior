load("~/Documents/GenAI-Behavior/nick_final_gpt_corpus.RData")
load("~/Documents/GenAI-Behavior/nick_final_copilot_corpus.RData")
load("~/Documents/GenAI-Behavior/final_gemini_corpus.RData")
load("~/Documents/GenAI-Behavior/final_claude_corpus.RData")

# Load necessary library
library(quanteda)

# Function to rename document names sequentially
rename_docs <- function(corpus, start_index) {
  num_docs <- ndoc(corpus)  # Get number of documents
  new_docnames <- paste0("doc", seq(from = start_index, length.out = num_docs))
  docnames(corpus) <- new_docnames
  return(corpus)
}

# Rename documents for each corpus
final_gpt_corpus <- rename_docs(final_gpt_corpus, 1)          # doc1 - doc10
final_copilot_corpus <- rename_docs(final_copilot_corpus, 11) # doc11 - doc20
final_gemini_corpus <- rename_docs(final_gemini_corpus, 21)   # doc21 - doc30
final_claude_corpus <- rename_docs(final_claude_corpus, 31)   # doc31 - doc40

# Combine all renamed corpora
combined_corpus <- final_gpt_corpus + final_copilot_corpus + final_gemini_corpus + final_claude_corpus

# Verify document names are unique
docnames(combined_corpus)

save(combined_corpus, file = "final_combined.rdata")


# Create Document-Feature Matrix (DFM)
combined_dfm <- dfm(combined_corpus)

# Remove sparse terms
combined_dfm_trimmed <- dfm_trim(combined_dfm, min_termfreq = 30)

# Apply TF-IDF
combined_dfm_tfidf <- dfm_tfidf(combined_dfm_trimmed)

# Remove zero-variance features
non_zero_variance_dfm <- combined_dfm_tfidf[, apply(combined_dfm_tfidf, 2, var) != 0]

# Convert to matrix and scale
dfm_matrix_scaled <- scale(as.matrix(non_zero_variance_dfm))

# Run PCA
pca_result <- prcomp(dfm_matrix_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

biplot(pca_result, scale = 0)

library(ggplot2)

# Extract PCA scores and loadings
pca_scores <- as.data.frame(pca_result$x)
pca_scores$Document <- rownames(pca_scores)

pca_loadings <- as.data.frame(pca_result$rotation)
pca_loadings$Feature <- rownames(pca_loadings)

# Scale loadings for better visualization
scaling_factor <- max(abs(pca_scores$PC1)) / max(abs(pca_loadings$PC1)) * 0.5
pca_loadings[, 1:2] <- pca_loadings[, 1:2] * scaling_factor

# Create PCA Biplot
ggplot() +
  geom_point(data = pca_scores, aes(x = PC1, y = PC2), color = "blue", size = 3) +
  geom_text(data = pca_scores, aes(x = PC1, y = PC2, label = Document), vjust = 1.5, size = 3) +
  geom_segment(data = pca_loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.2, "inches")), color = "red") +
  geom_text(data = pca_loadings, aes(x = PC1, y = PC2, label = Feature), vjust = 1, hjust = 1, size = 3, color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  ggtitle("PCA Biplot with Loadings") +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  theme_minimal()