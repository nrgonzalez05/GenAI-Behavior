## gemini pca

load("~/Documents/GenAI-Behavior/final_gemini_corpus.RData")

# Load the necessary library
library(quanteda)


# Assuming you already have a cleaned corpus 'final_gemini_corpus'
# Step 1: Convert the corpus to a Document-Feature Matrix (DFM)
gemini_mani_dfm <- dfm(final_gemini_corpus)

# Step 2: (Optional) Remove sparse terms (terms that appear in very few documents)
gemini_mani_dfm_trimmed <- dfm_trim(gemini_mani_dfm, min_termfreq = 5)

# Step 3: (Optional) Apply TF-IDF (Term Frequency-Inverse Document Frequency) to weight terms
gemini_mani_dfm_tfidf <- dfm_tfidf(gemini_mani_dfm_trimmed)

# Step 4: Remove zero-variance features (columns with constant values across all documents)
# Calculate variance for each feature (column) in the DFM
variance_values <- apply(gemini_mani_dfm_tfidf, 2, var)

# Remove features (columns) that have zero variance
non_zero_variance_dfm <- gemini_mani_dfm_tfidf[, variance_values != 0]

# Step 5: Convert the DFM to a matrix and scale it
dfm_matrix_scaled <- scale(as.matrix(non_zero_variance_dfm))

# Step 6: Run PCA on the scaled DFM
pca_result_gemini <- prcomp(dfm_matrix_scaled, center = TRUE, scale. = TRUE)

# Step 7: Summary of PCA results (variance explained)
summary(pca_result_gemini)

# Step 8: View the loadings (which words contribute to each principal component)
pca_result_gemini$rotation

# Step 9: View the principal component scores (for each document)
pca_result_gemini$x

# Step 10: Plot the PCA results with quadrants

# Biplot of the first two principal components
biplot(pca_result_gemini, scale = 0)

# Get the first and second principal component scores (for documents)
pc1_scores <- pca_result_gemini$x[, 1]
pc2_scores <- pca_result_gemini$x[, 2]

# Calculate the means of the first and second principal components
pc1_mean <- mean(pc1_scores)
pc2_mean <- mean(pc2_scores)

# Add lines to draw quadrants at the mean of the first and second components
abline(h = pc2_mean, v = pc1_mean, col = "red", lty = 2)  # Horizontal and vertical lines

# Optionally, add labels to the quadrants
text(pc1_mean, pc2_mean, labels = "Center", pos = 4, col = "red", cex = 0.8)

# Add additional lines if you want more prominent quadrants (optional)
abline(h = 0, v = 0, col = "blue", lty = 2)  # Lines through zero (the origin)
