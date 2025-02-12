# Load necessary libraries
library(quanteda)
library(ggplot2)

# Step 1: Convert the corpus to a Document-Feature Matrix (DFM)
gpt_dfm <- dfm(final_gpt_corpus)

# Step 2: (Optional) Remove sparse terms (terms that appear in very few documents)
gpt_dfm_trimmed <- dfm_trim(gpt_dfm, min_termfreq = 1)

# Step 3: (Optional) Apply TF-IDF (Term Frequency-Inverse Document Frequency) to weight terms
gpt_dfm_tfidf <- dfm_tfidf(gpt_dfm_trimmed)

# Step 4: Remove zero-variance features (columns with constant values across all documents)
# Identify features with zero variance
non_zero_variance_dfm <- gpt_dfm_tfidf[, apply(gpt_dfm_tfidf, 2, var) != 0]

# Step 5: Convert the DFM to a matrix and scale it
dfm_matrix_scaled <- scale(as.matrix(non_zero_variance_dfm))

# Step 6: Run PCA on the scaled DFM
pca_result <- prcomp(dfm_matrix_scaled, center = TRUE, scale. = TRUE)

# Step 7: Summary of PCA results (variance explained)
summary(pca_result)

# Step 8: View the loadings (which words contribute to each principal component)
pca_result$rotation

# Step 9: View the principal component scores (for each document)
pca_result$x

# Step 10: Plot the PCA results
# Biplot of the first two principal components
biplot(pca_result, scale = 0)

# Step 11: Extract PCA Scores and Loadings

# Get the first and second principal component scores (for documents)
pc1_scores <- pca_result$x[, 1]
pc2_scores <- pca_result$x[, 2]

# Calculate the means of the first and second principal components
pc1_mean <- mean(pc1_scores)
pc2_mean <- mean(pc2_scores)

# Add lines to draw quadrants at the mean of the first and second components
abline(h = pc2_mean, v = pc1_mean, col = "red", lty = 2)  # Horizontal and vertical lines

# Optionally, add labels to the quadrants
text(pc1_mean, pc2_mean, labels = "Center", pos = 4, col = "red", cex = 0.8)

# Add additional lines if you want more prominent quadrants (optional)
abline(h = 0, v = 0, col = "blue", lty = 2)  # Lines through zero (the origin)

# Step 12: Extract Loadings (word contributions to PCs)
pca_loadings <- as.data.frame(pca_result$rotation)

# Step 13: Plot the Loadings for PC1 and PC2
ggplot(pca_loadings, aes(x = PC1, y = PC2, label = rownames(pca_loadings))) +
  geom_point(color = "blue") +        # Plot points for each word
  geom_text(vjust = 1, hjust = 1, size = 3) +   # Label each point
  ggtitle("PCA Loading Plot: PC1 vs PC2") +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  theme_minimal()

# Step 14: Create a PCA Biplot with Loadings

# Extract PCA scores (document positions in PC space)
pca_scores <- as.data.frame(pca_result$x)
pca_scores$Document <- rownames(pca_scores)  # Label documents

# Extract PCA loadings (word contributions to PCs)
pca_loadings <- as.data.frame(pca_result$rotation)
pca_loadings$Feature <- rownames(pca_loadings)  # Label words

# Scale loadings to match the PCA scores for better visualization
scaling_factor <- max(abs(pca_scores$PC1)) / max(abs(pca_loadings$PC1)) * 0.5
pca_loadings[, 1:2] <- pca_loadings[, 1:2] * scaling_factor  # Scale PC1 and PC2 for words

# Create the Biplot
ggplot() +
  # Plot Document Scores (each point represents a document)
  geom_point(data = pca_scores, aes(x = PC1, y = PC2), color = "blue", size = 3) +
  geom_text(data = pca_scores, aes(x = PC1, y = PC2, label = Document), vjust = 1.5, size = 3) +
  
  # Plot Feature Loadings (each arrow represents a word contribution)
  geom_segment(data = pca_loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.2, "inches")), color = "red") +
  geom_text(data = pca_loadings, aes(x = PC1, y = PC2, label = Feature), vjust = 1, hjust = 1, size = 3, color = "red") +
  
  # Add Quadrants
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  
  # Labels and Theme
  ggtitle("PCA Biplot with Loadings") +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  theme_minimal()