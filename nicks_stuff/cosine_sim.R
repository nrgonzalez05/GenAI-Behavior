##cosine sim

load("~/Documents/GenAI-Behavior/final_combined.rdata")

# Load necessary libraries
library(quanteda)
library(lsa)
library(ggplot2)
library(SnowballC)

# Step 1: Create Document-Feature Matrix (DFM) from the combined corpus
combined_dfm <- dfm(combined_corpus)

# Step 2: Apply TF-IDF to the DFM (documents as rows, terms as columns)
combined_dfm_tfidf <- dfm_tfidf(combined_dfm)

# Step 3: Convert the DFM to a matrix (representing document vectors)
text_matrix <- as.matrix(combined_dfm_tfidf)

# Step 4: Get document names (assuming the first column in the DFM is document names)
doc_names <- docnames(combined_corpus)  # Extract document names (1-40)

# Step 5: Compute the cosine similarity between documents (whole document vectors)
# Calculate similarity between rows (documents) instead of columns (terms)
cosine_sim_matrix <- cosine(text_matrix)  # This will calculate similarity between document vectors

# Step 6: Convert cosine similarity matrix into a data frame for ggplot
cosine_sim_df <- as.data.frame(as.table(cosine_sim_matrix))

# Rename columns for clarity
colnames(cosine_sim_df) <- c("Document1", "Document2", "CosineSimilarity")

# Step 7: Filter out self-similarity (cosine similarity of 1 with itself) to improve plot readability
cosine_sim_df <- cosine_sim_df[cosine_sim_df$Document1 != cosine_sim_df$Document2, ]

# Step 8: Plot the cosine similarity matrix as a heatmap
ggplot(cosine_sim_df, aes(Document1, Document2, fill = CosineSimilarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(x = "Document", y = "Document", fill = "Cosine Similarity") +
  ggtitle("Cosine Similarity Between Documents 1-40")



# Load necessary libraries
library(quanteda)
library(lsa)
library(pheatmap)

# Step 1: Create Document-Feature Matrix (DFM) from the combined corpus
combined_dfm <- dfm(combined_corpus)

# Step 2: Apply TF-IDF to the DFM (documents as rows, terms as columns)
combined_dfm_tfidf <- dfm_tfidf(combined_dfm)

# Step 3: Convert the DFM to a matrix (representing document vectors)
text_matrix <- as.matrix(combined_dfm_tfidf)

# Step 4: Get document names (assuming the first column in the DFM is document names)
doc_names <- docnames(combined_corpus)  # Extract document names (1-40)

# Step 5: Compute the cosine similarity between documents (whole document vectors)
cosine_sim_matrix <- cosine(text_matrix)  # This will calculate similarity between document vectors

# Step 6: Plot the cosine similarity matrix as a heatmap using pheatmap
pheatmap(cosine_sim_matrix, 
         cluster_rows = TRUE,  # Optional: cluster documents by similarity
         cluster_cols = TRUE,  # Optional: cluster documents by similarity
         labels_row = doc_names,  # Use document names as row labels
         labels_col = doc_names,  # Use document names as column labels
         main = "Cosine Similarity Heatmap Between Documents",
         fontsize_row = 3,
         fontsize_col = 4,           # Reduce font size for document labels
         # Control plot size to fit everything
)

# Load necessary libraries
library(quanteda)
library(lsa)
library(ggplot2)
library(Rtsne)

# Step 1: Create Document-Feature Matrix (DFM) from the combined corpus
combined_dfm <- dfm(combined_corpus)

# Step 2: Apply TF-IDF to the DFM (documents as rows, terms as columns)
combined_dfm_tfidf <- dfm_tfidf(combined_dfm)

# Step 3: Convert the DFM to a matrix (representing document vectors)
text_matrix <- as.matrix(combined_dfm_tfidf)

# Step 4: Compute the cosine similarity between documents (whole document vectors)
cosine_sim_matrix <- cosine(text_matrix)  # This calculates similarity between document vectors

# Step 5: Perform t-SNE for dimensionality reduction
tsne_model <- Rtsne(cosine_sim_matrix, dims = 2, pca = T, check_duplicates = F)

# Step 6: Create a data frame with t-SNE results
tsne_df <- data.frame(
  X = tsne_model$Y[, 1],  # First dimension (X axis)
  Y = tsne_model$Y[, 2],  # Second dimension (Y axis)
  Document = docnames(combined_corpus)  # Document labels
)

# Step 7: Plot the documents based on t-SNE results
ggplot(tsne_df, aes(x = X, y = Y, label = Document)) +
  geom_point(color = "blue", size = 3) +  # Plot points for each document
  geom_text(aes(label = Document), vjust = 1, hjust = 1, size = 3) +  # Label each point
  ggtitle("t-SNE Plot of Document Similarity") +
  xlab("t-SNE Dimension 1") +
  ylab("t-SNE Dimension 2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate labels if needed