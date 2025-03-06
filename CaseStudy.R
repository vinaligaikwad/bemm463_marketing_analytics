# Load necessary libraries
library(readxl)      # Read Excel files
library(dplyr)       # Data manipulation
library(ggplot2)     # Visualization
library(cluster)     # Clustering algorithms
library(factoextra)  # Cluster visualization

# Load dataset
file_path <- "txt.xlsx"
df <- read_excel(file_path)

# Select relevant features for clustering
cluster_data <- df %>%
  select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style)

# Standardize the data
cluster_data_scaled <- scale(cluster_data)

# 1️⃣ Elbow Method to confirm 4 clusters
png("elbow_method.png", width = 800, height = 600)
p <- fviz_nbclust(cluster_data_scaled, kmeans, method = "wss") +
  ggtitle("🔹 Elbow Method for Optimal Clusters") +
  theme_minimal(base_size = 14) +  # Clean theme with larger font
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "darkblue"),  # Center title
        axis.title = element_text(face = "bold", size = 14),  # Bold axes labels
        axis.text = element_text(size = 12),  
        plot.background = element_rect(fill = "white", color = NA)) +  # Clean background
  geom_vline(xintercept = 4, linetype = "dashed", color = "red", linewidth = 1.2) +  # Vertical dashed reference line
  geom_point(aes(x = 4, y = fviz_nbclust(cluster_data_scaled, kmeans, method = "wss")$data$y[4]), 
             color = "blue", size = 5, shape = 21, fill = "yellow", stroke = 1.5)  # Highlight elbow point
print(p)  
dev.off()

set.seed(123)
kmeans_result <- kmeans(cluster_data_scaled, centers = 4, nstart = 25)

# Compute silhouette scores
silhouette_score <- silhouette(kmeans_result$cluster, dist(cluster_data_scaled))

# Generate and save the silhouette plot
png("silhouette_plot_k4.png", width = 800, height = 600)
fviz_silhouette(silhouette_score) +
  ggtitle("🔹 Silhouette Plot for K = 4") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "darkblue"),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12))
dev.off()

# 3️⃣ Perform K-Means Clustering with **exactly 4 clusters**
set.seed(123)
kmeans_result <- kmeans(cluster_data_scaled, centers = 4, nstart = 25)

# Assign cluster labels
df$Segment <- as.factor(kmeans_result$cluster)

# 4️⃣ Visualize K-Means Clustering
png("kmeans_clustering.png", width = 800, height = 600)
fviz_cluster(kmeans_result, data = cluster_data_scaled, 
             geom = "point", ellipse.type = "convex", 
             palette = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00")) + 
  ggtitle("🔹 K-Means Cluster Scatter Plot (K = 4)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "darkblue"),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12))
dev.off()

# 5️⃣ Hierarchical Clustering & Dendrogram
dist_matrix <- dist(cluster_data_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")

png("dendrogram_4_clusters.png", width = 800, height = 600)
plot(hc, main = "🔹 Dendrogram for 4 Clusters", 
     xlab = "Observations", ylab = "Height",
     cex.main = 1.5, col.main = "darkblue")  # Bold and centered title

# Highlight cluster borders with distinct colors (but no coloring inside)
rect.hclust(hc, k = 4, border = c("red", "blue", "green", "purple"))

dev.off()

# Assign hierarchical cluster labels
df$Hierarchical_Segment <- cutree(hc, k = 4)

# Analyze segment characteristics
segment_summary <- df %>%
  group_by(Segment) %>%
  summarise(across(c(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style, Age, Income), mean, na.rm = TRUE))

# Print segment summary
print(segment_summary)

