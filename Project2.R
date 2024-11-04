#1. preprocess the data

# Load the data sets

# Load libraries
library(dplyr)
library(readr)

# Load the datasets
#vaalikone_questions <- read.csv("C:/Users/sever/OneDrive - University of Eastern Finland/TKT/6. vuosi/DNK/Projects/2/vaalikone_questions_all.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
vaalikone_questions <- read.csv("C:/Users/severi.moisio/OneDrive - University of Eastern Finland/TKT/6. vuosi/DNK/Projects/2/vaalikone_questions_all.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
#vaalikone_profiles <- read.csv("C:/Users/sever/OneDrive - University of Eastern Finland/TKT/6. vuosi/DNK/Projects/2/vaalikone_profiles_all.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
vaalikone_profiles <- read.csv("C:/Users/severi.moisio/OneDrive - University of Eastern Finland/TKT/6. vuosi/DNK/Projects/2/vaalikone_profiles_all.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)


# Check the structure of the data
head(vaalikone_questions)
str(vaalikone_questions)
summary(vaalikone_questions)

#OUtliers handling

library(ggplot2)

# Create boxplots for count columns
vaalikone_counts <- vaalikone_questions %>%
  select(starts_with("Count"))  # Selecting only columns related to counts

# Check for outliers in each count column
boxplot(vaalikone_counts, main = "Boxplots for Count Columns", las = 2, col = "skyblue")


# Define a function to detect outliers based on IQR
detect_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- column[column < lower_bound | column > upper_bound]
  return(outliers)
}

# Apply the function to each count column
outliers_list <- lapply(vaalikone_counts, detect_outliers)

# Print outliers for each count column
outliers_list

# Check unique values in each Q column to see if there are uncommon categories
lapply(vaalikone_questions %>% select(starts_with("Q")), unique)


# Define a function to detect and remove outliers using IQR
remove_outliers <- function(data, column_name) {
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  # Keep rows within bounds
  data <- data %>%
    filter(data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound)
  return(data)
}

# Apply the function to each count column
for (column in colnames(vaalikone_counts)) {
  vaalikone_questions <- remove_outliers(vaalikone_questions, column)
}

# Check the resulting dataset
summary(vaalikone_questions)

#Check dataset again
# Create boxplots for count columns
vaalikone_counts <- vaalikone_questions %>%
  select(starts_with("Count"))  # Selecting only columns related to counts

boxplot(vaalikone_counts, main = "Boxplots for Count Columns", las = 2, col = "skyblue")

# Handle missing values
library(tidyr)

colSums(is.na(vaalikone_questions))
vaalikone_questions <- vaalikone_questions %>% drop_na()



library(dplyr)

# Convert "Q" columns to factor and "W" columns to ordered factor
vaalikone_questions <- vaalikone_questions %>%
  mutate(
    across(starts_with("Q"), as.factor),
    across(starts_with("W"), ~ factor(.x, ordered = TRUE, levels = c("low", "medium", "high")))
  )

# Verify conversions
str(vaalikone_questions)

sapply(vaalikone_questions, class)

# Convert character columns to factors
vaalikone_questions <- vaalikone_questions %>%
  mutate(across(where(is.character), as.factor))

#2. Clustering
install.packages("Rtsne")
library(cluster)
library(Rtsne)

# Calculate Gower distance. Use Gower distance due to the mixed data types.
gower_dist <- daisy(vaalikone_questions, metric = "gower")

# Hierarchical clustering. Hierarchical clustering with Ward’s method is chosen for its adaptability to Gower distance and ease of visualization.
clustering <- hclust(gower_dist, method = "ward.D2")

#Visualize

num_clusters <- 4
clusters <- cutree(clustering, k = num_clusters)

# Convert Gower distance matrix to a format suitable for t-SNE
gower_dist_matrix <- as.matrix(gower_dist)
tsne_result <- Rtsne(gower_dist_matrix, is_distance = TRUE)

# Create a data frame with t-SNE results and cluster assignments
tsne_data <- data.frame(
  X = tsne_result$Y[, 1],
  Y = tsne_result$Y[, 2],
  cluster = factor(clusters)  # Convert clusters to a factor for coloring
)

# Visualize the clusters with geom_point
ggplot(tsne_data, aes(x = X, y = Y, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "t-SNE Plot of Hierarchical Clustering", x = "t-SNE Dimension 1", y = "t-SNE Dimension 2") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster")


# Merge clusters with profiles
vaalikone_questions <- vaalikone_questions %>% mutate(cluster = clusters)
merged_data <- merge(vaalikone_profiles, vaalikone_questions, by = "ID")

length(merged_data$cluster)
length(merged_data$Party)

# Check alignment with political parties
table(merged_data$cluster, merged_data$Party)


#Visualize political parties in the clusters
ggplot(merged_data, aes(x = cluster, fill = Party)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Political Parties Across Clusters", x = "Cluster", y = "Count") +
  theme_minimal()

#Age and education levell differences in the clusters
demographic_summary <- merged_data %>%
  group_by(cluster) %>%
  summarise(across(c(Age, EduLvl), list(mean = mean, sd = sd), na.rm = TRUE))
print(demographic_summary)

colnames(merged_data)
