######################## SCRIPT FOR EV EXPLORATORY ANALYSIS ####################

# ==============================================================================
#                              1. Data Loading
# ==============================================================================

# Load pachages
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(ggrepel)
library(naniar)
library(cluster)
library(missRanger)
library(dbscan)
library(mclust)
library(RColorBrewer)
library(rnaturalearth)
library(sf)
library(NbClust)
library(factoextra)


# Read Excel file
data <- read_excel("ev_dataset.xlsx")

# Quick look
glimpse(data)

# ==============================================================================
#                     2. Data Processing and Exploration
# ==============================================================================

# ==============================================================================
# 2.1 Basic manipulation
# ==============================================================================

# Transform certain variables to per 100.000 inhabitants
data_per100 <- data %>%
  mutate(
    BEV = BEV / (Population / 100000),
    PHEV = PHEV / (Population / 100000),
    H2 = H2 / (Population / 100000),
    LPG = LPG / (Population / 100000),
    CNG = CNG / (Population / 100000),
    Slow_AC = Slow_AC / (Population / 100000),
    Medium_speed_AC = Medium_speed_AC / (Population / 100000),
    Fast_AC = Fast_AC / (Population / 100000),
    Slow_DC = Slow_DC / (Population / 100000),
    Fast_DC = Fast_DC / (Population / 100000),
    Ultra_DC_I = Ultra_DC_I / (Population / 100000),
    Ultra_DC_II = Ultra_DC_II / (Population / 100000),
  )

# Drop European Union row
data_per100 <- data_per100 %>%
  filter(Country != "EU")

# Count missing values in each variable
colSums(is.na(data_per100))

# Transform yes/no variables to 1/0
data_per100 <- data_per100 %>%
  mutate(across(
    where(~ all(na.omit(.x) %in% c("Yes", "No", "yes", "no"))),
    ~ ifelse(tolower(.x) == "yes", 1, 0)
  ))

# ==============================================================================
# 2.2 Check correlations
# ==============================================================================

# =================== Correlation matrix for numeric variables =================
# (repeat the same code changing year and variables)

# Choose year
data_correl <- data_per100 %>%
  filter(Year == 2024)

# Select numeric columns only
data_correl <- data_correl %>%
  select(where(is.numeric))

# Compute correlations
cor_matrix <- cor(data_correl, use = "pairwise.complete.obs")

# Print the correlations of BEV with all others
cor_matrix["BEV", ] %>% sort(decreasing = TRUE)

# Turn BEV correlations into a dataframe
bev_cor_df <- tibble(
  Variable = names(cor_matrix["BEV", ]),
  Correlation = cor_matrix["BEV", ]
) %>%
  arrange(desc(Correlation))

# Plot
ggplot(bev_cor_df, aes(x = reorder(Variable, Correlation), 
                       y = 1, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  coord_flip() +  # makes it horizontal
  theme_minimal() +
  labs(
    title = "Correlation of BEV with Other Variables (2020)",
    x = NULL, y = NULL,
    fill = "Correlation"
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )

# ======================= Scatter plots for numeric variables ==================
data_per100 %>%
  filter(Year == 2024) %>%
  ggplot(aes(x = Towns_suburbs, y = BEV, label = Country)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(vjust = -0.5, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "BEV per 100k vs Townd and Suburbs % in total land area (2024)",
       x = "Towns and Suburbs (%)",
       y = "BEV per 100k")

# =========================== Factor variables =================================
policy_vars <- c("Acquisition", "Purchase", "Company_cars", "Ownership", "Infrastructure")

data_per100 <- data_per100 %>%
  mutate(Policy_Score = rowSums(across(all_of(policy_vars)), na.rm = TRUE))

data_per100 %>%
  filter(Year == 2024) %>%
  ggplot(aes(x = reorder(Country, Policy_Score), y = Policy_Score)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "EV Policy Intensity by Country (2020)",
       x = "Country", y = "Policy Score (0–5)")

data_per100 %>%
  ggplot(aes(x = factor(Year), y = reorder(Country, Policy_Score), fill = Policy_Score)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue") +
  theme_minimal() +
  labs(title = "EV Policy Intensity by Country and Year",
       x = "Year", y = "Country", fill = "Score (0–5)")

# Scatter for BEV and factor variables
# Scatter one year
data_per100 %>%
  filter(Year == 2024) %>%
  ggplot(aes(x = Policy_Score, y = BEV)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "BEV Adoption by Policy Score (2024)",
    x = "Policy Score",
    y = "BEV per 100k"
  )

# Scatter all years
data_per100 %>%
  ggplot(aes(x = Policy_Score, y = BEV)) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "BEV and Policy Score (2020–2024)",
    x = "Policy Score",
    y = "BEV per 100k",
    color = "Year"
  )

# Boxplot for BEV and Policy_score
data_per100 %>%
  filter(Year == 2024) %>%
  mutate(Policy_Score = factor(Policy_Score)) %>%
  ggplot(aes(x = Policy_Score, y = BEV, fill = Policy_Score)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "BEV and Policy Score (2024)",
    x = "Policy Score (0–5)",
    y = "BEV per 100k"
  )

# ========== Drop variables that had minor correlation with BEV and PHEV =======
data_per100 <- data_per100[, !names(data_per100) %in% c("H2", "CNG", "LPG", "Acquisition", "Ownership", "Company_cars", "Purchase", "Infrastructure", "Slow_AC", "Fast_AC", "Slow_DC", "Fast_DC")]

# ==============================================================================
# 2.3 Pivot to wide format
# ==============================================================================

# Create static variables dataset
static_vars <- data_per100 %>%
  group_by(Country) %>%
  summarise(
    Cities = first(Cities),
    Towns_suburbs = first(Towns_suburbs),
    Rural = first(Rural),
    Target_2018 = first(Target_2018),
    Target_2023 = first(Target_2023),
    Political_Stability = first(Political_Stability),
    .groups = "drop")

# Create dynamic variables dataset
data_wide <- data_per100 %>%
  pivot_wider(
    id_cols = Country,
    names_from = Year,
    values_from = c(BEV, PHEV, Medium_speed_AC, Ultra_DC_I,	Ultra_DC_II, GDPpc, Electricity, Diesel, Gasoline, Policy_Score))

# Remove columns that are completely NA
data_wide <- data_wide %>%
  select(where(~ !all(is.na(.))))

# Merge the two datasets
final_data <- data_wide %>%
  left_join(static_vars, by = "Country")

rm(data_wide)
rm(static_vars)


# Convert ONLY true Yes/No character columns to factors in final_data
final_data <- final_data %>%
  mutate(across(where(~ is.numeric(.x) && all(na.omit(.x) %in% c(0, 1))),
                ~ as.factor(.x)))

sapply(final_data, class)

final_data$NA_count <- rowSums(is.na(final_data))

final_data %>%
  arrange(desc(NA_count)) %>%
  select(Country, NA_count) %>%
  print(n = Inf)

final_data <- final_data %>%
  select(-c(NA_count))

# ==============================================================================
# 2.4 Impute NAs
# ==============================================================================

# Build matrix to impute
X <- final_data %>%
  select(-Country) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))

# Prune truly unusable columns
X <- X %>%
  select(where(~ sum(!is.na(.x)) >= 3)) %>%  
  select(where(~ dplyr::n_distinct(na.omit(.x)) >= 2))

# impute
set.seed(123)
X_imp <- missRanger(
  X,
  pmm.k = 3, # predictive mean matching for numeric cols (reduces bias)
  num.trees = 500,
  verbose = TRUE,
  seed = 123
)

# Write imputed numerics back into final_data
final_data[, colnames(X_imp)] <- X_imp

# Drop variables
final_data <- final_data[, !names(final_data) %in% c("Population", "Land_area", "Highway", "Cities", "Towns_suburbs", "Rural", "Target_2018", "Target_2023","Infrastructure_2020", "Infrastructure_2021", "Infrastructure_2022")]

# ==============================================================================
#                                  3. Clustering
# ==============================================================================

final_data <- final_data %>%
  mutate(across(where(is.factor), ~ as.numeric(as.character(.x))))

# Exclude label
data_for_clustering <- final_data %>%
  select(-Country)

# Scaling
data_for_clustering <- scale(data_for_clustering)

# Gower distance
gower_dist <- daisy(data_for_clustering, metric = "gower")

# ==============================================================================
# 3.1 Hierarchical clustering
# ==============================================================================
hclust_gower <- hclust(as.dist(gower_dist), method = "ward.D2")

# Dendrogram
plot(hclust_gower, labels = final_data$Country, cex = 0.7, hang = -1,
     main = "Hierarchical Clustering (Gower + Ward.D2)")

# Choose k and cut tree
k <- 5
clusters <- cutree(hclust_gower, k = k)

# Attach clusters back to final_data
final_data$Cluster <- clusters

# Quick checks
print(table(final_data$Cluster))
final_data %>%
  group_by(Cluster) %>%
  summarise(Countries = paste(Country, collapse = ", "), .groups = "drop") %>%
  print(n = Inf)

# Silhouette to assess k
# Try several k and pick the best average silhouette
sil_width <- sapply(2:8, function(kk) {
  sil <- silhouette(cutree(hclust_gower, k = kk), gower_dist)
  mean(sil[, 3])
})
plot(2:8, sil_width, type = "b", xlab = "Number of clusters (k)",
     ylab = "Average silhouette width", main = "Silhouette vs k")

# Show the silhouette for chosen k
sil <- silhouette(final_data$Cluster, gower_dist)
summary(sil)         # look at Average silhouette width
plot(sil, border = NA, main = paste("Silhouette plot (k =", k, ")"))

# ==============================================================================
# 3.2 PAM clustering
# ==============================================================================
pam_fit <- pam(gower_dist, k = 5)
final_data$PAM_Cluster <- pam_fit$clustering

table(final_data$PAM_Cluster)

final_data %>%
  group_by(PAM_Cluster) %>%
  summarise(Countries = paste(Country, collapse = ", ")) %>%
  arrange(PAM_Cluster)

# plot clusters
mds2 <- cmdscale(gower_dist, k = 2)
plot_df <- data.frame(
  Dim1 = mds2[,1],
  Dim2 = mds2[,2],
  Cluster = factor(pam_fit$clustering),
  Country = final_data$Country
)

ggplot(plot_df, aes(Dim1, Dim2, color = Cluster, label = Country)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +
  theme_minimal() +
  labs(title = "PAM clusters (Gower) via MDS", x = "Dim 1", y = "Dim 2")

# Check silhouette
sil_width <- c()  # empty vector to store silhouette widths

# test k = 2 to 8
for (k in 2:8) {
  pam_fit <- pam(gower_dist, k = k, diss = TRUE)
  sil_width[k] <- pam_fit$silinfo$avg.width
}

# plot silhouette results
plot(2:8, sil_width[2:8],
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters (k)",
     ylab = "Average silhouette width",
     main = "Choosing Optimal k for PAM Clustering")


# ==============================================================================
# 3.3 K-means clustering
# ==============================================================================
set.seed(123)  # for reproducibility

# Run K-means (set k to your desired number of clusters)
k <- 4
kmeans_result <- kmeans(data_for_clustering, centers = k, nstart = 25)

# Combine results
cluster_table <- data.frame(
  Country = final_data$Country,
  Cluster = kmeans_result$cluster
)

plot_df_kmeans <- data.frame(
  Dim1    = mds2[,1],
  Dim2    = mds2[,2],
  Cluster = factor(kmeans_result$cluster),
  Country = final_data$Country
)

ggplot(plot_df_kmeans, aes(Dim1, Dim2, color = Cluster, label = Country)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +
  theme_minimal() +
  labs(
    title = "K-means clusters via MDS",
    x = "Dim 1",
    y = "Dim 2"
  )
# Print countries grouped by cluster
for (k in sort(unique(cluster_table$Cluster))) {
  cat("\nCluster", k, ":\n")
  cat(paste(cluster_table$Country[cluster_table$Cluster == k], collapse = ", "), "\n")
}

# ==============================================================================
# 3.4 GMM clustering
# ==============================================================================

# Choose number of clusters
k <- 4

# Run GMM with k components
set.seed(123)
gmm_result <- Mclust(data_for_clustering, G = k)

# Add cluster labels to your data
final_data$cluster <- gmm_result$classification

# ---- Print countries grouped by cluster ----
cluster_table <- data.frame(
  Country = final_data$Country,
  Cluster = final_data$cluster
)

for (cl in sort(unique(cluster_table$Cluster))) {
  cat(cl, "\t", cl, " ",
      paste(cluster_table$Country[cluster_table$Cluster == cl],
            collapse = ", "), "\n")
}

plot_df_gmm <- data.frame(
  Dim1    = mds2[,1],
  Dim2    = mds2[,2],
  Cluster = factor(gmm_result$classification),
  Country = final_data$Country
)

ggplot(plot_df_gmm, aes(Dim1, Dim2, color = Cluster, label = Country)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +
  theme_minimal() +
  labs(
    title = "GMM Clusters via MDS Projection",
    x = "Dim 1",
    y = "Dim 2"
  )

# ==============================================================================
# 3.5 DBSCAN clustering
# ==============================================================================
for (e in c(0.1, 0.5, 1, 2, 3, 4, 5)) {
  model <- dbscan(data_for_clustering, eps = e, minPts = 3)
  cat("eps =", e, "→ clusters found:", length(unique(model$cluster[model$cluster != 0])), "\n")
}

# ==============================================================================
# 3.6 PAM clustering with PCA
# ==============================================================================

#======================== Preparation ==========================================

# select only numeric columns
data_pca <- final_data %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(),
                ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

# scale
data_pca <- scale(data_pca)

# check variables that have 0 variance
which(apply(data_pca, 2, function(x) sd(x, na.rm = TRUE) == 0))

data_pca <- as.data.frame(data_pca)
data_pca <- data_pca %>%
  select(where(~ !all(is.na(.x))))

pca_res <- prcomp(data_pca, center = TRUE, scale. = TRUE)

summary(pca_res)

pca_res$rotation

# scree plot
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 100))

# ================================= Clustering =================================
# Use 2 PCs
m <- 2
scores <- pca_res$x[, 1:m, drop = FALSE]

# check the best number of k
nb <- NbClust(scores, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans", index ="all")

# Keep country names as rownames for sanity
rownames(scores) <- final_data$Country

# Try different numbers of clusters
ks <- 2:10
wss <- sapply(ks, function(k) {
  pam(dist(scores), k = k)$objective[1]  # within-cluster sum of distances
})

# Plot the elbow
plot(ks, wss, type = "b", pch = 19,
     xlab = "Number of clusters (k)",
     ylab = "Within-cluster sum of squares (WSS)",
     main = "Elbow Method for Optimal k")


# Evaluate silhouette widths for reference
ks   <- 2:10
sils <- sapply(ks, function(k) pam(dist(scores), k = k)$silinfo$avg.width)
plot(ks, sils, type = "b", pch = 19,
     xlab = "Number of clusters (k)", ylab = "Average silhouette width")

# Then pick manually
k <- 3

# Final PAM with best k
set.seed(123)
pam_pc <- pam(dist(scores), k = k)

# Attach clusters
final_data$PC_Cluster <- pam_pc$clustering

# Countries by cluster
final_data %>%
  arrange(PC_Cluster, Country) %>%
  group_by(PC_Cluster) %>%
  summarise(Countries = paste(Country, collapse = ", "), .groups = "drop")

pam_pc$silinfo$avg.width

# ============================= Visualization ======================================
# Combine scores + countries + cluster
plot_df <- data.frame(scores,
                      Country = final_data$Country,
                      Cluster = factor(final_data$PC_Cluster))
ggplot(plot_df, aes(x = PC1, y = PC2, color = Cluster, label = Country)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +
  theme_minimal() +
  labs(
    title = paste0("Clusters in PCA space (k = ", length(unique(final_data$PC_Cluster)), ")"),
    x = "PC1",
    y = "PC2",
    color = "Cluster"
  )

# ================================= Map graph ==================================
# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world %>% filter(region_un == "Europe")

# Merge your data with the Europe map
europe_data <- left_join(europe, final_data, by = c("name" = "Country"))

ggplot(europe_data) +
  geom_sf(aes(fill = as.factor(PC_Cluster)), color = "white", size = 0.3) +
  scale_fill_viridis_d(option = "plasma") +
  coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +
  theme_minimal(base_size = 14) +
  labs(
    fill = "Cluster",
    title = "European Country Clusters",
    subtitle = "PCA + PAM results"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# ==============================================================================
# 3.7 Check differences insides clusters
# ==============================================================================

head(sort(abs(pca_res$rotation[, "PC1"]), decreasing = TRUE), 30)
head(sort(abs(pca_res$rotation[, "PC2"]), decreasing = TRUE), 30)
head(sort(abs(pca_res$rotation[, "PC3"]), decreasing = TRUE), 10)
head(sort(abs(pca_res$rotation[, "PC4"]), decreasing = TRUE), 10)
#=============================================================================
# Compute cluster means for all numeric variables
cluster_means <- final_data %>%
  group_by(PC_Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Compute range (max - min across clusters) for each variable
importance <- cluster_means %>%
  select(-PC_Cluster) %>%
  summarise(across(everything(), ~ max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Range_Diff") %>%
  arrange(desc(Range_Diff))

# Show top 10 variables that differ most across clusters
head(importance, 30) %>%
  print(n = 30)

#===============================================================================
# Cluster and global means
means_cluster <- final_data %>%
  group_by(PC_Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

means_global <- final_data %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Calculate deviation from global mean
diffs <- means_cluster %>%
  mutate(across(-PC_Cluster, ~ .x - means_global[[cur_column()]]))

# Long format for plotting or viewing
diffs_long <- diffs %>%
  pivot_longer(-PC_Cluster, names_to = "Variable", values_to = "Deviation") %>%
  arrange(PC_Cluster, desc(abs(Deviation)))

# Show top 10 distinguishing variables per cluster
diffs_long %>%
  group_by(PC_Cluster) %>%
  slice_max(abs(Deviation), n = 20) %>%
  print(n = 90)


# ==============================================================================
#                                 4. Extra graphs
# ==============================================================================

# ==============================================================================
# 4.1 Line graphs 
# ==============================================================================

# ============================ BEV per final cluster ===========================
cluster_list <- list(
  Pioneer = c("Belgium", "Denmark", "Finland", "Luxembourg", "Netherlands", "Sweden"),
  Advanced = c("Austria", "France", "Germany"),
  Developing = c("Ireland", "Italy", "Malta", "Portugal", "Slovenia", "Spain"),
  Limited = c("Bulgaria", "Croatia", "Cyprus", "Czechia", "Estonia", "Greece",
              "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia")
)

# Convert the cluster list into a clean dataframe
cluster_df <- stack(cluster_list) %>%
  dplyr::rename(Country = values, EV_Market = ind)

# Merge clusters into a SAFE copy of the dataset
data_clustered_tmp <- final_data %>%
  dplyr::left_join(cluster_df, by = "Country")

# Keep only what we need (avoid conflicts with other Cluster columns)
plot_data <- data_clustered_tmp %>%
  dplyr::select(Country, EV_Market, starts_with("BEV_")) %>%
  tidyr::pivot_longer(
    cols = starts_with("BEV_"),
    names_to = "Year",
    values_to = "BEV"
  ) %>%
  dplyr::mutate(
    Year = as.numeric(gsub("BEV_", "", Year))
  )

# Compute average BEV per EV market group per year
avg_cluster <- plot_data %>%
  dplyr::group_by(EV_Market, Year) %>%
  dplyr::summarise(
    Average_BEV = mean(BEV, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
ggplot(avg_cluster, aes(x = Year, y = Average_BEV, color = EV_Market)) +
  geom_line(size = 1.3) +
  geom_point(size = 2.5) +
  theme_minimal() +
  labs(
    title = "Average BEV registrations over time per EV market group",
    x = "Year",
    y = "Average BEV registrations",
    color = "EV Market"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    legend.title = element_text(face = "bold")
  )

#======================= charging infrastructure per cluster ===================
groups <- list(
  Pioneer   = c("Belgium", "Denmark", "Finland", "Luxembourg", "Netherlands", "Sweden"),
  Advanced  = c("Austria", "France", "Germany"),
  Developing = c("Ireland", "Italy", "Malta", "Portugal", "Slovenia", "Spain"),
  Limited   = c("Bulgaria", "Croatia", "Cyprus", "Czechia", "Estonia", "Greece",
                "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia")
)

# Convert to a 2-column data frame
group_df <- stack(groups)
colnames(group_df) <- c("Country", "EV_Market")

# Merge group labels
data_inf <- final_data %>%
  left_join(group_df, by = "Country")

# Pivot infrastructure variables (2020–2024)
# Select variables for every year
infra_data <- data_inf %>%
  select(
    Country, EV_Market,
    matches("Medium_speed_AC_20"),
    matches("Ultra_DC_I_20"),
    matches("Ultra_DC_II_20")
  ) %>%
  pivot_longer(
    cols = -c(Country, EV_Market),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(gsub(".*_(20[0-4][0-9])$", "\\1", Variable)),
    Category = case_when(
      grepl("Medium_speed_AC", Variable) ~ "Medium_speed_AC",
      grepl("Ultra_DC_I",      Variable) ~ "Ultra_DC_I",
      grepl("Ultra_DC_II",     Variable) ~ "Ultra_DC_II"
    )
  )

# Compute total infrastructure per country per year
infra_yearly <- infra_data %>%
  group_by(Country, EV_Market, Year) %>%
  summarise(Total_Infrastructure = sum(Value, na.rm = TRUE), .groups = "drop")

# Compute averages per EV Market group
infra_avg <- infra_yearly %>%
  group_by(EV_Market, Year) %>%
  summarise(Avg_Infrastructure = mean(Total_Infrastructure, na.rm = TRUE), .groups = "drop")

# Plot the 4 lines (Pioneer, Advanced, Developing, Limited)
ggplot(infra_avg, aes(x = Year, y = Avg_Infrastructure, color = EV_Market)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Average charging infrastructure over time per cluster",
    x     = "Year",
    y     = "Average number of charging stations",
    color = "EV Market"
  )

#===================== Average GDP per capita per cluster ======================
group_list <- list(
  Pioneer = c("Belgium", "Denmark", "Finland", "Luxembourg", "Netherlands", "Sweden"),
  Advanced = c("Austria", "France", "Germany"),
  Developing = c("Ireland", "Italy", "Malta", "Portugal", "Slovenia", "Spain"),
  Limited = c("Bulgaria", "Croatia", "Cyprus", "Czechia", "Estonia", "Greece",
              "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia")
)

# Convert the list into a lookup table
country_groups <- stack(group_list)
colnames(country_groups) <- c("Country", "Group")

# Merge with your main dataset
df_gdp <- final_data %>%
  left_join(country_groups, by = "Country") %>%
  filter(!is.na(Group)) %>%   # keep only countries in your 4 groups
  select(Country, Group, GDPpc_2020:GDPpc_2024)

# Reshape to long format
df_long <- df_gdp %>%
  pivot_longer(
    cols = starts_with("GDPpc_"),
    names_to = "Year",
    values_to = "GDPpc"
  ) %>%
  mutate(
    Year = as.numeric(gsub("GDPpc_", "", Year))
  )

# Compute group × year averages
df_avg <- df_long %>%
  group_by(Group, Year) %>%
  summarise(Avg_GDPpc = mean(GDPpc, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(df_avg, aes(x = Year, y = Avg_GDPpc, color = Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Average GDP per capita over time per cluster",
    x = "Year",
    y = "Average GDP per Capita",
    color = "EV Market"
  )

#===============================================================================
# 4.2 Bar charts
#===============================================================================

#========================== Zero Emission Target of 2023 =======================
data_per100 %>%
  filter(Year == 2024) %>%
  ggplot(aes(x = reorder(Country, Towns_suburbs), y = Towns_suburbs)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Zero Emission Target of 2023",
    x = "Country",
    y = "Zero Emission"
  )

#============================== EV totals per country ==========================
plot_df <- data_per100 %>%
  # filter(Country %in% countries_to_show) %>%   # <- uncomment if you want a subset
  group_by(Country) %>%
  summarize(
    BEV_sum  = sum(BEV,  na.rm = TRUE),
    PHEV_sum = sum(PHEV, na.rm = TRUE)
  ) %>%
  mutate(Total = BEV_sum + PHEV_sum) %>%
  # reshape long for stacked bars
  pivot_longer(c(BEV_sum, PHEV_sum), names_to = "Type", values_to = "EV_sum")

ggplot(
  plot_df,
  aes(x = reorder(Country, Total), y = EV_sum, fill = Type)
) +
  geom_col() +  # stacked by default
  coord_flip() +  # flip for readability; remove if you want vertical bars
  labs(
    title = "EV totals per country (sum over 2020-2024)",
    x = "Country",
    y = "Sum of BEV & PHEV"
  ) +
  scale_fill_manual(
    values = c(BEV_sum = "#1f77b4", PHEV_sum = "#ff7f0e"),
    labels = c(BEV_sum = "BEV", PHEV_sum = "PHEV"),
    name = "Type"
  ) +
  theme_minimal(base_size = 12)

#======================= Sum of electricity prices per country =================
plot_df_elec <- data_per100 %>%
  group_by(Country) %>%
  summarize(
    Electricity_sum = sum(Electricity, na.rm = TRUE)
  )

ggplot(
  plot_df_elec,
  aes(x = reorder(Country, Electricity_sum), y = Electricity_sum)
) +
  geom_col(fill = "#4a90e2") +
  coord_flip() +
  labs(
    title = "Sum of electricity prices per country (sum over 2020-2024)",
    x = "Country",
    y = "Sum of Electricity Prices"
  ) +
  theme_minimal(base_size = 12)

#======================= Sum of GDP per capita per country =====================
plot_df_gdp <- data_per100 %>%
  group_by(Country) %>%
  summarize(
    GDPpc_sum = sum(GDPpc, na.rm = TRUE)
  )

ggplot(
  plot_df_gdp,
  aes(x = reorder(Country, GDPpc_sum), y = GDPpc_sum)
) +
  geom_col(fill = "#7cb342") +
  coord_flip() +
  labs(
    title = "Sum of GDP per capita per country (sum over 2020–2024)",
    x = "Country",
    y = "Sum of GDP per capita"
  ) +
  theme_minimal(base_size = 12)

#====================== Total charging stations per country ====================
plot_chargers <- data_per100 %>%
  # filter(Country %in% countries_to_show) %>%   # <- uncomment if you want a subset
  group_by(Country) %>%
  summarize(
    Medium_speed_AC_sum = sum(Medium_speed_AC, na.rm = TRUE),
    Ultra_DC_I_sum      = sum(Ultra_DC_I,     na.rm = TRUE),
    Ultra_DC_II_sum     = sum(Ultra_DC_II,    na.rm = TRUE)
  ) %>%
  mutate(
    Total = Medium_speed_AC_sum + Ultra_DC_I_sum + Ultra_DC_II_sum
  ) %>%
  pivot_longer(
    c(Medium_speed_AC_sum, Ultra_DC_I_sum, Ultra_DC_II_sum),
    names_to  = "Type",
    values_to = "Charger_sum"
  )

ggplot(
  plot_chargers,
  aes(x = reorder(Country, Total), y = Charger_sum, fill = Type)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total charging stations per country (sum over 2020-2024)",
    x = "Country",
    y = "Sum of charging stations"
  ) +
  theme_minimal(base_size = 12)

#=================== Sum of diesel and gasoline prices per country =============
fuel_plot <- data_per100 %>%
  group_by(Country) %>%
  summarize(
    Diesel_sum      = sum(Diesel,      na.rm = TRUE),
    Gasoline_sum    = sum(Gasoline,    na.rm = TRUE)
  ) %>%
  mutate(
    Total =  Diesel_sum + Gasoline_sum
  ) %>%
  pivot_longer(
    c(Diesel_sum, Gasoline_sum),
    names_to = "Fuel_Type",
    values_to = "Fuel_sum"
  )

ggplot(
  fuel_plot,
  aes(x = reorder(Country, Total), y = Fuel_sum, fill = Fuel_Type)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sum of diesel and gasoline prices per country (sum over 2020-2024)",
    x = "Country",
    y = "Sum of Diesel and Gasoline Prices"
  ) +
  theme_minimal(base_size = 12)

