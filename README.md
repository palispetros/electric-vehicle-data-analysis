# EV Data Analysis: Electric Vehicle Adoption in the European Union
This project analyzes the adoption patterns of electric vehicles (EVs) across European Union countries over the period 2020–2024. It investigates spatial and temporal differences in EV uptake, charging infrastructure development, energy prices, and policy intensity. The workflow combines data preprocessing, exploratory data analysis (EDA), dimensionality reduction (PCA), and multiple clustering techniques to identify meaningful country-level EV market segments. A final cluster typology is derived by synthesizing results from multiple clustering approaches and incorporating domain-specific interpretation.

## Dataset
The dataset includes:
- New registrations of alternative fuel vehicles (BEV, PHEV, H2, LPG, CNG)
- Recharging infrastructure (AC, DC and Ultra fast)
- Energy prices (electricity, gasoline, diesel)
- Socioeconomic indicators (GDP per capita, urbanization, political stability index)
- Policy indicators (EV-related incentives)

## Methods Overview
- Data preprocessing (normalization, imputation, feature engineering)
- Exploratory data analysis (correlation and trend analysis)
- Clustering analysis (hierarchical, k-means, PAM, GMM, DBSCAN)
- Dimensionality reduction (PCA)
- Cluster validation (silhouette, elbow method, NbClust)

## Technologies
The project is implemented in R using the following libraries:
- dplyr, tidyr (data manipulation)
- ggplot2 (visualization)
- cluster, factoextra (clustering & evaluation)
- mclust (Gaussian mixture models)
- missRanger (missing data imputation)
- NbClust (cluster validation)
- sf, rnaturalearth (geospatial mapping)

## How to Run
1. Clone this repository: git clone https://github.com/palispetros/electric-vehicle-data-analysis.git
2. Open the project in RStudio.
3. Install required packages:
   install.packages(c(
  "dplyr", "ggplot2", "tidyr", "readxl", "ggrepel",
  "naniar", "cluster", "missRanger", "dbscan",
  "mclust", "RColorBrewer", "rnaturalearth", "sf",
  "NbClust", "factoextra"))
4. Run the main script: source("ev_data_analysis.R")

## Project Structure
```
electric-vehicle-data-analysis/
├── results/
│   ├── clustering_results/
│   ├── correlation_analysis/
│   ├── exploratory_analysis/
│   ├── final_cluster_typology/
│
├── ev_data_analysis.R
├── ev_dataset.xlsx
└── README.md
```

## Author
Petros Palis
