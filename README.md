# EV Data Analysis: Electric Vehicle Adoption in the European Union
This project analyzes the adoption patterns of electric vehicles (EVs) across European Union countries over time. It focuses on understanding spatial and temporal differences in EV uptake, charging infrastructure development, energy prices, and policy intensity. The workflow combines data preprocessing, exploratory data analysis (EDA), dimensionality reduction, and multiple clustering techniques to identify meaningful country-level EV market segments.

## Dataset
The dataset includes:
- New registrations of alternative fuel vehicles (BEV, PHEV, H2, LPG, CNG)
- Recharging infrastructure (AC, DC and Ultra fast)
- Energy prices (electricity, gasoline, diesel)
- Socioeconomic indicators (GDP per capita, urbanization, political stability index)
- Policy indicators (EV-related incentives)

## Methods
### 1. Data Preprocessing
- Normalization per 100,000 inhabitants
- Missing values imputation using missRanger
- Feature filtering and transformation
- Wide-format reshaping for time-series analysis
### 2. Exploratory Data Analysis (EDA)
- Correlation analysis between EV adoption and explanatory variables
- Temporal trend visualization (2020–2024)
- Policy score construction from regulatory indicators
- Distribution and relationship analysis via scatterplots and heatmaps
### 3. Clustering Analysis
- Multiple clustering approaches were applied for robustness:
- Hierarchical clustering (Gower distance + Ward linkage)
- PAM clustering (medoid-based, robust to mixed data)
- K-means clustering (Euclidean space on scaled features)
- Gaussian Mixture Models (mclust) (probabilistic clustering)
- DBSCAN (density-based structure detection)
- PCA + PAM clustering (dimensionality-reduced space)
### 4. Evaluation
- Silhouette analysis for cluster validity
- Elbow method for optimal k selection
- NbClust multi-index validation
- Comparative cluster stability across methods

## Technologies
- **R**
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
4. Run the main script: source("ev_data_analysis.r")

## Project Structure
ev-data-analysis/
├── ev_data_analysis.r    # Main analysis pipeline
├── ev_dataset.xlsx       # Dataset
├── plots/                # Generated visualizations
└── README.md

## Author
Petros Palis
