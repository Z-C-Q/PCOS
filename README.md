
# Polycystic Ovary Syndrome Burden in China (1990–2021, Projection to 2035)
## Insights from the Global Burden of Disease Study 2021

## 🧬 Project Description

This project aims to visualize and analyze the burden of **Polycystic Ovary Syndrome (PCOS)** among Chinese women aged 10–54 years, based on data from the **Global Burden of Disease Study 2021 (GBD 2021)**. The study evaluates long-term trends in PCOS **incidence**, **prevalence**, and **disability-adjusted life-years (DALYs)** between **1990 and 2021**, and provides **projections through 2035** using Age–Period–Cohort modeling. Decomposition analysis was conducted to quantify the contributions of **epidemiological change**, **population growth**, and **aging**.

## 📊 Data Source

All data are derived from the publicly available **Global Burden of Disease 2021** database, accessible via the [Global Health Data Exchange](https://ghdx.healthdata.org/gbd-results-tool). The PCOS burden data include:
- **Incidence**, **prevalence**, and **DALYs**
- Age range: **10–54 years**
- Population: **Chinese females**
- Timeframe: **1990 to 2021**
- Projections: **2022 to 2035**

Estimates are age-standardized and include 95% uncertainty intervals (UIs). The study uses **DisMod-MR 2.1**, Bayesian age–period–cohort (BAPC) models, and decomposition analysis based on counterfactual approaches.

## 🔍 Features

- 📈 **Temporal Trends (1990–2021)**  
  Visualization of age-specific and age-standardized trends in incidence, prevalence, and DALYs.

- 🧬 **Age–Period–Cohort (APC) Modeling & Forecasting**  
  Predictive analysis of PCOS burden through **2035**, using Bayesian hierarchical models and MCMC simulation.

- ⚖️ **Decomposition Analysis**  
  Quantification of the role of population growth, aging, and epidemiological risk in burden changes.

- 👩‍⚕️ **Age-Stratified Focus**  
  Special attention to early adolescent groups (10–14 and 15–19 years), highlighting the trend toward earlier onset.

## 📦 Dependencies

To reproduce this project, please ensure the following R environment and packages:

```r
R version 4.4.1 (2024-06-14)
Platform: aarch64-apple-darwin20 (macOS 15.0.1)

Required R packages:
- tidyverse (>= 2.0.0)
- ggplot2 (>= 3.5.1)
- readxl, readr, data.table
- BAPC, INLA, Epi, fanplot
- segmented, broom, ggsci
- sf, rnaturalearth, patchwork
- shiny, shinydashboardPlus, shinyWidgets, DT

Time zone: Asia/Shanghai
```

You can install all packages with:

```r
packages <- c("tidyverse", "ggplot2", "readxl", "readr", "data.table", "BAPC", "INLA", "Epi", 
              "fanplot", "segmented", "broom", "ggsci", "sf", "rnaturalearth", "patchwork", 
              "shiny", "shinydashboardPlus", "shinyWidgets", "DT")
install.packages(packages)
```

## 🗂️ Directory Structure

```
📁 /data                # Raw GBD 2021 PCOS data files (CSV, Excel)
📁 /scripts             # R scripts for data preprocessing and modeling
📁 /figures             # Output plots: trends, decomposition, projections
📁 /shiny_app           # Optional Shiny app for interactive visualization
README.Rmd             # This file
```



## 🔐 Ethics Statement

This project uses publicly available, anonymized aggregate data from the GBD 2021. No individual-level or sensitive data were accessed.
