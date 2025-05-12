# EAI Analysis Scripts

This directory contains scripts and instructions for the  analysis of the Electricity Access Indicator (EAI) data. This stage focuses on uncovering spatial patterns, quantifying inequality, understanding temporal trends, and forecasting future EAI trajectories.

## Overview of Analysis Workflow

The primary analytical tasks performed in this stage include:

1.  **Spatial Analysis of EAI Patterns:** Identifying regions with divergent electrification trajectories (1992–2022) by integrating spatial clustering (Getis-Ord Gi*) and trend analysis (Mann-Kendall test) on the 0.1-degree EAI data. This results in the classification of 16 distinct EAI development patterns (see Table S1 for decision matrix). *(Note: This multi-step spatial pattern analysis was primarily conducted using ArcGIS Pro 3.1).*
2.  **Inequality Assessment (Theil Index):** Calculating the population-weighted Theil index to quantify macro-level EAI inequality, including its decomposition into within-region and between-region components. A sensitivity analysis of the Theil index calculations is also performed (see Figure S11).
3.  **Temporal Analysis of EAI Trends:** Grouping countries with similar EAI trajectories (1992-2022) using Dynamic Time Warping (DTW)-based time series clustering (Partitioning Around Medoids - PAM algorithm). This also includes calculating and ranking EAI growth rates and analyzing infrastructure for fast-paced countries.
4.  **Predictive Analysis:** Developing and applying models (ARIMA and LNOB) to forecast EAI trajectories up to 2030 and calculating required growth multipliers.



## Inputs

* Processed 0.1-degree gridded EAI data (1992-2022) from the `02_estimation_and_validation/` stage.
* National-level EAI time series data (1992-2022) from the `02_estimation_and_validation/` stage.
* World Population Prospects (WPP) data for population weighting in Theil index calculations [See `/data/README.md` for source].
* Country classification data (income level, region) [See `/data/README.md` or Table S6 for source].


## Outputs

The main outputs expected from this analysis stage include (many will be figures or data for figures, stored in `/results`):

* Spatial pattern classification maps/data (output from ArcGIS Pro, supporting Figure 2 and SI Figure S3, based on Table S1).
* Theil index values (global, regional, within-region, between-region) and sensitivity analysis results (data for Figure 2f, Figure S11).
* Country cluster assignments from DTW analysis and associated silhouette scores (data for Figure 3).
* ARIMA and LNOB model forecast data for each country up to 2030 (data for Figure 5, SI Figure S5).
* Required Growth Multiplier values.
* Figures and tables for the main manuscript and Supplementary Information.



Refer to the main manuscript's Methods section for complete procedural details on each analytical technique and the interpretation of results.
