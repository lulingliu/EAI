# EAI Estimation and Validation

This directory contains scripts and instructions for the core Electricity Access Indicator (EAI) estimation and its subsequent validation.

## Overview of Estimation and Validation Workflow

The primary goals of this stage are:
1.  **EAI Estimation:** To calculate the EAI (percentage of population with electricity access) by integrating processed Nighttime Light (NTL) data with gridded population data (GlobPOP).
    * Pixel-level (30 arc-seconds) electrified population density is mapped. *(Note: This step was primarily conducted using raster calculations in QGIS, applying the derived Electrified Mask to the population density data, as described in the main manuscript's Methods section).*
    * EAI is then estimated at a 0.1-degree spatial resolution.
    * National and sub-national level EAI estimates are aggregated from 30 arc-seconds resolution data.
2.  **Benchmarking and Validation:** To assess the accuracy and reliability of the NTL-derived EAI estimates by comparing them against official statistical data from sources like the International Energy Agency (IEA), World Bank, and Demographic and Health Surveys (DHS) Program. Key metrics include Pearson correlation coefficient (R), Root Mean Square Error (RMSE), and Mean Absolute Error (MAE).
3.  **Disparity Analysis:** To analyze regional and income-based disparities in electricity access using the derived EAI estimates.



## Inputs

* Processed NTL data (specifically binary electrified area masks from the `01_preprocessing/` stage, or the thresholds to apply them).
* Processed GlobPOP population density data (from the `01_preprocessing/` stage, likely at 30 arc-seconds).
* GADM administrative boundary files (national and sub-national) [See `/data/README.md` for source].
* Official statistical EAI data (World Bank, DHS Program) [See `/data/README.md` and Table S4 for sources].
* World Bank list of economies (for income-based analysis) [See `/data/README.md` for source].

## Outputs

The main outputs expected from this estimation and validation stage include:

* **Primary Datasets (Archived on Zenodo):**
    * 0.1-degree gridded EAI maps (1992-2022).
    * Aggregated national-level EAI estimates (1992-2022) in CSV and Shapefile.
    * Pixel-level (30 arc-seconds) Electricity Accessed Population Density maps (1992-2022).




Refer to the main manuscript's Methods section for complete procedural details on EAI calculation (Equations 6 & 7) and validation.
