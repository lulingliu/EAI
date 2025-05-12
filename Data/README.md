# Input Data

This document provides an overview and access instructions for the input datasets.

**Important Note:** Due to size limitations, the raw datasets themselves are not stored in this GitHub repository. Users should download the data directly from the official sources listed below.

---

## 1. Primary Raster Datasets

These are the core raster datasets used for estimating the Electricity Access Indicator (EAI).

### 1.1. DMSP-CCNL (Defense Meteorological Satellite Program - Consistent & Corrected Nighttime Light)

* **Description:** Harmonized nighttime light data from the DMSP Operational Linescan System, corrected for inter-calibration, sensor saturation, and spatial spillover. Used as an input for EAI estimation for the earlier period.
* **Years:** 1992-2013
* **Resolution:** 30 arc-seconds
* **Coordinate System:** WGS-84
* **Unit:** Digital Number (DN)
* **Source URL:** [https://doi.org/10.5281/zenodo.6644980](https://doi.org/10.5281/zenodo.6644980)

### 1.2. SNPP-VIIRS Annual VNL V2 (Suomi National Polar-orbiting Partnership - Visible Infrared Imaging Radiometer Suite)

* **Description:** Annual nighttime light data derived from NPP-VIIRS monthly cloud-free average radiance products, preprocessed with annual median composites and noise reduction. Used as an input for EAI estimation for the later period.
* **Years:** 2012-2022
* **Resolution:** 15 arc-seconds
* **Coordinate System:** WGS-84
* **Unit:** nW/cm²/sr (nanowatts per square centimeter per steradian)
* **Source URLs:**
  * V2.1 (2012-2021): [https://eogdata.mines.edu/nighttime_light/annual/v21/](https://eogdata.mines.edu/nighttime_light/annual/v21/)
  * V2.2 (2022): [https://eogdata.mines.edu/nighttime_light/annual/v22/](https://eogdata.mines.edu/nighttime_light/annual/v22/)

### 1.3. GlobPOP

* **Description:** A continuous global gridded population dataset developed using a data fusion framework. Selected as the primary population input due to its high spatiotemporal consistency. Used for EAI estimation and calculating unelectrified population.
* **Years:** 1992-2022
* **Resolution:** 30 arc-seconds (used at this resolution and also aggregated to 0.1-degree for matching NTL data scale in EAI estimation)
* **Coordinate System:** WGS-84
* **Unit:** persons/km² (population density)
* **Source URL:** [https://doi.org/10.5281/zenodo.11179644](https://doi.org/10.5281/zenodo.11179644)

---

## 2. Ancillary Datasets

These datasets were used for various supporting analyses, validation, and contextualization.

### 2.1. GADM (Database of Global Administrative Areas)

* **Description:** Vector dataset providing global administrative boundaries. Used to calculate EAI at different scales (national, sub-national) and for comparison with statistical EAI data.
* **Format:** Vector (Shapefiles)
* **Year:** Version 4.0 
* **Unit:** Not applicable (geospatial boundaries)
* **Source URL:** [https://gadm.org/download/world40.html](https://gadm.org/download/world40.html) (Note: The study used specific historical boundaries where necessary, particularly for sub-national DHS comparisons, sometimes referencing the Spatial Demographic and Health Surveys Program Data Package.)

### 2.2. Global Roads Open Access Data Set, Version 1 (gROADSv1)

* **Description:** Integrates publicly available road data into a unified global dataset. Utilized as ground truth for validating NTL thresholds, assuming roads in electrified areas exhibit stable lighting.
* **Format:** Vector
* **Year:** Data vintage primarily around 2010 (Version 1 released 2013)
* **Unit:** Not applicable (geospatial road networks)
* **Source URL:** [https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1](https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1)

### 2.3. World Population Prospects (WPP)

* **Description:** Provides national-level population estimates and projections from the United Nations. Used to compute unelectrified population totals at global, regional, and income-group scales, and to weight national-level Theil index calculations.
* **Format:** Table
* **Year:** WPP 2024 
* **Unit:** Number of persons
* **Source URL:** [https://population.un.org/wpp/](https://population.un.org/wpp/)

### 2.4. World Bank: Access to electricity (% of population)

* **Description:** Statistical data on the percentage of the population with access to electricity. Used for validating national-scale EAI estimates. Data derived from household surveys and reports, with modeling used to impute missing data by leading organizations (IEA, World Bank, etc. for Tracking SDG 7).
* **Format:** Table
* **Years:** 1992-2022 (as available and used in the study)
* **Unit:** % of population
* **Source URL:** [https://data.worldbank.org/indicator/EG.ELC.ACCS.ZS](https://data.worldbank.org/indicator/EG.ELC.ACCS.ZS)

### 2.5. World Bank: List of Economies

* **Description:** Provides classifications of countries by income level and regional grouping. Used for conducting regional and income-based analyses.
* **Format:** Table
* **Year:** 2022 (classification used for the study period)
* **Unit:** country classifications
* **Source URL:** [https://datacatalog.worldbank.org/search/dataset/0037712/World-Bank-Official-Country-Classifications](https://datacatalog.worldbank.org/search/dataset/0037712/World-Bank-Official-Country-Classifications).

### 2.6. Global Power Plant Database v1.3.0

* **Description:** A comprehensive, open-source database of power plants worldwide. Used to analyze the EAI and infrastructure gaps for countries exhibiting fast EAI progress but not yet at 100% access.
* **Format:** Vector (Points)
* **Year:** Database version 1.3.0 (data primarily reflects up to 2018-2021 depending on region)
* **Unit:** Not applicable (power plant locations and attributes)
* **Source URL:** [https://datasets.wri.org/dataset/globalpowerplantdatabase](https://datasets.wri.org/dataset/globalpowerplantdatabase)

### 2.7. Sub-national EAI Statistical Data (from DHS Program)

* **Description:** Sub-national level EAI estimates derived from DHS Program compiler household surveys (Demographic and Health Survey, DHS; AIDS Indicator Survey, AIS; Malaria Indicator Survey, MIS). Used for validating sub-national EAI estimates, prioritizing the 'Population with electricity' metric.
* **Format:** Survey microdata/tabulations
* **Years:** Various, corresponding to available surveys for 59 countries.
* **Unit:** % of population
* **Source URL:** [https://www.statcompiler.com/](https://www.statcompiler.com/)   
