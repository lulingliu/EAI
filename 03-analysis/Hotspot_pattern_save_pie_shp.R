



# 1: Load the shapefile
shp_path <- "F:/Data_Projects/Proj_EAI_2024/InputData/Inputs_Bound_wgs84/Globe_Country_simple_region.shp"
shp <- st_read(shp_path)

# 2: Load EAI  table
eai_path <- "F:/Data_Projects/Proj_EAI_2024/Table/EAI/计算EAI/EAI_Level_0_1992_2022.csv"
eai_table <- read_csv(eai_path) %>%
  dplyr::select(Name, SOC, EAI_2022)


# 3: Load pattern table
table_path <- 'F:/Data_Projects/Proj_EAI_2024/Table/EAI/Analysis/EAI_HotSpot_wgs84New_stat_SOC_Updated.xlsx'
table <- read_excel(table_path)





# Pivoting the table to wide format
wide_data <- table %>%
  dplyr::select(-c(Category,Count)) %>%
  pivot_wider(
    names_from = Pattern,     # Column to create new column names
    values_from = c(Percent), # Columns to fill values
    values_fill = list(Percent = 0) # Fill missing values with 0
  )

# View the result
print(wide_data)




# Define pattern levels
pattern_levels <- c(
  "new hot spot", "consecutive hot spot", "intensifying hot spot", 
  "persistent hot spot", "diminishing hot spot", "sporadic hot spot", 
  "oscillating hot spot", "historical hot spot", "no pattern detected", 
  "new cold spot", "consecutive cold spot", "intensifying cold spot", 
  "persistent cold spot", "diminishing cold spot", "sporadic cold spot", 
  "oscillating cold spot", "historical cold spot"
)


# Reorder columns to match pattern_levels
# Get the new column names (excluding identifiers like ELEMID, SOC)
identifier_cols <- c("ELEMID", "SOC")
pattern_cols <- intersect(pattern_levels, colnames(wide_data))

# Reorder columns
wide_data_reorder <- wide_data %>%
  dplyr::select(all_of(identifier_cols), all_of(pattern_cols))

# View the reordered table
print(wide_data_reorder)



# Step 3: Merge shp(217 rows) and EAI table(212 rows), result is 212 rows
merged_shp <- shp %>%
  inner_join(eai_table, by = "SOC")


# Merge pattern with shapefile
pie_shp <- merged_shp %>%
  inner_join(wide_data_reorder, by = "SOC")

pie_shp <- pie_shp %>%
  mutate(area = as.numeric(st_area(geometry))) %>%  # Calculate area
  mutate(size_factor = scales::rescale(area, to = c(2, 8)))  # Normalize area to range (1, 5)

st_write(pie_shp, "F:/Data_Projects/Proj_EAI_2024/Figure/EAI_hotspot_pattern/data_table_fig_Pattern/pie_country_region.shp",
         append=FALSE ) # overwrite layer

print(colnames(pie_shp))
# "new hot spot"           
# "consecutive hot spot"   
# "intensifying hot spot"  
# "persistent hot spot"    
# "diminishing hot spot"   
# "sporadic hot spot"      
# "oscillating hot spot"  
# "historical hot spot"    
# "no pattern detected"    
# "new cold spot"          
# "consecutive cold spot"  
# "intensifying cold spot" 
# "persistent cold spot"   
# "diminishing cold spot" 
# "sporadic cold spot"     
# "oscillating cold spot"  
# "historical cold spot"   

pie_shp_read <- st_read("F:/Data_Projects/Proj_EAI_2024/Figure/EAI_hotspot_pattern/data_table_fig_Pattern/pie_country_region.shp")

colnames(pie_shp_read)

# "nwhtspt"  
# "cnscths"  
# "intnshs"  
# "prssths"  
# "dmnshhs"  
# "sprdchs"  
# "oscllhs"  
# "hstrchs"  
# "npttrnd"  
# "nwcldsp" 
# "cnsctcs"  
# "intnscs"  
# "prsstcs"  
# "dmnshcs"  
# "sprdccs"  
# "oscllcs"  
# "hstrccs"  

# "area"     
# "sz_fctr" 