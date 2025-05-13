# Load required libraries
library(sf)
library(dplyr)

# File paths
shp_path <- "D:\\Data_Projects\\Proj_EAI_2024\\InputData\\Inputs_Bound_wgs84\\Globe_Country_simple_region.shp"

df_path = "D:/Data_Projects/Proj_EAI_2024/InputData/Inputs_PanelData/WorldBank_EAI/WB_Income_Group_byCountry/"

csv_file <- paste0(df_path, "212_Economies_IncomeGroup_CLASS.csv")

clust_path = "D:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_table/EAI_clust_3_SOC.csv"


# Read shapefile and CSV
shp_data <- st_read(shp_path)
csv_data <- read.csv(csv_file)
clust_info = read_csv(clust_path)  %>% 
  select(-c(state_5type,state_4type))

# Merge shapefile and CSV based on the common column "SOC"
merged_data <- shp_data %>%
  inner_join(csv_data, by = "SOC") %>%
  inner_join(clust_info, by = "SOC")

length(unique(shp_data$SOC))
length(unique(merged_data$SOC))

# ------ 修改字段Income和state_3type ------
# 目的： 论文图4的地图，2元色彩
# > unique(merged_data$Income)
# [1] "High income"         "Low income"          "Lower middle income" "Upper middle income"
# > unique(merged_data$state_3type)
# [1] 2 3 1


unique(merged_data$Income)
# "Low income"   = 1        
# "Lower middle income"  = 2
# "Upper middle income" = 2 
# "High income" = 3

unique(merged_data$state_3type)
# 原来的数值含义： 2=slow 3=fast 1=middle
# 现在的数值含义：1=slow  3=fast 2=middle


# 更新Income分类映射
merged_data$Income <- recode(merged_data$Income,
                             "Low income" = 1,
                             "Lower middle income" = 2,
                             "Upper middle income" = 2,
                             "High income" = 3)

# 更新state_3type分类映射
merged_data$state_3type <- recode(merged_data$state_3type,
                                  `1` = 2,  # middle -> middle
                                  `2` = 1,  # slow -> slow
                                  `3` = 3)  # fast -> fast


View(merged_data)

# Save the merged shapefile
output_path <- "D:\\Data_Projects\\Proj_EAI_2024\\InputData\\Inputs_Bound_wgs84\\Globe_region_income_recoded.shp"
st_write( merged_data, output_path, append = FALSE )




# 导出属性表为CSV
output_csv_path <- "D:/Data_Projects/Proj_EAI_2024/InputData/Inputs_Bound_wgs84/Globe_region_income_recoded属性表.csv"
write.csv(st_drop_geometry(merged_data), output_csv_path, row.names = FALSE)

message("属性表已成功导出为CSV文件：", output_csv_path)

# 导出属性表为xlsx
library(openxlsx)
output_xlsx_path <- "D:/Data_Projects/Proj_EAI_2024/InputData/Inputs_Bound_wgs84/Globe_region_income_recoded属性表.xlsx"
write.xlsx(st_drop_geometry(merged_data), output_xlsx_path, rowNames = FALSE)
