# *********************************************************
# 文件名: merge_eai_with_shp_allYear.R
# 目的: 将包含1992年到2022年全部EAI数据添加到新的SHP文件的属性中，保存为一个新的SHP文件
# 输入:
#  - Globe_Boundary_New.shp: 包含全球边界信息的矢量地图
#  - EAI_NTL_Results_1992_2022.xlsx: 包含从1992年到2022年每年各国EAI_NTL数据的表格
# 输出:
#  - 一个新的SHP文件，包含31年的EAI数据添加为新属性列
# 使用方法:
#  - 确保安装了所有必要的库和依赖项。
#  - 设置正确的文件路径和文件名。
#  - 运行该脚本以将全部EAI数据添加到新的SHP文件中。
# *********************************************************

# 载入必要的库
library(sf)
library(tidyverse)
library(readxl)

# 读取空间数据函数
read_spatial_data <- function(shp_path) {
  shp_name <- paste0(shp_path, 'Globe_Boundary_New.shp')
  spatial_data <- st_read(shp_name)
  return(spatial_data)
}

# 读取所有年份的EAI数据函数
read_EAI_data <- function(EAI_table_path) {
  EAI_table <- paste0(EAI_table_path, 'EAI_NTL_Results_1992_2022.xlsx')
  EAI_data <- read_excel(EAI_table)
  return(EAI_data)
}

# 合并EAI数据到空间数据函数
merge_EAI_with_spatial_data <- function(spatial_data, EAI_data, year) {
  EAI_year <- EAI_data %>% filter(Year == year)
  merged_data <- spatial_data %>% left_join(EAI_year, by = "SOC")
  return(merged_data)
}

# 合并所有年份的EAI数据到空间数据并保存新的SHP数据函数
merge_all_EAI_with_spatial_data <- function(spatial_data, EAI_data_all) {
  for (year in 1992:2022) {
    merged_data <- merge_EAI_with_spatial_data(spatial_data, EAI_data_all, year)
    col_name <- paste("EAI_", year, sep = "")
    spatial_data <- merged_data %>% select(-Year) %>% rename(!!col_name := EAI_NTL)
  }
  return(spatial_data)
}

# 指定文件路径
shp_path <- "C:/Users/cheng/Desktop/Data_Thesis_EAI/Shp_boundary/"
EAI_table_path <- "C:/Users/cheng/Desktop/Table_Thesis_EAI/国家EAI/"
save_folder <- "C:/Users/cheng/Desktop/Data_Thesis_EAI/Merged_EAI_SHp_country/"

# 读取空间数据和EAI数据
spatial_data <- read_spatial_data(shp_path)
EAI_data_all <- read_EAI_data(EAI_table_path)

# 合并31年的EAI数据到空间数据
merged_data <- merge_all_EAI_with_spatial_data(spatial_data, EAI_data_all)

# 保存包含31年EAI数据的新SHP文件
save_path  = paste0(save_folder, "EAI_NTL_1992to2022.shp")
print(save_path)
st_write(merged_data, ,delete_dsn = TRUE)

cat("31年的EAI数据已成功合并到新的SHP数据文件。\n")
