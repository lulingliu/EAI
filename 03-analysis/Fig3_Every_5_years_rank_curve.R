

# Note： 绘制成每个收入组内的时序热点


# Load libraries
library(readr)
library(tidyverse)

# Input data path
df_path = "D:/Data_Projects/Proj_EAI_2024/InputData/Inputs_PanelData/WorldBank_EAI/WB_Income_Group_byCountry/"
df_name = paste0(df_path, '212_Economies_IncomeGroup_CLASS.csv')
add_info = read_csv(df_name)



eai_path  = 'D:/Data_Projects/Proj_EAI_2024/Table/EAI/计算EAI/EAI_Level_0_1992_2022.csv'
eai_data = read_csv(eai_path)



# 使用 pivot_longer() 将宽表转换为长表
eai_long_data <- eai_data %>%
  pivot_longer(
    cols = starts_with("EAI"),  # 选择以 EAI 开头的列
    names_to = "Year",           # 创建一个名为 Year 的新列
    values_to = "EAI"            # 创建一个名为 EAI 的新列
  )

# 去掉 "EAI_" 前缀
eai_long_data$Year <- gsub("EAI_", "", eai_long_data$Year)
eai_long_data = eai_long_data %>%
  mutate(Year = as.numeric(Year))



# Calculate the power access growth rate
eai_five_year_growth <- eai_long_data %>%
  mutate(Period = 1992 + ((Year - 1992) %/% 5 * 5))

eai_five_year_growth <- eai_five_year_growth %>%
  arrange(SOC, Year) %>%
  group_by(SOC, Period) %>%
  summarize(Start_EAI = first(EAI),
            End_EAI = nth(EAI, 5, default = last(EAI)), 
            Start_Year = first(Year),
            End_Year = if_else(n() >= 5, Start_Year + 4, last(Year)), .groups = 'drop') %>%
  filter(End_Year > Start_Year) %>%
  mutate(EAI_Growth_5yr = (End_EAI - Start_EAI) / Start_EAI * 100)




# Calculate quantile scores within each income group
# 收入分组内部的分位数
eai_growth_quantile_innerJoin <- eai_five_year_growth %>%
  inner_join(add_info, by = c("SOC" = "SOC")) %>%
  group_by(Income, Period) %>%  # Group by Income and Period
  mutate(Quantile = ntile(EAI_Growth_5yr, 100)) %>%
  ungroup()

eai_growth_quantile_leftJoin <- eai_five_year_growth %>%
  left_join(add_info, by = c("SOC" = "SOC")) %>%
  group_by(Income, Period) %>%  # Group by Income and Period
  mutate(Quantile = ntile(EAI_Growth_5yr, 100)) %>%
  ungroup()

length(unique(eai_five_year_growth$SOC))
length(unique(eai_growth_quantile_innerJoin$SOC))
length(unique(eai_growth_quantile_leftJoin$SOC))

# ------ 找到不匹配的8个国家（204/212） ------
# 找出表格中列Region为NA的行，所在列SOC的值
soc_values <- eai_growth_quantile_leftJoin$SOC[is.na(eai_growth_quantile_leftJoin$Region)]

# Print SOC values
print(unique(soc_values))

# "AIA" "COK" "ESH" "GUF" "MSR" "MTQ" "REU" "WLF"
# AIA: Anguilla （安圭拉）
# COK: Cook Islands （库克群岛）
# ESH: Western Sahara （西撒哈拉）
# GUF: French Guiana （法属圭亚那）
# MSR: Montserrat （蒙特塞拉特）
# MTQ: Martinique （马提尼克）
# REU: Réunion （留尼汪）
# WLF: Wallis and Futuna （瓦利斯和富图纳）
# 加载所需的包
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# 获取世界地图数据
world <- ne_countries(scale = "medium", returnclass = "sf")

# 要标注的国家ISO代码
iso_codes <- c("AIA", "COK", "ESH", "GUF", "MSR", "MTQ", "REU", "WLF")

# 筛选需要标注的国家数据
annotated_countries <- world[world$iso_a3 %in% iso_codes, ]

# 提取标注用的坐标
annotated_countries$coords <- st_centroid(annotated_countries$geometry)

# 绘图
ggplot(data = world) +
  geom_sf(fill = "gray85", color = "white") +
  geom_sf(data = annotated_countries, fill = "red", color = "black") +
  geom_text(data = annotated_countries,
            aes(x = st_coordinates(coords)[, 1], 
                y = st_coordinates(coords)[, 2], 
                label = name),
            size = 3, color = "blue", check_overlap = TRUE) +
  labs(title = "国家标注地图", 
       caption = "数据来源: rnaturalearth") +
  theme_minimal()


# ------ 8个国家信息加入add_info -----

eai_growth_quantile <- eai_five_year_growth %>%
  inner_join(add_info, by = c("SOC" = "SOC")) %>%
  group_by(Income, Period) %>%  # Group by Income and Period
  mutate(Quantile = ntile(EAI_Growth_5yr, 100)) %>%
  ungroup()



# Order by income level and then by SOC
# 如果income内没有100个国家，排序最大值就不是100， 需要修改代码。
eai_growth_quantile_order <- eai_growth_quantile %>%
  mutate(Income = factor(Income, levels = c("High income", "Upper middle income", 
                                            "Lower middle income", "Low income"))) %>%
  arrange(Income, SOC) %>% 
  mutate(SOC = factor(SOC, levels = unique(SOC)))


# 导出保存结果
result_path = "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/table_fig3_heatmap/eai_growth_quantile_order.csv"
write_csv(eai_growth_quantile_order, file = result_path)

# Scale quantile to a 0-1 range
eai_growth_quantile_order_01 <- eai_growth_quantile_order %>%
  mutate(Quantile = (Quantile - 1) / 99)




# ------ bump图 ------
library(ggbump)
library(ggplot2)
library(dplyr)
library(scales)
library(MetBrewer)


# 读取聚类表格
clust_path = "D:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_table/EAI_clust_3_SOC.csv"
clust_info = read_csv(clust_path) 

# 合并聚类信息
eai_growth_quantile_order_clust = eai_growth_quantile_order %>%
  inner_join(clust_info,by = c("SOC" = "SOC")) %>%
  select(-c(state_5type,state_4type))
  
View(eai_growth_quantile_order_clust)
colnames(eai_growth_quantile_order_clust)
unique(eai_growth_quantile_order_clust$state_3type)



# ------ 各个(Income) 内不同增长类型 (state_3type) 国家数量 ------
# 统计每个 Income 中不同 state_3type 的国家数量
state_type_summary <- eai_growth_quantile_order_clust %>%
  group_by(Income, state_3type) %>%
  summarize(Count = n(), .groups = 'drop')

print(state_type_summary)




# ------ bump标注: rank+pace聚类+高增长国家标注 ------



# Find this in ~/Analysis/Fig4_PowerPlant_EAI_POP.R
selected_high <- unique(shp_data_high_growth_countries $SOC)
selected_high

eai_growth_quantile_order_clust = eai_growth_quantile_order_clust %>%
  mutate(state_3type = factor(state_3type, levels = c(3,1,2))) 

# === 绘图 ===
plot_Income <- eai_growth_quantile_order_clust |> 
  ggplot(aes(x = Period, y = Quantile, group = SOC, color = as.factor(state_3type))) + 
  
  # 基础线条和点
  geom_bump(linewidth = 0.6, smooth = 6) + 
  geom_point(color = "white", size = 4) +
  geom_point(aes(color = state_3type), size = 2) +
  
  # 原始SOC文本标注
  geom_text(aes(label = SOC), x = 2018, hjust = 0,
            color = "grey60", family = "Arial", size = 4,
            data = eai_growth_quantile_order %>%
              filter(!SOC %in% selected_high)  %>%
              slice_max(Period, by = SOC, n = 1) ) +
  geom_text(aes(label = SOC), x = 2018, hjust = 0,
            color = "#0c2c84", family = "Arial", size = 4,fontface = "bold",
            data = eai_growth_quantile_order %>%
              filter(SOC %in% selected_high)  %>%
              slice_max(Period, by = SOC, n = 1) ) +

  # 设置颜色、坐标轴和标签
  scale_color_manual(values = c("1" = "#a6bddb", "2" = "#ece7f2", "3" = "#2b8cbe"), 
                     #  values = c("1" = "#A5B68D", "2" = "#ECDFCC", "3" = "#DA8359"), 
                     # values = c("1" = "#1b9e77", "2" = "#d95f02", "3" = "#7570b3"),
                     labels = c("1" = "middle pace", "2" = "slow pace", "3" = "fast pace")) +  
  scale_x_continuous(limits = c(1992, 2020), expand = c(0.01, 0),
                     breaks = seq(1992, 2017, by = 5)) +
  scale_y_reverse(breaks = c(100, 75, 50, 25, 10, 5, 1), 
                  expand = c(0.02, 0), 
                  labels = number_format(suffix = ".")) +
  # 图例与主题设置
  labs(x = "Period start year", y = "EAI growth rate rank", color = "", title = NULL, subtitle = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "top",
        legend.text = element_text(size = 18, face = "bold", color = "black"),  # 图例字体
        axis.text = element_text(size = 18, face = "bold", color = "black"),    # 坐标轴字体
        axis.title = element_text(size = 18, face = "bold", color = "black"),   # 坐标轴标题字体
        strip.text = element_text(size = 18, face = "bold", color = "black"),   # 分面标题字体
        panel.grid = element_blank()) + 
  
  # 按收入分面显示
  facet_wrap(~ Income, scales = "free_y",nrow = 1)


print(plot_Income)



# Save the heatmap
fig_path = "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Figure-3-Heatmap/"

ggsave(paste0(fig_path, "Quantile_Heatmap_x时期y排名国家_1x4_蓝色.jpg"),
       plot_Income, width = 26, height = 14, dpi = 400)
ggsave(paste0(fig_path, "Quantile_Heatmap_x时期y排名国家_1x4_蓝色.pdf"), 
       plot_Income, width = 24, height = 10, device = cairo_pdf)



ggsave(paste0(fig_path, "Quantile_Heatmap_x时期y排名国家_2x2.jpg"),
       plot_Income, width = 18, height = 16, dpi = 400)
ggsave(paste0(fig_path, "Quantile_Heatmap_x时期y排名国家_2x2.pdf"), 
       plot_Income, width = 18, height = 16, device = cairo_pdf)
