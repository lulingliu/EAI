# Edit: 2024-12-28
# path:D:/Data_Projects/Proj_EAI_2024/Code/Code_2024/Thesis_code/Fig4_Every_5_years_hotspot.R
# Note： 修改代码，绘制成每个收入组内的时序热点，而不是全部国家的排序热点



library(dplyr)
library(tidyverse)

library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggbreak) # Set Axis Break for 'ggplot2'



# Input data path

# 指定路径
# Include the region/income level with matched SOC
df_name = paste0(df_path, 'add_info.csv')
add_info <- read_csv(df_name)


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

head(eai_long_data)

# merge
eai_group = eai_long_data %>%
  left_join(add_info , by= c("SOC" = "SOC")) 

# 显示合并后数据集的结构
str(eai_group)

# Checking for missing values
any(is.na(eai_group)) 
# Remove rows with any NA values
eai_group <- na.omit(eai_group)
# Verifying if all missing values are handled
any(is.na(eai_group)) # Should return FALSE

length(unique(eai_group$SOC)) # 204



plot_data = eai_group
plot_data$Year <- as.factor(plot_data$Year)

# Reshape data into long format
plot_data_long <- plot_data %>%
  gather(variable, value, EAI)  
View(plot_data_long)



# ------- EAI时序箱型图-大洲-Income-1992-2022 ------

plot = ggplot(plot_data_long, aes(x = Year, y = value, color = Income, fill = Income )) +
  geom_boxplot(outlier.shape = NA, coef = 0, size = 0.2 ) +
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete( breaks = as.factor(c(1992,2002, 2012,2022))) +
  labs(title = "", x = "", y = "EAI(%)") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text = element_text(size = 16,color = 'black',family = 'sans'),
        axis.title = element_text(size = 16,color = 'black',face = "bold",family = 'sans'),
        text  = element_text(size = 18,color = 'black', family = 'sans'),
        legend.title = element_blank(),
        legend.text = element_text(size = 18,color = "black",face = "bold",family = 'sans')) +
  facet_wrap(~Region, scales = "free_y", ncol = 1)

# EAI时序箱型图-大洲-Income-1992-2022
plot

# Save the plot
fig_path = "F:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/"
ggsave(paste0(fig_path, "SI_f1_boxplot_income_region.jpg"),
       plot, width = 18, height = 14, dpi = 400)

# Save as editable PDF
ggsave(paste0(fig_path, "SI_f1_boxplot_income_region.pdf"), 
       plot, width = 18, height = 14, device = cairo_pdf)










# Calculate the power access growth rate
eai_five_year_growth <- eai_group %>%
  mutate(Period = 1992 + ((Year - 1992) %/% 5 * 5))

eai_five_year_growth <- eai_five_year_growth %>%
  arrange(SOC, Year) %>%
  group_by(SOC, Period) %>%
  summarize(Start_EAI_NTL = first(EAI_NTL),
            # 获取每组中的第5个元素，如果没有则用最后一个，处理不满5年的周期
            End_EAI_NTL = nth(EAI_NTL, 5, default = last(EAI_NTL)), 
            # 增加以确保考虑开始和结束年份在分组内的回顾
            Start_Year = first(Year),
            End_Year = if_else(n() >= 5, Start_Year + 4, last(Year)), .groups = 'drop') %>%
  # 选取每个周期内的第一个
  filter(End_Year > Start_Year) %>%# 移除只有一个年份数据的情况
  mutate(EAI_Growth_5yr = (End_EAI_NTL - Start_EAI_NTL) / Start_EAI_NTL * 100)

print(head(eai_five_year_growth), digits = 10)
print(tail(eai_five_year_growth), digits = 10)




# Calculate quantile scores
eai_growth_quantile <- eai_five_year_growth %>%
  left_join(add_info, by = c("SOC" = "SOC")) %>%
  filter(SOC != "MDV") %>%
  group_by(Period) %>%
  mutate(Quantile = ntile(EAI_Growth_5yr, 100)) %>%
  ungroup()


# 按照收入水平排列后再按国家代码SOC排序
# 为SOC分配权重，以便其排序对应于按Income分组的顺序
# Order by income level and then by SOC
eai_growth_quantile_order <- eai_growth_quantile %>%
  mutate(Income = factor(Income, levels = c("High income", "Upper middle income", 
                                            "Lower middle income", "Low income"))) %>%
  arrange(Income, SOC) %>% 
  mutate(SOC = factor(SOC, levels = unique(SOC)))

# Scale quantile to a 0-1 range
eai_growth_quantile_order_01 <- eai_growth_quantile_order %>%
  mutate(Quantile = (Quantile - 1) / 99)




# ------ 哑铃图 ------

# 提取1992年和2022年的EAI数据
eai_1992_2022 <- eai_group %>%
  filter(Year %in% c(1992, 2022)) %>%
  select(SOC, Year, EAI_NTL) %>%
  pivot_wider(names_from = Year, values_from = EAI_NTL, names_prefix = "EAI_") %>%
  rename(EAI_1992 = EAI_1992, EAI_2022 = EAI_2022)

# 合并收入数据
eai_1992_2022 <- eai_1992_2022 %>%
  left_join(add_info, by = "SOC") %>%
  filter(!is.na(EAI_2022)) # 确保2022年数据存在

str(eai_1992_2022)

library(ggplot2)
library(ggalt)

# 绘制哑铃图: 显示1992年和2022年间，按国家（SOC）比较的EAI值
ggplot(eai_1992_2022, aes(x = EAI_1992, xend = EAI_2022,y = reorder(Name, EAI_1992)  )) +
  geom_dumbbell(color = "lightblue", size = 4,  color_xend = "red") +
  labs(title = "EAI Value Comparison: 1992 vs 2022",
       x = "EAI Value",
       y = "Countries (SOC)") +
  theme_minimal() +
  facet_wrap(~ Income, scales = "free_y") +  # 按收入水平分面
  theme(axis.text.y = element_text(size = 8))




library(dplyr)
library(tidyr)

# 计算每五年的EAI值
eai_five_years <- eai_group %>%
  filter(Year %% 5 == 0) %>%
  select(SOC, Year, EAI_NTL) %>%
  pivot_wider(names_from = Year, values_from = EAI_NTL, names_prefix = "EAI_") %>%
  rename_at(vars(starts_with("EAI_")), ~ gsub("EAI_", "EAI_", .))

# 添加收入数据
eai_five_years <- eai_five_years %>%
  left_join(add_info, by = "SOC") %>%
  filter(!is.na(EAI_2020)) # 仅保留有2022年数据的国家

library(ggplot2)
library(ggalt)
library(reshape2)

# 将数据从宽格式转换为长格式，这样每对EAI值（每五年）可以在同一个图中绘制
eai_five_long <- melt(eai_five_years, id.vars = c("SOC", "Income"),
                      variable.name = "Year", value.name = "EAI_Value")

# 提取年份并创建新的变量
eai_five_long$Year <- as.numeric(gsub("EAI_", "", eai_five_long$Year))

# 分组并绘图
ggplot(eai_five_long, aes(x = EAI_Value, y = reorder(SOC, EAI_Value))) +
  geom_dumbbell(aes(xend = lead(EAI_Value)), color = "lightblue", 
                size = 4, color_x = "blue", color_xend = "red") +
  labs(title = "EAI Value Comparison Every 5 Years",
       x = "EAI Value",
       y = "Countries (SOC)") +
  theme_minimal() +
  facet_wrap(~ Income, scales = "free_y") +  # 按收入水平分面
  theme(axis.text.y = element_text(size = 8))
