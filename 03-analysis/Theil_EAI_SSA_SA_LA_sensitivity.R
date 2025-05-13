 # 承接代码中的数据
# /Fig6_EAI_accuracy_NoAccPOP_&FigS6S7.R
# /Theil_EAI_SSA_SA_LA_Income_1992to2022.R

data_filtered <- group_dataC_NTL %>%
  filter(Region %in% target_regions) %>%
  mutate(Region = case_when(
    Region == "Latin America & Caribbean" ~ "Latin America",
    Region %in% c("South Asia", "East Asia & Pacific") ~ "East and South Asia",
    TRUE ~ Region
  ))


# 复制原始数据以避免污染
data_sensitivity <- data_filtered 



# 按区域计算组内泰尔指数（调整后的数据）
theil_sensitivity <- data_sensitivity %>%
  filter(Year == 2022) %>%
  group_by(Region) %>%
  summarise(
    Theil_Within_Sensitivity = calculate_theil(EAI_NTL, POP),
    .groups = "drop"
  )

# 合并原始结果与敏感性测试结果
theil_comparison <- theil_results %>%  # 原始结果
  left_join(theil_sensitivity, by = "Region") %>%
  mutate(
    Absolute_Change = Theil_Within_Sensitivity - Theil_Within,
    Relative_Change = Absolute_Change / Theil_Within
  )



library(ggplot2)
library(tidyr)

# 长格式转换以便绘图
theil_long <- theil_comparison %>%
  pivot_longer(
    cols = c(Theil_Within, Theil_Within_Sensitivity),
    names_to = "Dataset",
    values_to = "Theil_Index"
  ) %>%
  mutate(Dataset = ifelse(Dataset == "Theil_Within", "Original", "Sensitivity"))

# 绘制柱状对比图
ggplot(theil_long, aes(x = Region, y = Theil_Index, fill = Dataset)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = round(Theil_Index, 3)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(values = c("Original" = "#377EB8", "Sensitivity" = "#E41A1C")) +
  labs(
    title = "Sensitivity Analysis: Impact of Removing Small Countries (Population <1M)",
    x = "",
    y = "Theil Index (2022)",
    fill = "Dataset"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



# 按年份计算敏感性泰尔指数
theil_sensitivity_time <- data_sensitivity %>%
  group_by(Year, Region) %>%
  summarise(
    Theil_Within_Sensitivity = calculate_theil(EAI_NTL, POP),
    .groups = "drop"
  )

# 对比时间趋势
plot_SI_Theil_a = ggplot(theil_sensitivity_time, aes(x = Year, y = Theil_Within_Sensitivity, color = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  ylim(0,0.25)+
  scale_color_manual(values = c(
    "Sub-Saharan Africa" = "#624E88",
    "East and South Asia" =  "#8967B3",
    "Latin America" = "#CB80AB")) +
  theme_minimal(base_size = 18, base_family = "sans")+
  labs(title = NULL,
       color = "Region after removing small countries\n(Population < 1 million)" , # 设置图例标题
       y= "Theil Index")#"Sensitivity Test: Theil Index Trend After Removing Small Countries")

plot_SI_Theil_a


library(dplyr)
library(ggplot2)
library(scales)  

# 定义不同人口阈值
thresholds <- c(5e5, 1e6, 5e6)

# 计算不同阈值下的泰尔指数
theil_thresholds <- lapply(thresholds, function(t) {
  
  data_sensitivity <- data_filtered %>% 
    filter(Year == 2022, POP >= t)
  
  data_sensitivity %>%
    group_by(Region) %>%
    summarise(Theil = calculate_theil(EAI_NTL, POP), .groups = "drop") %>%
    mutate(Threshold = factor(paste0("≥ ", comma(t)), 
                              levels = paste0("≥ ", comma(thresholds))))  # 确保一致性
  
}) %>% bind_rows()

# 定义颜色
threshold_colors <- setNames(c("#FCE7C8", "#FADA7A", "#F0A04B"), 
                             paste0("≥ ", comma(thresholds)))  # 确保键名匹配

# 绘图
plot_SI_Theil_b <- ggplot(theil_thresholds, aes(x = Region, y = Theil, fill = Threshold)) +
  geom_col(position = position_dodge(), width = 0.7) +
  scale_fill_manual(values = threshold_colors) +  # 颜色匹配
  theme_minimal(base_size = 18) +
  labs(
    title = NULL,#"Theil Index by Population Threshold (2022)",
    x = "Region",
    y = "Theil Index (2022)",
    fill = "Population Threshold"
  ) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# 显示图表
print(plot_SI_Theil_b)




# Add (a) label to final_plot
plot_SI_Theil_a_labeled <- ggdraw(plot_SI_Theil_a) +
  draw_label("(a)", x = 0.08, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 18)

# Add (b) label to plotC
plot_SI_Theil_b_labeled <- ggdraw(plot_SI_Theil_b) +
  draw_label("(b)", x = 0.08, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 18)

# Combine the labeled plots vertically
plot_SI_Theil <- plot_grid(
  plot_SI_Theil_a_labeled,
  plot_SI_Theil_b_labeled,
  ncol = 1,
  rel_heights = c(0.6, 0.4)
)

# Display the final plot

plot_SI_Theil

# ------ Save  as PDF ------


# Set the output file path and filename

plot_SI_Theil_pdf <- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/SI-Figure/Fig_SI_Theil_sensitivity.pdf"
ggsave(plot_SI_Theil_pdf, plot_SI_Theil, width = 12, height = 8)


# Set the output file path and filename
plot_SI_Theil_jpg<- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/SI-Figure/Fig_SI_Theil_sensitivity.jpg"

# Save the plot as a high-resolution (400 DPI) JPG
ggsave(filename = plot_SI_Theil_jpg, plot = plot_SI_Theil, 
       device = "jpg", dpi = 400, width = 12, height = 8, units = "in")

