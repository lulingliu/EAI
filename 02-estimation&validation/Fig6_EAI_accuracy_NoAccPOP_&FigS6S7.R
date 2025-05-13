

# ------ load packages -------

library(readxl)     # For reading Excel files
library(tidyverse)

library(ggpubr)
library(cowplot)

# ------ table path ------

level0_path1 = "F:/Data_Projects/Proj_EAI_2024/EAI_Zenodo/Level0_EAI/level_0_EAI_NTL_POP.csv"
level0_path2 = "F:/Data_Projects/Proj_EAI_2024/EAI_Zenodo/Level0_EAI/level_0_EAI_NTL_POP_WB.csv"

Level0Accuracy_plotA_path = "D:/Data_Projects/Proj_EAI_2024/Table/EAI/level_0_EAI_214_bothNTLWB_1992_2022.csv"


Level1Accuracy_plotB_path = "D:/Data_Projects/Proj_EAI_2024/InputData/Inputs_PanelData/DHS-STAT/DHS_data_合并了之前13到17_for_plot.xlsx"

pop_path = "F:/Data_Projects/Proj_EAI_2024//Table/EAI/WPP2022_1992_2022_pop_change.xlsx"

# ------- read table ------

level0_data1 = read_csv(level0_path1)
# Rows: 6572 Columns: 10          
# chr (2): Name, SOC
# dbl (8): Year, EAI_NTL, POP_1k, PopChange, PopGrowthRate, AccPop_NTL, NoAccPop_NTL, POP

level0_data2 = read_csv(level0_path2)
# Rows: 6079 Columns: 10
# chr (2): CountryName, SOC
# dbl (8): Year, EAI_WB, POP_1k, PopChange, PopGrowthRate, AccPop_WB, NoAccPop_WB, POP

level0_data3 = read_csv(Level0Accuracy_plotA_path)
# Rows: 5766 Columns: 6 
# chr (3): Name, SOC, CountryName
# dbl (3): Year, EAI_NTL, EAI_WB

level1_data = read_excel(Level1Accuracy_plotB_path) %>%
  select(ISO,Source,Year,Country,EAI_Stat,EAI_NTL) %>%
  rename(SOC = ISO)  %>%
  rename(EAI_WB = EAI_Stat)

colnames(level1_data)
glimpse(level1_data)
# Rows: 658， Columns: 6
# chr (3):  SOC, Country, Source
# dbl (3): Year, EAI_NTL, EAI_Stat

# 之前的数据：Rows: 226 Columns: 8
# chr (3): GID_1, SOC, NAME_0, subregion, Sources
# dbl (3): Year, EAI_NTL, EAI_WB


pop_data = read_excel(pop_path)[, 1:4]
str(pop_data)
colnames(pop_data) = c( 'SOC','NAME', 'Year', 'POP_1k')
pop_data = pop_data %>% mutate(POP = POP_1k *1000)
  

# 打印数据的前几行
print(head(level0_data1))
length(unique(level0_data1$SOC)) # 212

# 打印数据的前几行
print(head(level0_data2))
length(unique(level0_data2$SOC)) # 215

# 打印数据的前几行
print(head(level0_data3))
length(unique(level0_data3$SOC)) # 203

# 打印数据的前几行
print(head(level1_data))
length(unique(level1_data$SOC)) 



# ------ Income-region-group-clust -----

df_path = "F:/Data_Projects/Proj_EAI_2024/InputData/Inputs_PanelData/WorldBank_EAI/WB_Income_Group_byCountry/"
csv_file <- paste0(df_path, "212_Economies_IncomeGroup_CLASS.csv")

clust_path = "F:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_table/EAI_clust_3_SOC.csv"


IncomeGroup_data <- read.csv(csv_file)
# Rows: 226 Columns: 4
# chr (4): Name, SOC, Region, Income


clust_data = read_csv(clust_path)  %>%  select(-c(state_5type,state_4type))
# Rows: 212 Columns: 2
# chr (1): SOC
# dbl (1): state_3type

unique(IncomeGroup_data$Income)
unique(clust_data$state_3type)

# Merge IncomeGroup_data and clust_data based on the common column "SOC"
IncomeGroup_clust <- IncomeGroup_data %>%  inner_join(clust_data, by = "SOC")
# Rows: 212 Columns: 5
# chr (4): Name, SOC, Region, Income
# dbl (1): state_3type

str(IncomeGroup_clust)
# 更新state_3type分类映射
IncomeGroup_clust$state_3type <- recode(IncomeGroup_clust$state_3type,
                                  `1` = "Middle pace",  # middle 
                                  `2` = "Slow pace",  # slow 
                                  `3` = "Fast pace")  # fast 

output_csv_path <- "D:/Data_Projects/Proj_EAI_2024/InputData/Inputs_Bound_wgs84/Globe_IncomeGroup_clust_212.csv"
write.csv(IncomeGroup_clust, output_csv_path, row.names = FALSE)

# ------ 精度表：国家 ------

# 计算并打印每一年的 R 相关系数
cor_results <- level0_data3 %>%
  group_by(Year) %>%
  summarise(cor_coef = cor(EAI_NTL, EAI_WB, use = "complete.obs"),# 计算相关系数
            N = sum(!is.na(EAI_NTL) & !is.na(EAI_WB) ) , # 仅计算非 NA 的观测数
            RMSE = sqrt(mean((EAI_NTL[!is.na(EAI_NTL) & !is.na(EAI_WB)] - EAI_WB[!is.na(EAI_NTL) & !is.na(EAI_WB)])^2)),
            MAE = mean(abs(EAI_NTL[!is.na(EAI_NTL) & !is.na(EAI_WB)] - EAI_WB[!is.na(EAI_NTL) & !is.na(EAI_WB)]))
  )

# 打印相关系数结果
print(cor_results,n=31)

# 计算全部参与计算的 R 相关系数，RMSE，MAE，并按年统计参与计算的 N 数量
overall_results <- level0_data3 %>%
  summarise(
    cor_coef = cor(EAI_NTL, EAI_WB, use = "complete.obs"), # 计算相关系数
    N = sum(!is.na(EAI_NTL) & !is.na(EAI_WB)),              # 计算所有参与计算的非 NA 的观测数
    RMSE = sqrt(mean((EAI_NTL[!is.na(EAI_NTL) & !is.na(EAI_WB)] - EAI_WB[!is.na(EAI_NTL) & !is.na(EAI_WB)])^2)),
    MAE = mean(abs(EAI_NTL[!is.na(EAI_NTL) & !is.na(EAI_WB)] - EAI_WB[!is.na(EAI_NTL) & !is.na(EAI_WB)]))
  )

# 打印相关系数结果，包括 N，RMSE 和 MAE
print(overall_results)
# cor_coef     N     RMSE   MAE
# 0.872        5766  15.4  10.2



# ------ 精度表：次国家 ------

# 计算并打印每一年的 R 相关系数
cor_results2 <- level1_data %>%
  group_by(Year) %>%
  summarise(cor_coef = cor(EAI_NTL, EAI_WB, use = "complete.obs"),# 计算相关系数
            N = sum(!is.na(EAI_NTL) & !is.na(EAI_WB) ) , # 仅计算非 NA 的观测数
            RMSE = sqrt(mean((EAI_NTL[!is.na(EAI_NTL) & !is.na(EAI_WB)] - EAI_WB[!is.na(EAI_NTL) & !is.na(EAI_WB)])^2)),
            MAE = mean(abs(EAI_NTL[!is.na(EAI_NTL) & !is.na(EAI_WB)] - EAI_WB[!is.na(EAI_NTL) & !is.na(EAI_WB)]))
  )


# 打印相关系数结果
cor_results2
print(cor_results2,n=23)

# 计算全部参与计算的 R 相关系数，RMSE，MAE，并按年统计参与计算的 N 数量
overall_results2 <- level1_data %>%
  summarise(
    cor_coef = cor(EAI_NTL, EAI_WB, use = "complete.obs"), # 计算相关系数
    N = sum(!is.na(EAI_NTL) & !is.na(EAI_WB)),              # 计算所有参与计算的非 NA 的观测数
    RMSE = sqrt(mean((EAI_NTL[!is.na(EAI_NTL) & !is.na(EAI_WB)] - EAI_WB[!is.na(EAI_NTL) & !is.na(EAI_WB)])^2)),
    MAE = mean(abs(EAI_NTL[!is.na(EAI_NTL) & !is.na(EAI_WB)] - EAI_WB[!is.na(EAI_NTL) & !is.na(EAI_WB)]))
  )

# 打印相关系数结果，包括 N，RMSE 和 MAE
print(overall_results2)

#  cor_coef  N    RMSE  MAE
#  0.871     226  13.7  9.96



# 精度绘图函数
plot_func_combined_allyear <- function(data, cor_result) {
  
  p <- ggplot(data, aes(x = EAI_NTL, y = EAI_WB)) +
    geom_point(size = 2, aes(color = Income), alpha = 0.9) +  # 固定点大小，颜色由 EAI_NTL 决定
    geom_smooth(method = "lm", se = TRUE, color = "black", fill = "blue", level = 0.95) +  # 黑色拟合线，天蓝色置信区间
    # scale_color_viridis(option = "A") +  # 使用 Viridis 色板
    scale_color_manual(values = c("#ff7477","#e69597", "#ceb5b7","#b5d6d6")) +
    labs(x = NULL, y = NULL) +  # 清除单独子图的x和y轴标签
    theme_minimal(base_size = 18, base_family = "sans") +
    theme(
      legend.position = 'none', #"right",
      axis.text.x = element_text(size = 16, color = "black"),
      axis.text.y = element_text(size = 16, color = "black"),
      axis.title.x = element_text(face = "bold", size = 18, color = "black"),
      axis.title.y = element_text(face = "bold", size = 18, color = "black"),
      plot.title = element_text(hjust = 0.5, size = 18, color = "black", face = "bold"),
      strip.text = element_text(face = "bold",size = 16),  # 分面标签字体大小
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line = element_blank(),  # 移除轴线
      panel.border = element_rect(color = "grey60", fill = NA)  # 保留面板边框
    )
  
  # 添加坐标轴刻度
  p <- p + scale_x_continuous(
    breaks = seq(0, 100, by = 25),  # 内部刻度
    labels = seq(0, 100, by = 25)   # 内部刻度标签
  ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 25),  # 内部刻度
      labels = seq(0, 100, by = 25)   # 内部刻度标签
    )
  
  # 在外部标记坐标轴刻度
  p <- p + theme(
    axis.ticks.x = element_line(size = 0.4),
    axis.ticks.y = element_line(size = 0.4),
    axis.ticks.length = unit(0.1, "cm"),
    axis.ticks.x.top = element_blank(),  # 移除上面的刻度
    axis.ticks.y.right = element_blank()  # 移除右边的刻度
  )
  
  
  # 根据每个年份的相关系数添加 R 相关系数注释
  p <- p + geom_label(data = cor_result,
                      aes(x = Inf, y = -Inf, 
                          label = paste("National level\n",
                                        "N =", N,"\n",
                                        "R =", round(cor_coef, 3), "\n",
                                        "  RMSE =", round(RMSE, 3), "\n",
                                        "MAE =", round(MAE, 3))),
                      color = "black",#fill = "white",  # 如果需要，添加背景以增强可读性
                      hjust = 1.1, vjust = -0.1, size = 4,
                      inherit.aes = FALSE  # 避免使用主图中的aes映射
  )
  
  return(p)
  
}


# ------ 子图:A 国家精度 B 次国家精度 ------

# 创建两个子图
group_dataA = level0_data3 %>%  inner_join(IncomeGroup_clust, by="SOC") 
group_dataB = level1_data %>%  inner_join(IncomeGroup_clust, by="SOC")

# Reordering factor levels
group_dataA$Income <- factor(group_dataA$Income, levels = c("Low income", "Lower middle income", "Upper middle income","High income"))
group_dataB$Income <- factor(group_dataB$Income, levels = c("Low income", "Lower middle income", "Upper middle income","High income"))

str(group_dataA)
str(group_dataB)

# "National level"
plotA <- plot_func_combined_allyear(group_dataA, overall_results)
# leveltext="Sub-national level"
plotB <- plot_func_combined_allyear(group_dataB, overall_results2)

print(plotA)
print(plotB)


# 提取图例
legend <- get_legend(
  ggplot(group_dataA, aes(x = EAI_NTL, y = EAI_WB, color = Income)) +
    geom_point() +
    # scale_color_viridis(option = "A", name = "EAI (%)") +·
    scale_color_manual(values = c("#ff7477","#e69597", "#ceb5b7","#b5d6d6"),  name = "Income") +
    theme_minimal(base_size = 18, base_family = "sans") + 
    theme(legend.position = "right",
          legend.text = element_text(face = "bold", size = 16, color = "black"),
          legend.title = element_text(face = "bold", size = 16, color = "black"))
)

# 合并子图和图例
combined_plot <- plot_grid(
  plotA + labs(y = "EAI (Statistics)",x = "EAI (NTL)"), 
  plotB + labs(y = "EAI (Statistics)",x = "EAI (NTL)"),
  ncol = 2, align = "h" #, rel_heights = c(1, 1)
)


final_plot <- plot_grid(combined_plot, legend, ncol = 2, rel_widths = c(0.8, 0.2))
final_plot


# ------ 子图: C 无电人口 ------

# 创建两个子数据
group_dataC_NTL = level0_data1 %>%  inner_join(IncomeGroup_clust, by="SOC") 
group_dataC_WB = level0_data2 %>%  inner_join(IncomeGroup_clust, by="SOC")
colnames(group_dataC_NTL)
colnames(group_dataC_WB)

group_dataC_POP = pop_data %>%  inner_join(IncomeGroup_clust, by="SOC") 

# 分别创建
# 总结表sum_dataC_NTL,  列为：Year, Type, NoAccPop_NTL
# 总结表sum_dataC_WB,  列为：Year, Type, NoAccPop_WB
# Type包含不同Income，不同Region，不同state_3type以及不分类型的Total（命名为“World"） 
# 最终Type值（固定顺序）应该为"World",
#            "East Asia & Pacific", "Europe & Central Asia","South Asia",
#            "Latin America & Caribbean", "North America" ,
#            "Middle East & North Africa", "Sub-Saharan Africa" ,
#            "Low income", "Lower middle income", "Upper middle income","High income",

# 合并表sum_dataC_NTL和sum_dataC_NTL行
# 合并表sum_dataC，列为：Year, Type, NoAccPop, Population_Type
# Population_Type值为NTL和WB

# Step 1: Define the fixed Type order
type_order <- c(
  "World",
  "East Asia & Pacific", "Europe & Central Asia", "South Asia",
  "Latin America & Caribbean", "North America",
  "Middle East & North Africa", "Sub-Saharan Africa",
  "High income",  "Upper middle income", "Lower middle income", "Low income"
)

# Step 2: Create summarization function
summarize_data <- function(data, value_column, type_column) {
  bind_rows(
    data %>% group_by(Year) %>% 
      summarize({{ value_column }} := sum({{ value_column }}, na.rm = TRUE), Type = "World"),
    data %>% group_by(Year, {{ type_column }}) %>% 
      summarize({{ value_column }} := sum({{ value_column }}, na.rm = TRUE)) %>% 
      rename(Type = {{ type_column }})
  ) %>% 
    ungroup() %>% 
    mutate(Type = factor(Type, levels = type_order))
}

# Step 3: Summarize data for NoAccPop_NTL and NoAccPop_WB
sum_dataC_NTL <- summarize_data(group_dataC_NTL, NoAccPop_NTL, Region) %>% 
  bind_rows(summarize_data(group_dataC_NTL, NoAccPop_NTL, Income)) %>% 
  bind_rows(summarize_data(group_dataC_NTL, NoAccPop_NTL, state_3type))

sum_dataC_WB <- summarize_data(group_dataC_WB, NoAccPop_WB, Region) %>% 
  bind_rows(summarize_data(group_dataC_WB, NoAccPop_WB, Income)) %>% 
  bind_rows(summarize_data(group_dataC_WB, NoAccPop_WB, state_3type))


# Step 4: Combine the summarized data for NTL and WB
sum_dataC_NTL <- sum_dataC_NTL %>% mutate(Population_Type = "NTL")
sum_dataC_WB <- sum_dataC_WB %>% mutate(Population_Type = "WB")

sum_dataC <- bind_rows(
  sum_dataC_NTL %>% rename(NoAccPop = NoAccPop_NTL),
  sum_dataC_WB %>% rename(NoAccPop = NoAccPop_WB)
)

output_csv_path <- "F:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/table_fig5/全球和分区无电人口NTL和WB.csv"
write.csv(sum_dataC_plot, output_csv_path, row.names = FALSE)

output_csv_path <- "F:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/table_fig5/全球和分区-总人口.csv"
write.csv(sum_dataC_plot_POP, output_csv_path, row.names = FALSE)

# Step 5: Arrange by Year and Type
sum_dataC_plot <- sum_dataC %>% 
  arrange(Year, factor(Type, levels = type_order)) %>%
 filter(!is.na(Type))

# str(sum_dataC_plot)
# sum_dataC_plot %>% filter(is.na(Type))


sum_dataC_POP <- summarize_data(group_dataC_POP, POP, Region) %>% 
  bind_rows(summarize_data(group_dataC_POP, POP, Income)) %>% 
  bind_rows(summarize_data(group_dataC_POP, POP, state_3type))

sum_dataC_plot_POP <- sum_dataC_POP %>% 
  arrange(Year, factor(Type, levels = type_order)) %>%
  filter(!is.na(Type))


plotC = ggplot(sum_dataC_plot, aes(x = Year, y = NoAccPop)) +
  # 修改点图层，添加图例
  geom_point(data = sum_dataC_plot_POP, 
             aes(x = Year, y = POP, shape = "Total population"),
             color = "#c44601", fill = "#c44745", size = 2, alpha = 0.7) +
  geom_bar( aes(fill = Population_Type), 
            stat = "identity", position = "dodge") +
  facet_wrap(~ Type, scales = "free_y", ncol = 4) + 
  labs(title = "",
       x = "Year",
       y = "Population (Million)",
       fill = "Population without access to electricity",
       shape = "") +  # 空字符串移除形状图例标题
  theme_minimal(base_size = 16,base_family = "sans") +
  # 坐标轴标题、分面标题和图例全部字体加粗，16字号
  # 所有字体颜色黑色
  theme(    axis.title = element_text(face = "bold", size = 16, color = "black"),
            strip.text = element_text(face = "bold", size = 16, color = "black"),
            legend.text = element_text( face = "bold",size = 16, color = "black"),
            legend.title = element_text(face = "bold", size = 16, color = "black"),
            axis.text = element_text(color = 'black'),
            legend.position = "bottom",  # 将图例放在底部
            #legend.box = "vertical"      # 图例垂直排列
            legend.box = "horizontal"
            ) +
  # 设置x轴标签
  scale_x_continuous(breaks = c(1992, 2002, 2012, 2022)) +
  # 点的图例标注： "Total population - by World Population Prospect" 
  scale_shape_manual(
    values = c("Total population" = 21),  # 21 表示实心圆
    labels = c("Total population" = "Total population")
  )+
  # bar 的图例标注：
  # "Population with no access to electricity - by NTL"
  # "Population with no access to electricity - by World Bank"
  scale_fill_manual(values = c("NTL" = "#ff7477", "WB" = "#5dace0"),
                    labels = c(
                      "NTL" = "NTL",
                      "WB" = "Statistics")
  )+
  # 格式化y轴标签,单位是百万
  scale_y_continuous(
    labels = function(x) paste0(x / 1e6, "M") # Format y-axis labels in millions with 'M' prefix
  )

plotC 

# ------ 合并plot ------



# Add (a) label to final_plot
labeled_final_plot <- ggdraw(final_plot) +
  draw_label("(a)", x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 18)

# Add (b) label to plotC
labeled_plotC <- ggdraw(plotC) +
  draw_label("(b)", x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 18)

# Combine the labeled plots vertically
final_combined_plot <- plot_grid(
  labeled_final_plot,
  labeled_plotC,
  ncol = 1,
  rel_heights = c(0.4, 0.6)
)

# Display the final plot
final_combined_plot



# ------ Save as PDF ------


# Set the output file path and filename

plot_D_pdf <- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Figure-5-Comparison/EAI_accuracallyear_NoAccPOP.pdf"
ggsave(plot_D_pdf, final_combined_plot, width = 15, height = 14)


# Set the output file path and filename
plot_D_jpg<- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Figure-5-Comparison/EAI_accuracallyear_NoAccPOP.jpg"

# Save the plot as a high-resolution (400 DPI) JPG
ggsave(filename = plot_D_jpg, plot = final_combined_plot, 
       device = "jpg", dpi = 400, width = 15, height = 14, units = "in")



# ------ Fig S6： Annual acuuracy L0 ------

plot_func_combined <- function(data, cor_results) {
  p <- ggplot(data, aes(x = EAI_NTL, y = EAI_WB)) +
    geom_point(size = 2, aes(color = SOC), alpha = 0.9) +  # 固定点大小，颜色由 SOC 决定
    geom_smooth(method = "lm", se = TRUE, color = "black", fill = "blue", level = 0.95) +  # 黑色拟合线，天蓝色置信区间
    facet_wrap(~ Year, ncol = 5) +  # 按年份分面
    labs(y = "EAI (Statistics)",x = "EAI (NTL)")+  # 图的x和y轴标题
    # 新增图例格式设置
    guides(
      color = guide_legend(
        ncol =30,                  # 设置3列图例
        keywidth = unit(0.5, "cm"),  # 减小色块宽度
        title.position = "top",      # 标题位置
        label.position = "bottom"    # 标签位置
      )
    ) +
    theme_minimal(base_size = 18, base_family = "sans") +
    theme(
      legend.position = 'bottom', #"right",
      legend.direction = "horizontal",  # 水平方向
      legend.text = element_text(size = 8),  # 减小标签字体
      legend.key.size = unit(0.3, "cm"),     # 减小色块尺寸
      legend.spacing.x = unit(0.2, "cm"),    # 列间距
      legend.box.margin = margin(t = 1, b = 1),  # 图例边距
      
      axis.text.x = element_text(size = 16, color = "black"),
      axis.text.y = element_text(size = 16, color = "black"),
      axis.title.x = element_text(face = "bold", size = 18, color = "black"),
      axis.title.y = element_text(face = "bold", size = 18, color = "black"),
      plot.title = element_text(hjust = 0.5, size = 18, color = "black", face = "bold"),
      strip.text = element_text(face = "bold",size = 16),  # 分面标签字体大小
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line = element_blank(),  # 移除轴线
      panel.border = element_rect(color = "grey60", fill = NA)  # 保留面板边框
      # panel.border = element_blank(),        # 移除面板边框
      # axis.line = element_line(color = "black")  # 保留轴线
    )
  
  # 添加坐标轴刻度
  p <- p + scale_x_continuous(
    breaks = seq(0, 100, by = 50),  # 内部刻度
    labels = seq(0, 100, by = 50)   # 内部刻度标签
  ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 50),  # 内部刻度
      labels = seq(0, 100, by = 50)   # 内部刻度标签
    )
  
  # 在外部标记坐标轴刻度
  p <- p + theme(
    axis.ticks.x = element_line(size = 0.4),
    axis.ticks.y = element_line(size = 0.4),
    axis.ticks.length = unit(0.1, "cm"),
    axis.ticks.x.top = element_blank(),  # 移除上面的刻度
    axis.ticks.y.right = element_blank()  # 移除右边的刻度
  )
  
  
  
  # 根据每个年份的相关系数添加 R 相关系数注释
  p <- p + geom_label(data = cor_results,
                      aes(x = Inf, y = -Inf, 
                          label = paste("N =", N,"\n",
                                        "R =", round(cor_coef, 2), "\n",
                                        "  RMSE =", round(RMSE, 2), "\n",
                                        "MAE =", round(MAE, 2))),
                      color = "black",#fill = "white",  # 如果需要，添加背景以增强可读性
                      hjust = 1.1, vjust = -0.1, size = 2.5,
                      inherit.aes = FALSE  # 避免使用主图中的aes映射
  )
  
  return(p)
  
}

# "National level"
plotA_SI <- plot_func_combined(group_dataA, cor_results)

print(plotA_SI)

# ------ Save  as PDF ------


# Set the output file path and filename

plotA_SI_pdf <- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Figure-5-Comparison/FIg_SI_EAI_accuracy_L0_perYear.pdf"
ggsave(plotA_SI_pdf, plotA_SI, width = 12, height = 16)


# Set the output file path and filename
plotA_SI_jpg<- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Figure-5-Comparison/FIg_SI_EAI_accuracy_L0_perYear.jpg"

# Save the plot as a high-resolution (400 DPI) JPG
ggsave(filename = plotA_SI_jpg, plot = plotA_SI, 
       device = "jpg", dpi = 400, width = 12, height = 16, units = "in")



# ------ Fig S7： Annual acuuracy L1 ------

# "Sub-national level"
plotB_SI <- plot_func_combined(group_dataB, cor_results2)

print(plotB_SI)

# ------ Save  as PDF ------


# Set the output file path and filename

plotB_SI_pdf <- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Figure-5-Comparison/FIg_SI_EAI_accuracy_L1_perYear.pdf"
ggsave(plotB_SI_pdf, plotB_SI, width = 12, height = 14)


# Set the output file path and filename
plotB_SI_jpg<- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Figure-5-Comparison/FIg_SI_EAI_accuracy_L1_perYear.jpg"

# Save the plot as a high-resolution (400 DPI) JPG
ggsave(filename = plotB_SI_jpg, plot = plotB_SI, 
       device = "jpg", dpi = 400, width = 12, height = 14, units = "in")
