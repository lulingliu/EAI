# ************************************************************************************

# code：F:/Data_Projects/Proj_EAI_2024/Code/Code_2024/Thesis_code/Fig1_EAI_stat.R

# tif
# 0.1度的EAI，值范围是0-100

# table1
# 1992-2022年不同收入水平国家内，像元尺度用电人口比例EAI（0-100）不同区间的百分比变化
# 负数代表从原来的EAI减少到现在的EAI
# 正数代表从原来的EAI增加到现在的EAI

# table2
# 1992-2022年不同大洲的国家内部，像元尺度用电人口比例EAI（0-100）不同区间的百分比变化
# 负数代表从原来的EAI减少到现在的EAI
# 正数代表从原来的EAI增加到现在的EAI

# 绘制地图
# 上面是1992年的EAI，下面是2022年的EAI
# 中间是两个table的条形图
# EAI采用渐变色


# 修改： 2024-10-19
# 只保留统计的图，颜色做顺眼一点


# **************************************************************************************

# ------ load package ------

# 加载必要的库
library(terra)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(RColorBrewer)
library(sf)
library(wesanderson)

# 设置文件路径
tif_path <- 'F:/Data_Projects/Proj_EAI_2024/Process/R14C36_EAI/'
tif1_path <- paste0(tif_path, 'R14C36_EAI_1992.tif')
tif2_path <- paste0(tif_path, 'R14C36_EAI_2022.tif')

# 读取TIFF文件
eai_1992 <- rast(tif1_path)
eai_2022 <- rast(tif2_path)

# 将Raster数据转换为数据框，并处理NA值
eai_1992_df <- as.data.frame(eai_1992, xy = TRUE, na.rm = TRUE)
colnames(eai_1992_df) <- c("x", "y", "EAI_1992")

eai_2022_df <- as.data.frame(eai_2022, xy = TRUE, na.rm = TRUE)
colnames(eai_2022_df) <- c("x", "y", "EAI_2022")

# 定义颜色渐变（根据数据范围动态调整）
all_eai_values <- c(eai_1992_df$EAI_1992, eai_2022_df$EAI_2022)
eai_min <- min(all_eai_values, na.rm = TRUE)
eai_max <- max(all_eai_values, na.rm = TRUE)
# eai_colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(100)
# 反向渐变
eai_colors <- colorRampPalette(rev(brewer.pal(9, "YlGnBu")))(100)


# 绘制1992年EAI地图
map_1992 <- ggplot() +
  geom_raster(data = eai_1992_df, aes(x = x, y = y, fill = EAI_1992)) +
  scale_fill_gradientn(colors = eai_colors, 
                       limits = c(eai_min, eai_max), 
                       name = "EAI 1992") +
  theme_minimal() +
  coord_fixed() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# 绘制2022年EAI地图
map_2022 <- ggplot() +
  geom_raster(data = eai_2022_df, aes(x = x, y = y, fill = EAI_2022)) +
  scale_fill_gradientn(colors = eai_colors, 
                       limits = c(eai_min, eai_max), 
                       name = "EAI 2022") +
  theme_minimal() +
  coord_fixed() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# ------ table data -------

# 表格数据1
table1 <- data.frame(
  Income = c("High income", "Low income", "Lower middle income", "Upper middle income"),
  Percent_0_25 = c(-37.46, -5.96, -29.85, -41.97),
  Percent_26_50 = c(3.39, 1.86, 3.53, 5.20),
  Percent_51_75 = c(5.25, 1.66, 4.92, 7.23),
  Percent_76_100 = c(35.10, 3.19, 21.71, 30.06)
)
# Reordering factor levels
table1$Income <- factor(table1$Income, levels = c("Low income", "Lower middle income", "Upper middle income","High income"))



# 表格数据2
table2 <- data.frame(
  Region = c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", 
             "Middle East & North Africa", "North America", "South Asia", "Sub-Saharan Africa"),
  Percent_0_25 = c(-30.73, -45.51, -38.80, -45.24, -10.25, -46.02, -32.85),
  Percent_26_50 = c(-1.13, 3.93, 5.68, 4.70, 2.94, 3.03, 5.94),
  Percent_51_75 = c(2.44, 7.02, 7.03, 5.67, 2.55, 7.11, 6.07),
  Percent_76_100 = c(29.46, 34.63, 26.16, 35.29, 5.34, 42.19, 20.99)
)
# Reordering factor levels
table2$Region <- factor(table2$Region, levels = 
                          c("East Asia & Pacific", "Europe & Central Asia", "South Asia", 
                            "Latin America & Caribbean", "North America", 
                            "Middle East & North Africa","Sub-Saharan Africa"))


# 转换为长格式
table1_long <- pivot_longer(table1, cols = starts_with("Percent"), 
                            names_to = "EAI_Interval", values_to = "Percent_change")

table2_long <- pivot_longer(table2, cols = starts_with("Percent"), 
                            names_to = "EAI_Interval", values_to = "Percent_change")

# Update EAI_Interval values for custom labels
table1_long$EAI_Interval <- factor(table1_long$EAI_Interval,
                                   levels = c("Percent_0_25", "Percent_26_50", "Percent_51_75", "Percent_76_100"),
                                   labels = c("[0 - 25%]", "[26% - 50%]", "[51% - 75%]", "[76% - 100%]"))
table2_long$EAI_Interval <- factor(table2_long$EAI_Interval,
                                   levels = c("Percent_0_25", "Percent_26_50", "Percent_51_75", "Percent_76_100"),
                                   labels = c("[0 - 25%]", "[26% - 50%]", "[51% - 75%]", "[76% - 100%]"))


# ------- plot bar -------

# 绘制表格1条形图
bar1 <- ggplot(table1_long, aes(x = EAI_Interval, y = Percent_change, fill = Income)) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_fill_manual(values = c(
  #   "#ffa69e",  # Color for "High income"
  #   "#6c8dfa",  # Color for "Low income"
  #   "#ffc09f", # Color for "Lower middle income"
  #   "#95b8d1"# Color for "Upper middle income"
  # )) +
  scale_fill_manual(values = c("#ff7477","#e69597", "#ceb5b7","#b5d6d6")) +
  theme_minimal() +
  labs(title = "",  x = "EAI Intervals", y = "Change of EAI Pixel Percentage (%)") +
  ylim(-50,50)+
  theme(axis.text = element_text(color ='black',size = 24, face = "bold"),
        axis.title = element_text(size = 24, face = "bold"),      
        legend.text = element_text(size = 24, face = "bold"),      
        legend.title = element_blank(),
        legend.position = c(0.98, 0.2),  # place in top-left   
        legend.justification = c(0.98,0.2),  # Legend points to its top-left corner
        legend.background = element_rect(fill = "white",color = "grey")  # Set legend background color to white
  )
bar1

# 绘制表格2条形图
bar2 <- ggplot(table2_long, aes(x = EAI_Interval, y = Percent_change, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  # scale_fill_brewer(palette = "Paired") +
  # scale_fill_manual(values = wes_palette("Zissou1",n=7 ))+
  # scale_fill_manual(values = c("#6c8dfa","#a7bed3", "#c6e2e9", "#ffee93","#f6ac69", "#ffcaaf", "#f49097")) + # 彩虹渐变
  # scale_fill_manual(values = c("#ff7477", "#ffaaaa","#e69597","#ceb5b7", "#b5d6d6","#b2e2e2","#9cf6f6"))+ # 红蓝渐变
  # scale_fill_manual(values = c("#a7bed3", "#b0cde3", "#c6e2e9", "#d0eff0", "#f1ffc4", "#f5ffb0", "#ffcaaf"))+# 莫兰蒂色带
  # scale_fill_manual(values = c("#7a9bb2", "#8aaed4", "#a4c3d2", "#b4d6d8", "#d0d68e", "#d1d64f", "#e9a79d"))+# 莫兰蒂色带
  # scale_fill_manual(values = c("#7a9bb2", "#8aaed4", "#a4c3d2", "#b4d6d8", "#d1a8a1", "#e9a79d", "#ffb0b0"))+
  scale_fill_manual(values = c("#6a9bcf", "#7cb2e5", "#8fcbf3", "#e0d96c", "#ffee93", "#e9a79d", "#ffcaaf"))+
  theme_minimal() +
  labs(title = "", x = "EAI Intervals", y = "Change of EAI Pixel Percentage (%)") +
  ylim(-50,50)+
  theme(axis.text = element_text(color ='black',size = 24, face = "bold"),
        axis.title = element_text(size = 24, face = "bold"),      
        legend.text = element_text(size = 24, face = "bold"),      
        legend.title = element_blank(),
        legend.position = c(0.98, 0.05),  # place in top-left   
        legend.justification = c(0.98,0.05),  # Legend points to its top-left corner
        legend.background = element_rect(fill = "white",color = "grey")  # Set legend background color to white
        )
bar2



# 组合条形图
bars_combined <- grid.arrange(bar1, bar2, ncol = 2)
# bars_combined <- arrangeGrob(bar1, bar2, ncol = 1)

# 调整最终组合图的布局：map_1992在顶部，bars_combined在中间，map_2022在底部
final_plot <- grid.arrange(
  map_1992,
  bars_combined,
  map_2022,
  nrow = 3,
  heights = c(2, 1, 2) # 调整各部分高度比例
)

# 显示图形
print(final_plot)

library(cowplot)
# 组合图形
final_plot <- plot_grid(
  map_1992,
  bars_combined,
  map_2022,
  ncol = 1,
  rel_heights = c(2, 1, 2))

# 显示图形
print(final_plot)
print(bars_combined)
bars_combined

# Set the output file path and filename
output_file <- "F:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Fig1_EAI_stat_nomap.jpg"

# Save the plot as a high-resolution (400 DPI) JPG
ggsave(filename = output_file, plot = final_plot, 
       device = "jpg", dpi = 400, width = 12, height = 20, units = "in")

ggsave(filename = output_file, plot = bars_combined, 
       device = "jpg", dpi = 400, width = 12, height = 8, units = "in")







# ------ save as pdf ------


# Set the output file path and filename
bar1_file<- "F:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Fig1_EAI_stat_income.pdf"

# Save the plot as a high-resolution PDF
ggsave(filename = bar1_file, plot = bar1 , 
       device = "pdf", width = 10, height = 7, units = "in")


# Set the output file path and filename
bar2_file<- "F:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Fig1_EAI_stat_region.pdf"

# Save the plot as a high-resolution PDF
ggsave(filename = bar2_file, plot = bar2 , 
       device = "pdf", width = 10, height = 7, units = "in")

