library(ggrepel)
library(ggplot2)
library(readr)
library(tidyverse)

set.seed(42)


path = "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/table_fig3_heatmap/发电厂叠加EAI和POP-tidy.csv"

df_all = read_csv(path)



# Rows: 160 Columns: 14 
# chr (6): country, NAME, SOC, Group, Region, Income
# dbl (8): PowerPlantCount, capacityMW_Sum, Year, EAI_NTL, POP_1k, AccPop_NTL, NoAccPop_NTL, Per_Capita_Capacity

df_all <- df_all %>%
  mutate(Income = factor(Income, 
                         levels = c("High income", 
                                    "Upper middle income", 
                                    "Lower middle income", 


library(ggplot2)
library(ggrepel)

# ------ plotA ------
plot_A = ggplot(df_all, aes(PowerPlantCount, EAI_NTL)) +
  # geom_point(aes(size = NoAccPop_NTL),color = "#78ABA8", alpha = 0.8) +
  geom_point(aes(color = Income,size = POP_1k *1000), alpha = 0.7) +
  
  
  geom_text_repel(data = df ,aes(x=PowerPlantCount, y=EAI_NTL,label = SOC),
                  max.overlaps = 100,  size = 4.5,  # 缩小字体 ,增加允许的最大重叠标签数量
                  min.segment.length = 0,
                  xlim = c(0, stats_summary[1,]$Median_Plants),  # X轴的标签限制
                  ylim = c(0, stats_summary[1,]$Median_EAI_NTL), # Y轴的标签限制
                  box.padding = 0.5, point.padding = 0.3, # 增加点与标签的距离
                  show.legend = FALSE,
                  nudge_x = .15,nudge_y = 1,
                  segment.curvature = 0.1,segment.ncp = 3, segment.angle = 20) +
  scale_color_manual(values = c("High income" = "#ae017e",
                                "Upper middle income" = "#f768a1",
                                "Lower middle income" = "#fa9fb5",
                                "Low income" = "#fcc5c0")) +
  scale_size(range = c(4, 12),
             name = "Population"  ) + # 添加换行的图例标题
  scale_x_log10() +
  ylim(0, 100) +  # 设置 Y 轴范围为 0 到 100
  labs(x = "Log10(Number of power plants)", 
       y = "EAI (%)", 
       title = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        legend.title = element_text(size = 18, face = "bold", color = "black"),
        legend.text = element_text(size = 16, face = "bold", color = "black"),  # 图例字体
        axis.text = element_text(size = 16, face = "bold", color = "black"),    # 坐标轴字体
        axis.title = element_text(size = 18, face = "bold", color = "black"),   # 坐标轴标题字体
  )





# ------ plotB ------

plot_B = ggplot(df_all, aes(Per_Capita_Capacity_kW, EAI_NTL)) +
  geom_point(aes(color = Income,size = POP_1k *1000 ), alpha = 0.7) +
  
  geom_text_repel(data = df ,aes(x=Per_Capita_Capacity_kW, y=EAI_NTL,label = SOC),
                  max.overlaps = 200,  size = 4.5,  # 缩小字体 ,增加允许的最大重叠标签数量
                  min.segment.length = 0,
                  #xlim = c(0, stats_summary[1,]$Median_Capacity),  # X轴的标签限制
                  #ylim = c(0, stats_summary[1,]$Median_EAI_NTL), # Y轴的标签限制
                  box.padding = 0.5, point.padding = 0.3, # 增加点与标签的距离
                  show.legend = FALSE,
                  nudge_x = .15,nudge_y = 1,
                  segment.curvature = 0.1,segment.ncp = 3, segment.angle = 20) +
  scale_color_manual(values = c("High income" = "#ae017e",
                                "Upper middle income" = "#f768a1",
                                "Lower middle income" = "#fa9fb5",
                                "Low income" = "#fcc5c0")) +
  scale_size(range = c(4, 12),
             name = "Population"  ) + # 添加换行的图例标题
  scale_x_log10() +
  ylim(0, 100) +  # 设置 Y 轴范围为 0 到 100
  labs(x = "Log10(Per Capita Generation Capacity (kW) )", 
       y = "EAI (%)", 
       title = NULL,) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        legend.title = element_text(size = 18, face = "bold", color = "black"),
        legend.text = element_text(size = 16, face = "bold", color = "black"),  # 图例字体
        axis.text = element_text(size = 16, face = "bold", color = "black"),    # 坐标轴字体
        axis.title = element_text(size = 18, face = "bold", color = "black"),   # 坐标轴标题字体
  )

# ------ plotC ------

plot_C = ggplot(df, aes(x = reorder(country, NoAccPop_NTL), y = NoAccPop_NTL, fill=Income)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Lower middle income" = "#fa9fb5",
                               "Low income" = "#fcc5c0")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))+
  labs(x = NULL, y = "2022 Population without access to electricity (Million)", 
       title = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        legend.title = element_text(size = 18, face = "bold", color = "black"),
        legend.text = element_text(size = 16, face = "bold", color = "black"),  # 图例字体
        axis.text = element_text(size = 16, face = "bold", color = "black"),    # 坐标轴字体
        axis.title = element_text(size = 18, face = "bold", color = "black"))

# ------ 图例图层  ------

library(ggplot2)
library(cowplot)
library(ggrepel)
library(patchwork)

# 定义通用主题和颜色方案
common_theme <- theme_minimal(base_size = 16) +
  theme(
    legend.title = element_text(size = 18, face = "bold", color = "black"),
    legend.text = element_text(size = 16, face = "bold", color = "black"),
    axis.text = element_text(size = 16, face = "bold", color = "black"),
    axis.title = element_text(size = 18, face = "bold", color = "black"),
    legend.position = "none"
  )

income_colors <- c(
  "High income" = "#ae017e",
  "Upper middle income" = "#f768a1",
  "Lower middle income" = "#fa9fb5",
  "Low income" = "#fcc5c0"
)

# # 创建图例图层
# legend_plot <- ggplot(df_all, aes(Per_Capita_Capacity_kW, EAI_NTL)) +
#   geom_point(aes(color = Income, size = POP_1k * 1000), alpha = 0.8) +
#   scale_color_manual(values = income_colors) +
#   scale_size(range = c(2, 8), name = "Population") +
#   theme_minimal() +
#   theme(
#     legend.position = "right",
#     legend.title = element_text(size = 18, face = "bold", color = "black"),
#     legend.text = element_text(size = 16, face = "bold", color = "black")
#   ) +
#   labs(x = NULL, y = NULL)
# 

# ------ all  ------

#  创建最终组合图:plot_A + plot_B + plot_C + 图例

# # Extract the legend from the legend_plot
# legend <- cowplot::get_legend(
#   plot_B +
#     theme(
#       legend.position = "right",
#       legend.title = element_text(size = 18, face = "bold", color = "black"),
#       legend.text = element_text(size = 16, face = "bold", color = "black"),
#       legend.box.margin = margin(0, 0, 0, 12)
#     )
# )

# ---- 添加序号 ------


# 修改 plot_A，添加副标题
plot_A <- plot_A +
  labs(subtitle = "(a)") +
  theme(
    plot.subtitle = element_text(
      size = 18, face = "bold", 
      color = "black", hjust = 0,
      margin = margin(b = 10)
    )
  )

# 修改 plot_B，添加副标题
plot_B <- plot_B +
  labs(subtitle = "(b)") +
  theme(
    plot.subtitle = element_text(
      size = 18, face = "bold", 
      color = "black", hjust = 0,
      margin = margin(b = 10)
    )
  )

# 修改 plot_C，添加副标题
plot_C <- plot_C +
  labs(subtitle = "(c)") +
  theme(
    plot.subtitle = element_text(
      size = 18, face = "bold", 
      color = "black", hjust = 0,
      margin = margin(b = 10)
    )
  )


# ------ 添加边缘密度图 ------

library(ggExtra)

new_plotA <- ggMarginal(
  plot_A,type = "boxplot",
  groupColour = TRUE, groupFill = TRUE, size = 10,
  margins = "x",  alpha = 0.3
)


new_plotB <- ggMarginal(
  plot_B,type = "boxplot",
  groupColour = TRUE, groupFill = TRUE, size = 10,
  margins = "both",  alpha = 0.3
)

new_plotB

# 组合最终图表
final_plot <- plot_grid(
  new_plotA, new_plotB, plot_C, legend,
  nrow = 1,
  rel_widths = c(3, 3, 2, 1),
  align = 'h',
  axis = 'tb'
)





# 创建共享图例
legend <- get_legend(
  plot_B +
    theme(
      legend.position = "right",
      legend.box.spacing = unit(1, "cm"),
      legend.key.size = unit(1, "cm"),
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 16)
    )
)





# 保存图表
fig_path = "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/Figure-3-Heatmap/"

ggsave(paste0(fig_path, "Fig4_Scatterplot_HighGrowth_EAI.jpg"),
       final_plot, width = 25, height = 8, dpi = 400)

ggsave(paste0(fig_path, "Fig4_Scatterplot_HighGrowth_EAI.pdf"),
       final_plot, width = 25, height = 8)







# ------ 保存高增加国家SOC和MAp------

# high_growth_countries <- eai_growth_quantile_order_clust %>%
#   filter(state_3type == 3 & EAI_Growth_5yr > 20 ) %>%
#   filter( Income == "Lower middle income" |  Income == "Low income" ) %>%
#   select(SOC, Income, Period, EAI_Growth_5yr) 

# View(high_growth_countries)
# selected <- unique(high_growth_countries $SOC)
# # [1] "AGO" "BEN" "BTN" "CMR" "COM" "HTI" "KEN" "KHM" "KIR" "LAO" "LSO" "MMR" "MRT" "NPL" "PNG" "SLB" "TLS" "TZA" "VUT" "AFG"
# # [21] "BDI" "BFA" "CAF" "COD" "ERI" "ETH" "GIN" "GMB" "GNB" "LBR" "MDG" "MLI" "MOZ" "MWI" "NER" "PRK" "RWA" "SDN" "SLE" "SOM"
# # [41] "TCD" "TGO" "UGA" "ZMB"

# # File paths
# shp_path <- "D:\\Data_Projects\\Proj_EAI_2024\\InputData\\Inputs_Bound_wgs84\\Globe_Country_simple_region.shp"
# shp_data <- st_read(shp_path)

# shp_data_high_growth_countries = shp_data %>%
#   filter(SOC %in% selected) %>%
#   inner_join(df, by = "SOC") %>%
#   select(-c(NAME,Region.y,country))

# head(shp_data_high_growth_countries)
# colnames(shp_data_high_growth_countries)

# selected_high <- unique(shp_data_high_growth_countries $SOC)
# selected_high

# # Save the  shapefile
# output_path <- "D:\\Data_Projects\\Proj_EAI_2024\\InputData\\Inputs_Bound_wgs84\\high_growth_countries.shp"
# st_write( shp_data_high_growth_countries, output_path, append = FALSE )
