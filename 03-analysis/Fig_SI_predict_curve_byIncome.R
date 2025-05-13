library(tidyverse)

data_path = "D:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_table/Final_EAI_Predictions.csv"
data = read_csv(data_path)
# Rows: 9964 Columns: 4  
# chr (2): SOC, Type
# dbl (2): Year, EAI_NTL  

unique(data$Type) # "Origin" "ARIMA"  "SDG7"  
length(unique(data$SOC)) # 212

IncomeGroup_clust_path <- "D:/Data_Projects/Proj_EAI_2024/InputData/Inputs_Bound_wgs84/Globe_IncomeGroup_clust_212.csv"
Group_data <- read.csv(IncomeGroup_clust_path)
# [212 × 5]
# chr (5): Name, SOC, Region,  Income, state_3type

class_data = data %>% inner_join(Group_data, by="SOC")
# Rows: 9964 Columns: 8  
# chr (2): SOC, Type, Name,  Region,  Income, state_3type
# dbl (2): Year, EAI_NTL 


output_dir <- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V3/SI_Figure/"


# Original data with Type = 'Origin'
origin_data <- class_data %>%   filter(Type = 'Origin')
  

# Plot
combined_plot <- ggplot(class_data, aes(x = Year, y = EAI_NTL, color = Type)) +
    geom_line(size = 1) +
    geom_point(data = origin_data, aes(x = Year, y = EAI_NTL), color = "black") +
    scale_color_manual(values = c("Origin" = "black", "ARIMA" = "green", "SDG7" = "blue")) +
    labs(
      title = NULL,
      x = "Year",
      y = "EAI (%)",
      color = "Prediction Type"
    ) +
    theme_minimal() +
    ylim(0, 100)

  
# Save the plot
ggsave(
    filename = paste0(output_dir, income, "_Predict.jpg"),
    plot = combined_plot,
    width = 8,
    height = 6,
    dpi = 400
  )



# ------ 折线图-by income,保存 ------

# 确保输入目录和必要的库加载
library(dplyr)
library(ggplot2)

output_dir <- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V3/SI_Figure/"

# 按 Income 分组并绘制带分面的图
for (income in unique(class_data$Income)) {
  # 筛选出当前 Income 的数据
  income_data <- class_data %>%
    filter(Income == income)
  
  # 提取 Origin 数据用于标记
  origin_data <- income_data %>%
    filter(Type == "Origin")
  
  # 创建带分面的图
  combined_plot <- ggplot(income_data, aes(x = Year, y = EAI_NTL, color = Type, group = Type)) +
    geom_line( data= income_data %>% 
                 mutate(name2 = SOC)%>% 
                 dplyr::select(-SOC), 
               aes(group = name2), 
               color="grey", size=0.5, alpha=0.5) +
    geom_line( aes(color = Type, group = Type), size = 1) +  # 绘制折线
    #geom_point(data = origin_data, aes(x = Year, y = EAI_NTL), color = "black", size = 2) +  # 绘制 Origin 的点
    scale_color_manual(
      values = c(
        "Origin" = "#7469B6", # label as "Origin"
        "ARIMA" = "#AFD198", # label as "ARIMA"
        "SDG7" = "#F6995C" # label as "LNOB"
      )
    ) +
    labs(
      title = NULL,
      x = "Year",
      y = "EAI (%)",
      color = "Prediction Type"
    ) +
    facet_wrap(~ SOC, scales = "free_y") +  # 根据 SOC 创建分面
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 14),  # 分面标签样式
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    ) +
    ylim(0, 100)
  
  # 保存图像
  ggsave(
    filename = paste0(output_dir, "SI_Predict_", income, ".jpg"),
    plot = combined_plot,
    width = 12,
    height = 10,
    dpi = 400
  )
}

# ------ 折线图-by pace,保存 ------

# 确保输入目录和必要的库加载
library(dplyr)
library(ggplot2)

output_dir <- "D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V3/SI_Figure/"

# 按 pace 分组并绘制带分面的图
for (pace in unique(class_data$state_3type)) {
  
  # 筛选出当前 pace 的数据
  pace_data <- class_data %>%
    filter(SOC != 'GIB' & SOC != 'MCO' ) %>%
    filter(SOC != 'MHL' & SOC != 'GRL' ) %>%
    filter(state_3type == pace)
  
  # # 提取分面数量
  # num_facets <- pace_data %>%
  #   pull(SOC) %>%
  #   unique() %>%
  #   length()
  # 
  # # 动态调整图像尺寸
  # # 每行 8 个分面，每个分面宽 2，高 2
  # num_rows <- ceiling(num_facets / 8)  # 计算行数
  # width <- 8 * 1.5  # 每行宽度 = 分面数 * 单分面宽度
  # height <- num_rows * 1.5  # 高度 = 行数 * 单分面高度
  
  
  # 创建带分面的图
  combined_plot <- ggplot(pace_data, aes(x = Year, y = EAI_NTL, color = Type, group = Type)) +
    geom_point( data= pace_data %>% 
                 mutate(name2 = SOC)%>% 
                 filter(Year < 2023  )%>%
                 dplyr::select(-SOC), 
               aes(group = name2), 
               color="grey", size=0.5, alpha=0.5) +
    geom_line( aes(color = Type, group = Type), size = 1) +  # 绘制折线
    scale_color_manual(
      values = c(
        "Origin" = "#7469B6", # label as "Origin"
        "ARIMA" = "#AFD198", # label as "ARIMA"
        "SDG7" = "#F6995C" # label as "LNOB"
      ),
      labels = c(
        "Origin" = "Origin",
        "ARIMA" = "ARIMA",
        "SDG7" = "LNOB"
      ),
      breaks = c("Origin", "ARIMA", "SDG7")  # 设置图例顺序
    ) +
    labs(
      title = NULL,
      x = "Year",
      y = "EAI (%)",
      color = NULL,
    ) +
    scale_x_continuous(
      limits = c(1990, 2030),
      breaks = c(1992, 2022)  # 设置具体的x轴刻度
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = c(0, 50, 100)        # 设置具体的y轴刻度
    ) +
    facet_wrap(~ SOC) +  # 根据 SOC 创建分面
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 16),  # 分面标签样式
      axis.title = element_text(size = 16, color = "black"),
      axis.text.x = element_text(size = 14, color = "black"),
      axis.text.y = element_blank(),  # 默认不显示 y 轴标注
      legend.title = element_text(size = 16, color = "black"),
      legend.text = element_text(size = 16, color = "black"),
      panel.spacing = unit(0.5, "lines")  # 调整分面间距
     ) 
    # ylim(0, 100)+
    # xlim(1990,2030)
  
  # 为最左侧的分面添加 y 轴标注
  combined_plot <- combined_plot + 
    theme(
      axis.text.y.left = element_text(size = 14, color = "black"),
      strip.placement = "outside"  # 分面标题放置在外部
    )
  
  # 根据 Income 设置图像宽度和高度
  plot_width <- ifelse(pace == "Slow pace", 22, 
                       ifelse(pace == "Fast pace", 14, 16))
  plot_height <- ifelse(pace == "Slow pace", 18, 
                        ifelse(pace == "Fast pace", 12, 14))
  
  
  
  
  # 保存图像
  ggsave(
    filename = paste0(output_dir, "SI_Predict_", pace, ".jpg"),
    plot = combined_plot,
    width = plot_width,
    height = plot_height,
    dpi = 400
  )
  

  ggsave(filename = paste0("D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V3/SI_Figure_PDF/", 
                           "SI_Predict_", pace, ".pdf"), 
         plot = combined_plot, 
         width = plot_width,
         height = plot_height
  )
  
}
