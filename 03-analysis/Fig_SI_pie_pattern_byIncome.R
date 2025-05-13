library(dplyr)
library(ggplot2)


IncomeGroup_clust_path <- "D:/Data_Projects/Proj_EAI_2024/InputData/Inputs_Bound_wgs84/Globe_IncomeGroup_clust_212.csv"
Group_data <- read.csv(IncomeGroup_clust_path)
# [212 × 5]
# chr (5): Name, SOC, Region,  Income, state_3type

table_path = 'D:/Data_Projects/Proj_EAI_2024/Table/EAI/Analysis/EAI_HotSpot_wgs84New_stat_SOC_Updated.xlsx'
table = read_excel(table_path)
# [1,947 × 6]
# dbl (4): ELEMID, Category, Count, Percent
# chr (2): SOC, Pattern 

colnames(table)
head(table)
unique(table$Pattern)

# 合并表格
pattern_group_data = table %>% inner_join(Group_data, by="SOC")


# =========================== 每个 SOC 绘制不同 Pattern 的 Percent 饼图  =================================

# Define the pattern levels in the desired order
pattern_levels <- c(
  "new hot spot", 
  "consecutive hot spot", 
  "intensifying hot spot", 
  "persistent hot spot", 
  "diminishing hot spot", 
  "sporadic hot spot", 
  "oscillating hot spot", 
  "historical hot spot", 
  "no pattern detected", 
  "new cold spot", 
  "consecutive cold spot", 
  "intensifying cold spot", 
  "persistent cold spot", 
  "diminishing cold spot", 
  "sporadic cold spot", 
  "oscillating cold spot", 
  "historical cold spot"
)

# Update the 'Pattern' column to be a factor with the specified levels
pattern_group_data$Pattern <- factor(pattern_group_data$Pattern, levels = pattern_levels)

# 定义颜色映射
color_mapping <- c(
  "new cold spot" = "#084594", 
  "consecutive cold spot" = "#2171b5", 
  "intensifying cold spot" = "#4292c6",  
  "persistent cold spot" = "#3288BD",  
  "diminishing cold spot" = "#6baed6",  
  "sporadic cold spot" = "#9ecae1",  # 
  "oscillating cold spot" = "#c6dbef",  
  "historical cold spot" = "#deebf7",  
  
  "no pattern detected" = "#FFFFBF",  # 黄色
  
  "new hot spot" = "#67001F" , 
  "consecutive hot spot" = "#800026",  
  "intensifying hot spot" = "#BD0026", 
  "persistent hot spot" = "#E31A1C",  
  "diminishing hot spot" = "#FC4E2A",  # 红橙
  "sporadic hot spot" = "#FD8D3C",  # 深橙
  "oscillating hot spot" = "#FEB24C",  # 橙色
  "historical hot spot" = "#FFEDA0"  # 浅橙
)

# 按Income分组并循环绘制饼图, 每个Income内按照SOC分面
fig_path <- 'D:/Data_Projects/Proj_EAI_2024/Figure/Fig_V2/SI-Figure/'

# 按照Income分组
unique_incomes <- unique(pattern_group_data$Income)

# ------ 饼图 ------

for (income in unique_incomes) {
  income_data <- pattern_group_data %>%
    filter(Income == income)
  

    # 检查是否有数据
    if (nrow(income_data) >= 1) {
      
      # 创建饼图（带分面）
      p <- ggplot(income_data, aes(x = "", y = Percent, fill = Pattern)) +
        geom_bar(width = 1, stat = "identity", color = 'white') +
        coord_polar(theta = "y", start = 0) +
        scale_fill_manual(values = color_mapping) +
        facet_wrap(~ SOC,  scales = "free") +  # 根据 SOC 创建分面
        labs(
          title = NULL,#paste("SOC:", soc),
          x = NULL,
          y = NULL,
          fill = "Pattern"
        ) +
        theme_minimal() +
        theme(
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 14),
          strip.text = element_text(face = "bold", size = 14)  # 分面标签样式
        )
      
      # 保存饼图为文件
      ggsave(
        filename = paste0(fig_path, "SI_",income, "_Pattern_PieChart.jpg"),
        plot = p,
        dpi = 400,
        width = 16,
        height = 12
      )
    }
}


# ------ 环形图 ------

for (income in unique_incomes) {
  income_data <- pattern_group_data %>%
    filter(Income == income)
  
  # 检查是否有数据
  if (nrow(income_data) >= 1) {
    # 计算最大百分比的Pattern
    income_data <- income_data %>%
      arrange(SOC, desc(Percent)) %>%  # 按SOC分组并降序排列
      group_by(SOC) %>%
      mutate(
        max_label = ifelse(row_number() == 1, paste0(round(Percent, 1), "%"), NA),  # 仅最大值显示百分比
        ymax = cumsum(Percent),  # 环形图的起始点
        ymin = lag(ymax, default = 0),  # 环形图的结束点
        labelPosition = (ymax + ymin) / 2  # 标签位置
      ) %>%
      ungroup()
    
    # 创建环形图
    p <- ggplot(income_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Pattern)) +
      geom_rect(color = "white") +  # 绘制环形分块
      geom_text(
        aes(x = 2.5, y = labelPosition, label = max_label),
        size = 5, color = "black", na.rm = TRUE  # 仅显示非NA标签
      ) +
      scale_fill_manual(values = color_mapping) +  # 自定义颜色映射
      coord_polar(theta = "y") +  # 使用极坐标系生成环形图
      xlim(c(1, 4)) +  # 设置 x 范围以形成环形
      facet_wrap(~ SOC, scales = "free") +  # 按 SOC 分面
      labs(
        title = NULL,
        x = NULL,
        y = NULL,
        fill = "Pattern"
      ) +
      theme_void() +  # 清除多余背景
      theme(
        legend.position = "right",  # 图例放在右边
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(face = "bold", size = 14)  # 分面标签样式
      )
    
    # 根据 Income 设置图像宽度和高度
    plot_width <- ifelse(income == "High income", 18, 
                         ifelse(income == "Low income", 12, 16))
    plot_height <- ifelse(income == "High income", 16, 
                          ifelse(income == "Low income", 10, 12))
    
    # 保存图像为文件
    ggsave(
      filename = paste0(fig_path, "SI_", income, "_Pattern_DoughnutChart.jpg"),
      plot = p,
      dpi = 400,
      width = plot_width,
      height = plot_height
    )
    
    path_pdf <- paste0(fig_path, "SI_", income, "_Pattern_DoughnutChart.pdf")
    ggsave(path_pdf, plot = p, 
           width = plot_width,
           height = plot_height
           )
    
    
  }
}


