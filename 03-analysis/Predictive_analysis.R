

# ------ 采用DTW时序聚类  ------

library(dtwclust)

# 将 EAI_NTL 转换为一个列表，每个元素代表一段时间的序列
data <- level_0_EAI_POP %>%
  
  dplyr::select(SOC, Year, EAI_NTL)

time_series_list <- split(data$EAI_NTL, data$SOC)

# 使用 dtwclust 进行聚类
clusters <- tsclust(time_series_list,        # 用 dtwclust 包中的 tsclust 函数进行基于 DTW 距离的聚类。参数说明：
                    type = "partitional",    # 指定聚类方法为分区聚类（类似 K-means）
                    k = 2,                   # 可能分类数：2/3/4/5
                    distance = "dtw_basic",  # distance = "dtw_basic"：使用基本 DTW 距离。
                    centroid = "pam")        # 指定聚类算法使用 PAM（Partitioning Around Medoids）来计算聚类中心。

# 查看每个簇的分配
cluster_assignment <- clusters@cluster

# 利用聚类结果标记原数据的 state 列
data$state_2type <- unlist(lapply(cluster_assignment, function(x) rep(x, length(time_series_list[[x]]))))

# 打印前几行结果
head(data)
View(data)

# 按照 SOC 分类并绘制
ggplot(data, aes(x = Year, y = EAI_NTL, color = SOC)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ state_3type, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = "EAI_NTL", color = "SOC") +
  theme_minimal() +
  ylim(0,100)



# ------ save the clustered results data ------
write.csv(data,
          file = 'F:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_table/EAI_clust_345.csv',
          row.names = FALSE)

plot(clusters)



# ------ 判断分为几类曲线 ------

# colnames(data)
# [1] "SOC"         "Year"        "EAI_NTL"     "state_5type" "state_4type" "state_3type"

# 不管是分成3/4/5类
# X :Year, Y: EAI_NTL, point plot
# 画图1（3类）：3张子图分别代表每一类的曲线，绘制该类别的平均时序曲线，然后在图中列出所有属于该类的SOC
# 画图2（4类）：4张子图分别代表每一类的曲线，绘制该类别的平均时序曲线，然后在图中列出所有属于该类的SOC
# 画图3（5类）：5张子图分别代表每一类的曲线，绘制该类别的平均时序曲线，然后在图中列出所有属于该类的SOC

library(ggplot2)
library(dplyr)
library(gridExtra)


plot_by_type <- function(data, type_column, n_classes) {
  # 获取每类的平均曲线数据
  data_avg <- data %>%
    group_by(!!sym(type_column), Year) %>%
    summarize(mean_EAI_NTL = mean(EAI_NTL, na.rm = TRUE), .groups = 'drop')
  
  # 获取每类的SOC列表
  soc_labels <- data %>%
    group_by(!!sym(type_column)) %>%
    summarize(SOC_list = paste(unique(SOC), collapse = ", "), .groups = 'drop') %>%
    mutate(SOC_list_wrapped = gsub("(([^,]+,){10})", "\\1\n", SOC_list))
  
  # 初始化子图列表
  plot_list <- list()
  
  # 为每一类生成子图
  for (i in 1:n_classes) {
    type_value <- unique(data[[type_column]])[i]
    subset_avg <- data_avg %>% filter(!!sym(type_column) == type_value)
    subset_soc <- soc_labels %>% filter(!!sym(type_column) == type_value)
    
    # 打印SOC列表（每10个换行后的）
    cat("Class", type_value, "SOC List:\n", subset_soc$SOC_list_wrapped, "\n\n")
    
    p <- ggplot(subset_avg, aes(x = Year, y = mean_EAI_NTL)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = paste("Class", type_value),
           x = "Year", y = "EAI_NTL",
           subtitle = paste("SOC:\n", subset_soc$SOC_list_wrapped)) +
      theme_minimal()+
      ylim(0,100)
    
    plot_list[[i]] <- p
  }
  
  # 使用 grid.arrange 将所有子图显示在一个画布上
  do.call(grid.arrange, c(plot_list, ncol = 2))
}


# 图1：3类
p1 = plot_by_type(data, "state_3type", 3)
# Save the plot
ggsave(filename = paste0("F:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_3.jpg"),
       plot = p1, width = 12, height = 10, dpi = 400)


# 图2：4类
p2 = plot_by_type(data, "state_4type", 4)
# Save the plot
ggsave(filename = paste0("F:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_4.jpg"),
       plot = p2, width = 12, height = 10, dpi = 400)


# 图3：5类
p3 = plot_by_type(data, "state_5type", 5)
# Save the plot
ggsave(filename = paste0("F:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_5.jpg"),
       plot = p3, width = 12, height = 10, dpi = 400)




# ------ 使用轮廓系数（Silhouette Score）进行评估 -------

library(cluster)

# 计算不同聚类数量的轮廓系数
calculate_silhouette <- function(clusters, data_list) {
  # 根据聚类结果生成簇标签
  cluster_labels <- clusters@cluster
  # 合并所有时间序列数据
  dist_matrix <- proxy::dist(data_list, method = "dtw_basic")
  
  # 计算轮廓系数
  silhouette_scores <- silhouette(cluster_labels, dist_matrix)
  avg_silhouette <- mean(silhouette_scores[, 3])  # 平均轮廓系数
  return(avg_silhouette)
}

# 计算3类、4类和5类的轮廓系数
sil_scores <- c()

for (k in 3:5) {
  clusters <- tsclust(time_series_list,
                      type = "partitional",
                      k = k,
                      distance = "dtw_basic",
                      centroid = "pam")
  
  sil_scores[k - 1] <- calculate_silhouette(clusters, time_series_list)
}

names(sil_scores) <- paste0("Classes_", 3:5)

# 输出轮廓系数
print(sil_scores)
# Classes_3 Classes_4 Classes_5 
# 0.6770318 0.5486206 0.4636473 

# 轮廓系数衡量了簇内相似性与簇间差异性。
# 值越高表示聚类效果越好，通常轮廓系数在 0.5 以上为良好，接近 1 表示聚类效果最佳。






# ------ 按当前预测：3类 ------

library(forecast)
library(dplyr)
library(prophet)

# data_avg 是一个包含三列 state_3type, Year, EAI_NTL 的数据框
# 包含每类均值的时间序列数据
data_avg <- data %>%
  group_by(state_3type, Year) %>%
  summarize(EAI_NTL = mean(EAI_NTL, na.rm = TRUE), .groups = 'drop')


predict_future <- function(data, n.ahead = 8) {
  
  # 线性回归模型
  fit_lm <- lm(EAI_NTL ~ Year, data = data)
  future_years <- seq(max(data$Year) + 1, by = 1, length.out = n.ahead)
  forecast_lm <- predict(fit_lm, newdata = data.frame(Year = future_years))
  
  # 指数模型
  # fit_exp <- nls(EAI_NTL ~ a * exp(b * Year), data = data, start = list(a = 1, b = 0.01))
  # fit_exp <- nls(EAI_NTL ~ a * exp(b * Year), data = data, start = list(a = 1, b = 0.01),
  #                control = nls.control(maxiter = 100))
  # fit_exp <- tryCatch(nls(EAI_NTL ~ a * exp(b * Year), 
  #                         data = data, 
  #                         start = list(a = 10, b = 0.01), 
  #                         control = nls.control(maxiter = 200, tol = 1e-05),
  #                         algorithm = "port"),
  #                     error = function(e) rep(NA, n.ahead))
  # 
  # 
  # # forecast_exp <- predict(fit_exp, newdata = data.frame(Year = future_years))
  # forecast_exp <- ifelse(is.null(fit_exp), rep(NA, n.ahead), 
  #                        predict(fit_exp, newdata = data.frame(Year = future_years)))
  
  
  # ARIMA 模型
  fit_arima <- auto.arima(data$EAI_NTL) # 自动选择ARIMA模型
  forecast_arima <- forecast(fit_arima, h = n.ahead)$mean
  
  # ETS 模型
  fit_ets <- ets(data$EAI_NTL) # 指数平滑状态空间模型
  forecast_ets <- forecast(fit_ets, h = n.ahead)$mean
  

  
  data.frame(
    Year = c(data$Year, future_years),
    Linear_Pred = c(data$EAI_NTL, forecast_lm),
    Arima_Pred = c(data$EAI_NTL, forecast_arima),
    ETS_Pred = c(data$EAI_NTL, forecast_ets)

  )
}

plot_predictions <- function(class_data, class_id) {
  forecast_data <- predict_future(class_data)
  
  ggplot(forecast_data, aes(x = Year)) +
    geom_line(aes(y = Linear_Pred), color = "blue", size = 1, linetype = "dashed") +
    # geom_line(aes(y = Exp_Pred), color = "red", size = 1, linetype = "dotted") +
    geom_line(aes(y = Arima_Pred), color = "green", size = 1, linetype = "longdash") +
    geom_line(aes(y = ETS_Pred), color = "purple", size = 1, linetype = "twodash") +

    geom_point(data = class_data, aes(y = EAI_NTL), color = "black") +
    labs(title = paste("Class", class_id, "Growth Prediction"),
         x = "Year", y = "EAI_NTL", legend = 'color') +
    theme_minimal() +
    ylim(0, 100)
}

# 循环3个类的预测
# 选择模型：比较所有模型的AIC和BIC值。通常，AIC/BIC值越小，模型越优。
for (class_id in unique(data_avg$state_3type)) {
  print(paste0('Start predict type ',class_id,' .'))
  class_data <- data_avg %>% filter(state_3type == class_id)
  print(plot_predictions(class_data, class_id))
  
  # 模型拟合
  fit_linear <- lm(EAI_NTL ~ Year, data = class_data)
  fit_arima <- auto.arima(class_data$EAI_NTL)
  fit_ets <- ets(class_data$EAI_NTL)
  ts_data <- ts(class_data$EAI_NTL, start = min(class_data$Year), frequency = 1)
  nnar_model <- nnetar(ts_data)
  prophet_data <- class_data[, c("Year", "EAI_NTL")]
  colnames(prophet_data) <- c("ds", "y")
  prophet_model <- prophet(prophet_data)
  
  # 计算AIC和BIC
  aic_values <- c(AIC(fit_linear), AIC(fit_arima), AIC(fit_ets))
  bic_values <- c(BIC(fit_linear), BIC(fit_arima), BIC(fit_ets))

  names(aic_values) <- c("Linear", "ARIMA", "ETS")
  names(bic_values) <- c("Linear", "ARIMA", "ETS")
  
  # # 计算AIC和BIC
  # aic_values <- c(AIC(fit_linear), AIC(fit_arima), AIC(fit_ets) )
  # bic_values <- c(BIC(fit_linear), BIC(fit_arima), BIC(fit_ets) )
  # 
  # names(aic_values) <- c("Linear", "ARIMA", "ETS")
  # names(bic_values) <- c("Linear", "ARIMA", "ETS")
  
  # 打印AIC和BIC值
  print(paste("Class", class_id, "AIC values:", paste(names(aic_values), aic_values, sep = ":", collapse = ", ")))
  print(paste("Class", class_id, "BIC values:", paste(names(bic_values), bic_values, sep = ":", collapse = ", ")))
  
  # 选择最优模型
  best_model_aic <- names(which.min(aic_values))
  best_model_bic <- names(which.min(bic_values))
  
  print(paste("Class", class_id, "Best AIC model:", best_model_aic))
  print(paste("Class", class_id, "Best BIC model:", best_model_bic))
} 

# 不用指数模型预测: 只有第一类可以正常预测，其他类在使用指数模型时都报错

# 逐个类别预测
class_id = 2
print(paste0('Start predict type ',class_id,' .'))
class_data <- data_avg %>% filter(state_3type == class_id)
print(plot_predictions(class_data, class_id))



# ------ 逐个国家预测:函数 ------

# SOC：国家代码
# Year: 年份
# EAI_NTL：用电人口比例
# 原始数据： SOC,Year(1992-2022),EAI_NTL
# 预测：SOC, Year(2023-2030)，EAI_NTL,Type(ARIMA, SDG7)
# 最终数据：SOC,Year(1992-2030),EAI_NTL,Type(Origin,ARIMA, SDG7) 

# 原始数据：
data <- level_0_EAI_POP %>%
  dplyr::select(SOC, Year, EAI_NTL)

# 为原始数据增加一列，Type，值为’Origin'

# 预测1：按照之前的结果，无论是哪一类曲线都是ARIMA的效果最好，所以全部国家都用ARIMA 预测
# 预测年份是2023-2030，模型是ARIMA, 预测值放在EAI_NTL，标签列Type值为‘ARIMA’

# 预测2：把所有2030的EAI_NTL目标设为100
# 预测年份是2023-2030，模型是线性模型，预测值放在EAI_NTL，标签列Type值为‘SDG7’


library(forecast)
library(dplyr)

predict_future_ARIMA <- function(data, n.ahead = 8) {
  
  future_years <- seq(max(data$Year) + 1, by = 1, length.out = n.ahead)
  
  # ARIMA 模型
  fit_arima <- auto.arima(data$EAI_NTL) # 自动选择ARIMA模型
  forecast_arima <- forecast(fit_arima, h = n.ahead)$mean
  
  # Create data frame for predicted values
  arima_forecast <- data.frame(
    SOC = unique(data$SOC),
    Year = future_years,
    EAI_NTL = forecast_arima,
    Type = 'ARIMA'
  )
  
  return(arima_forecast)
}


# Prediction Function for Linear Model to meet SDG7 target
# 线性回归模型: 注意是先设置2030年的EAI_NTL为100,然后往回插值
predict_future_SDG7 <- function(data, target = 100, n.ahead = 8) {
  future_years <- seq(max(data$Year) + 1, by = 1, length.out = n.ahead)
  current_value <- tail(data$EAI_NTL, 1)
  
  # Linear interpolation from current value to target
  forecast_sdg7 <- seq(current_value, target, length.out = n.ahead)
  
  # Create data frame for predicted values
  sdg7_forecast <- data.frame(
    SOC = unique(data$SOC),
    Year = future_years,
    EAI_NTL = forecast_sdg7,
    Type = 'SDG7'
  )
  
  return(sdg7_forecast)
}

# Function to combine original and predicted data
combine_predictions <- function(class_data) {
  # Original data with Type = 'Origin'
  origin_data <- class_data %>%
    mutate(Type = 'Origin')
  
  # Generate predictions
  arima_data <- predict_future_ARIMA(class_data)
  sdg7_data <- predict_future_SDG7(class_data)
  
  # Combine all data
  combined_data <- bind_rows(origin_data, arima_data, sdg7_data)
  
  return(combined_data)
}

# Initialize empty data frame to store results
final_data <- data.frame()


# Function to combine original and predicted data and plot
plot_predictions <- function(class_data, soc) {
  # Original data with Type = 'Origin'
  origin_data <- class_data %>%
    mutate(Type = 'Origin')
  
  # Generate predictions
  arima_data <- predict_future_ARIMA(class_data)
  sdg7_data <- predict_future_SDG7(class_data)
  
  # Combine all data
  forecast_data <- bind_rows(origin_data, arima_data, sdg7_data)
  
  # Plot
  combined_plot <- ggplot(forecast_data, aes(x = Year, y = EAI_NTL, color = Type)) +
    geom_line(size = 1) +
    geom_point(data = origin_data, aes(x = Year, y = EAI_NTL), color = "black") +
    scale_color_manual(values = c("Origin" = "black", "ARIMA" = "green", "SDG7" = "blue")) +
    labs(
      title = paste("Country", soc, "Growth Prediction"),
      x = "Year",
      y = "EAI_NTL",
      color = "Prediction Type"
    ) +
    theme_minimal() +
    ylim(0, 100)
  
  # Save the plot
  output_dir <- "F:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/"
  ggsave(
    filename = paste0(output_dir, soc, "_Predict.jpg"),
    plot = combined_plot,
    width = 8,
    height = 6,
    dpi = 400
  )
}


# ------ soc的预测：for 循环 ------


# Loop through each SOC and predict 

for (soc in unique(data$SOC)) {
  print(paste0("Start predicting for country ", soc, "...\n"))
  class_data <- data %>% filter(SOC == soc)
  
  # plot_predictions(class_data, soc)
  
  # Combine original and predicted data for each SOC
  combined_data <- combine_predictions(class_data)
  
  # Append to the final data table
  final_data <- bind_rows(final_data, combined_data)
} 

# Save the final data table to a CSV file
write.csv(final_data, "F:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_table/Final_EAI_Predictions.csv", row.names = FALSE)

print("Final data table saved successfully.")



# ------ 将3类增长类型标签添加到数据列 ------

table1 = read.csv('F:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_table/EAI_clust_3_SOC.csv')

# Perform a left join to add state_3type from table1 to table2 based on SOC
merged_table <- final_data %>%
  left_join(table1 %>% 
              dplyr::select(SOC, state_3type), by = "SOC")

# Save the final data table to a CSV file
write.csv(merged_table, 
          "F:/Data_Projects/Proj_EAI_2024/Figure/EAI_state_curves/EAI_clust_table/Final_EAI_Predictions_3clust.csv", 
          row.names = FALSE)

