# 定义目标收入分组（根据World Bank标准）
income_groups <- c(
  "Low income",
  "Lower middle income",
  "Upper middle income",
  "High income"
)

# 筛选数据并清理收入分组名称
data_income <- group_dataC_NTL %>%
  filter(Income %in% income_groups) %>%
  mutate(Income = factor(Income, levels = income_groups))  # 确保分组顺序

# 按年份和收入分组计算组内不平等
theil_within_income <- data_income %>%
  group_by(Year, Income) %>%
  summarise(
    Theil_Within = calculate_theil(EAI_NTL, POP),
    .groups = "drop"
  )

# 计算收入分组间的泰尔指数（组间不平等）
theil_between_income <- data_income %>%
  group_by(Year) %>%
  summarise(
    Theil_Between = {
      groups <- unique(Income)
      total_pop <- sum(POP, na.rm = TRUE)
      Y_total <- sum(EAI_NTL * POP, na.rm = TRUE)
      if (Y_total == 0) return(NA)
      theil_between <- 0
      for (grp in groups) {
        grp_data <- filter(cur_data(), Income == grp)
        y_grp <- sum(grp_data$EAI_NTL * grp_data$POP, na.rm = TRUE)
        s_grp <- sum(grp_data$POP, na.rm = TRUE) / total_pop
        theil_between <- theil_between + (y_grp / Y_total) * log((y_grp / Y_total) / s_grp)
      }
      theil_between
    },
    .groups = "drop"
  )


theil_income_series <- bind_rows(
  theil_within_income %>% rename(Theil = Theil_Within) %>% mutate(Type = "Within"),
  theil_between_income %>% rename(Theil = Theil_Between) %>% mutate(Income = "Between Groups", Type = "Between")
)

# 自定义颜色和主题
income_colors <- c(
  "High income" = "#4DAF4A",
  "Upper middle income" = "#377EB8",
  "Lower middle income" = "#FF7F00",
  "Low income" = "#E41A1C",
  "Between Groups" = "#984EA3"
)

# 绘制收入分组趋势图
ggplot(theil_income_series, aes(x = Year, y = Theil, color = Income, linetype = Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = income_colors) +
  scale_linetype_manual(values = c("Within" = "solid", "Between" = "dashed")) +
  labs(
    title = "Electricity Access Inequality by Income Groups (1992-2022)",
    subtitle = "Theil Index (Population-Weighted)",
    x = "Year",
    y = "Theil Index",
    color = "Income Group",
    linetype = "Inequality Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  ) # +
  #facet_wrap(~ Type, scales = "free_y")




# ------ 计算收入-区域交叉分组的不平等 ------
# 计算收入-区域交叉分组的不平等
theil_cross <- data_filtered %>%
  filter(Region %in% c("Sub-Saharan Africa", "South Asia", "Latin America")) %>%
  group_by(Year, Region, Income) %>%
  summarise(
    Theil_Within = calculate_theil(EAI_NTL, POP),
    .groups = "drop"
  )

# ------ Latin America 收入-区域交叉分组------
ggplot(theil_cross %>% filter(Region == "Latin America"), 
       aes(x = Year, y = Theil_Within, color = Income)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = income_colors) +
  labs(
    title = "Income-Driven Inequality in Latin America (1992-2022)",
    x = "Year",
    y = "Theil Index (Within Income Groups)"
  ) +
  theme_minimal()

# ------ South Asia 收入-区域交叉分组------
ggplot(theil_cross %>% filter(Region == "South Asia"), 
       aes(x = Year, y = Theil_Within, color = Income)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = income_colors) +
  labs(
    title = "Income-Driven Inequality in South Asia (1992-2022)",
    x = "Year",
    y = "Theil Index (Within Income Groups)"
  ) +
  theme_minimal()

# ------ Sub-Saharan Africa 收入-区域交叉分组------
ggplot(theil_cross %>% filter(Region == "Sub-Saharan Africa"), 
       aes(x = Year, y = Theil_Within, color = Income)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = income_colors) +
  labs(
    title = "Income-Driven Inequality in Sub-Saharan Africa (1992-2022)",
    x = "Year",
    y = "Theil Index (Within Income Groups)"
  ) +
  theme_minimal()
