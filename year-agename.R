df <- read.csv("china.csv")%>% 
  mutate(measure_name = if_else(measure_name == "DALYs (Disability-Adjusted Life Years)", "DALYs", measure_name))

for (i in unique(df$measure_name)) {
  
  
  df1 <- df %>%
    filter(measure_name == i) %>%
    filter(sex_name=="Female") %>% 
    filter(age_id %in% c(7:15)) %>% 
    filter(metric_name == "Rate") %>%
    filter(year %in% c(1990, 2000, 2010, 2021))
  
  # 加载必要的包
  library(ggplot2)
  library(dplyr)
  
  # 自定义 location_name 顺序
  custom_order <- c("10-14 years","15-19 years","20-24 years","25-29 years",
                    "30-34 years","35-39 years","40-44 years","45-49 years","50-54 years")
  
  # 对 location_name 因子化，按照指定顺序排序
  df1$age_name <- factor(df1$age_name, levels = custom_order)
  
  # 绘图
  p <- ggplot(df1, aes(x = age_name, y = val, fill = as.factor(year))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # 柱状图
    geom_errorbar(
      aes(ymin = lower, ymax = upper),  # 使用 lower 和 upper 列绘制误差线
      position = position_dodge(width = 0.8),  # 和柱状图对齐
      width = 0.2                          # 误差线宽度
    ) +
    scale_fill_manual(
      values = c(
        "1990" = "#E64B35",  # 红色
        "2000" = "#4DBBD5",  # 蓝色
        "2010" = "#01A087",  # 绿色
        "2021" = "#3C5488"   # 深蓝色
      ),
      name = ""
    ) +
    labs(
      title = "",
      y = paste0("ASR of ", i, "\n", "per 100,000 population in 1990,2000,2010,2021"),
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      panel.grid.minor.x = element_blank(),  # 不需要次级刻度线
      axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1, vjust = 1),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      legend.text = element_text(size = 14)
    )
  ggsave(paste0("Fig1",i,".pdf"),plot = p,width = 14,height = 10)
  write.csv(df1,paste0("Fig1-",i,".csv"),row.names = FALSE)
}
