library(scales)
library(sf)
library(tidyverse)
library(ggrepel)
library(ggsci)
library(patchwork)
library(tidyverse)
library(patchwork)
library(BAPC)
library(INLA)
library(dplyr)
library(data.table)
library(vroom)
library(car)# 异方差诊断
library(MASS)# 稳健回归
library(mgcv)# 提供洛伦兹曲线拟合 (样条函数等)
library(splines)# 拟合样条函数
library(ggrepel)
library(cowplot)
library(segmented)
library(strucchange)
library(ggpubr)
library(dplyr)
load("GBD.Rdata")




df=fread("1521.csv",header = T)  %>%
  mutate(measure_name = if_else(measure_name == "DALYs (Disability-Adjusted Life Years)", "DALYs", measure_name)) %>%
  mutate(measure_name = if_else(measure_name == "YLDs (Years Lived with Disability)", "YLDs", measure_name)) %>%
  mutate(measure_name = if_else(measure_name == "YLLs (Years of Life Lost)", "YLLs", measure_name)) %>%
  filter(year > 1989) %>% 
  filter(measure_id %in%c(1,2,5,6))


df1 <- df %>% 
  filter(location_id %in% c(1)) %>%
  filter(!metric_name == "Percent") %>%
  filter(sex_name=="Both") %>% 
  filter(age_id %in%c(1,6:20, 30, 31, 32, 235)) %>% 
  filter(year %in% c(1990:2021))  %>%
  mutate(x=paste0(age_name,metric_name)) %>% 
  filter(!x=="All agesRate") %>%
  dplyr::select(-x) %>% 
  group_by(location_id, location_name,
           age_id, age_name,
           sex_id, sex_name,
           year, 
           measure_id, measure_name,
           metric_id, metric_name) %>%
  summarise(
    val = sum(val, na.rm = TRUE),
    lower = sum(lower, na.rm = TRUE),
    upper = sum(upper, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cause_name = "Plastic Surgery",
         cause_id = NA) %>%  # 可选，视你是否需要添加 cause_id
  relocate(cause_name, .before = val)

for (i in unique(df$measure_name)) {print(i)
  
  
  df2 <- df1 %>% 
    filter(measure_name==i) %>% 
    filter(metric_name=="Rate") %>% 
    filter(year %in% c(1990,2021)) %>% 
    mutate(age_name = factor(age_name, levels = c("Age-standardized","<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
                                                  "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years", 
                                                  "50-54 years", "55-59 years","60-64 years" ,"65-69 years", "70-74 years", 
                                                  "75-79 years", "80-84 years", "85-89 years", "90-94 years", "95+ years"))) %>%
    arrange(age_name)
  
  
  library(ggplot2)
  library(ggsci)
  
  # 画图
  p <- ggplot(df2, aes(x = age_name, y = val, fill = factor(year))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  position = position_dodge(width = 0.8), 
                  width = 0.2, size = 0.8) +
    scale_fill_npg(name = "Year") +
    labs(x = "Age Group", y = paste0("Age-standardised ",i," rate per 100,000"), title = "") +
    theme_classic(base_size = 14, base_family = "sans") +
    theme(
      axis.text = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),  # x轴标签倾斜
      axis.title = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13, face = "bold")
    )
  
  print(p)
  
  ggsave(paste0("./fig2/agegroup-",i,".pdf"),plot = p,width  = 10,height = 6,dpi = 300)
  write.csv(df2,paste0("./fig2/agegroup-",i,".csv"))
}
