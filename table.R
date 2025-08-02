df=fread("china.csv",header = T) %>% mutate(measure_name = if_else(measure_name == "DALYs (Disability-Adjusted Life Years)", "DALYs", measure_name)) %>% 
  filter(year>1989)

df1 <- df %>% 
  filter(measure_name %in% c("DALYs","Incidence","Prevalence")) %>% 
  filter(location_name =="China") %>%
  #filter(age_name %in% c("Age-standardized", "All ages")) %>%
  filter(!metric_name == "Percent") %>%
  mutate(a = paste0(age_name, "-", metric_name)) %>%
  filter(!a == "All ages-Rate") %>%
  select(-a) %>% 
  filter(sex_name == "Female") %>% 
  filter(year %in% c(1990, 2021))  %>% 
  mutate(num = if_else(metric_name == "Number", 
                       sprintf("%.0f (%.0f, %.0f)", val, lower, upper), 
                       sprintf("%.2f (%.2f, %.2f)", val, lower, upper))) %>%
  mutate(yearmeasure_name = paste0(year, measure_name, "-", metric_name)) %>%
  select(location_id,location_name,cause_name,yearmeasure_name,age_name,num)

dfntd21 <- df1 %>%
  #dplyr::select(-"age_name") %>%
  pivot_wider(
    names_from = yearmeasure_name,
    values_from = num
  ) %>%
  select("location_id", "location_name", "cause_name","age_name",
         #"1990Deaths-Number", "1990Deaths-Rate", "2021Deaths-Number", "2021Deaths-Rate",
         "1990DALYs-Number", "1990DALYs-Rate", "2021DALYs-Number", "2021DALYs-Rate",
         "1990Incidence-Number", "1990Incidence-Rate", "2021Incidence-Number", "2021Incidence-Rate",
         "1990Prevalence-Number", "1990Prevalence-Rate", "2021Prevalence-Number", "2021Prevalence-Rate")

write.csv(dfntd21,paste0("Table1-D+I+P.csv"))









calculate_aapc <- function(data) {
  # Fit the initial linear model
  lm_model <- glm(val ~ year, data = data)
  # Fit the segmented model (you might need to specify the breakpoints if necessary)
  # Define a function to fit the segmented model
  fit_segmented_model <- function(model, breakpoints) {
    tryCatch({
      segmented(model, seg.Z = ~ year, psi = breakpoints)
    }, warning = function(w) {
      if (grepl("Breakpoint estimate", w$message)) {
        cat("Warning: Breakpoint estimate(s) outdistanced to allow finite estimates and st.errs.\n")
        NULL
      } else {
        warning(w)
        NULL
      }
    }, error = function(e) {
      cat("Error: Failed to fit the segmented regression model.\n")
      NULL
    })
  }
  
  # Fit the initial segmented model with breakpoints c(1997, 2005, 2012)
  segmented_model <- fit_segmented_model(lm_model, c(1997, 2005, 2012))
  
  # Check if the initial segmented model was successfully fitted
  if (!is.null(segmented_model)) {
    # Condition 1: If the initial segmented model was successfully fitted
    print(1)
  } else {
    # Condition 2: If the initial segmented model failed, try the alternative breakpoints c(1995, 2000, 2014)
    segmented_model <- fit_segmented_model(lm_model, c(1995, 2000, 2014))
    
    if (!is.null(segmented_model)) {
      # Condition 3: If the alternative segmented model was successfully fitted
      print(2)
    } else {
      # Condition 4: If both attempts failed, try the second alternative breakpoints c(1990, 2021)
      segmented_model <- fit_segmented_model(lm_model, c(1990, 2021))
      
      if (!is.null(segmented_model)) {
        # Condition 5: If the second alternative segmented model was successfully fitted
        print(3)
      } else {
        # Condition 6: If all attempts failed, print a message
        segmented_model <- fit_segmented_model(lm_model, c( 2021))
        
        if (!is.null(segmented_model)) {
          # Condition 5: If the second alternative segmented model was successfully fitted
          print(4)
        } else {
          cat("Failed to fit the segmented regression model.\n")
        }
        
      }
    }
  }
  # Check if the segmented model was successfully fitted
  
  # Print the estimated breakpoints
  plot(segmented_model)
  # Calculate AAPC
  aapc_result <- aapc(segmented_model, parm = "year")
  
  return(aapc_result)
}
# Apply the function to each location and store the results

library(broom)
library(segmented)


df=fread("china.csv",header = T) %>% 
  mutate(measure_name = if_else(measure_name == "DALYs (Disability-Adjusted Life Years)", "DALYs", measure_name)) %>% 
  filter(year>1989) %>% 
  filter(!metric_name=="Percent") %>% 
  mutate(a=paste0(age_name,"-", metric_name)) %>% 
  filter(!a=="All ages-Rate") %>% 
  select(-a)
dfx1=df %>% #filter(year %in% c(1990)) %>% 
  #filter(measure_name %in% c("Deaths","DALYs","Incidence")) %>% 
  filter(location_name =="China") %>% 
  filter(sex_name=="Female")



for (j in unique(df$measure_name)) {
  x = dfx1 %>% select(location_id, location_name) %>% unique()
  
  dfx2 = dfx1 %>% 
    filter(measure_name == j) %>% 
    filter(year > 1989) %>% 
    filter(age_id %in%c(7:15,22,27)) %>% 
    filter(metric_name == "Rate") %>% 
    filter(sex_name == "Female") 
  
  library(broom)
  decx = 3
  df_eapc <- dfx2 %>%
    group_by(location_id,age_name) %>%
    do(aapc = calculate_aapc(.)) %>% 
    unnest_wider(aapc) %>% 
    purrr::set_names("location_id","age_name", "AAPC", "sd", "low", "up") %>% 
    mutate(AAPCs_95_low_up = sprintf(paste0("%.", decx, "f(%.", decx, "f,%.", decx, "f)"), AAPC, low, up)) %>% 
    dplyr::select(location_id,age_name, AAPCs_95_low_up, AAPC,low,up)
  
  #colnames(df_eapc) = c("location_id", paste0("AAPC 95CI-Rate", j), paste0("AAPCs", j))
  
  x = left_join(x, df_eapc)
  
  # Save the results as a CSV file
  write.csv(x, paste0("Table1-","-AAPC1990-2021-Rate","-",j, ".csv"))
}





library(dplyr)

# 确保结果存储目录存在
if (!dir.exists("./Table1")) {
  dir.create("./Table1")
}

# 遍历每个 measure_name
for (i in unique(df$measure_name)) {
  # 创建存储当前 measure 的结果数据框
  final_results <- data.frame()
  
  # 过滤数据并清理
  df2 <- df %>%
    filter(measure_name == i) %>%
    filter(sex_name == "Both") %>%
    filter(location_name =="China") %>%
    filter(age_id %in% c(7:15,22,27)) %>%
    filter(metric_name == "Rate") %>%
    filter(!is.na(val), !is.infinite(val), val > 0)
  
  # 遍历每个 location_name
  for (loc in df2$age_name) {
    # 筛选当前 location_name 的数据
    df_loc <- df2 %>%
      filter(age_name == age_name)
    
    # 如果筛选后的数据行数大于 1，则计算 EAPC
    if (nrow(df_loc) > 1) {
      lm_fit <- lm(log(val) ~ year, data = df_loc)
      beta <- coef(lm_fit)["year"]
      se_beta <- summary(lm_fit)$coefficients["year", "Std. Error"]
      EAPC <- (exp(beta) - 1) * 100
      lower_CI <- (exp(beta - 1.96 * se_beta) - 1) * 100
      upper_CI <- (exp(beta + 1.96 * se_beta) - 1) * 100
      eapc_formatted <- sprintf("%.2f (%.2f, %.2f)", EAPC, lower_CI, upper_CI)
      
      # 将结果存储到 final_results 数据框
      final_results <- rbind(final_results, data.frame(
        Measure = i,
        agename = loc,
        Sex = "Both",
        EAPC = eapc_formatted,
        lower_CI = lower_CI,
        upper_CI = upper_CI
      ))
    }
  }
  
  # 设置 Location 列为有序因子，并按照指定顺序排序
  final_results$agename <- factor(final_results$agename, levels = c("China"
  ))
  
  # 按照 Location 的因子顺序排序
  final_results <- final_results 
  
  # 保存当前 measure_name 的结果为单独的 CSV 文件
  write.csv(final_results, paste0("Table1-EAPC-results-Rate-", i,".csv"), row.names = FALSE)
}









library(dplyr)

# 确保结果存储目录存在
if (!dir.exists("./Table1")) {
  dir.create("./Table1")
}

# 遍历每个 measure_name
for (i in unique(df$measure_name)) {
  message(paste("Processing measure:", i))
  final_results <- data.frame()
  
  # 过滤数据并清理
  df2 <- df %>%
    filter(measure_name == i) %>%
    filter(sex_name == "Both") %>%
    filter(location_name == "China") %>%
    filter(age_id %in% c(7:15, 22, 27)) %>%
    filter(metric_name == "Rate") %>%
    filter(!is.na(val), val > 0)
  
  if (nrow(df2) == 0) {
    message(paste("No valid data for measure:", i))
    next
  }
  
  # 遍历每个 age_name
  for (loc in unique(df2$age_name)) {
    message(paste("  Processing age group:", loc))
    df_loc <- df2 %>%
      filter(age_name == loc)
    
    if (nrow(df_loc) > 1) {
      lm_fit <- lm(log(val) ~ year, data = df_loc)
      beta <- coef(lm_fit)["year"]
      se_beta <- summary(lm_fit)$coefficients["year", "Std. Error"]
      EAPC <- (exp(beta) - 1) * 100
      lower_CI <- (exp(beta - 1.96 * se_beta) - 1) * 100
      upper_CI <- (exp(beta + 1.96 * se_beta) - 1) * 100
      eapc_formatted <- sprintf("%.2f (%.2f, %.2f)", EAPC, lower_CI, upper_CI)
      
      final_results <- rbind(final_results, data.frame(
        Measure = i,
        agename = loc,
        Sex = "Both",
        EAPC95 = eapc_formatted,
        EPACs =EAPC,
        lower_CI = lower_CI,
        upper_CI = upper_CI
      ))
    } else {
      message(paste("    Not enough data for age group:", loc))
    }
  }
  
  if (nrow(final_results) > 0) {
    write.csv(final_results, file.path("./Table1", paste0("Table1-EAPC-results-Rate-", i, ".csv")), row.names = FALSE)
  } else {
    message(paste("No results for measure:", i))
  }
}
