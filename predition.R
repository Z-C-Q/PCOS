
  
  




df <- fread("china.csv",header = T)%>%
  mutate(measure_name = if_else(measure_name == "DALYs (Disability-Adjusted Life Years)", "DALYs", measure_name))
colnames(df)

for (j in unique(df$measure_name)) {
  print(j)
  
  dfx <- df %>%  
    filter(measure_name == j) %>% 
    filter(location_id == 6) %>% 
    filter(sex_name=="Both") %>% 
    filter(metric_name == "Number") %>% 
    filter(age_id %in% c(1, 5:20, 30, 31, 32, 235))
  
  dfx=dfx
  preyear=2035
  addzero=F
  CI=F
  agename2035 <- data.frame(
    AgeGrp = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
  )
  agename2035$age_id <- c(1,6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 31, 32, 235, 235)
  # preyear=2035
  #loc=locdf2035 %>% filter(location_id==unique(dfx$location_id))
  print(unique((dfx$location_name)))
  ########################
  ### select one contry
  dfNum = dfx 
  
  ### cases number
  agegroup=dfNum %>% select(age_id,age_name) %>% unique() %>% arrange(age_id)
  Num_1 = dfNum %>% filter(metric_name=="Number") %>% ## number of case
    select(year,val,age_id) %>% 
    mutate(val=round(val,0))
  xg=setdiff(c(1,6:20,30,31,32,235), unique(Num_1$age_id))
  Numage=tibble(year=rep(c(1990:2021),length(xg)),
                val=0,
                age_id=rep(xg,each=32)
  )
  ### cases number
  agegroup=dfNum %>% select(age_id,age_name) %>% unique() %>% arrange(age_id)
  # Num = dfNum %>% filter(metric_name=="Number") %>% ## number of case
  #   select(year,val,age_id) %>% 
  #   mutate(val=round(val,0)) %>% # change to interger
  #   spread(age_id,val)%>% select(-1) %>% as.data.frame()
  Numx=rbind(Numage,Num_1) 
  Num=rbind(Numage,Num_1) %>% 
    spread(age_id,val)%>% select(-1) %>% as.data.frame()
  
  ### bing to five age group
  ## We have 5 age groups case
  #"0-19"  "20 to 39"  "40 to 59" "60 to 79" "80+"
  Num17 = Num 
  ## predict
  PreNum= tibble(year=rep(c(2022:preyear),each=length(colnames(Num17))),
                 ageid=rep(c(colnames(Num17)),preyear-2021),
                 val=NA) %>% 
    #mutate(ageid=as.integer(ageid),ageid=factor(ageid,level=agegroup$age_id)) %>% 
    spread(ageid,val)%>% select(-1) %>% as.data.frame() %>% 
    select(colnames(Num))
  
  x=dfage %>% filter(age_id %in% as.numeric(colnames(PreNum)))
  apcNum=rbind(Num17,PreNum)
  colnames(apcNum) = x$age_name
  #colnames(apcNum) = c(agegroup$age_name)
  rownames(apcNum) = 1990:preyear
  ## We have 5 age groups population
  #"0-19"  "20 to 39"  "40 to 59" "60 to 79" "80+"
  
  ########################
  ## Populations from GBD2100
  dfpop=GBDpop1990_2100 %>% filter(year %in% c(1990:preyear)) %>% # here with 2035
    filter(age_id %in% unique(Numx$age_id)) %>% 
    filter(location_id%in% unique(dfx$location_id)) %>% ## location name, should be consist with location_id
    filter(sex_name %in% unique(dfx$sex_name)) %>% select(-location_id,-location_name,-age_name,-sex_name) %>% 
    spread(age_id,val) %>%select(-1) %>% as.data.frame()
  
  Pop17=dfpop
  colnames(Pop17)= colnames(apcNum)
  ########################
  ## Apc-list with World (WHO 2000-2025) Standards database 
  data(whostandard)
  wstand =c(whostandard[1:4,2])/100
  # 标准年龄结构数据age_stand
  #  age_stand <- read.csv("age_stand.csv")
  wstand <- c(age_stand$std_population[1:2] %>% as.numeric() %>% sum(),
              age_stand$std_population[3:21] %>% as.numeric())/sum(age_stand$std_population[1:21])
  wstandx=data.frame(x=  wstand)
  wstandx$age_id=c(1,6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 30, 31, 32, 235)
  standpop=wstandx %>%   filter(age_id %in% unique(Numx$age_id)) 
  wstand2035=wstandx %>% filter(age_id %in% agegroup$age_id) %>% pull(x)
  
  # Calculate the total sum of the 'x' column
  total_sum <- sum(standpop$x)
  # Calculate the proportion for each age_id
  standpop$proportion <- standpop$x / total_sum
  if(addzero){
    apcNum = apcNum
    Pop17 =  Pop17
    standpop=standpop
  } else {
    # Detect columns with all 0 values
    zero_cols <- Num17 %>%
      select_if(~all(. == 0))
    # Remove the zero columns
    apcNum_updated <- Num17 %>%
      select(-all_of(names(zero_cols)))
    
    standpop=standpop %>% filter(age_id %in% colnames(apcNum_updated))
    x=dfage %>%  filter(age_id %in% colnames(apcNum_updated))
    apcNum = apcNum %>% select(x$age_name)
    Pop17 =  Pop17  %>% select(x$age_name)
    
  }
  
  new_proportions <- c(0.000396232, 0.00117777, 0.00864776, 
                       0.0101009, 0.0201613, 0.059934)
  
  # 使用 mutate() 和 case_when() 更新 proportion 列
  standpop <- standpop %>%
    mutate(
      proportion = case_when(
        age_id == 7 ~ new_proportions[1],
        age_id == 8 ~ new_proportions[2],
        age_id == 9 ~ new_proportions[3],
        age_id == 10 ~ new_proportions[4],
        age_id == 11 ~ new_proportions[5],
        age_id == 12 ~ new_proportions[6],
        TRUE ~ proportion  # 保留其他 age_id 的 proportion 不变
      )
    ) 
  
  
  
  ## Apc-list
  gloAPC = APCList(apcNum, Pop17, gf=5,agelab=colnames(Pop17))
  print(gloAPC)
  # perform retrospective projection for 16 years, see section 5
  np=preyear-2021
  #glores = BAPC(gloAPC, predict=list(npredict=np, retro=TRUE))
  
  ## Apc-list with World (WHO 2000-2025) Standards database 
  # Define the function to perform the BAPC analysis
  perform_BAPC <- function() {
    tryCatch({
      print("rw2")
      # First attempt
      glores <- BAPC(gloAPC, predict=list(npredict=np, retro=TRUE),
                     verbose=FALSE,
                     secondDiff = FALSE,
                     model=list(age=list(model="rw2",
                                         prior = "loggamma", param = c(1, 0.00005)),
                                period=list(include=TRUE, model="rw1",
                                            prior = "loggamma", param = c(1, 0.00005)),
                                cohort=list(include=TRUE, model="rw2",
                                            prior = "loggamma", param = c(1, 0.00005)),
                                overdis=list(include=TRUE, model="iid",
                                             prior = "loggamma", param = c(1, 0.005))),
                     stdweight=standpop$proportion)
      
      message("First attempt successful.")
      return(glores)
    }, error = function(e) {
      message("First attempt failed: ", e$message)
      message("Attempting second option...")
      print("seconds...")
      # Second attempt
      glores <- BAPC(gloAPC, predict=list(npredict=np, retro=TRUE),
                     verbose=FALSE, 
                     secondDiff = FALSE,
                     stdweight=standpop$proportion)
      message("Second attempt successful.")
      return(glores)
    })
  }
  # Perform the BAPC analysis
  glores <- perform_BAPC()
  # Now you can use glores as needed
  ## 95%CI
  #dfres= qapc(glores, percentiles=c(0.025, 0.5, 0.975))
  #agespec.proj(resci)[[1]]
  
  ########  ########  ########  ########  ########  ########  ########
  ## plot1 ASR
  
  dfax=agestd.rate(glores) %>% as_tibble() %>% 
    dplyr::mutate(Time=1990:preyear,group="ASR") %>% 
    purrr::set_names("val","sd","Time","group") %>% 
    dplyr::mutate(val=val*100000,sd=sd*100000)
  
  # Calculate multiple confidence intervals
  dfa <- dfax %>%
    dplyr::mutate(
      low_50 = val - 0.674 * sd,up_50 = val + 0.674 * sd,
      low_60 = val - 0.841 * sd,up_60 = val + 0.841 * sd,
      low_70 = val - 1.036 * sd,up_70 = val + 1.036 * sd,
      low_80 = val - 1.282 * sd,up_80 = val + 1.282 * sd,
      low_95 = val - 1.96 * sd, up_95 = val + 1.96 * sd
    )
  
  # Plot using ggplot2
  p1=ggplot(data = dfa, aes(Time, val)) +
    geom_point(color = 'black') +
    geom_line(color = '#a696c8') +
    geom_ribbon(aes(ymin = low_95, ymax = up_95), fill = '#6b6ecf', alpha = 0.2) +
    geom_ribbon(aes(ymin = low_80, ymax = up_80), fill = '#9e9ac8', alpha = 0.2) +
    geom_ribbon(aes(ymin = low_70, ymax = up_70), fill = '#bcbddc', alpha = 0.2) +
    geom_ribbon(aes(ymin = low_60, ymax = up_60), fill = '#dadaeb', alpha = 0.2) +
    geom_ribbon(aes(ymin = low_50, ymax = up_50), fill = '#efedf5', alpha = 0.2) +
    geom_point(data = dfa %>% filter(Time > 2021), aes(Time, val), color = "red") +
    geom_line(data = dfa %>% filter(Time > 2021), aes(Time, val), color = "red") +
    geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
    scale_x_continuous(breaks = seq(1990, preyear, by = 5)) +
    labs(y="ASR per 100 000",x="Year")+
    theme_classic() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = 12),
      legend.position = "none"
    )
  
  ########  ########  ########  ########  ########  ########  ########
  ## plot1 age specific number
  ########  ########  ########  ########  ########  ########  ########
  ## plot1 ALl age cases
  #BAPC::agestd.proj(result)
  dfax_cases=agestd.proj(glores) %>% as_tibble() %>% 
    dplyr::mutate(Time=1990:preyear,group="Number") %>% 
    purrr::set_names("val","sd","Time","group") 
  
  # Calculate multiple confidence intervals
  dfa_case <- dfax_cases %>%
    dplyr::mutate(
      low_50 = val - 0.674 * sd,up_50 = val + 0.674 * sd,
      low_60 = val - 0.841 * sd,up_60 = val + 0.841 * sd,
      low_70 = val - 1.036 * sd,up_70 = val + 1.036 * sd,
      low_80 = val - 1.282 * sd,up_80 = val + 1.282 * sd,
      low_95 = val - 1.96 * sd, up_95 = val + 1.96 * sd
    )
  
  # Plot using ggplot2
  p1x=ggplot(data = dfa_case, aes(Time, val)) +
    geom_point(color = 'black') +
    geom_line(color = '#a696c8') +
    geom_ribbon(aes(ymin = low_95, ymax = up_95), fill = '#6b6ecf', alpha = 0.2) +
    geom_ribbon(aes(ymin = low_80, ymax = up_80), fill = '#9e9ac8', alpha = 0.2) +
    geom_ribbon(aes(ymin = low_70, ymax = up_70), fill = '#bcbddc', alpha = 0.2) +
    geom_ribbon(aes(ymin = low_60, ymax = up_60), fill = '#dadaeb', alpha = 0.2) +
    geom_ribbon(aes(ymin = low_50, ymax = up_50), fill = '#efedf5', alpha = 0.2) +
    geom_point(data = dfa_case %>% filter(Time > 2021), aes(Time, val), color = "red") +
    geom_line(data = dfa_case %>% filter(Time > 2021), aes(Time, val), color = "red") +
    geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
    scale_x_continuous(breaks = seq(1990, preyear, by = 5)) +
    labs(y="Number of cases",x="Year")+
    theme_classic() +
    theme(
      strip.background = element_blank(),
      #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
      strip.text = element_text(size = 12),
      legend.position = "none"
    )
  ########  ########  ########  ########  ########  ########  ########
  ## plot1 age specific number
  
  ######################################################################
  ### Age specific rate
  #dfres= glores@agespec.proj
  dfres=BAPC::agespec.rate(glores)
  # Assuming dfres is a list where each element contains data for a specific age group
  age_groups <- names(dfres)
  # Create a function to process each age group
  process_age_group <- function(data, group_name) {
    data %>% as_tibble() %>%
      dplyr::mutate(Time = 1990:preyear, group = group_name) %>%
      purrr::set_names(c("val", "sd","Time", "group"))
  }
  
  # Use map2 from purrr to apply the function to each element in dfres along with the corresponding age group name
  combined_df <- map2_dfr(dfres, age_groups, process_age_group) %>% 
    dplyr::mutate(val=val*100000,sd=sd*100000) %>% 
    dplyr::mutate(
      low_50 = val - 0.674 * sd,up_50 = val + 0.674 * sd,
      low_60 = val - 0.841 * sd,up_60 = val + 0.841 * sd,
      low_70 = val - 1.036 * sd,up_70 = val + 1.036 * sd,
      low_80 = val - 1.282 * sd,up_80 = val + 1.282 * sd,
      low = val - 1.96 * sd, up = val + 1.96 * sd
    ) %>% filter(val>0 )
  
  ### ggplot
  p2=ggplot(data=combined_df,aes(Time,val))+
    geom_point(color = '#8ac6d1')+
    geom_line(color = '#8ac6d1')+
    geom_point(data=combined_df %>% filter(Time>2021),aes(Time,val,color="red"))+
    geom_line(data=combined_df %>% filter(Time>2021),aes(Time,val,color="red"))+
    geom_vline(xintercept=2021, linetype="dashed", color = "grey")+
    scale_x_continuous(breaks = seq(1990, preyear, by = 5))+
    facet_wrap(vars(group), ncol = 3,scales = "free")+
    labs(y="Prediction rate")+
    theme_classic()+
    theme(strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
          strip.text = element_text(size = 12),
          legend.position="none"
    )
  
  if(CI){
    p2=p2+
      geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.2)
    
  } else{
    p2=p2
  }
  
  
  
  ######################################################################
  ### Age specific rate
  #dfres= glores@agespec.proj
  dfres=BAPC::agespec.proj(glores)
  #dfres=BAPC::agespec.proj(glores)
  # Assuming dfres is a list where each element contains data for a specific age group
  age_groups <- names(dfres)
  # Create a function to process each age group
  process_age_group <- function(data, group_name) {
    data %>% as_tibble() %>%
      dplyr::mutate(Time = 1990:preyear, group = group_name) %>%
      purrr::set_names(c("val", "sd","Time", "group"))
  }
  
  # Use map2 from purrr to apply the function to each element in dfres along with the corresponding age group name
  combined_dfcase <- map2_dfr(dfres, age_groups, process_age_group) %>% 
    dplyr::mutate(val=val,sd=sd) %>% 
    dplyr::mutate(
      low_50 = val - 0.674 * sd,up_50 = val + 0.674 * sd,
      low_60 = val - 0.841 * sd,up_60 = val + 0.841 * sd,
      low_70 = val - 1.036 * sd,up_70 = val + 1.036 * sd,
      low_80 = val - 1.282 * sd,up_80 = val + 1.282 * sd,
      low = val - 1.96 * sd, up = val + 1.96 * sd
    ) %>% filter(val>0 )
  
  ### ggplot
  p3=ggplot(data=combined_dfcase,aes(Time,val))+
    geom_point(color = '#8ac6d1')+
    geom_line(color = '#8ac6d1')+
    #geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.2)+
    geom_point(data=combined_dfcase %>% filter(Time>2021),aes(Time,val,color="red"))+
    geom_line(data=combined_dfcase %>% filter(Time>2021),aes(Time,val,color="red"))+
    geom_vline(xintercept=2021, linetype="dashed", color = "grey")+
    scale_x_continuous(breaks = seq(1990, preyear, by = 5))+
    facet_wrap(vars(group), ncol = 3,scales = "free")+
    labs(y="Prediction cases")+
    theme_classic()+
    theme(strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
          strip.text = element_text(size = 12),
          legend.position="none"
    )
  
  if(CI){
    p3=p3+
      geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.2)
  } else{
    p3=p3
  }
  
  dfx=dfx
  apc_res= glores
  p1=p1
  p1x=p1x
  p2=p2
  p3=p3
  dfasr=dfa
  dfcomb=combined_df
  dfcase=dfa_case
  combined_dfcase=combined_dfcase
  
  # 保存 CSV 文件
  write.csv(dfasr, paste0("Fig7-",j, "-rate.csv"))
  
  # 保存 PDF 文件
  ggsave(filename = paste0("Fig7-",j, "-rate.pdf"), plot = p1, width = 14, height = 8)
}
