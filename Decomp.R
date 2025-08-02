
df <- fread("china.csv") %>% mutate(measure_name = if_else(measure_name == "YLDs (Years Lived with Disability)", "YLDs", measure_name))

for (i in unique(df$measure_name)) {
  
  
  dfx=df %>% filter(measure_name ==i) %>% 
    #filter(cause_name =="All conditions") %>% 
    filter(location_id==6) %>% 
    filter(sex_name =="Both") %>% 
    filter(metric_name=="Rate") %>% 
    dplyr::filter(age_id %in%  c(1,5:20,30,31,32,235)) %>%
    dplyr::filter(year %in% c(1990,2021))
  
  
    
    ## Rate
    dfx=dfx 
    labx=paste0(unique(dfx$measure_name))
    y1=1990
    y2=2021
    add=T
    
    ## Pop
    pop=population %>% filter(location_id %in% unique(dfx$location_id)) %>% 
      filter(age_id %in% unique(dfx$age_id)) %>% 
      filter(sex_id %in% unique(dfx$sex_id)) %>% 
      filter(year %in% c(y1,y2)) %>% 
      select(age_id,location_id,sex_id,year,val)
    
    #计算1990,2021年全人群的年龄构成
    # Assuming your data frame is named 'pop'
    pop_summary <- pop %>%
      group_by(year) %>%
      mutate(total_population = sum(val)) %>%  # Calculate total population for each year
      mutate(proportion = val / total_population) %>%  # Calculate proportion for each age group
      ungroup() %>% mutate(pop=val) %>% select(-val)
    
    dfy=left_join(dfx,pop_summary) %>% 
      select(location_name,age_name,sex_name,year,val,total_population,proportion,pop)
    
    # Function to calculate effects for a given dataset
    calculate_effects <- function(dfx) {
      # Split the data into 1990 and 2021 data frames
      df_1990 <- dfx %>% filter(year == y1)
      df_2021 <- dfx %>% filter(year == y2)
      
      # Calculate a_effect using the given formula
      a_effect <- round((sum(df_2021$proportion * df_1990$total_population * df_1990$val) + 
                           sum(df_2021$proportion * df_2021$total_population * df_2021$val)) / 3 +
                          (sum(df_2021$proportion * df_1990$total_population * df_2021$val) + 
                             sum(df_2021$proportion * df_2021$total_population * df_1990$val)) / 6 -
                          (sum(df_1990$proportion * df_1990$total_population * df_1990$val) + 
                             sum(df_1990$proportion * df_2021$total_population * df_2021$val)) / 3 -
                          (sum(df_1990$proportion * df_1990$total_population * df_2021$val) + 
                             sum(df_1990$proportion * df_2021$total_population * df_1990$val)) / 6, 3)
      
      # Calculate p_effect using the given formula
      p_effect <- round((sum(df_1990$proportion * df_2021$total_population * df_1990$val) + 
                           sum(df_2021$proportion * df_2021$total_population * df_2021$val)) / 3 +
                          (sum(df_1990$proportion * df_2021$total_population * df_2021$val) + 
                             sum(df_2021$proportion * df_2021$total_population * df_1990$val)) / 6 -
                          (sum(df_1990$proportion * df_1990$total_population * df_1990$val) + 
                             sum(df_2021$proportion * df_1990$total_population * df_2021$val)) / 3 -
                          (sum(df_1990$proportion * df_1990$total_population * df_2021$val) + 
                             sum(df_2021$proportion * df_1990$total_population * df_1990$val)) / 6, 3)
      
      # Calculate r_effect using the given formula
      r_effect <- round((sum(df_1990$proportion * df_1990$total_population * df_2021$val) + 
                           sum(df_2021$proportion * df_2021$total_population * df_2021$val)) / 3 +
                          (sum(df_1990$proportion * df_2021$total_population * df_2021$val) + 
                             sum(df_2021$proportion * df_1990$total_population * df_2021$val)) / 6 -
                          (sum(df_1990$proportion * df_1990$total_population * df_1990$val) + 
                             sum(df_2021$proportion * df_2021$total_population * df_1990$val)) / 3 -
                          (sum(df_1990$proportion * df_2021$total_population * df_1990$val) + 
                             sum(df_2021$proportion * df_1990$total_population * df_1990$val)) / 6, 3)
      
      # Calculate the overall difference
      overall_difference <- round(a_effect + p_effect + r_effect, 2)
      
      # Calculate the percentage contributions
      a_percent <- round(a_effect / overall_difference * 100, 2)
      p_percent <- round(p_effect / overall_difference * 100, 2)
      r_percent <- round(r_effect / overall_difference * 100, 2)
      
      # Combine the results into a data frame
      data.frame(
        Overall_Difference = round(overall_difference/100000,2),
        Aging = round(a_effect/100000,2),
        Population = round(p_effect/100000,2),
        Epidemiological_Change = round(r_effect/100000,2),
        Aging_Percentage = a_percent,
        Population_Percentage = p_percent,
        Epidemiological_Change_Percentage = r_percent
      )
    }
    
    # Group the data by sex and apply the calculate_effects function to each group
    combined_effects <- dfy %>% mutate(group=paste0(location_name,sex_name)) %>% 
      group_by(group) %>%
      group_modify(~ calculate_effects(.x)) %>%
      ungroup() 
    
    a=dfy %>% select(location_name,sex_name) %>% unique() %>% 
      mutate(group=paste0(location_name,sex_name)) %>% filter(group %in% combined_effects$group)
    # View the combined data frame
    combined_effects=left_join(a,combined_effects)
    dfp=combined_effects %>% select(1:7) %>% 
      gather("Effect","Value",-location_name,-sex_name,-group)
    
    p1=ggplot(dfp %>% filter(!Effect == "Overall_Difference"), aes(x= location_name,y=Value, fill= Effect)) +
      geom_bar(stat="identity",position = "stack") +
      coord_flip() + 
      facet_wrap(~sex_name)+
      scale_y_continuous(labels = label_number(unit = "K"))+
      labs(x = "", y = labx,fill="") +
      ggsci::scale_fill_npg() +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "top",
      axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12, face = "bold")
    )
  
  p2=p1+geom_point(data=dfp %>% filter(Effect == "Overall_Difference"), 
                   mapping=aes(x=location_name,y= Value),
                   fill='black',color='black',size=3)
  if(add){
    p=p2
  } else{
    p=p1
  }
  
 
  
  
  
  
  
  ggsave(paste0("Fig分解-",i,".pdf"), plot = p1, width = 12, height = 8)
  write.csv(combined_effects, paste0("Fig分解-",i,".csv"), row.names = FALSE)
}