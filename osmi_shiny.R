library(tidyverse)
library(ggrepel)
library(stringr)
library(forcats)
library(plotly)
library(RColorBrewer)
library(scales)
library(shiny)
library(shinydashboard)

# Functions
dx_tf_frame_mult <- function(cols){
  new_osmi %>% 
    select(dx_mental_health, one_of(cols)) %>% 
    gather_(key = 'group', value = 'T_F', cols) %>% 
    filter(T_F == T) %>% 
    group_by(group, dx_mental_health) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    filter_(!is.na(cols))
}

dx_tf_frame_single <- function(cols){
  new_osmi %>% 
    select(dx_mental_health, one_of(cols)) %>% 
    group_by_('dx_mental_health', cols) %>% 
    summarize(count=n()) %>%
    ungroup() %>%  
    rename_('group' = cols) %>% 
    filter(!is.na(group))
}


tf_plot<- function(tf_frame, title){
  ggplot(tf_frame, aes(x=group, y = count, fill = dx_mental_health, label =count)) +
    geom_bar(stat = 'identity', position='fill')+
    geom_label(position = 'fill', show.legend = F) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_minimal()+
    scale_fill_manual(values = c('#7fcdbb','#2c7fb8'), name = "Diagnosis of\nMental Illness") +
    scale_y_continuous(labels = percent_format(), name = 'Proportion') +
    theme(axis.text.x = element_text(angle = 90, size = 12),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 28),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          axis.title.y = element_text(size = 12)) +
    ggtitle(title)
}

tf_plot_unfilled <- function(tf_frame, title){
  ggplot(tf_frame, aes(x=group, y = count, fill = dx_mental_health, label =count)) +
    geom_bar(stat = 'identity', position='stack')+
    geom_label(position = 'stack', show.legend = F) +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_minimal()+
    scale_fill_manual(values = c('#7fcdbb','#2c7fb8'), name = "Diagnosis of\nMental Illness") +
    scale_y_continuous(labels = comma_format(), name = 'Gross Number') +
    theme(axis.text.x = element_text(angle = 90, size = 12),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 28),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          axis.title.y = element_text(size = 12)) +
    ggtitle(title)
}

dx_subsets<-function(osmi_subset){
  osmi_subset %>% 
    select(autism,aspergers,anxiety, substance_abuse,ocd,add,
           depression,gender_dysphoria,eating_disorder,adhd,ptsd) %>% 
    gather(key = condition, value = n, autism,aspergers,anxiety, substance_abuse,ocd,add,
           depression,gender_dysphoria,eating_disorder,adhd,ptsd) %>%
    group_by(condition) %>% 
    summarize(n=sum(n))
}
prevalence_plot <- function(prev_df,title, breaks_df, labels_df){
  ggplot(prev_df %>% filter(group != 'n_total'), aes(x = group, y=n, fill = condition)) +
    geom_bar(stat='identity', position='fill') +
    theme_minimal() +
    scale_fill_brewer(type = 'qual', palette = 3, name = "Mental Illness",
                      breaks = c('add','adhd','anxiety','aspergers','autism','depression','eating_disorder','gender_dysphoria','ocd','ptsd','substance_abuse'),
                      labels = c('ADD','ADHD','Anxiety','Aspergers','Autism','Depression','Eating Disorder','Gender Dysphoria','OCD','PTSD','Substance Abuse')) + 
    scale_y_continuous(labels = percent_format(), name = 'Proportion') +
    scale_x_discrete(breaks = breaks_df,
                     labels=labels_df) +
    theme(axis.text.x = element_text(angle = 90, size = 12),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 28),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          axis.title.y = element_text(size = 12)) +
    ggtitle(title)
}

prevalence_plot_unfilled <- function(prev_df,title, breaks_df, labels_df){
  ggplot(prev_df %>% filter(group != 'n_total'), aes(x = group, y=n, fill = condition)) +
    geom_bar(stat='identity', position='stack') +
    theme_minimal() +
    scale_fill_brewer(type = 'qual', palette = 3, name = "Mental Illness",
                      breaks = c('add','adhd','anxiety','aspergers','autism','depression','eating_disorder','gender_dysphoria','ocd','ptsd','substance_abuse'),
                      labels = c('ADD','ADHD','Anxiety','Aspergers','Autism','Depression','Eating Disorder','Gender Dysphoria','OCD','PTSD','Substance Abuse')) + 
    scale_y_continuous(labels = comma_format(), name = 'Gross Number') +
    scale_x_discrete(breaks = breaks_df,
                     labels=labels_df) +
    theme(axis.text.x = element_text(angle = 90, size = 12),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 28),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 16),
          axis.title.y = element_text(size = 12)) +
    ggtitle(title)
}

# Read in Data and build relevant frames
new_osmi <- read_csv('shiny_osmi.csv')

# Plots
dx_job_f <- dx_tf_frame_mult(c("back_end", "front_end", "design", "executive","supervisor","dev_ops", "developer",
                   "one_person_shop", "support", "sys_admin", "other_job", "dev_evangelist", 
                   "team_lead")) %>% 
  tf_plot('Type of Job')
dx_size_f <- dx_tf_frame_single('num_ee') %>% tf_plot('Size of Company')
dx_gender_f <- dx_tf_frame_mult(c('male','female','other_gender')) %>% tf_plot('Gender') 
dx_region_live_f <- dx_tf_frame_single('state_region_live') %>% tf_plot('Region of Residence')
dx_region_work_f <- dx_tf_frame_single('state_region_work') %>% tf_plot('Region of Job')
dx_continent_live_f <- dx_tf_frame_single('continent_live') %>% tf_plot('Continent of Residence')
dx_continent_work_f <- dx_tf_frame_single('continent_work') %>% tf_plot('Continent of Job')

dx_job <- dx_tf_frame_mult(c("back_end", "front_end", "design", "executive","supervisor","dev_ops", "developer",
                   "one_person_shop", "support", "sys_admin", "other_job", "dev_evangelist", 
                   "team_lead")) %>% 
  tf_plot_unfilled('Type of Job')
dx_size <- dx_tf_frame_single('num_ee') %>% tf_plot_unfilled('Size of Company')
dx_gender <- dx_tf_frame_mult(c('male','female','other_gender')) %>% tf_plot_unfilled('Gender') 
dx_region_live <- dx_tf_frame_single('state_region_live') %>% tf_plot_unfilled('Region of Residence')
dx_region_work <- dx_tf_frame_single('state_region_work') %>% tf_plot_unfilled('Region of Job')
dx_continent_live <- dx_tf_frame_single('continent_live') %>% tf_plot_unfilled('Continent of Residence')
dx_continent_work <- dx_tf_frame_single('continent_work') %>% tf_plot_unfilled('Continent of Job')

illness_job_f<-dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, back_end == T) %>% dx_subsets(), by = 'condition', suffix = c('_total','_back_end')) %>% 
  left_join(filter(new_osmi, front_end == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, design == T) %>% dx_subsets(), by = 'condition', suffix = c('_front_end','_design')) %>% 
  left_join(filter(new_osmi, executive == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, supervisor == T) %>% dx_subsets(), by = 'condition', suffix = c('_executive','_supervisor')) %>% 
  left_join(filter(new_osmi, dev_ops == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, developer == T) %>% dx_subsets(), by = 'condition', suffix = c('_dev_ops','_developer')) %>% 
  left_join(filter(new_osmi, one_person_shop == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, support == T) %>% dx_subsets(), by = 'condition', suffix = c('_one_person_shop','_support')) %>% 
  left_join(filter(new_osmi, sys_admin == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, other_job == T) %>% dx_subsets(), by = 'condition', suffix = c('_sys_admin','_other')) %>% 
  left_join(filter(new_osmi, dev_evangelist == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, team_lead == T) %>% dx_subsets(), by = 'condition', suffix = c('_dev_evangelist','_team_lead')) %>% 
  gather(key = group, value = n, n_total,n_back_end,n_front_end,n_design,n_executive,n_supervisor,
         n_dev_ops,n_developer,n_one_person_shop,n_support,n_sys_admin,n_other,
         n_dev_evangelist,n_team_lead) %>% 
  prevalence_plot('Mental Illness By Job', 
                  breaks_df=c('n_back_end','n_front_end','n_design','n_executive','n_supervisor','n_dev_ops','n_developer','n_one_person_shop','n_support','n_sys_admin','n_other','n_dev_evangelist','n_team_lead'),
                  labels_df=c('Back End','Front End','Design','Executive','Supervisor','Dev Ops','Developer (Any Kind)','One Person Shop','Support','Sys Admin','Other','Dev Evangelist','Team Lead'))

illness_job<-dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, back_end == T) %>% dx_subsets(), by = 'condition', suffix = c('_total','_back_end')) %>% 
  left_join(filter(new_osmi, front_end == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, design == T) %>% dx_subsets(), by = 'condition', suffix = c('_front_end','_design')) %>% 
  left_join(filter(new_osmi, executive == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, supervisor == T) %>% dx_subsets(), by = 'condition', suffix = c('_executive','_supervisor')) %>% 
  left_join(filter(new_osmi, dev_ops == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, developer == T) %>% dx_subsets(), by = 'condition', suffix = c('_dev_ops','_developer')) %>% 
  left_join(filter(new_osmi, one_person_shop == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, support == T) %>% dx_subsets(), by = 'condition', suffix = c('_one_person_shop','_support')) %>% 
  left_join(filter(new_osmi, sys_admin == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, other_job == T) %>% dx_subsets(), by = 'condition', suffix = c('_sys_admin','_other')) %>% 
  left_join(filter(new_osmi, dev_evangelist == T) %>% dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, team_lead == T) %>% dx_subsets(), by = 'condition', suffix = c('_dev_evangelist','_team_lead')) %>% 
  gather(key = group, value = n, n_total,n_back_end,n_front_end,n_design,n_executive,n_supervisor,
         n_dev_ops,n_developer,n_one_person_shop,n_support,n_sys_admin,n_other,
         n_dev_evangelist,n_team_lead) %>% 
  prevalence_plot_unfilled('Mental Illness By Job', 
                           breaks_df=c('n_back_end','n_front_end','n_design','n_executive','n_supervisor','n_dev_ops','n_developer','n_one_person_shop','n_support','n_sys_admin','n_other','n_dev_evangelist','n_team_lead'),
                           labels_df=c('Back End','Front End','Design','Executive','Supervisor','Dev Ops','Developer (Any Kind)','One Person Shop','Support','Sys Admin','Other','Dev Evangelist','Team Lead'))

illness_size_f <- dx_subsets(filter(new_osmi,num_ee == '1-5')) %>% 
  left_join(filter(new_osmi,num_ee == '6-25') 
            %>% dx_subsets(), by = 'condition', suffix = c('_1_5','_6_25')
  ) %>% 
  left_join(filter(new_osmi,num_ee == '26-100') 
            %>% dx_subsets(), by = 'condition'
  ) %>%
  left_join(filter(new_osmi,num_ee == '100-500') 
            %>% dx_subsets(), by = 'condition', suffix = c('_26_100','_100_500')
  ) %>%
  left_join(filter(new_osmi,num_ee == '500-1000') 
            %>% dx_subsets(), by = 'condition'
  ) %>%
  left_join(filter(new_osmi,num_ee == 'More than 1000') 
            %>% dx_subsets(), by = 'condition', suffix = c('_500_1000','_1000_plus')
  ) %>%  
  gather(key = group, value = n, n_1_5,n_6_25,n_26_100,n_100_500,n_500_1000,n_1000_plus) %>% 
  mutate(group = fct_relevel(group,'n_1_5','n_6_25','n_26_100','n_100_500','n_500_1000','n_1000_plus' )) %>% 
  prevalence_plot('Mental Illness by Company Size',
                  c('n_1_5','n_6_25','n_26_100','n_100_500','n_500_1000','n_1000_plus'),
                  c('1-5','6-25','26-100','100-500','500-1000','1000+'))

illness_size <- dx_subsets(filter(new_osmi,num_ee == '1-5')) %>% 
  left_join(filter(new_osmi,num_ee == '6-25') 
            %>% dx_subsets(), by = 'condition', suffix = c('_1_5','_6_25')
  ) %>% 
  left_join(filter(new_osmi,num_ee == '26-100') 
            %>% dx_subsets(), by = 'condition'
  ) %>%
  left_join(filter(new_osmi,num_ee == '100-500') 
            %>% dx_subsets(), by = 'condition', suffix = c('_26_100','_100_500')
  ) %>%
  left_join(filter(new_osmi,num_ee == '500-1000') 
            %>% dx_subsets(), by = 'condition'
  ) %>%
  left_join(filter(new_osmi,num_ee == 'More than 1000') 
            %>% dx_subsets(), by = 'condition', suffix = c('_500_1000','_1000_plus')
  ) %>%  
  gather(key = group, value = n, n_1_5,n_6_25,n_26_100,n_100_500,n_500_1000,n_1000_plus) %>% 
  mutate(group = fct_relevel(group,'n_1_5','n_6_25','n_26_100','n_100_500','n_500_1000','n_1000_plus' )) %>% 
  prevalence_plot_unfilled('Mental Illness by Company Size',
                  c('n_1_5','n_6_25','n_26_100','n_100_500','n_500_1000','n_1000_plus'),
                  c('1-5','6-25','26-100','100-500','500-1000','1000+'))

illness_gender_f <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, male == T) %>% dx_subsets(), by='condition', suffix = c('_total','_male')) %>% 
  left_join(filter(new_osmi, female == T) %>% dx_subsets(), by='condition') %>% 
  left_join(filter(new_osmi, other_gender == T) %>% dx_subsets(), by='condition', suffix = c('_female','_other')) %>% 
  gather(key = group, value = n, n_male,n_female,n_other) %>% 
  prevalence_plot('Mental Illness by Gender',
                  c('n_male','n_female','n_other'),
                  c('Cis Male','Cis Fmeale','Other'))

illness_gender <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, male == T) %>% dx_subsets(), by='condition', suffix = c('_total','_male')) %>% 
  left_join(filter(new_osmi, female == T) %>% dx_subsets(), by='condition') %>% 
  left_join(filter(new_osmi, other_gender == T) %>% dx_subsets(), by='condition', suffix = c('_female','_other')) %>% 
  gather(key = group, value = n, n_male,n_female,n_other) %>% 
  prevalence_plot_unfilled('Mental Illness by Gender',
                  c('n_male','n_female','n_other'),
                  c('Cis Male','Cis Fmeale','Other'))

illness_region_work_f <- dx_subsets(filter(new_osmi, state_region_work == 'Midwest')) %>% 
  left_join(filter(new_osmi, state_region_work == 'South') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Midwest','_South')) %>%
  left_join(filter(new_osmi, state_region_work == 'West') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, state_region_work == 'Northeast') %>% 
              dx_subsets(), by = 'condition', suffix = c('_West','_Northeast')) %>%  
  gather(key = group, value = n, n_Midwest, n_South, n_West, n_Northeast) %>% 
  prevalence_plot('Mental Illness by Region (Work)',
                  c('n_Midwest', 'n_South', 'n_West', 'n_Northeast'),
                  c('Midwest','South','West','Northeast'))

illness_region_work <- dx_subsets(filter(new_osmi, state_region_work == 'Midwest')) %>% 
  left_join(filter(new_osmi, state_region_work == 'South') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Midwest','_South')) %>%
  left_join(filter(new_osmi, state_region_work == 'West') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, state_region_work == 'Northeast') %>% 
              dx_subsets(), by = 'condition', suffix = c('_West','_Northeast')) %>%  
  gather(key = group, value = n, n_Midwest, n_South, n_West, n_Northeast) %>% 
  prevalence_plot_unfilled('Mental Illness by Region (Work)',
                  c('n_Midwest', 'n_South', 'n_West', 'n_Northeast'),
                  c('Midwest','South','West','Northeast'))

illness_region_live_f<-dx_subsets(filter(new_osmi, state_region_live == 'Midwest')) %>% 
  left_join(filter(new_osmi, state_region_live == 'South') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Midwest','_South')) %>%
  left_join(filter(new_osmi, state_region_live == 'West') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, state_region_live == 'Northeast') %>% 
              dx_subsets(), by = 'condition', suffix = c('_West','_Northeast')) %>%  
  gather(key = group, value = n, n_Midwest, n_South, n_West, n_Northeast) %>%  
  prevalence_plot('Mental Illness by Region (Live)',
                  c('n_Midwest', 'n_South', 'n_West', 'n_Northeast'),
                  c('Midwest','South','West','Northeast'))

illness_region_live <-dx_subsets(filter(new_osmi, state_region_live == 'Midwest')) %>% 
  left_join(filter(new_osmi, state_region_live == 'South') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Midwest','_South')) %>%
  left_join(filter(new_osmi, state_region_live == 'West') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, state_region_live == 'Northeast') %>% 
              dx_subsets(), by = 'condition', suffix = c('_West','_Northeast')) %>%  
  gather(key = group, value = n, n_Midwest, n_South, n_West, n_Northeast) %>%  
  prevalence_plot_unfilled('Mental Illness by Region (Live)',
                  c('n_Midwest', 'n_South', 'n_West', 'n_Northeast'),
                  c('Midwest','South','West','Northeast'))

illness_continent_live_f <- dx_subsets(filter(new_osmi, continent_live == 'Europe')) %>% 
  left_join(filter(new_osmi, continent_live == 'North America') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Europe','_North_America')) %>%
  left_join(filter(new_osmi, continent_live == 'Oceania') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, continent_live == 'South America') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Oceania','_South_America')) %>%
  left_join(filter(new_osmi, continent_live == 'Asia') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, continent_live == 'Africa') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Asia','_Africa')) %>% 
  gather(key = group, value = n, n_Europe, n_North_America, n_Oceania, n_South_America, n_Asia, n_Africa) %>% 
  prevalence_plot('Mental Illness by Continent (Live)',
                  c('n_Europe', 'n_North_America', 'n_Oceania', 'n_South_America', 'n_Asia', 'n_Africa'),
                  c('Europe','North America','Oceania','South America','Asia','Africa'))

illness_continent_live <- dx_subsets(filter(new_osmi, continent_live == 'Europe')) %>% 
  left_join(filter(new_osmi, continent_live == 'North America') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Europe','_North_America')) %>%
  left_join(filter(new_osmi, continent_live == 'Oceania') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, continent_live == 'South America') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Oceania','_South_America')) %>%
  left_join(filter(new_osmi, continent_live == 'Asia') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, continent_live == 'Africa') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Asia','_Africa')) %>% 
  gather(key = group, value = n, n_Europe, n_North_America, n_Oceania, n_South_America, n_Asia, n_Africa) %>% 
  prevalence_plot_unfilled('Mental Illness by Continent (Live)',
                  c('n_Europe', 'n_North_America', 'n_Oceania', 'n_South_America', 'n_Asia', 'n_Africa'),
                  c('Europe','North America','Oceania','South America','Asia','Africa'))

illness_continent_work_f <- dx_subsets(filter(new_osmi, continent_work == 'Europe')) %>% 
  left_join(filter(new_osmi, continent_work == 'North America') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Europe','_North_America')) %>%
  left_join(filter(new_osmi, continent_work == 'Oceania') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, continent_work == 'South America') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Oceania','_South_America')) %>%
  left_join(filter(new_osmi, continent_work == 'Asia') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, continent_work == 'Africa') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Asia','_Africa')) %>% 
  gather(key = group, value = n, n_Europe, n_North_America, n_Oceania, n_South_America, n_Asia, n_Africa) %>% 
  prevalence_plot('Mental Illness by Continent (Work)',
                  c('n_Europe', 'n_North_America', 'n_Oceania', 'n_South_America', 'n_Asia', 'n_Africa'),
                  c('Europe','North America','Oceania','South America','Asia','Africa'))

illness_continent_work <- dx_subsets(filter(new_osmi, continent_work == 'Europe')) %>% 
  left_join(filter(new_osmi, continent_work == 'North America') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Europe','_North_America')) %>%
  left_join(filter(new_osmi, continent_work == 'Oceania') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, continent_work == 'South America') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Oceania','_South_America')) %>%
  left_join(filter(new_osmi, continent_work == 'Asia') %>% 
              dx_subsets(), by = 'condition') %>% 
  left_join(filter(new_osmi, continent_work == 'Africa') %>% 
              dx_subsets(), by = 'condition', suffix = c('_Asia','_Africa')) %>% 
  gather(key = group, value = n, n_Europe, n_North_America, n_Oceania, n_South_America, n_Asia, n_Africa) %>% 
  prevalence_plot_unfilled('Mental Illness by Continent (Work)',
                  c('n_Europe', 'n_North_America', 'n_Oceania', 'n_South_America', 'n_Asia', 'n_Africa'),
                  c('Europe','North America','Oceania','South America','Asia','Africa'))

dx_benefits_f <- dx_tf_frame_single('employer_mental_health_benefits') %>%
  filter(!is.na(group)) %>% 
  tf_plot('Employer Offers Mental Health Benefits')

dx_benefits <- dx_tf_frame_single('employer_mental_health_benefits') %>%
  filter(!is.na(group)) %>% 
  tf_plot_unfilled('Employer Offers Mental Health Benefits')

dx_discussed_f <- dx_tf_frame_single('employer_discussed_mental_health') %>% 
  filter(!is.na(group)) %>% 
  tf_plot('Employer Ever Formally Discussed Mental Health')

dx_discussed <- dx_tf_frame_single('employer_discussed_mental_health') %>% 
  filter(!is.na(group)) %>% 
  tf_plot_unfilled('Employer Ever Formally Discussed Mental Health')

dx_offers_f <- dx_tf_frame_single('employer_offers_mh_resources') %>% 
  filter(!is.na(group)) %>% 
  tf_plot('Employer Offers Mental Health Resources')
dx_offers <- dx_tf_frame_single('employer_offers_mh_resources') %>% 
  filter(!is.na(group)) %>% 
  tf_plot_unfilled('Employer Offers Mental Health Resources')

dx_leave_f <- dx_tf_frame_single('difficulty_asking_for_leave') %>% 
  filter(!is.na(group)) %>% 
  tf_plot('Asking For Leave Based On Mental Health Would Be:')
dx_leave <- dx_tf_frame_single('difficulty_asking_for_leave') %>% 
  filter(!is.na(group)) %>% 
  tf_plot_unfilled('Asking For Leave Based On Mental Health Would Be:')

dx_consequence_f <- dx_tf_frame_single('discussing_mental_health_consequences') %>% 
  filter(!is.na(group)) %>% 
  tf_plot('Would Discussing Mental Health with Employer Have Negative Consequences')
dx_consequence <- dx_tf_frame_single('discussing_mental_health_consequences') %>% 
  filter(!is.na(group)) %>% 
  tf_plot_unfilled('Would Discussing Mental Health with Employer Have Negative Consequences')

dx_comfort_f <- dx_tf_frame_single('comfort_discussing_mh_coworkers') %>% 
  filter(!is.na(group)) %>% 
  tf_plot('Comfortable Discussing Mental Health Disorder With Coworkers')
dx_comfort <- dx_tf_frame_single('comfort_discussing_mh_coworkers') %>% 
  filter(!is.na(group)) %>% 
  tf_plot_unfilled('Comfortable Discussing Mental Health Disorder With Coworkers')

dx_supervisor_f <- dx_tf_frame_single('comfort_discussing_mh_supervisor') %>% 
  filter(!is.na(group)) %>% 
  tf_plot('Comfortable Discussing Mental Health Disorder With Supervisor')
dx_supervisor <- dx_tf_frame_single('comfort_discussing_mh_supervisor') %>% 
  filter(!is.na(group)) %>% 
  tf_plot_unfilled('Comfortable Discussing Mental Health Disorder With Supervisor')

dx_phys_f <- dx_tf_frame_single('mh_serious_as_physical') %>% 
  filter(!is.na(group)) %>% 
  tf_plot('Employer Takes Mental Health As Serious As Physical Health')
dx_phys <- dx_tf_frame_single('mh_serious_as_physical') %>% 
  filter(!is.na(group)) %>% 
  tf_plot_unfilled('Employer Takes Mental Health As Serious As Physical Health')

dx_unresponsive_f <- dx_tf_frame_single('observed_neg_consequences') %>%
  filter(!is.na(group)) %>% 
  tf_plot('Observed/Experienced Unsupportive Response to Mental Health Issue')
dx_unresponsive <- dx_tf_frame_single('observed_neg_consequences') %>%
  filter(!is.na(group)) %>% 
  tf_plot_unfilled('Observed/Experienced Unsupportive Response to Mental Health Issue')

illness_benefits_f <- dx_subsets(filter(new_osmi, employer_mental_health_benefits == 'Not eligible for coverage / N/A')) %>% 
  left_join(filter(new_osmi, employer_mental_health_benefits == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_not_eligible','_No')) %>%
  left_join(filter(new_osmi, employer_mental_health_benefits == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, employer_mental_health_benefits == "I don't know") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Dont_know')) %>%
  gather(key = group, value = n, n_not_eligible, n_No, n_Yes, n_Dont_know) %>% 
  prevalence_plot('Employer Offers Mental Health Benefits',
                  c('n_No', 'n_Yes', 'n_Dont_know', 'n_not_eligible'),
                  c('No','Yes',"I Don't Know",'Not Eligible'))

illness_benefits <- dx_subsets(filter(new_osmi, employer_mental_health_benefits == 'Not eligible for coverage / N/A')) %>% 
  left_join(filter(new_osmi, employer_mental_health_benefits == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_not_eligible','_No')) %>%
  left_join(filter(new_osmi, employer_mental_health_benefits == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, employer_mental_health_benefits == "I don't know") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Dont_know')) %>%
  gather(key = group, value = n, n_not_eligible, n_No, n_Yes, n_Dont_know) %>% 
  prevalence_plot_unfilled('Employer Offers Mental Health Benefits',
                  c('n_No', 'n_Yes', 'n_Dont_know', 'n_not_eligible'),
                  c('No','Yes',"I Don't Know", 'Not Eligible'))

illness_discussed_f <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, employer_discussed_mental_health == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, employer_discussed_mental_health == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, employer_discussed_mental_health == "I don't know") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Dont_know')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Dont_know) %>% 
  prevalence_plot('Employer Ever Formally Discussed Mental Health',
                  c('n_No', 'n_Yes', 'n_Dont_know'),
                  c('No','Yes',"I Don't Know"))

illness_discussed <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, employer_discussed_mental_health == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, employer_discussed_mental_health == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, employer_discussed_mental_health == "I don't know") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Dont_know')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Dont_know) %>% 
  prevalence_plot_unfilled('Employer Ever Formally Discussed Mental Health',
                  c('n_No', 'n_Yes', 'n_Dont_know'),
                  c('No','Yes',"I Don't Know"))

illness_offers_f <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, employer_offers_mh_resources == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, employer_offers_mh_resources == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, employer_offers_mh_resources == "I don't know") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Dont_know')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Dont_know) %>% 
  prevalence_plot('Employer Offers Mental Health Benefits',
                  c('n_No', 'n_Yes', 'n_Dont_know'),
                  c('No','Yes',"I Don't Know"))

illness_offers <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, employer_offers_mh_resources == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, employer_offers_mh_resources == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, employer_offers_mh_resources == "I don't know") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Dont_know')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Dont_know) %>% 
  prevalence_plot_unfilled('Employer Offers Mental Health Benefits',
                  c('n_No', 'n_Yes', 'n_Dont_know'),
                  c('No','Yes',"I Don't Know"))

illness_leave_f <- dx_subsets(filter(new_osmi, difficulty_asking_for_leave == 'Very easy')) %>%
  left_join(filter(new_osmi, difficulty_asking_for_leave == 'Somewhat easy') %>% 
              dx_subsets(), by = 'condition', suffix = c('_very_easy','_somewhat_easy')) %>%
  left_join(filter(new_osmi, difficulty_asking_for_leave == "Neither easy nor difficult") %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, difficulty_asking_for_leave == 'Very difficult') %>% 
              dx_subsets(), by = 'condition', suffix = c('_not_easy_nor_diff','_very_difficult')) %>%
  left_join(filter(new_osmi, difficulty_asking_for_leave == "Somewhat difficult") %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, difficulty_asking_for_leave == "I don't know") %>% 
              dx_subsets(), by = 'condition', suffix = c('_somewhat_difficult','_dont_know')) %>%
  gather(key = group, value = n, n_very_easy, n_somewhat_easy, n_not_easy_nor_diff, n_very_difficult, n_somewhat_difficult, n_dont_know) %>% 
  mutate(group = fct_relevel(group, 'n_very_easy', 'n_somewhat_easy', 'n_not_easy_nor_diff', 'n_somewhat_difficult', 'n_very_difficult', 'n_dont_know')) %>% 
  prevalence_plot('Asking For Leave Based On Mental Health Would Be:',
                  c('n_very_easy', 'n_somewhat_easy', 'n_not_easy_nor_diff', 'n_very_difficult', 'n_somewhat_difficult', 'n_dont_know'),
                  c('Very Easy','Somewhat Easy','Not Easy Nor Difficult','Very Difficult','Somewhat Difficult',"I Don't Know"))

illness_leave <- dx_subsets(filter(new_osmi, difficulty_asking_for_leave == 'Very easy')) %>%
  left_join(filter(new_osmi, difficulty_asking_for_leave == 'Somewhat easy') %>% 
              dx_subsets(), by = 'condition', suffix = c('_very_easy','_somewhat_easy')) %>%
  left_join(filter(new_osmi, difficulty_asking_for_leave == "Neither easy nor difficult") %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, difficulty_asking_for_leave == 'Very difficult') %>% 
              dx_subsets(), by = 'condition', suffix = c('_not_easy_nor_diff','_very_difficult')) %>%
  left_join(filter(new_osmi, difficulty_asking_for_leave == "Somewhat difficult") %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, difficulty_asking_for_leave == "I don't know") %>% 
              dx_subsets(), by = 'condition', suffix = c('_somewhat_difficult','_dont_know')) %>%
  gather(key = group, value = n, n_very_easy, n_somewhat_easy, n_not_easy_nor_diff, n_very_difficult, n_somewhat_difficult, n_dont_know) %>% 
  mutate(group = fct_relevel(group, 'n_very_easy', 'n_somewhat_easy', 'n_not_easy_nor_diff', 'n_somewhat_difficult', 'n_very_difficult', 'n_dont_know')) %>% 
  prevalence_plot_unfilled('Asking For Leave Based On Mental Health Would Be:',
                  c('n_very_easy', 'n_somewhat_easy', 'n_not_easy_nor_diff', 'n_very_difficult', 'n_somewhat_difficult', 'n_dont_know'),
                  c('Very Easy','Somewhat Easy','Not Easy Nor Difficult','Very Difficult','Somewhat Difficult',"I Don't Know"))

illness_consequence_f <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, discussing_mental_health_consequences == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, discussing_mental_health_consequences == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, discussing_mental_health_consequences == "Maybe") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Maybe')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Maybe) %>% 
  prevalence_plot('Would Discussing Mental Health with Employer Have Negative Consequences',
                  c('n_No', 'n_Yes', 'n_Maybe'),
                  c('No','Yes','Maybe'))

illness_consequence <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, discussing_mental_health_consequences == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, discussing_mental_health_consequences == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, discussing_mental_health_consequences == "Maybe") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Maybe')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Maybe) %>% 
  prevalence_plot_unfilled('Would Discussing Mental Health with Employer Have Negative Consequences',
                  c('n_No', 'n_Yes', 'n_Maybe'),
                  c('No','Yes','Maybe'))

illness_comfort_f <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, comfort_discussing_mh_coworkers == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, comfort_discussing_mh_coworkers == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, comfort_discussing_mh_coworkers == "Maybe") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Maybe')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Maybe) %>% 
  prevalence_plot('Comfortable Discussing Mental Health Disorder With Coworkers',
                  c('n_No', 'n_Yes', 'n_Maybe'),
                  c('No','Yes','Maybe'))

illness_comfort <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, comfort_discussing_mh_coworkers == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, comfort_discussing_mh_coworkers == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, comfort_discussing_mh_coworkers == "Maybe") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Maybe')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Maybe) %>% 
  prevalence_plot_unfilled('Comfortable Discussing Mental Health Disorder With Coworkers',
                  c('n_No', 'n_Yes', 'n_Maybe'),
                  c('No','Yes','Maybe'))


illness_supervisor_f <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, comfort_discussing_mh_supervisor == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, comfort_discussing_mh_supervisor == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, comfort_discussing_mh_supervisor == "Maybe") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Maybe')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Maybe) %>% 
  prevalence_plot('Comfortable Discussing Mental Health Disorder With Supervisor',
                  c('n_No', 'n_Yes', 'n_Maybe'),
                  c('No','Yes','Maybe'))

illness_supervisor <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, comfort_discussing_mh_supervisor == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, comfort_discussing_mh_supervisor == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, comfort_discussing_mh_supervisor == "Maybe") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Maybe')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Maybe) %>% 
  prevalence_plot_unfilled('Comfortable Discussing Mental Health Disorder With Supervisor',
                  c('n_No', 'n_Yes', 'n_Maybe'),
                  c('No','Yes','Maybe'))

illness_phys_f <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, mh_serious_as_physical == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, mh_serious_as_physical == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, mh_serious_as_physical == "I don't know") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Dont_know')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Dont_know) %>% 
  prevalence_plot('Employer Takes Mental Health As Serious As Physical Health',
                  c('n_No', 'n_Yes', 'n_Dont_know'),
                  c('No','Yes',"I Don't Know"))

illness_phys <- dx_subsets(new_osmi) %>% 
  left_join(filter(new_osmi, mh_serious_as_physical == 'No') %>% 
              dx_subsets(), by = 'condition', suffix = c('_total','_No')) %>%
  left_join(filter(new_osmi, mh_serious_as_physical == 'Yes') %>% 
              dx_subsets(), by = 'condition') %>%
  left_join(filter(new_osmi, mh_serious_as_physical == "I don't know") %>% 
              dx_subsets(), by = 'condition', suffix = c('_Yes','_Dont_know')) %>%
  gather(key = group, value = n, n_No, n_Yes, n_Dont_know) %>% 
  prevalence_plot_unfilled('Employer Takes Mental Health As Serious As Physical Health',
                  c('n_No', 'n_Yes', 'n_Dont_know'),
                  c('No','Yes',"I Don't Know"))

illness_unresponsive_f <- dx_subsets(filter(new_osmi, observed_neg_consequences == 'Yes')) %>% 
  left_join(filter(new_osmi, observed_neg_consequences == 'No') %>% dx_subsets(), by='condition',suffix = c('_yes','_no')) %>% 
  gather(key = group, value = n, n_no, n_yes) %>% 
  prevalence_plot('Observed/Experienced Unsupportive Response to Mental Health Issue',
                  c('n_no', 'n_yes'),
                  c('No','Yes'))

illness_unresponsive <- dx_subsets(filter(new_osmi, observed_neg_consequences == 'Yes')) %>% 
  left_join(filter(new_osmi, observed_neg_consequences == 'No') %>% dx_subsets(), by='condition',suffix = c('_yes','_no')) %>% 
  gather(key = group, value = n, n_no, n_yes) %>% 
  prevalence_plot_unfilled('Observed/Experienced Unsupportive Response to Mental Health Issue',
                  c('n_no', 'n_yes'),
                  c('No','Yes'))


#Shiny
ui <- dashboardPage(
  dashboardHeader(title = 'OSMI Data Visualized',
                  titleWidth = 500),
  dashboardSidebar(
    selectInput('fill_bars','Bar Type:', c('Filled' = 'filled', 'Unfilled' = 'unfilled')),
    selectInput('plot_type', 'Level of Detail:', c('Presence of Any Condition' = 'dx', 'All Condition Detail' = 'illness')),
    selectInput('question', 'Question', c(
      'Job Type' = 'job',
      'Company Size' = 'size',
      'Gender' = 'gender',
      'US Region (Live)' = 'region_live',
      'US Region (Work)' = 'region_work',
      'Continent (Live)' = 'continent_live',
      'Continent (Work)' = 'continent_work',
      'Employer Offers Mental Health Benefits' = 'benefits',
      'Employer Ever Formally Discussed Mental Health' = 'discussed',
      'Employer Offers Mental Health Resources' = 'offers',
      'Asking For Leave Based On Mental Health Would Be:' = 'leave',
      'Would Discussing Mental Health with Employer Have Negative Consequences' = 'consequence',
      'Comfortable Discussing Mental Health Disorder With Coworkers' = 'comfort',
      'Comfortable Discussing Mental Health Disorder With Supervisor' = 'supervisor',
      'Employer Takes Mental Health As Serious As Physical Health' = 'phys',
      'Observed/Experienced Unsupportive Response to Mental Health Issue' = 'unresponsive'
    ), selected = 'benefits')
  ),
  dashboardBody(
    plotOutput('osmi_plot', height = '650px'),
    includeMarkdown('osmi.md')
  )
)

server <- function(input,output){
  output$osmi_plot <- renderPlot({get(paste0(
    ifelse(input$plot_type == 'illness', 'illness_','dx_'),
    input$question,
    ifelse(input$fill_bars == 'filled', '_f','')))})
}

shinyApp(ui,server)
