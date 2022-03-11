library(DT)
library(tidyverse)

source("moduleChangeTheme.R")
source("similarity_pages_input_server.R")
source("similarity_pages_output_server.R")

train_set = read_csv("Data/Train Set.csv")
similarity_scores=read_csv("Data/Similarity Scores.csv") %>% 
  #add identifying info to tibble
  left_join(.,train_set %>% select(seas_id,player_id,player,season),by=c('seas_id_base'='seas_id')) %>%
  left_join(.,train_set %>% select(seas_id,player_id,player,season,age,experience,type:first_year_percent_of_cap),
            by=c('to_compare'='seas_id'))

server <- function(input, output,session) {
  
  sim_page_input_server(id="hist",df=train_set)
  sim_page_output_server(id="hist",df=train_set,sim_scores_df=similarity_scores)
  
  #from https://github.com/nik01010/dashboardThemeSwitcher
  serverChangeTheme(id = "moduleChangeTheme")
}