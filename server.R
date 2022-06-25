library(DT)
library(tidyverse)
library(plotly)
library(ggdark)

source("moduleChangeTheme.R")
source("similarity_pages_input_server.R")
source("similarity_pages_output_server.R")
source("projections_server.R")

train_eval = read_csv("Data/Train & Eval Set Combined.csv") %>% left_join(.,read_csv("Data/Player Photos.csv")) %>%
  mutate(urlPlayerThumbnail=paste('<img src =',' "',urlPlayerThumbnail,'" ', 'height="60"></img>', sep = ""))
similarity_scores=read_csv("Data/Similarity Scores.csv") %>% 
  #add identifying info to tibble
  left_join(.,train_eval %>% select(seas_id,player_id,season),by=c('seas_id_base'='seas_id')) %>%
  left_join(.,train_eval %>% select(seas_id,player_id,season),
            by=c('to_compare'='seas_id'))

options = read_csv("Data/Options.csv") %>% mutate(across(c(`Y1S2 Cap %`,`S1Y2 Cap %`),~parse_number(.)/100)) %>%
  mutate(across(c(total_Y1S2,total_S1Y2,`2021 Option`),~parse_number(.)))
non_options = read_csv("Data/Non-Option Contracts.csv") %>% mutate(across(c(`Y1S2 Cap %`,`S1Y2 Cap %`),~parse_number(.)/100)) %>%
  mutate(across(c(total_Y1S2,total_S1Y2),~parse_number(.)))

server <- function(input, output,session) {
  
  sim_page_input_server(id="hist",df=train_eval)
  sim_page_output_server(id="hist",df=train_eval,sim_scores_df=similarity_scores,show_future=TRUE)

  sim_page_input_server(id="curr",df=train_eval %>% filter(season==2022))
  sim_page_output_server(id="curr",df=train_eval,
                         sim_scores_df=similarity_scores %>% filter(season.x==2022,season.y!=2022),
                         show_future=FALSE)
  
  proj_server(id="opt_proj",df=options)
  proj_server(id="non_opt_proj",df=non_options,option_contract=FALSE)
  
  #from https://github.com/nik01010/dashboardThemeSwitcher
  serverChangeTheme(id = "moduleChangeTheme")
}