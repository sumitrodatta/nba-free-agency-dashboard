library(DT)
library(tidyverse)
library(plotly)
library(ggdark)
library(readxl)

source("moduleChangeTheme.R")
source("similarity_pages_input_server.R")
source("similarity_pages_output_server.R")
source("projections_server.R")
source("actuals_server.R")

current_year=2022

train_eval = read_csv("Data/Train & Eval Set Combined.csv") %>% left_join(.,read_csv("Data/Player Photos.csv")) %>%
  mutate(urlPlayerThumbnail=paste('<img src =',' "',urlPlayerThumbnail,'" ', 'height="60"></img>', sep = ""))
similarity_scores=read_csv("Data/Similarity Scores.csv") %>% 
  #add identifying info to tibble
  left_join(.,train_eval %>% select(seas_id,player_id,season),by=c('seas_id_base'='seas_id')) %>%
  left_join(.,train_eval %>% select(seas_id,player_id,season),
            by=c('to_compare'='seas_id'))

options = read_csv("Data/Options.csv")
non_options = read_csv("Data/Non-Option Contracts.csv")

actuals = read_excel("Data/Actual Contracts.xlsx",
                     col_types = c("text","numeric","numeric","numeric","date","text"))

formatted_actuals=left_join(actuals,
                            train_eval %>% filter(season==current_year) %>% 
                              select(player,type,age,photo=urlPlayerThumbnail)) %>%
  relocate(photo,age,type,.after="player") %>% mutate(source=paste0('<a href="',source,'" target="_blank">Link</a>')) %>%
  #when formatted, date jumps one day ahead
  mutate(date=date+24*60*60)

server <- function(input, output,session) {
  
  # sendSweetAlert(title = "Information",
  #                html=TRUE,
  #                text=tags$span("The Plotly graphs don't seem to load correctly while using Safari,",
  #                               " so I would strongly suggest to alternatively use Chrome or Firefox instead.",
  #                               tags$br(),tags$br(),
  #                               "In addition, I don't believe the web app is mobile-friendly",
  #                               " (those updates are hopefully for next year!), so I would recommend",
  #                               " navigating using desktop views."),
  #                type="info")
  
  sim_page_input_server(id="hist",df=train_eval)
  sim_page_output_server(id="hist",df=train_eval,sim_scores_df=similarity_scores,show_future=TRUE)

  sim_page_input_server(id="curr",df=train_eval %>% filter(season==current_year))
  sim_page_output_server(id="curr",df=train_eval,
                         sim_scores_df=similarity_scores %>% filter(season.x==current_year,season.y!=current_year),
                         show_future=FALSE)
  
  proj_server(id="opt_proj",df=options)
  proj_server(id="non_opt_proj",df=non_options,option_contract=FALSE)
  
  actuals_server(id="actuals",df=formatted_actuals)
  
  #from https://github.com/nik01010/dashboardThemeSwitcher
  serverChangeTheme(id = "moduleChangeTheme")
}