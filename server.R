library(DT)
library(tidyverse)
library(plotly)
library(ggdark)
library(readxl)
library(DBI)
library(dbplyr)
library(RSQLite)

source("moduleChangeTheme.R")
source("similarity_pages_input_server.R")
source("similarity_pages_output_server.R")
source("projections_server.R")
source("actuals_server.R")

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "Data/free_agent_db.sqlite")

current_year=2023

actuals = read_excel("Data/Actual Contracts.xlsx",
                     col_types = c("text","numeric","numeric","numeric","date","text"))

formatted_actuals=left_join(actuals,
                            dbReadTable(con,"train_eval") %>% filter(season==current_year) %>% 
                              select(player,type,age,photo=urlPlayerThumbnail)) %>%
  relocate(photo,age,type,.after="player") %>% 
  mutate(source=if_else(is.na(source),NA,paste0('<a href="',source,'" target="_blank">Link</a>'))) %>%
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
  
  sim_page_input_server(id="hist",df=dbReadTable(con,"train_eval"))
  sim_page_output_server(id="hist",df=dbReadTable(con,"train_eval"),
                         sim_scores_df=dbReadTable(con,"similarity_scores"),show_future=TRUE)

  sim_page_input_server(id="curr",df=dbReadTable(con,"train_eval") %>% filter(season==current_year))
  sim_page_output_server(id="curr",df=dbReadTable(con,"train_eval"),
                         sim_scores_df=dbReadTable(con,"similarity_scores") %>% 
                           filter(season.x==current_year,season.y!=current_year),
                         show_future=FALSE)
  
  proj_server(id="opt_proj_player",df=dbReadTable(con,"options") %>% filter(option_type=="Player") %>% select(-option_type))
  proj_server(id="opt_proj_club",df=dbReadTable(con,"options") %>% filter(option_type=="Club") %>% select(-option_type))
  proj_server(id="non_opt_proj_rfa",df=dbReadTable(con,"non_options") %>% filter(type=="RFA") %>% select(-type),
              option_contract=FALSE)
  proj_server(id="non_opt_proj_ufa",df=dbReadTable(con,"non_options") %>% filter(type=="UFA") %>% select(-type),
              option_contract=FALSE)
  
  actuals_server(id="actuals",df=formatted_actuals)
  
  #from https://github.com/nik01010/dashboardThemeSwitcher
  serverChangeTheme(id = "moduleChangeTheme")
}