library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(plotly)

source("moduleChangeTheme.R")
source("similarity_pages_input_ui.R")
source("similarity_pages_output_ui.R")
source("projections_ui.R")

train_eval = read_csv("Data/Train & Eval Set Combined.csv")

ui <- dashboardPage(
  # set title width or else title is cutoff
  dashboardHeader(title = "NBA Free Agency Dashboard", titleWidth = 300),
  dashboardSidebar(sidebarMenu(
    menuItem("Similarity Scores", tabName = "similarity_scores"),
    menuItem("Similarity Scores 2022", tabName = "sim_scores_curr"),
    menuItem("2022 FA Projections", tabName = "projections"),
    menuItem("Change Theme", tabName = "tabThemes"),
    menuItem("About & FAQ",tabName="about_page")
  )),
  dashboardBody(uiChangeThemeOutput(),
                tabItems(
                  tabItem(tabName = "similarity_scores",
                          fluidPage(
                            fluidRow(sim_page_input_ui(id = "hist", df = train_eval,show_future=TRUE)),
                            hr(),
                            fluidRow(sim_page_output_ui(id = "hist", df = train_eval))
                          )
                          ),
                  tabItem(tabName = "sim_scores_curr",
                          fluidPage(
                            fluidRow(sim_page_input_ui(id = "curr", df = train_eval %>% filter(season==2022),show_future=FALSE)),
                            hr(),
                            fluidRow(sim_page_output_ui(id = "curr", df = train_eval))
                          )
                  ),
                  tabItem(tabName = "tabThemes", uiChangeThemeDropdown()),
                  tabItem(tabName = "projections", 
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Option Projections",
                                       fluidPage(br(),
                                         proj_ui(id="opt_proj")
                                         )
                                       ),
                              tabPanel("Non-Option Projections",
                                       fluidPage(br(),
                                         proj_ui(id="non_opt_proj")
                                         )
                              )
                            )
                          )),
                  tabItem(tabName = "about_page",
                          includeHTML("about_page.html")
                  )
                ))
)