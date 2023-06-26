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

current_year=2023

ui <- dashboardPage(
  # set title width or else title is cutoff
  dashboardHeader(title = "NBA Free Agency Dashboard", titleWidth = 300),
  dashboardSidebar(sidebarMenu(
    menuItem("Similarity Scores", tabName = "similarity_scores"),
    menuItem(paste("Similarity Scores",current_year), tabName = "sim_scores_curr"),
    menuItem(paste(current_year,"FA Projections"), tabName = "projections"),
    menuItem(paste(current_year,"FA Actual Contracts"), tabName = "actuals"),
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
                            fluidRow(sim_page_input_ui(id = "curr", df = train_eval %>% filter(season==current_year),show_future=FALSE)),
                            hr(),
                            fluidRow(sim_page_output_ui(id = "curr", df = train_eval))
                          )
                  ),
                  tabItem(tabName = "tabThemes", uiChangeThemeDropdown()),
                  tabItem(tabName = "projections",
                          fluidPage(
                              p("In the years-first & salary-second model (Y1S2): a random forest is used as the years prediction, and the mean
                                of a random forest & a support vector machine is used as the salary prediction."),
                              p("In the salary-first & years-second model (S1Y2): random forests are used for both components."),
                              hr(),
                            tabsetPanel(
                              tabPanel("Player Option Projections",
                                       fluidPage(br(),
                                         proj_ui(id="opt_proj_player")
                                         )
                                       ),
                              tabPanel("Club Option Projections",
                                       fluidPage(br(),
                                                 proj_ui(id="opt_proj_club")
                                       )
                              ),
                              tabPanel("RFA Projections",
                                       fluidPage(br(),
                                         proj_ui(id="non_opt_proj_rfa")
                                         )
                              ),
                              tabPanel("UFA Projections",
                                       fluidPage(br(),
                                                 proj_ui(id="non_opt_proj_ufa")
                                       )
                              )
                            )
                          )),
                  tabItem(tabName = "about_page",
                          includeHTML("about_page.html")
                  ),
                  tabItem(tabName="actuals",
                          fluidPage(
                            proj_ui(id="actuals")
                            )
                          )
                ))
)