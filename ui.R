library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(tidyverse)
library(DT)

source("moduleChangeTheme.R")

train_set = read_csv("Data/Train Set.csv")

ui <- dashboardPage(
  dashboardHeader(title = "NBA Free Agency Dashboard", titleWidth = 300),
  dashboardSidebar(sidebarMenu(
    menuItem("Similarity Scores", tabName = "similarity_scores"),
    menuItem("Change Theme", tabName = "tabThemes")
  )),
  dashboardBody(uiChangeThemeOutput(),
                tabItems(
                  tabItem(tabName = "similarity_scores",
                          fluidPage(
                            fluidRow(
                              column(
                                width = 4,
                                pickerInput(
                                  inputId = "historical_fa_name",
                                  label = "Free Agent Name",
                                  choices = (train_set %>% distinct(player_id, player) %>%
                                               arrange(player) %>% pull(player)),
                                  options = pickerOptions(
                                    liveSearch = TRUE,
                                    liveSearchNormalize = TRUE,
                                    size = 10
                                  ),
                                  selected = "DeMar DeRozan"
                                ),
                                pickerInput(
                                  inputId = "historical_fa_yr",
                                  label = "Free Agency Year",
                                  choices = NULL,
                                  selected = 2016
                                )
                              ),
                              column(width = 8,
                                     DT::dataTableOutput("sel_table"))
                            ),
                            fluidRow(DT::dataTableOutput("sim_table")))),
                  tabItem(tabName = "tabThemes", uiChangeThemeDropdown())
                ))
)