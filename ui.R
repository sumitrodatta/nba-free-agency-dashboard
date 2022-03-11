library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(tidyverse)
library(DT)
library(shinycssloaders)

source("moduleChangeTheme.R")
source("similarity_pages_input_ui.R")
source("similarity_pages_output_ui.R")

train_set = read_csv("Data/Train Set.csv")

ui <- dashboardPage(
  # set title width or else title is cutoff
  dashboardHeader(title = "NBA Free Agency Dashboard", titleWidth = 300),
  dashboardSidebar(sidebarMenu(
    menuItem("Similarity Scores", tabName = "similarity_scores"),
    menuItem("Change Theme", tabName = "tabThemes")
  )),
  dashboardBody(uiChangeThemeOutput(),
                tabItems(
                  tabItem(tabName = "similarity_scores",
                          fluidPage(
                            fluidRow(sim_page_input_ui(id = "hist", df = train_set)),
                            hr(),
                            fluidRow(sim_page_output_ui(id="hist"))
                          )
                          ),
                  tabItem(tabName = "tabThemes", uiChangeThemeDropdown())
                ))
)