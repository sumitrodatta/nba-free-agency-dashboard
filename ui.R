library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(tidyverse)
library(DT)
library(shinycssloaders)

source("moduleChangeTheme.R")
source("similarityPagesUI.R")

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
                          fluidPage(sim_page_ui(
                            id = "hist", df = train_set
                          ))),
                  tabItem(tabName = "tabThemes", uiChangeThemeDropdown())
                ))
)