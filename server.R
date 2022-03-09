library(DT)
library(tidyverse)

source("moduleChangeTheme.R")
source("similarityPagesServer.R")

train_set = read_csv("Data/Train Set.csv")

server <- function(input, output,session) {
  
  sim_page_server(id="hist",df=train_set)
  
  #from https://github.com/nik01010/dashboardThemeSwitcher
  serverChangeTheme(id = "moduleChangeTheme")
}