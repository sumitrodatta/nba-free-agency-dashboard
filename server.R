library(DT)
library(tidyverse)

source("moduleChangeTheme.R")

train_set=read_csv("Data/Train Set.csv")

similarity_scores=read_csv("Data/Similarity Scores.csv") %>% 
  left_join(.,train_set %>% select(seas_id,player,season),by=c('seas_id_base'='seas_id')) %>%
  left_join(.,train_set %>% select(seas_id,player,season,age,experience,type:first_year_percent_of_cap),
            by=c('to_compare'='seas_id')) %>% 
  group_by(seas_id_base) %>% slice_max(similarity,n=5) %>%
  ungroup()

top_5_sim_scores=similarity_scores %>% 
  group_by(seas_id_base) %>% slice_max(similarity,n=5) %>%
  ungroup()

server <- function(input, output,session) {
  names=reactive({filter(top_5_sim_scores,player.x==input$historical_fa_name)})
  
  observeEvent(names(),{
    freezeReactiveValue(input,"historical_fa_yr")
    choices=unique(names()$season.x)
    updatePickerInput(session=session,inputId="historical_fa_yr",choices=choices)})

  output$sim_table <- DT::renderDataTable(
    {
    req(input$historical_fa_yr)
    datatable(names() %>% filter(season.x==input$historical_fa_yr) %>% select(-c(seas_id_base:season.x)),
              options = list(lengthChange = FALSE,dom='t'),rownames=FALSE) %>% 
      formatPercentage(c("similarity","first_year_percent_of_cap"),digits=2)
  },
  )
  
  output$sel_table <- DT::renderDataTable(
    {
      req(input$historical_fa_yr)
      a<-names() %>% filter(season.x==input$historical_fa_yr)
      datatable(train_set %>% filter(seas_id %in% a$seas_id_base) %>% 
                  select(season,player:experience,type:first_year_percent_of_cap),
                options = list(lengthChange = FALSE,dom='t'),rownames=FALSE) %>% 
        formatPercentage("first_year_percent_of_cap",digits=2)
    },
  )
  
  output$stats_table <- DT::renderDataTable(
    {
      req(input$historical_fa_yr)
      a<-names() %>% filter(season.x==input$historical_fa_yr)
      datatable(train_set %>% filter(seas_id %in% a$seas_id_base|seas_id %in% a$to_compare) %>%
                  select(-c(seas_id,player_id)) %>% relocate(player),
                extensions="FixedColumns",
                options=list(scrollX=TRUE,lengthChange = FALSE,dom='t',fixedColumns = list(leftColumns = 4)),
                rownames = FALSE)
    },
  )
  
  #from https://github.com/nik01010/dashboardThemeSwitcher
  callModule(module = serverChangeTheme, id = "moduleChangeTheme")
}