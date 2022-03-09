
similarity_scores=read_csv("Data/Similarity Scores.csv") %>% 
  #add identifying info to tibble
  left_join(.,train_set %>% select(seas_id,player,season),by=c('seas_id_base'='seas_id')) %>%
  left_join(.,train_set %>% select(seas_id,player,season,age,experience,type:first_year_percent_of_cap),
            by=c('to_compare'='seas_id'))

top_5_sim_scores=similarity_scores %>% 
  group_by(seas_id_base) %>% slice_max(similarity,n=5) %>%
  ungroup()

sim_page_server<-function(id,df){
  moduleServer(id,
               function(input, output,session) {
                 names=reactive({filter(top_5_sim_scores,player.x==input$historical_fa_name)})
                 
                 # update choices of year depending on which player was selected
                 observeEvent(names(),{
                   freezeReactiveValue(input,"historical_fa_yr")
                   choices=unique(names()$season.x)
                   updatePickerInput(session=session,inputId="historical_fa_yr",choices=choices)})
                 
                 
                 output$sim_table <- DT::renderDataTable(
                   {
                     req(input$historical_fa_yr)
                     datatable(names() %>% filter(season.x==input$historical_fa_yr) %>% select(-c(seas_id_base:season.x)),
                               #since only five (max six with ties), don't need pagination on tables
                               options = list(lengthChange = FALSE,dom='t'),rownames=FALSE) %>% 
                       formatPercentage(c("similarity","first_year_percent_of_cap"),digits=2)
                   },
                 )
                 
                 output$sel_table <- DT::renderDataTable(
                   {
                     req(input$historical_fa_yr)
                     a<-names() %>% filter(season.x==input$historical_fa_yr)
                     datatable(df %>% filter(seas_id %in% a$seas_id_base) %>% 
                                 select(season,player:experience,type:first_year_percent_of_cap),
                               options = list(lengthChange = FALSE,dom='t'),rownames=FALSE) %>% 
                       formatPercentage("first_year_percent_of_cap",digits=2)
                   },
                 )
                 
                 output$stats_table <- DT::renderDataTable(
                   {
                     req(input$historical_fa_yr)
                     a<-names() %>% filter(season.x==input$historical_fa_yr)
                     datatable(df %>% filter(seas_id %in% a$seas_id_base|seas_id %in% a$to_compare) %>%
                                 select(-c(seas_id,player_id)) %>% relocate(player),
                               extensions="FixedColumns",
                               options=list(scrollX=TRUE,lengthChange = FALSE,dom='t',fixedColumns = list(leftColumns = 4)),
                               rownames = FALSE)
                   },
                 )
  }
  )
}