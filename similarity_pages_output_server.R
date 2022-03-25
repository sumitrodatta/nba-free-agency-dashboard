#filter based off which plot group choice made
grouped_plots<-function(group_of_vars,plot_df){
  if (group_of_vars=="Availability"){
    plot_df<-plot_df %>% filter(str_detect(variable,"^g(s)?|mp"))
  }
  else if (group_of_vars=="3-Point Shooting"){
    plot_df<-plot_df %>% filter(str_detect(variable,"^x3pa|x3p_per_|x3p_last")) %>%
      mutate(variable=factor(
        variable,levels=c("x3p_per_game","x3pa_per_game","x3p_last_3_yrs_per_game","x3pa_last_3_yrs_per_game")))
  }
  else if (group_of_vars=="2-Point Shooting"){
    plot_df<-plot_df %>% filter(str_detect(variable,"^x2pa|x2p_per_|x2p_last")) %>%
      mutate(variable=factor(
        variable,levels=c("x2p_per_game","x2pa_per_game","x2p_last_3_yrs_per_game","x2pa_last_3_yrs_per_game")))
  }
  else if (group_of_vars=="Free Throw Shooting"){
    plot_df<-plot_df %>% filter(str_detect(variable,"^fta|ft_per_|ft_last")) %>%
      mutate(variable=factor(
        variable,levels=c("ft_per_game","fta_per_game","ft_last_3_yrs_per_game","fta_last_3_yrs_per_game")))
  }
  else if (group_of_vars=="Passing"){
    plot_df<-plot_df %>% filter(str_detect(variable,"^(ast|tov)")) %>%
      mutate(variable=factor(
        variable,levels=c("ast_per_game","tov_per_game","ast_last_3_yrs_per_game","tov_last_3_yrs_per_game")))
  }
  else if (group_of_vars=="Advanced Cumulative"){
    plot_df<-plot_df %>% filter(str_detect(variable,"[o|d]ws|^vorp$"))
  }
  else if (group_of_vars=="Advanced Rate"){
    plot_df<-plot_df %>% filter(str_detect(variable,"pos_vorp|ws_per_48"))
  }
  else if (group_of_vars=="Counting Stats Current Yr"){
    plot_df<-plot_df %>% filter(str_detect(variable,"(orb|drb|ast|stl|blk|tov)_per_game")) %>%
      mutate(variable=factor(
        variable,levels=c("orb_per_game","drb_per_game",
                          "ast_per_game","stl_per_game",
                          "blk_per_game","tov_per_game")))
  }
  else if (group_of_vars=="Counting Stats Last 3 Yrs"){
    plot_df<-plot_df %>% filter(str_detect(variable,"(orb|drb|ast|stl|blk|tov)_last")) %>%
      mutate(variable=factor(
        variable,levels=c("orb_last_3_yrs_per_game","drb_last_3_yrs_per_game",
                          "ast_last_3_yrs_per_game","stl_last_3_yrs_per_game",
                          "blk_last_3_yrs_per_game","tov_last_3_yrs_per_game")))
  }
  p<-plot_df %>% ggplot(aes(x=players,y=value,fill=variable))+
    geom_col(position="dodge")+
    labs(x="players")+
    dark_theme_gray()
  return(p)
}


sim_page_output_server <- function(id, df, sim_scores_df,show_future) {
  moduleServer(id,
               function(input, output, session) {
                 # have reactive filter of similarity rather than static load
                 filtered = reactive({
                   req(input$historical_fa_yr)
                   a <- sim_scores_df %>% filter(player_id.x == input$historical_fa_name,
                                                 seas_id_base == input$historical_fa_yr)
                   if(!input$same_player_comp){
                     a=a %>% filter(player_id.y!=input$historical_fa_name)
                   }
                   if(show_future){
                     if(!input$future_comp){
                       a=a %>% filter(season.y<=(a %>% select(season.x) %>% pull(season.x)))
                     }
                   }
                   if(input$one_row_per_comp){
                     a<-a %>% group_by(player_id.y) %>% slice_max(similarity) %>% ungroup()
                   }
                   a %>% slice_max(similarity, n = 5)
                 })
                 
                 filtered_df=reactive({df %>% filter(seas_id %in% filtered()$seas_id_base |
                                             seas_id %in% filtered()$to_compare) %>% 
                   mutate(players=paste0(player," (",season,")"))})
                 
                 plottable_vars=names(df %>% select(-c(seas_id:experience,type)))
                 
                 y_options=reactive({plottable_vars[plottable_vars != input$xcol]})
                 
                 observeEvent(y_options(),{
                   choices=plottable_vars[plottable_vars != input$xcol]
                   updatePickerInput(session=session,inputId="ycol",choices=choices)
                 }
                 )
                 
                 output$sim_table <- DT::renderDataTable({
                   req(input$historical_fa_yr)
                   datatable(
                     df %>% filter(seas_id %in% filtered()$to_compare) %>%
                       left_join(.,filtered() %>% select(similarity,to_compare),by=c("seas_id"="to_compare")) %>%
                       select(similarity,photo=urlPlayerThumbnail,player,season,age,experience,type:first_year_percent_of_cap) %>%
                       arrange(desc(similarity)),
                     #since only five (max six with ties), don't need pagination on tables
                     options = list(
                       scrollX = TRUE,
                       lengthChange = FALSE, dom =
                                      't'),
                     rownames = FALSE,
                     escape=FALSE
                   ) %>%
                     formatPercentage(c("similarity", "first_year_percent_of_cap"), digits =
                                        2)
                 })
                 
                 output$sel_table <- DT::renderDataTable({
                   req(input$historical_fa_yr)
                   datatable(
                     df %>% filter(seas_id==input$historical_fa_yr) %>%
                       select(
                         season,
                         photo=urlPlayerThumbnail,
                         player:experience,
                         type:first_year_percent_of_cap
                       ),
                     options = list(
                       scrollX = TRUE,
                       lengthChange = FALSE, dom =
                                      't'),
                     rownames = FALSE,
                     escape=FALSE
                   ) %>%
                     formatPercentage("first_year_percent_of_cap", digits =
                                        2)
                 })
                 
                 output$stats_table <- DT::renderDataTable({
                   req(input$historical_fa_yr)
                   datatable(
                     filtered_df() %>%
                       select(-c(seas_id, player_id,players)) %>% relocate(player),
                     extensions = "FixedColumns",
                     options = list(
                       scrollX = TRUE,
                       lengthChange = FALSE,
                       dom = 't',
                       fixedColumns = list(leftColumns = 4)
                     ),
                     rownames = FALSE
                   )
                 })
                 
                 output$two_vars_plot<-renderPlotly({
                   req(input$historical_fa_yr)
                   p<-filtered_df() %>% ggplot(aes(x=.data[[input$xcol]],y=.data[[input$ycol]],fill=players))+
                     geom_point(size=5)+
                     dark_theme_gray()
                   ggplotly(p)
                 })
                 
                 output$group_plots <- renderPlotly({
                   req(input$historical_fa_yr)
                   a=filtered_df() %>%
                     select(-type) %>% pivot_longer(.,cols=g_percent:percent_of_pos_vorp,names_to="variable") %>%
                     mutate(players=fct_reorder(players,seas_id))
                   choice=grouped_plots(group_of_vars=input$group,plot_df=a)
                   ggplotly(choice)
                 })
                 
               })
}