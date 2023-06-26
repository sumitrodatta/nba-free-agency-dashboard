proj_server<-function(id,df,option_contract=TRUE){
  moduleServer(id,
               function(input, output, session) {
                 formatted_df=datatable(df %>% relocate(photo=urlPlayerThumbnail,.before=everything()) %>% 
                                          select(-player_id) %>% 
                                          rename("Y1S2 Cap %"="yr1_cap_percent_Y1S2",
                                                 "S1Y2 Cap %"="yr1_cap_percent_S1Y2"),
                                        filter = list(position = 'top', clear = FALSE),
                                        escape = FALSE,
                                        options=list(
                                          scrollX = TRUE,dom="tip",
                                          columnDefs=list(list(targets = 0, 
                                                               searchable = FALSE,
                                                               orderable = FALSE))),
                                        rownames = FALSE,
                                        selection = "single") %>% 
                   formatPercentage(c("Y1S2 Cap %", "S1Y2 Cap %"), digits = 2) %>%
                   formatCurrency(c("total_Y1S2","total_S1Y2"), digits=0)
                 if (option_contract){
                   formatted_df=formatted_df %>% formatCurrency("option_amt",digits=0)
                 }
                 output$contracts <- DT::renderDataTable(formatted_df)
               })
}