proj_server<-function(id,df,option_contract=TRUE,cap_number){
  moduleServer(id,
               function(input, output, session) {
                 df=df %>% 
                   mutate(total_Y1S2=round(`Y1S2 Cap %`*cap_number*((1.05)^(yrs_Y1S2)-1)/0.05,
                                           digits=-4)) %>%
                   mutate(total_S1Y2=round(`S1Y2 Cap %`*cap_number*((1.05)^(yrs_S1Y2)-1)/0.05,
                                           digits=-4))
                 formatted_df=datatable(df,
                                        filter = list(position = 'top', clear = FALSE),
                                        options=list(scrollX = TRUE,dom="tip"),rownames = FALSE,
                                        selection = "single") %>% 
                   formatPercentage(c("Y1S2 Cap %", "S1Y2 Cap %"), digits = 2) %>%
                   formatCurrency(c("total_Y1S2","total_S1Y2"), digits=0)
                 if (option_contract){
                   formatted_df=formatted_df %>% formatCurrency("2021 Option",digits=0)
                 }
                 output$contracts <- DT::renderDataTable(formatted_df)
               })
}