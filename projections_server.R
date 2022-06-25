proj_server<-function(id,df,option_contract=TRUE){
  moduleServer(id,
               function(input, output, session) {
                 formatted_df=datatable(df,filter = list(position = 'top', clear = FALSE),
                                        options=list(scrollX = TRUE),rownames = FALSE) %>% 
                   formatPercentage(c("Y1S2 Cap %", "S1Y2 Cap %"), digits = 2) %>%
                   formatCurrency(c("total_Y1S2","total_S1Y2"), digits=0)
                 if (option_contract){
                   formatted_df=formatted_df %>% formatCurrency("2021 Option",digits=0)
                 }
                 output$contracts <- DT::renderDataTable(formatted_df)
               })
}