actuals_server<-function(id,df){
  moduleServer(id,
               function(input, output, session) {
                 formatted_df=datatable(df %>% filter(!is.na(player)),
                                        filter = list(position = 'top', clear = FALSE),
                                        options=list(
                                          scrollX = TRUE,dom="tip",
                                          columnDefs=list(list(targets = c(1,-1), 
                                                               searchable = FALSE,
                                                               orderable = FALSE))),
                                        rownames = FALSE,
                                        escape = FALSE) %>% 
                   formatPercentage("1st Yr % of Salary Cap", digits = 2) %>%
                   formatCurrency("Total Value", digits=0) %>%
                   formatDate("date",method="toLocaleDateString")
                 output$contracts <- DT::renderDataTable(formatted_df)
               })
}