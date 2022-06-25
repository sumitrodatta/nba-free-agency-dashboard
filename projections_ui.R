proj_ui<-function(id){
  ns=NS(id)
  tagList(
    DT::dataTableOutput(ns("contracts"))
  )
}