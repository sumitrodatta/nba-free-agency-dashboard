sim_page_output_ui<-function(id){
  ns=NS(id)
  tagList(
  withSpinner(tagList(DT::dataTableOutput(ns("sel_table")),
                      DT::dataTableOutput(ns("sim_table"))),
              type = 6,
              proxy.height = '100px'
  )
  )
}

