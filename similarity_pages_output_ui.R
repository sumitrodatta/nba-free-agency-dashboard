sim_page_output_ui<-function(id,df){
  ns=NS(id)
  tagList(
  withSpinner(tagList(DT::dataTableOutput(ns("sel_table")),br(),
                      DT::dataTableOutput(ns("sim_table"))),
              type = 6,
              proxy.height = '100px'
  ),hr(),
  tabsetPanel(
    tabPanel("Plot", fluidPage(
      br(),
      dropdown(
        tags$h3("List of Inputs"),
        pickerInput(
          inputId = ns('xcol'),label = 'X Variable',choices = names(df %>% select(-c(seas_id:experience,type))),
          options = pickerOptions(liveSearch = TRUE,liveSearchNormalize = TRUE,
            size = 5)
        ),
        pickerInput(
          inputId = ns('ycol'),label = 'Y Variable',choices = NULL,
          options = pickerOptions(liveSearch = TRUE,liveSearchNormalize = TRUE,
                                  size = 5)
        ),
        status = "danger",style = "unite",icon = icon("cog", lib = "glyphicon"),
        width = "300px"
      ),
      br(),
      withSpinner(plotlyOutput(ns("two_vars_plot")),
                  type = 6,
                  proxy.height = '100px'
      )
    )),
    tabPanel("Stats",withSpinner(DT::dataTableOutput(ns("stats_table")),
                                 type = 6,
                                 proxy.height = '100px')),
    
    tabPanel("Grouped Graphs", fluidPage(
      br(),
      dropdown(
        pickerInput(
          inputId = ns('group'),label = 'Group',choices = c(
            "Availability","Minutes Per Game",
            "3-Point Shooting","2-Point Shooting","Free Throw Shooting",
            "Passing","Rebounding","Steals, Blocks & Fouls",
            "Advanced Cumulative","Advanced Rate",
            "Counting Stats Current Yr","Counting Stats Last 3 Yrs"
          )
        ),
        status = "danger", style = "unite", icon = icon("cog", lib = "glyphicon"),
        width = "300px"
      ),
      br(),
      withSpinner(plotlyOutput(ns("group_plots")),
                  type = 6,
                  proxy.height = '100px'
      )
    ))
  )
  )
}

