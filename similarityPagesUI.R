sim_page_ui<-function(id,df){
  ns=NS(id)
  tagList(
        pickerInput(
          inputId = ns("historical_fa_name"),
          label = "Free Agent Name",
          # use player id as unique underlying var, but use player names for easier user experience
          choices = ({
            player_choices=df %>% distinct(player_id, player) %>% arrange(player)
            setNames(player_choices$player_id, nm=player_choices$player)
          }
          ),
          options = pickerOptions(
            liveSearch = TRUE,
            liveSearchNormalize = TRUE,
            size = 10
          )),
        # choices null because depend on player selected
        pickerInput(
          inputId = ns("historical_fa_yr"),
          label = "Free Agency Year",
          choices = NULL
        ),
  withSpinner(tagList(DT::dataTableOutput(ns("sel_table")),DT::dataTableOutput(ns("sim_table"))),
              type = 6,
              proxy.height = '100px'
  )
  )
}

