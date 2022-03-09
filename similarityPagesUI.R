sim_page_ui<-function(id,df){
  ns=NS(id)
  tagList(
        pickerInput(
          inputId = ns("historical_fa_name"),
          label = "Free Agent Name",
          choices = (
            df %>% distinct(player_id, player) %>%
              arrange(player) %>% pull(player)
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
  DT::dataTableOutput(ns("sel_table")),
  DT::dataTableOutput(ns("sim_table")))
}

