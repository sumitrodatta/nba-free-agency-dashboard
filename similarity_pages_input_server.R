sim_page_input_server <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 names = reactive({
                   df %>% filter(player_id == input$historical_fa_name)
                 })
                 
                 # update choices of year depending on which player was selected
                 observeEvent(names(), {
                   freezeReactiveValue(input, "historical_fa_yr")
                   year_choices = names() %>% distinct(season, seas_id)
                   updatePickerInput(
                     session = session,
                     inputId = "historical_fa_yr",
                     choices = setNames(year_choices$seas_id, year_choices$season)
                   )
                 })
               })
}