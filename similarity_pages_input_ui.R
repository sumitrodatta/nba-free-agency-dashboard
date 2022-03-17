sim_page_input_ui <- function(id, df) {
  ns = NS(id)
  tagList(
    column(
      width = 6,
      pickerInput(
        inputId = ns("historical_fa_name"),
        label = "Free Agent Name",
        # use player id as unique underlying var, but use player names for easier user experience
        choices = ({
          player_choices = df %>% distinct(player_id, player) %>% arrange(player)
          setNames(player_choices$player_id, nm = player_choices$player)
        }),
        options = pickerOptions(
          liveSearch = TRUE,
          liveSearchNormalize = TRUE,
          size = 10
        )
      ),
      br(),
      switchInput(
        inputId = ns("same_player_comp"),
        value = TRUE,
        label = "Include self comparisons?",
        labelWidth = "200px",
        onLabel = "Yes",
        offLabel = "No",
        onStatus = "success",
        offStatus = "danger",
        inline = TRUE
      ),
    ),
    column(
      width = 6,
      # choices null because depend on player selected
      pickerInput(
        inputId = ns("historical_fa_yr"),
        label = "Free Agency Year",
        choices = NULL
      ),
      br(),
      switchInput(
        inputId = ns("one_row_per_comp"),
        value = FALSE,
        label = "Keep only best comparison?",
        labelWidth = "200px",
        onLabel = "Yes",
        offLabel = "No",
        onStatus = "success",
        offStatus = "danger",
        inline = TRUE
      )
    )
  )
}