sim_page_input_ui <- function(id, df,show_future) {
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
        selected=df %>% slice_max(vorp_last_3_yrs,n=1) %>% pull(player_id),
        options = pickerOptions(
          liveSearch = TRUE,
          liveSearchNormalize = TRUE,
          size = 10
        )
      ),
      # choices null because depend on player selected
      pickerInput(
        inputId = ns("historical_fa_yr"),
        label = "Free Agency Year",
        choices = NULL
      )
    ),
    column(
      width = 6,
      switchInput(
        inputId = ns("same_player_comp"),
        value = TRUE,
        label = "Include self comparisons?",
        labelWidth = "200px",
        onLabel = "Yes",
        offLabel = "No",
        onStatus = "success",
        offStatus = "danger",
        inline = TRUE,
        width="100%"
      ),
      if (show_future){
        switchInput(
          inputId = ns("future_comp"),
          value = TRUE,
          label = "Future comparisons?",
          labelWidth = "200px",
          onLabel = "Yes",
          offLabel = "No",
          onStatus = "success",
          offStatus = "danger",
          inline = TRUE,
          width="100%"
        )        
      },
      switchInput(
        inputId = ns("one_row_per_comp"),
        value = FALSE,
        label = "Keep only best comparison?",
        labelWidth = "200px",
        onLabel = "Yes",
        offLabel = "No",
        onStatus = "success",
        offStatus = "danger",
        inline = TRUE,
        width="100%"
      )
      )
  )
}