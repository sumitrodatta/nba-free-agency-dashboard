


sim_page_server <- function(id, df, sim_scores_df) {
  moduleServer(id,
               function(input, output, session) {
                 names = reactive({
                   sim_scores_df %>% filter(player_id.x == input$historical_fa_name)
                 })
                 
                 # update choices of year depending on which player was selected
                 observeEvent(names(), {
                   freezeReactiveValue(input, "historical_fa_yr")
                   year_choices = names() %>% distinct(season.x, seas_id_base)
                   updatePickerInput(
                     session = session,
                     inputId = "historical_fa_yr",
                     choices = setNames(year_choices$seas_id_base, year_choices$season.x)
                   )
                 })
                 
                 # have reactive filter of similarity rather than static load
                 filtered = reactive({
                   req(input$historical_fa_yr)
                   a <-
                     names() %>% filter(seas_id_base == input$historical_fa_yr)
                   a %>% slice_max(similarity, n = 5) %>% ungroup()
                 })
                 
                 output$sim_table <- DT::renderDataTable({
                   req(input$historical_fa_yr)
                   datatable(
                     filtered() %>% select(-c(seas_id_base:player_id.y)),
                     #since only five (max six with ties), don't need pagination on tables
                     options = list(lengthChange = FALSE, dom =
                                      't'),
                     rownames = FALSE
                   ) %>%
                     formatPercentage(c("similarity", "first_year_percent_of_cap"), digits =
                                        2)
                 },)
                 
                 output$sel_table <- DT::renderDataTable({
                   req(input$historical_fa_yr)
                   datatable(
                     df %>% filter(seas_id %in% filtered()$seas_id_base) %>%
                       select(
                         season,
                         player:experience,
                         type:first_year_percent_of_cap
                       ),
                     options = list(lengthChange = FALSE, dom =
                                      't'),
                     rownames = FALSE
                   ) %>%
                     formatPercentage("first_year_percent_of_cap", digits =
                                        2)
                 },)
                 
                 output$stats_table <- DT::renderDataTable({
                   req(input$historical_fa_yr)
                   datatable(
                     df %>% filter(
                       seas_id %in% filtered()$seas_id_base |
                         seas_id %in% filtered()$to_compare
                     ) %>%
                       select(-c(seas_id, player_id)) %>% relocate(player),
                     extensions = "FixedColumns",
                     options = list(
                       scrollX = TRUE,
                       lengthChange = FALSE,
                       dom = 't',
                       fixedColumns = list(leftColumns = 4)
                     ),
                     rownames = FALSE
                   )
                 },)
               })
}