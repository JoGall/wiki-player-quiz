source("./global.R")

shinyServer(function(input, output, session) {
  
  # filter data by country
  selectedData <- reactive({
    player_url_df %>% 
      filter(Team %in% input$team_selection,
             Season %in% input$season_range,
             Apps >= input$min_apps)
  })

  # scrape career history for random player in selection
  player_career_table_html <- eventReactive(input$new_question, {
    
    i <- sample(1:nrow(selectedData()), 1)

    player_url_stub <- selectedData()$Link[i]
    
    str_glue("https://en.wikipedia.org/wiki/{player_url_stub}") %>%
      read_html() %>%
      html_nodes(xpath = '//*[@class="infobox vcard"]')
  },
  ignoreNULL = FALSE)
  
  #render career history as HTML table
  output$career_table <- renderPrint({
    player_career_table_html() %>%
      get_player_career_table() %>% 
      xtable() %>% 
      print(type = "html", 
            include.rownames = F, 
            html.table.attributes="class='infobox vcard' style='width:22em;line-height: 1.2em'")
  })
  
  # # show player name
  player_name <- reactive({
    player_career_table_html() %>%
      get_player_name()
  })
  
  player_name_text <- eventReactive(input$show_answer, {
    player_name()
  })
  
  output$answer <- renderText({
    player_name_text()
  })
  
  # match user answer to true name
  user_answer_correct_status <- eventReactive(input$user_answer, {
    
    acceptable_answers <- c(player_name_text(),
                            extract_surname(player_name_text())) %>% 
      tolower() %>% 
      stringi::stri_trans_general("Latin-ASCII")
    
    user_answer <- input$user_answer %>% 
      tolower() %>% 
      stringi::stri_trans_general("Latin-ASCII")
    
    ifelse(user_answer %in% acceptable_answers, 1, 0)
  })
  
  output$answer_message <- renderText({
   ifelse(user_answer_correct_status() == 0, "...", "CORRECT!")
  })
  
  # logic to change from guess state to answer state
  values <- reactiveValues(guess_state = 1)
  
  observeEvent(input$show_answer, {
    values$guess_state <- 0
  })
  
  observeEvent(input$new_question, {
    values$guess_state <- 1
    # updateTextInput("user_answer",
    #                 "Enter answer:",
    #                 value="")
  })
  
  output$guess_state <- renderText({
    values$guess_state
  })
  
})

