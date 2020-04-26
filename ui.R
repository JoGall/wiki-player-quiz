source("./global.R")

fluidPage(titlePanel("Wikipedia Quiz"),
          sidebarLayout(
            sidebarPanel(
              checkboxGroupInput("team_selection", "Teams:",
                                 choices = team_list,
                                 selected = team_list),
              sliderInput("season_range", "Seasons:",
                          min = 2009,
                          max = 2009,
                          value = 2009,
                          sep = ""),
              sliderInput(
                "min_apps",
                "Minimum appearances",
                min = 0,
                max = 30,
                value = 10)
            ),
            
            mainPanel(
              # table
              htmlOutput('career_table'),
              br(),
              # user answer input box
              textInput("user_answer",
                        "Enter answer:",
                        value = ""),
              verbatimTextOutput('guess_state'),
              # user guess state
              # answer shown state
              conditionalPanel(
                condition = "output.guess_state == 1",
                actionButton("show_answer", "Show answer")
              ),
              # answer shown state
              conditionalPanel(
                condition = "output.guess_state == 0",
                h3(verbatimTextOutput('answer')),
                verbatimTextOutput('answer_message'),
                htmlOutput("full_infobox")
              ),
              hr(),
              actionButton("new_question", "Another!")
            )
          ))