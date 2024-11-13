rm(list=ls())

library(shinyjs)
library(shiny)
library(tidyverse)
library(shinydashboard)
library(flexdashboard)
library(readxl)
library(DT)
library(formattable)
library(shinyBS)


##Turn off scientific notation
options(scipen = 999)


## Load in payrolls data
payrolls <- read.csv("payrolls.csv", check.names = FALSE)

payrolls <- payrolls[,-1]



## Load in free agent batters
free_agent_batters <- read.csv("free_agent_batters.csv", check.names = FALSE)

free_agent_batters <- free_agent_batters[,-1]


## Load in free agent pitchers
free_agent_pitchers <- read.csv("free_agent_pitchers.csv", check.names = FALSE)

free_agent_pitchers <- free_agent_pitchers[,-1]





## Load in hitter and pitcher projections
hitter_projections <- read.csv("hitter_projections.csv")

pitcher_projections <- read.csv("pitcher_projections.csv")


hitter_projections <- hitter_projections[,-1]
pitcher_projections <- pitcher_projections[,-1]



## Set column headers for consistency

names(hitter_projections) <- c("Name", "HR_Projection2024", "BABIP_Projection2024", "wOBA_Projection2024", 
                               "wRC+_Projection2024", "Off_Projection_2024", "Def_Projection2024", "OPS_Projection2024")


names(pitcher_projections) <- c("Name", "K/9_Projection2024", "BB/9_Projection2024","LOB%_Projection2024",
                                "GB%_Projection2024", "HR/FB_Projection_2024", "ERA_Projection2024", "FIP_Projection2024")



## Load in payrolls at begging of 2024 free agency
FA_start_payrolls <- read_excel("2024 Team Spending.xlsx")

FA_start_payrolls <- FA_start_payrolls[order(FA_start_payrolls$Team), ]



## Load in team batting and pitching stats from previous season
team_pitching2023 <- read.csv("team_pitching2023.csv")

team_batting2023 <- read.csv("team_batting2023.csv")



team_batting2023 <- team_batting2023[, -1]

team_pitching2023 <- team_pitching2023[, -1]



## Set column headers for consistency

names(team_batting2023) <- c("Team", "HR_Rank2023", "OPS_Rank2023", "BABIP_Rank2023",
                             "wRC+_Rank2023","wOBA_Rank2023","Def_Rank2023","Off_Rank2023")


names(team_pitching2023) <- c("Team", "ERA_Rank2023", "K/9_Rank2023", "BB/9_Rank2023", "LOB%_Rank2023",
                              "FIP_Rank2023","GB%_Rank2023","HR/FB_Rank2023")





shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
  }
  inputs
}


#################### Define UI ####################
#################### Define UI ####################
#################### Define UI ####################


ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .center-text {
        text-align: center;
      }
      
      .custom-padding {
          padding-top: 20px;
      }   
        
        .gauge-color {
          background-color: #00cc66; 
        }
    "
    ))
  ),
  
  
  
  ## Set app title and team logo in title panel
  titlePanel(
    fluidRow(
      column(4,"Player Acquisition Tool", class = "title-spacing"),
      column(4, uiOutput("logo_output"), class = "center-text")
    )
  ),
  
  
  br(),
  
  
  ## Create a new Row in the UI for selectInputs
  fluidRow(
    
    
    ## Create column to house dropdowns and radio buttons
    column(4,
           
           ## Create team selection dropdown
           selectInput("team", "Team:", choices = unique(FA_start_payrolls$Team)),
           
           
           
           
           ## Create radio buttons to select free agent position
           radioButtons("position", "Position:", 
                        choices = c("Pitchers", "Position Players"),
                        selected = "Pitchers"),
           
           
           ## Create conditional dropdown for primary position 
           uiOutput("primary_position_dropdown"),
           
           
           
    ),
    
    ## Fill conditional panel for prior season team rankings and set layout
    
    column(4,
           
           conditionalPanel(
             condition = "input.position == `Position Players`",
             
             fluidRow(
               column(10, strong(tags$h3("2023 Season Ranks", class = "center-text")
               ))),
             fluidRow(
               column(5, tags$h4(htmlOutput("HR_ranks"), class = "center-text")
               ),
               column(5, tags$h4(htmlOutput("BABIP_ranks"), class = "center-text")
               )),
             fluidRow(
               column(5, tags$h4(htmlOutput("wOBA_ranks"), class = "center-text")
               ),
               column(5, tags$h4(htmlOutput("wRC+_ranks"), class = "center-text")
               )),
             fluidRow(
               column(5, tags$h4(htmlOutput("Off_ranks"), class = "center-text")
               ),
               column(5, tags$h4(htmlOutput("Def_ranks"), class = "center-text")
               )),
             fluidRow(
               column(10, tags$h4(htmlOutput("OPS_ranks"), class = "center-text")
               ))
             
             
           ),
           
           conditionalPanel(
             condition = "input.position == `Pitchers`",
             
             fluidRow(
               column(10, strong(tags$h3("2023 Season Ranks", class = "center-text")
               ))),
             fluidRow(
               column(5, tags$h4(htmlOutput("K/9_ranks"), class = "center-text")
               ),
               column(5, tags$h4(htmlOutput("BB/9_ranks"), class = "center-text")
               )),
             fluidRow(
               column(5, tags$h4(htmlOutput("LOB%_ranks"), class = "center-text")
               ),
               column(5, tags$h4(htmlOutput("GB%_ranks"), class = "center-text")
               )),
             fluidRow(
               column(5, tags$h4(htmlOutput("HR/FB_ranks"), class = "center-text")
               ),
               column(5, tags$h4(htmlOutput("ERA_ranks"), class = "center-text")
               )),
             fluidRow(
               column(10, tags$h4(htmlOutput("FIP_ranks"), class = "center-text")
               ))
             
             
           )
           
    ),
    
    
    
    
    
    ## Display payroll gauge
    column(4, align = "center", strong(h2("Team Payroll")),
           gaugeOutput("payrollGauge"),          
           p(strong(tags$h4(htmlOutput("remaining_payroll"))), style = "margin-top: -50px;")
           
           
    ),
    
    ## Create button for player projections 
    column(4,
           actionButton("show_modal_button", "Show 2024 Projections")
    )
  ),
  
  fluidRow(  
    
    div(class = "custom-padding"),
    
    ## Display free agent table
    column(12, align = "center",
           
           DT::dataTableOutput("freeAgent_table")
    )
    
    
  ),
  
  
  
  # JavaScript code to handle checkbox clicks
  tags$script(HTML('
    $(document).on("click", "[id^=checkb_]", function(){
      var checkedIndexes = [];
      $("[id^=checkb_]").each(function(index) {
        if ($(this).prop("checked")) {
          var id = this.getAttribute("id");
          var i = parseInt(/checkb_(\\d+)/.exec(id)[1]);
          checkedIndexes.push(i);
        }
      });
      Shiny.setInputValue("checkedIndexes", checkedIndexes);
    });
  ')),
  
  
  
  # Create modal window 
  uiOutput("modal"),
  uiOutput("batter_modal"),
  uiOutput("pitcher_modal")
  
  
  
  
)




#################### Define server ####################
#################### Define server ####################
#################### Define server ####################



server <- function(input, output) {
  
  
  ## Link logo to user input team selection
  
  output$logo_output <- renderUI({
    img_path <- switch(input$team,
                       "ARI" = "ARI.png",
                       "ATL" = "ATL.png",
                       "BAL" = "BAL.png",
                       "BOS" = "BOS.png",
                       "CHC" = "CHC.png",
                       "CHW" = "CHW.png",
                       "CIN" = "CIN.png",
                       "CLE" = "CLE.png",
                       "COL" = "COL.png",
                       "DET" = "DET.png",
                       "HOU" = "HOU.png",
                       "KC" = "KC.png",
                       "LAA" = "LAA.png",
                       "LAD" = "LAD.png",
                       "MIA" = "MIA.png",
                       "MIL" = "MIL.png",
                       "MIN" = "MIN.png",
                       "NYM" = "NYM.png",
                       "NYY" = "MYY.png",
                       "OAK" = "OAK.png",
                       "PHI" = "PHI.png",
                       "PIT" = "PIT.png",
                       "SD" = "SD.png",
                       "SEA" = "SEA.png",
                       "SF" = "SF.png",
                       "STL" = "STL.png",
                       "TB" = "TB.png",
                       "TEX" = "TEX.png",
                       "TOR" = "TOR.png",
                       "WSH" = "WSH.png",
    )
    
    ## Set image size 
    tags$img(src = img_path, width = 100, height = 100)
    
  })
  
  
  ## Set payrolls to display based on team selection
  filtered_payrolls <- reactive({
    
    subset(FA_start_payrolls, Team == input$team)
    
  })
  
  
  player_aav <- reactiveVal(0)
  
  
  # Define reactive values for current and max payroll
  current_payroll <- reactiveVal(0)
  max_payroll <- reactiveVal(0)
  
  # Update current and max payroll based on team selection
  observe({
    max_payroll_value <- filtered_payrolls()$CurrentPayroll
    current_payroll_value <- filtered_payrolls()$StartingPayroll
    
    
    # Reset current payroll if no checkboxes are checked
    if (!is.null(input$checkedIndexes)) {
      if (!is.null(selected_data())) {
        payroll_values <- as.numeric(selected_data())
        current_payroll_value <- current_payroll_value + sum(payroll_values)
      }
    }
    
    
    # Update reactive values
    max_payroll(max_payroll_value)
    current_payroll(current_payroll_value)
    
  })
  
  # Update current payroll based on selected player
  observeEvent(input$player_table_rows_selected, {
    selected_player <- freeAgent_table$AAV_Pred[input$player_table_rows_selected]
    
    
    # Update current payroll if a player is selected
    current_payroll_value <- current_payroll()
    current_payroll_value <- current_payroll_value + selected_player
    current_payroll(current_payroll_value)
    
  })
  
  # Render gauge based on team selection
  output$payrollGauge <- renderGauge({
    
    average_contract <- round((mean(free_agent_batters$AAV_Pred) +
                                 mean(free_agent_pitchers$AAV_Pred)) / 2)
    
    team <- input$team
    
    
    # Minimum payroll on gauge set as $10 million less than the lowest team payroll
    min_payroll <- min(FA_start_payrolls$StartingPayroll) - 10000000
    
    
    # Get current and max payroll values
    current_payroll_value <- current_payroll()
    max_payroll_value <- max_payroll()
    
    
    
    # Pass parameters to gauge function and set color ranges 
    gauge(current_payroll_value, min = min_payroll, max = max_payroll_value, gaugeSectors(
      success = c(min_payroll, max_payroll_value - 2 * average_contract),
      warning = c(max_payroll_value - 2 * average_contract, max_payroll_value - average_contract),
      danger = c(max_payroll_value - average_contract, max_payroll_value),
      colors = c("#00cc66","#ffa500", "#ff6666")
    ))
    
  })
  
  
  ## Choose which table to display based on radio button selection
  position_selection <- reactive({
    
    switch(input$position,
           "Position Players" = free_agent_batters,
           "Pitchers" = free_agent_pitchers)
    
  })
  
  
  # Add checkboxes to the dataframe
  free_agent_batters$Select <- shinyInput(checkboxInput, nrow(free_agent_batters), "checkb_")  
  
  
  # Move the 'Select' column to the first position
  free_agent_batters <- free_agent_batters[c("Select", names(free_agent_batters)[-which(names(free_agent_batters) == "Select")])]
  
  
  # Add checkboxes to the dataframe
  free_agent_pitchers$Select <- shinyInput(checkboxInput, nrow(free_agent_pitchers), "checkb_")
  
  free_agent_pitchers <- free_agent_pitchers %>% relocate(Select)
  
  
  
  js <- c(
    "$('[id^=checkb_]').on('click', function(){",
    "  var id = this.getAttribute('id');",
    "  var i = parseInt(/checkb_(\\d+)/.exec(id)[1]);",
    "  var value = $(this).prop('checked');",
    "  var info = [{row: i, col: 1, value: value}];",
    "  Shiny.setInputValue('freeAgent_table_cell_edit:DT.cellInfo', info);",
    "})"
  )
  
  
  
  ## Output free agent table based on radio button position selection 
  output$freeAgent_table <- DT::renderDataTable({
    
    if (input$position == "Position Players") {
      # Filter data based on primary_position selection
      filtered_data <- position_selection()  # Assuming position_selection() returns the entire dataset
      
      if (!is.null(input$primary_position) && input$primary_position != "All") {
        filtered_data <- filtered_data[filtered_data$Pos == input$primary_position, ]
      }
    } else if (input$position == "Pitchers") {
      # Filter data based on other conditions for Pitchers
      # Adjust this part based on how data for Pitchers should be filtered
      filtered_data <- position_selection()  # Assuming position_selection() returns the entire dataset
    } else {
      # Handle other cases or return NULL
      filtered_data <- NULL
    }
    
    datatable(
      filtered_data,
      rownames = TRUE,
      escape = FALSE,
      editable = list(target = "cell", disable = list(columns = 1)),
      selection = "multiple",
      callback = JS(js),
      options = list(
        pageLength = 5,
        columnDefs = list(
          list(
            targets = 6,  # Assuming the 6th column is the "Salary" column (change if necessary)
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display') {",
              "    return '$' + parseFloat(data).toLocaleString('en-US', {maximumFractionDigits: 0});",
              "  } else {",
              "    return data;",
              "  }",
              "}"
            )
          )
        )
      )
    )
  })
  
  
  ## Create reactive value for storing checkbox activity
  selected_data <- reactiveVal(NULL)
  
  
  # Extract data based on checked checkboxes and store it in the reactive value
  observeEvent(input$checkedIndexes, {
    
    checked_indexes <- input$checkedIndexes
    if (!is.null(checked_indexes) && length(checked_indexes) > 0) 
    {
      position_data <- position_selection()  # Get the selected table data
      selected_data(position_data[checked_indexes, "AAV_Pred" ])
    } else {
      selected_data(NULL)
    }
  })
  
  
  ## Generate primary position dropdown
  output$primary_position_dropdown <- renderUI({
    if (input$position == "Position Players") {
      selectInput("primary_position", "Primary position:", 
                  choices = c("All","DH","C", "1B", "2B","3B","SS","OF"))
    } 
    else {
      
    }
  })
  
  
  
  
  ## Team batting ranks output
  output$HR_ranks <- renderUI({
    
    selected_team <- team_batting2023[team_batting2023$Team == input$team, ]
    
    output_text <- paste("HR: ", selected_team$HR_Rank2023)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  
  output$OPS_ranks <- renderUI({
    
    selected_team <- team_batting2023[team_batting2023$Team == input$team, ]
    
    output_text <- paste("OPS: ", selected_team$OPS_Rank2023)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  output$BABIP_ranks <- renderUI({
    
    selected_team <- team_batting2023[team_batting2023$Team == input$team, ]
    
    output_text <- paste("BABIP: ", selected_team$BABIP_Rank2023)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  output$`wRC+_ranks` <- renderUI({
    
    selected_team <- team_batting2023[team_batting2023$Team == input$team, ]
    
    output_text <- paste("wRC+: ", selected_team$`wRC+_Rank2023`)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  output$wOBA_ranks <- renderUI({
    
    selected_team <- team_batting2023[team_batting2023$Team == input$team, ]
    
    output_text <- paste("wOBA: ", selected_team$wOBA_Rank2023)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  output$Def_ranks <- renderUI({
    
    selected_team <- team_batting2023[team_batting2023$Team == input$team, ]
    
    output_text <- paste("Def: ", selected_team$Def_Rank2023)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  output$Off_ranks <- renderUI({
    
    selected_team <- team_batting2023[team_batting2023$Team == input$team, ]
    
    output_text <- paste("Off: ", selected_team$Off_Rank2023)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  
  
  ## Team pitching ranks output 
  output$ERA_ranks <- renderUI({
    
    selected_team <- team_pitching2023[team_pitching2023$Team == input$team, ]
    
    output_text <- paste("ERA: ", selected_team$ERA_Rank2023)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  
  output$`K/9_ranks` <- renderUI({
    
    selected_team <- team_pitching2023[team_pitching2023$Team == input$team, ]
    
    output_text <- paste("K/9: ", selected_team$`K/9_Rank2023`)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  output$`BB/9_ranks` <- renderUI({
    
    selected_team <- team_pitching2023[team_pitching2023$Team == input$team, ]
    
    output_text <- paste("BB/9: ", selected_team$`BB/9_Rank2023`)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  output$`LOB%_ranks` <- renderUI({
    
    selected_team <- team_pitching2023[team_pitching2023$Team == input$team, ]
    
    output_text <- paste("LOB%: ", selected_team$`LOB%_Rank2023`)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  output$FIP_ranks <- renderUI({
    
    selected_team <- team_pitching2023[team_pitching2023$Team == input$team, ]
    
    output_text <- paste("FIP: ", selected_team$FIP_Rank2023)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  output$`GB%_ranks` <- renderUI({
    
    selected_team <- team_pitching2023[team_pitching2023$Team == input$team, ]
    
    output_text <- paste("GB%: ", selected_team$`GB%_Rank2023`)
    
    shiny::HTML(output_text)
    
  })
  
  
  
  output$`HR/FB_ranks` <- renderUI({
    
    selected_team <- team_pitching2023[team_pitching2023$Team == input$team, ]
    
    output_text <- paste("HR/FB: ", selected_team$`HR/FB_Rank2023`)
    
    shiny::HTML(output_text)
    
  })
  
  
  # Initialize reactiveValues 
  selected_names <- reactiveValues(value = NULL)
  selected_row_data <- reactiveValues(value = NULL)
  modal_open <- reactiveVal(FALSE)
  modal_close <- reactiveVal(FALSE)
  
  
  
  observeEvent(input$freeAgent_table_rows_selected, {
    req(input$freeAgent_table_rows_selected)
    
    selected_rows <- as.integer(input$freeAgent_table_rows_selected)
    
    # Determine the data frame based on position
    data_frame <- if (input$position == "Position Players") {
      hitter_projections
    } else {
      pitcher_projections
    }
    
    # Check if selected_row is within bounds
    if (length(selected_rows) > 0) {
      selected_names$value <- data_frame[selected_rows, "PlayerName"]
      selected_row_data$value <- data_frame[selected_rows, ]
    }
  })
  
  
  
  observeEvent(input$show_modal_button, {
    modal_open(TRUE)
  })
  
  ## Render and format modal output 
  output$modal <- renderUI({
    req(input$show_modal_button)
    if (modal_open()) {
      showModal(modalDialog(
        title = "2024 Projections",
        DT::dataTableOutput("selected_row_table"),
        size = "l", 
        tags$style(type = "text/css", ".modal-dialog { width: 90%; }")
      ))
    }
  })
  
  
  
  output$selected_row_table <- DT::renderDataTable({
    req(selected_row_data$value)
    # Convert the selected row data into a data frame with appropriate column names
    data_frame <- as.data.frame(selected_row_data$value)
    colnames(data_frame) <- names(selected_row_data$value)
    datatable(data_frame, options = list(pageLength = 5))
  })
  
  
  output$batter_modal <- DT::renderDataTable({
    req(selected_names$value)
    subset_data <- subset(hitter_projections, Name %in% selected_names$value)
    datatable(subset_data, options = list(pageLength = 5))
    
  })
  
  output$pitcher_modal <- DT::renderDataTable({
    req(selected_names$value)
    subset_data <- subset(pitcher_projections, Name %in% selected_names$value)
    datatable(subset_data, options = list(pageLength = 5))
  })
  
  observeEvent(input$modal, {
    modal_close(TRUE)
  })
  
  
  observeEvent(modal_close(), {
    if (modal_close()) {
      modal_open(FALSE)
      modal_close(FALSE)
      selected_names$value <- NULL
      selected_row_data$value <- NULL
    }
  })
  
  
  
  ## Add remaining payroll output 
  output$remaining_payroll <- renderUI({
    
    max_payroll <- max_payroll()
    current_payroll <- current_payroll()
    
    remaining <- max_payroll - current_payroll
    
    output_text <- paste("Remaining:", currency(remaining))
    
    shiny::HTML(output_text)
    
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


