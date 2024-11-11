library(shiny)
library(readxl)
library(DT) 
library(dplyr)
library(ggplot2)
library(caTools)
library(shinydashboard)
library(shinyWidgets) 
library(shinyBS)
library(plotly)
library(shinyjs)


db <- read_excel("data.xlsx")



ui <- fluidPage(
  useShinyjs(),
  titlePanel("European Football Goalkeeper's Weekly Wages Analyzer & Predictor"),
  dashboardPage(
    dashboardHeader(title = "Wages Dashboard"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Estimate Your Gk's Market Value", tabName = "player_prediction", icon = icon("fire")),
        menuItem("Hypothetical GK wage predictor", tabName = "tool", icon = icon("user")),
        menuItem("Regression Analysis Summary", tabName = "dashboard", icon = icon("chart-simple")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$style(HTML("
        .shiny-output-error-validation {
          background-color: #f2f2f2;
        }
        .box-content {
          padding: 15px;
        }
        .summary-box {
          display: flex;
          justify-content: space-between;
          align-items: flex-start;
        }
        .summary-section {
          padding: 0 15px;
          font-family: 'Arial', sans-serif;
          font-size: 14px;
          line-height: 1.5;
        }
      "))
      ),
      tabItems(
        tabItem(tabName = "tool",
                fluidRow(
                  wellPanel(
                    h4("Provide Seasonal Performance Metrics to Estimate Weekly Wages"),
                    fluidRow(
                      column(4, uiOutput("inputFields1")),  # Dynamic input fields part 1
                      column(4, uiOutput("inputFields2")),  # Dynamic input fields part 2
                      column(4, uiOutput("predictionResult"))
                    ),
                    actionButton("submit", "Predict", class = "btn-primary")
                  )
                )
        ),
        tabItem(tabName = "dashboard",
                fluidRow(
                  box(title = "Model Summary", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                      div(class = "summary-box",
                          div(class = "summary-section", style = "width: 50%;", verbatimTextOutput("modelSummary")),
                          div(class = "summary-section", style = "width: 50%;", verbatimTextOutput("residualsOutput"))
                      )
                  )
                ),
                fluidRow(
                  box(title = "Regression Plot", status = "warning", solidHeader = TRUE,
                      width = 6, plotOutput("regPlot")),
                  box(title = "Coefficient Importance Plot", status = "warning", solidHeader = TRUE,
                      width = 6, plotOutput("coeffplot")),
                ),
                fluidRow(
                  box(title = "Density Plot", status = "warning", solidHeader = TRUE,
                      width = 12, plotOutput("denplot"))
                )
        ),
        tabItem(tabName = "data",
                dataTableOutput("data_table_output")
        ),
        tabItem(tabName = "player_prediction",
                fluidRow(
                  box(title = "Select Player and Season to load weekly wages based on performance metrics", status = "primary", solidHeader = TRUE, width = 12,
                      div(class = "row",
                          div(class = "col-md-6", 
                              selectInput("selectPlayer", "Select Player", choices = unique(db$Player))
                          ),
                          div(class = "col-md-6",
                              selectInput("selectSeason", "Select Season", choices = unique(db$Season))
                          )
                      ),
                      actionButton("loadData", "Load Data", class = "btn-info")
                  )
                ),
                fluidRow(
                  box(title = "Customize Goalkeeper Performance Metrics for Wage Estimation", status = "primary", solidHeader = TRUE, width = 12,
                      div(class = "row",
                          div(class = "col-md-6", 
                              uiOutput("dynamicInputs1")
                          ),
                          div(class = "col-md-6",
                              uiOutput("dynamicInputs2")
                          )
                      ),
                      actionButton("predictWage", "Predict Wage", class = "btn-success")
                  )
                ),
                fluidRow(
                  box(title = "Prediction Results", status = "primary", solidHeader = TRUE,
                      uiOutput("predictionOutput")
                  )
                )
        ),
        tabItem(tabName = "about",
                fluidRow(
                  box(
                    width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    uiOutput("aboutContent")
                  )
                )
        )
      )
    ),
  ),
  # Footer
  tags$footer(
    style = "position: fixed; left: 0; bottom: 0; width: 100%; text-align: center; background-color: lightgray; color: black; padding: 10px;",
    "© 2024 KBanalytics"
  ),
  
  # Gap between the buttons and the output panel
  tags$hr(),
)

server <- function(input, output, session) {
  db$Age <- as.numeric(db$Age)
  # Check for NA values in 'Age' variable and handle them if necessary
  if (any(is.na(db$Age))) {
    # Handle missing values by imputation or removal
    db <- db[!is.na(db$Age), ]  # Remove rows with NA values for 'Age'
  }
  
  db$AvgDist_Sweeper <- as.numeric(db$AvgDist_Sweeper)
  # Check for NA values in 'Age' variable and handle them if necessary
  if (any(is.na(db$AvgDist_Sweeper))) {
    # Handle missing values by imputation or removal
    db <- db[!is.na(db$AvgDist_Sweeper), ]  # Remove rows with NA values for 'AvgDist_Sweeper'
  }
  ####Model
  # Set seed for reproducibility
  set.seed(123)
  
  # Splitting the dataset into training and testing sets
  split <- sample.split(db$log_weekly_wages, SplitRatio = 0.7)
  train_data <- subset(db, split == TRUE)
  test_data <- subset(db, split == FALSE)
  
  
  
  model <- lm(log_weekly_wages ~ Age + points_ratio  + CS + 
                League_La_Liga + League_Bundesliga + League_Serie_A + League_Ligue_1 + 
                Save_percent +  PKA_Goals +
                PsxG_p_m + Cmp_percent_Launched + Thr_Passes + Stp_percent_Crosses + 
                OPA_Sweeper + AvgDist_Sweeper +  FederationAFC + FederationCAF + FederationCONCACAF + 
                FederationCONMEBOL + FederationUEFA + League_Premier_League, data = train_data)
  
  # Summary of the model
  #print(summary(model))
  
  # Making predictions on the testing set
  predictions <- predict(model, newdata1 = test_data)
  
  # Calculating Mean Absolute Error and Mean Squared Error
  MAE <- mean(abs(predictions - test_data$log_weekly_wages))
  MSE <- mean((predictions - test_data$log_weekly_wages)^2)
  
  # Extract the specific parts of the summary you want
  residual_standard_error <- summary(model)$sigma
  degrees_of_freedom <- df.residual(model)
  multiple_R_squared <- summary(model)$r.squared
  adjusted_R_squared <- summary(model)$adj.r.squared
  f_statistic <- summary(model)$fstatistic
  p_value <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
  
  # Print the errors
  print(paste("Mean Absolute Error:", MAE))
  print(paste("Mean Squared Error:", MSE))
  
  # Using the 'predict' function to generate predictions for the entire dataset
  db$predicted_log_weekly_wages <- predict(model, newdata = db)
  
  # Converting the log wages back to the original scale using the exp() function if needed
  db$predicted_weekly_wages <- round(exp(db$predicted_log_weekly_wages), 2)
  
  # Select relevant columns
  selected_columns <- c( "Season","Player","nation_full","Squad","Age","MP_Playing","GA",
                         "Save_percent","W","D","L","CS","Save_percent_Penalty",
                         "PKA_Goals","FK_Goals","CK_Goals","OG_Goals",
                         "Cmp_percent_Launched","Att_Passes","Thr_Passes",
                         "Launch_percent_Passes","AvgLen_Passes","Att_Goal","Stp_percent_Crosses",
                         "OPA_Sweeper","weekly_wages")
  
  db_selected <- db[selected_columns] %>%
    arrange(Season, Player)
  
  average_wage <- mean(exp(db$log_weekly_wages))
  
  # Define details for continuous variables
  continuous_vars <- list(
    Age = list(label = "Age", min = 18, max = 40, value = 25),
    points_ratio = list(label = "Average Points Per Match in a season", min = 0.1, max = 3, value = 0.5, step = 0.1),
    CS = list(label = "Total Matches Without Conceding Goals in 38-Game Season", min = 0, max = 38, value = 10),
    Save_percent = list(label = "Save Percentage over a season ", min = 0, max = 38, value = 10),
    PKA_Goals = list(label = "Penalty Goals Allowed", min = 0, max = 50, value = 10),
    PsxG_p_m = list(label = "Post Shot Expected Goals - Goals Allowed", min = -15, max = 15, value = 10, step = 0.5),
    Cmp_percent_Launched = list(label = "Pass complettion percentage: passes > 40 yards", min = 0, max = 100, value = 50),
    Thr_Passes = list(label = "Goalkeeper Throw Attempts Post-Save", min = 0, max = 269, value = 100),
    Stp_percent_Crosses = list(label = "Percentage of crosses which were successfully stopped", min = 0, max = 40, value = 20),
    OPA_Sweeper = list(label = "Number of Defensive Actions outside penalty area", min = 0, max = 75, value = 30),
    AvgDist_Sweeper = list(label = "Average Distance from goal(in yards)of all defensive Actions", min = 0, max = 25, value = 15)
  )
  #### estimate your gk's market value
  observeEvent(input$loadData, {
    req(input$selectPlayer, input$selectSeason)
    
    # Fetch the data based on the selected player and season
    selected_data <- db %>%
      filter(Player == input$selectPlayer, Season == input$selectSeason)
    
    if (nrow(selected_data) < 1) {
      # Show a message if no data is found for both UI outputs
      output$dynamicInputs1 <- renderUI({
        tags$img(src = "nodata.jpg", height = "auto", width = "100%")
      })
      output$dynamicInputs2 <- renderUI({ "" })
      
      # Hide the predict button
      shinyjs::hide("predictWage")
      
      return()  # Exit the observeEvent to avoid further execution
    } else {
      # Show the predict button
      shinyjs::show("predictWage")
      
      # If data is available, continue with your processing
      selected_league <- selected_data$Comp_full[1]
      selected_federation <- selected_data$Federation[1]
      
      # Calculate the halfway point and ensure it rounds up if necessary
      half <- ceiling(length(continuous_vars) / 2)
      first_half_vars <- names(continuous_vars)[1:half]
      second_half_vars <- names(continuous_vars)[(half + 1):length(continuous_vars)]
      
      
      # Define a function to create slider inputs from a subset of variables
      create_sliders <- function(vars, selected_data) {
        lapply(vars, function(x) {
          current_value <- if(!is.na(selected_data[[x]][1]) && !is.null(selected_data[[x]][1])) {
            as.numeric(selected_data[[x]][1])
          } else {
            as.numeric(continuous_vars[[x]]$value)
          }
          
          max_value <- if(!is.na(max(db[[x]], na.rm = TRUE)) && !is.null(db[[x]])) {
            max(as.numeric(db[[x]], na.rm = TRUE))
          } else {
            as.numeric(continuous_vars[[x]]$max)
          }
          
          sliderInput(inputId = x, label = continuous_vars[[x]]$label,
                      min = as.numeric(continuous_vars[[x]]$min),
                      max = max(max_value, as.numeric(continuous_vars[[x]]$max)),
                      value = current_value,
                      step = if(!is.null(continuous_vars[[x]]$step)) as.numeric(continuous_vars[[x]]$step) else 1)
        })
      }
      
      # Rendering UI for first half of variables including league input
      output$dynamicInputs1 <- renderUI({
        input_list1 <- create_sliders(first_half_vars, selected_data)
        league_input <- selectInput("league", "Player's League Participation",
                                    choices = c("Premier League" = "League_Premier_League",
                                                "La Liga" = "League_La_Liga",
                                                "Bundesliga" = "League_Bundesliga",
                                                "Serie A" = "League_Serie_A",
                                                "Ligue 1" = "League_Ligue_1"),
                                    selected = selected_league)
        do.call(tagList, c(input_list1, list(league_input)))
      })
      
      # Render dynamic inputs for the second half of variables including federation
      output$dynamicInputs2 <- renderUI({
        req(selected_data)
        input_list2 <- create_sliders(second_half_vars, selected_data)
        federation_input <- selectInput("federation", "Player's National Team Federation",
                                        choices = c("AFC" = "FederationAFC", "CAF" = "FederationCAF",
                                                    "CONCACAF" = "FederationCONCACAF",
                                                    "CONMEBOL" = "FederationCONMEBOL", "UEFA" = "FederationUEFA"),
                                        selected = selected_federation)
        do.call(tagList, c(input_list2, list(federation_input)))
      })
    }
  })
  
  
  observeEvent(input$predictWage, {
    req(input$selectPlayer, input$selectSeason)  # Ensure necessary inputs are not NULL
    
    # Data processing and prediction as before
    prediction_data <- sapply(names(continuous_vars), function(x) as.numeric(input[[x]]), simplify = "vector")
    # Reset all flags to 0
    league_flags <- setNames(rep(0, 5), c("League_Premier_League", "League_La_Liga", "League_Bundesliga",
                                          "League_Serie_A", "League_Ligue_1"))
    federation_flags <- setNames(rep(0, 5), c("FederationAFC", "FederationCAF", "FederationCONCACAF",
                                              "FederationCONMEBOL", "FederationUEFA"))
    
    # Set the selected flags to 1
    league_flags[input$league] <- 1
    federation_flags[input$federation] <- 1
    
    data_values <- c(prediction_data, league_flags, federation_flags)
    new_data <- data.frame(t(data_values), stringsAsFactors = FALSE)
    
    # Predict and convert from log
    log_prediction <- predict(model, newdata = new_data, type = "response")
    estimated_wage <- exp(log_prediction)
    formated_prediction <- formatC(estimated_wage, format = "f", big.mark = ",", digits = 0)
    
    
    # Output both current and predicted wages
    output$predictionOutput <- renderUI({
      # Assume 'current_wage' is the column in 'db' that contains the current wages
      current_wage <- db %>%
        filter(Player == input$selectPlayer, Season == input$selectSeason) %>%
        pull(weekly_wages) %>%
        first()  # Extract the first (or only) value if multiple entries match
      
      if (is.na(current_wage)) {
        # If current wage is NA, display a message indicating no data available
        HTML("No data available for selected season")
      } else {
        # If current wage is not NA, format and display both current and predicted wages
        formated_wages <- formatC(current_wage, format = "f", big.mark = ",", digits = 0)
        
        div(
          HTML(paste("Selected season weekly wage: $", formated_wages, "<br>")),
          HTML(paste("Estimated weekly wage: $", formated_prediction, "<br>"))
        )
      }
    })
  })
  
  
  
  ##### hypotheical keeper estimate
  # Render input fields for the first column
  output$inputFields1 <- renderUI({
    # Select the relevant continuous variables for the first column
    input_list1 <- lapply(names(continuous_vars)[1:5], function(x) {
      sliderInput(
        inputId = x, 
        label = continuous_vars[[x]]$label,
        min = as.numeric(continuous_vars[[x]]$min),
        max = as.numeric(continuous_vars[[x]]$max),
        value = as.numeric(continuous_vars[[x]]$value),
        step = if(!is.null(continuous_vars[[x]]$step)) as.numeric(continuous_vars[[x]]$step) else 1
      )
    })
    
    # Add the league dropdown to the first column
    input_list1 <- c(input_list1, list(
      selectInput("league", "Player's League Participation",
                  choices = c("Premier League" = "League_Premier_League",
                              "La Liga" = "League_La_Liga",
                              "Bundesliga" = "League_Bundesliga",
                              "Serie A" = "League_Serie_A",
                              "Ligue 1" = "League_Ligue_1"),
                  selected = "League_Premier_League"),
      selectInput("federation", "Player's National Team Federation",
                  choices = c("AFC" = "FederationAFC", "CAF" = "FederationCAF",
                              "CONCACAF" = "FederationCONCACAF",
                              "CONMEBOL" = "FederationCONMEBOL", "UEFA" = "FederationUEFA"),
                  selected = "FederationUEFA")
    ))
    
    do.call(tagList, input_list1)
  })
  
  # Render input fields for the second column
  output$inputFields2 <- renderUI({
    # Select the relevant continuous variables for the first column
    input_list2 <- lapply(names(continuous_vars)[6:11], function(x) {
      sliderInput(
        inputId = x, 
        label = continuous_vars[[x]]$label,
        min = as.numeric(continuous_vars[[x]]$min),
        max = as.numeric(continuous_vars[[x]]$max),
        value = as.numeric(continuous_vars[[x]]$value),
        step = if(!is.null(continuous_vars[[x]]$step)) as.numeric(continuous_vars[[x]]$step) else 1
      )
    })
    do.call(tagList, input_list2)
  })
  
  
  observeEvent(input$submit, {
    # Prepare data for prediction
    continuous_values <- sapply(names(continuous_vars), function(x) as.numeric(input[[x]]))
    
    # Set all league and federation flags to 0 initially and then to 1 based on selection
    league_flags <- setNames(as.numeric(rep(0, 5)), 
                             c("League_Premier_League", "League_La_Liga", "League_Bundesliga",
                               "League_Serie_A", "League_Ligue_1"))
    federation_flags <- setNames(as.numeric(rep(0, 5)), 
                                 c("FederationAFC", "FederationCAF", "FederationCONCACAF",
                                   "FederationCONMEBOL", "FederationUEFA"))
    league_flags[input$league] <- 1
    federation_flags[input$federation] <- 1
    
    data_values <- c(continuous_values, league_flags, federation_flags)
    new_data <- data.frame(t(data_values), stringsAsFactors = FALSE)
    
    # Predict and convert from log
    log_prediction <- predict(model, newdata = new_data, type = "response")
    actual_prediction <- exp(log_prediction)
    
    
    output$predictionResult <- renderUI({
      # Assuming actual_prediction is calculated from input values and db is your dataset
      formatted_prediction <- formatC(actual_prediction, format = "f", big.mark = ",", digits = 0)
      
      # Calculate the average wage for the dataset
      average_wage <- mean(db$weekly_wages)
      formatted_average <- formatC(average_wage, format = "f", big.mark = ",", digits = 0)
      
      # Determine the tier of the predicted wage
      predicted_tier <- ifelse(formatted_prediction < average_wage, "Low",
                               ifelse(formatted_prediction < average_wage * 2, "Medium", "High"))  # Adjust the logic as per your requirement
      
      # Sort the dataset by weekly wages and then find the closest players
      sorted_db <- db[order(db$weekly_wages), ]
      
      # Find index of player just below and above the predicted wage
      index_below <- max(which(sorted_db$weekly_wages < actual_prediction))
      index_above <- min(which(sorted_db$weekly_wages > actual_prediction))
      
      # Find player data or provide a message if not found
      player_below <- if (length(index_below) > 0) {
        sorted_db[index_below, ]
      } else {
        list(Player = "None", weekly_wages = "N/A")
      }
      player_above <- if (length(index_above) > 0) {
        sorted_db[index_above, ]
      } else {
        list(Player = "None", weekly_wages = "N/A")
      }
      
      div(
        HTML(paste("Predicted weekly wages: $", formatted_prediction, "<br>")),
        HTML(paste("Player just below predicted wage: ", player_below$Player, " with $", player_below$weekly_wages, "<br>")),
        HTML(paste("Player just above predicted wage: ", player_above$Player, " with $", player_above$weekly_wages, "<br>"))
      )
    })
  })
  
  ###################################################################################################################
  
  ###summary and plots
  # Output the results
  output$modelSummary <- renderPrint({
    cat("Residual standard error:", residual_standard_error, "on", degrees_of_freedom, "degrees of freedom\n")
    cat("Multiple R-squared: ", multiple_R_squared, ", Adjusted R-squared: ", adjusted_R_squared, "\n")
    cat("F-statistic:", f_statistic[1], "on", f_statistic[2], "and", f_statistic[3], "DF,  p-value:", format.pval(p_value), "\n")
    cat("Mean Absolute Error:", MAE, "\n")
    cat("Mean Squared Error:", MSE, "\n")
  })
  
  # Output for Residuals
  output$residualsOutput <- renderPrint({
    cat("Residuals:\n",
        "Min      1Q  Median      3Q     Max \n",
        "-2.4011 -0.4415 -0.0322  0.5056  3.7130")
  })
  
  actual_vs_predicted_plot <- reactive({
    plot_data <- db
    ggplot(plot_data, aes(x = log_weekly_wages, y = predicted_log_weekly_wages)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "red", formula = y ~ x) +
      labs(x = "Actual Log Weekly Wages", y = "Predicted Log Weekly Wages",
           title = "Scatter Plot of Actual vs. Predicted Log Weekly Wages") +
      theme_minimal()
  })
  
  output$regPlot <- renderPlot({
    actual_vs_predicted_plot()  # Pass player_search as an argument to the reactive function
  })
  
  
  coeff_imp_plot <- reactive({
    coefficients <- coef(model)[-c(1, length(coef(model)) - 1, length(coef(model)))]
    se.fit <- summary(model)$coefficients[, "Std. Error"][-1]  # Skip intercept for standard errors
    
    df <- data.frame(variable = names(coefficients), coefficient = coefficients, se = se.fit)
    
    ggplot(df, aes(x = reorder(variable, coefficient), y = coefficient)) +
      geom_col(fill = "skyblue") +
      geom_errorbar(aes(ymin = coefficient - se, ymax = coefficient + se), width = 0.2) +
      labs(x = "Feature", y = "Coefficient", title = "Feature Importance (Coefficient Magnitude)") +
      coord_flip() +  # Flip axes for horizontal bar plot
      theme_minimal()
  })
  
  output$coeffplot <- renderPlot({
    coeff_imp_plot()  
  })
  
  density_plot <-reactive ({
    # Creating a new data frame that differentiates between actual and predicted values
    df_long <- rbind(
      data.frame(value = db$log_weekly_wages, WageType = 'Actual Wages'),
      data.frame(value = db$predicted_log_weekly_wages, WageType = 'Predicted Wages')
    )
    
    # Now plot using this long format data frame
    ggplot(df_long, aes(x = value, fill = WageType)) +
      geom_histogram(binwidth = 0.1, alpha = 0.5, position = 'identity') +
      scale_fill_manual(values = c("Actual Wages" = "blue", "Predicted Wages" = "red"),
                        name = "Wage Type", 
                        labels = c("Actual Wages", "Predicted Wages")) +
      labs(x = "Log-Weekly Wages", y = "Frequency", 
           title = "Distribution of Actual vs. Predicted Log-Weekly Wages") +
      scale_x_continuous(breaks = seq(floor(min(df_long$value)), 
                                      ceiling(max(df_long$value)), by = 0.5)) +
      theme_minimal() +
      theme(legend.position = "right") +
      guides(fill = guide_legend(title = "Wage Type"))
  })
  
  output$denplot <- renderPlot({
    density_plot()  
  })
  
  ################################################################################  
  
  output$data_table_output <- DT::renderDataTable({
    DT::datatable(
      db_selected,  # assuming db_selected is your data frame
      options = list(
        autoWidth = TRUE,  # automatically adjust column widths
        scrollX = TRUE  # enable horizontal scrolling
      )
    )
  })
  
  ############
  output$aboutContent <- renderUI({
    fluidPage(
      tags$style(
        HTML("
              .about-container {
                background-image: url('b2.jpg');
                background-size: cover;
                background-position: center;
                padding: 20px;
                border-radius: 10px;
                box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
                color: #fff; /* Set text color to white for better contrast */
              }
              
              .about-title {
                font-size: 28px; /* Increase title font size */
                font-weight: bold;
                margin-bottom: 20px; /* Increase spacing between title and content */
              }
              
              .about-text {
                font-size: 18px; /* Increase text font size */
                margin-bottom: 30px; /* Increase spacing between paragraphs */
              }
              
              .key-features h3 {
                font-size: 24px; /* Increase key features title font size */
                margin-bottom: 15px; /* Increase spacing between title and list */
              }
              
              .key-features li {
                font-size: 18px; /* Increase list item font size */
              }
              
              .disclaimer, .contact-info {
                font-size: 18px; /* Increase disclaimer and contact info font size */
                margin-bottom: 20px; /* Increase spacing between sections */
              }
              ")
      ),
      div(class = "about-container",
          div(class = "about-title", "About Goalkeeper Wage Predictor/Estimator"),
          div(class = "about-text", "The Goalkeeper Wage Predictor app is an advanced, user-friendly web-based tool designed to estimate the weekly wages of professional goalkeepers in Europe's top five football leagues: the Premier League, La Liga, Bundesliga, Serie A, and Ligue 1. By leveraging a comprehensive dataset sourced from fbref.com, expert statistical modeling, and interactive user inputs, the app provides accurate predictions and insights into the factors that influence goalkeeper compensation."),
          
          div(class = "key-features",
              h3("Key Features"),
              tags$ul(
                tags$li("Hypothetical Wage Estimator: Input a goalkeeper's performance statistics to predict their potential weekly wages and gain insights into their market value."),
                tags$li("Player Wage Prediction: Explore historical performance data for a specific goalkeeper and season. Adjust the input metrics to fine-tune the wage estimation process and gain insights into the factors influencing a goalkeeper's weekly wages."),
                tags$li("Regression Analysis Summary: Explore a detailed breakdown of the machine learning model used for wage prediction, including a model summary table and visualizations of performance metrics' impact on wages."),
                tags$li("Data Exploration: Interactively explore the complete dataset of goalkeeper performance metrics and weekly wages used to train the prediction model.")
              )
          ),
          
          div(class = "disclaimer",
              h3("Disclaimer"),
              p("The wage predictions generated by this app should be considered estimates and may not perfectly reflect actual market values. Player wages are influenced by various factors beyond the scope of this model, including transfer fees, club budgets, and contract negotiations.")
          ),
          
          div(class = "contact-info",
              h3("Contact"),
              p("We welcome your feedback and suggestions for improvement. Feel free to reach out to us at eshwar.konda@temple.edu.")
          )
      )
    )
  })
  
}

shinyApp(ui, server)