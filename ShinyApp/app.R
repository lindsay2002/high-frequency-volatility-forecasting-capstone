#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(rugarch)
library(tidyr)
library(factoextra)
library(class)
library(vars)
library(rmgarch)
library(cluster)
library(data.table)
library(DT)
library(parallel)

#Load files
eval_df <- readRDS("./eval_df.rds")
forecasts_var1 <- readRDS("./forecasts_var1.rds")
forecasts_var2 <- readRDS("./forecasts_var2.rds")
forecasts_var3 <- readRDS("./forecasts_var3.rds")
forecasts_arma1 <- readRDS("./forecasts_arma1.rds")
forecasts_arma2 <- readRDS("./forecasts_arma2.rds")
forecasts_arma3 <- readRDS("./forecasts_arma3.rds")
log_return_list <- readRDS("./log_return_list.rds")
vol_list <- readRDS("./vol_list.rds")

stocktime_name <- eval_df |> pull(stock_time_id)

comp_vol <- function(x) {return(sqrt(sum(x ^ 2)))}

# Define UI for application that draws a histogram
ui <- fluidPage(
  #Theme
  withMathJax(),
  theme = shinytheme("flatly"),
  shinyWidgets::useShinydashboard(),
  
  navbarPage(
    title = 'DATA3888 Optiver Capstone Project',
    tabPanel('Prediction',
             sidebarLayout(
               sidebarPanel(
                 selectInput("stid", 
                             HTML("Step 1 <br><small>Choose your stock. Note: These are stocks under a specific time id.</small>"), 
                             stocktime_name, 
                             selected  = "stock_10_14126"),
                 selectInput("predictchoice", 
                             HTML("Step 2 <br><small>Is the below the value you want to predict? If not, change using drop-down.</small>"), 
                             choices = c("Log Return", "Realised Volatility"), 
                             selected  = "Log Return"),
                 checkboxInput("checkchoice", 
                               HTML("<small>The prediction is given based on the most optimal model. If you want to try different model, check this box.</small>"), 
                               FALSE),
                 conditionalPanel(
                   condition = "input.checkchoice == true",
                   selectInput("moremodelchoice", 
                               HTML("Step 3 <br><small>The prediction is given based on the most optimal model. If you want to try different model, change using drop-down.</small>"), 
                               choices = NULL, 
                               selected = NULL)
                 ),
                 width = 2
               ),
               mainPanel(
                 br(),
                 htmlOutput("optimalModel"),
                 br(),
                 HTML("<em>Note: All numeric values are rounded to three significant figures.</em>"),
                 br(),
                 br(),
                 conditionalPanel(
                   condition = "input.predictchoice == 'Log Return' &&  input.checkchoice == false",
                   tabBox(
                     title = "Full Result",
                     width = "100%",
                     height = "100%",
                     id = "tabset1",
                     tabPanel(
                       title = "Plot",
                       status = "primary",
                       plotlyOutput("plotlogroptimal", height = "500px", width = NULL)
                     ),
                     tabPanel(
                       title = "Data",
                       status = "primary",
                       dataTableOutput("predictDataTableOptimal")
                     )
                   ),
                   fluidRow(
                     column(width = 6,
                            box(title = "Key Metrics",
                                width = "100%",
                                status = "primary",
                                infoBoxOutput("startBox", width = NULL),
                                infoBoxOutput("endBox", width = NULL),
                                infoBoxOutput("rangeBox", width = NULL),
                            )
                     ),
                     column(width = 6,
                            box(title = "Forecast Period",
                                width = "100%", 
                                height = "100%",
                                status = "primary",
                                plotlyOutput("forecastlogroptimal", height = "250px", width = NULL)
                            )
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "input.predictchoice == 'Realised Volatility'  && input.checkchoice == false",
                   box(title = "Full Series",
                       width = "100%", 
                       height = "100%",
                       status = "primary",
                       plotlyOutput("plotvoloptimal", height = "500px", width = NULL)
                   )
                 ),
                 conditionalPanel(
                   condition = "input.predictchoice == 'Log Return' && input.checkchoice == true",
                   tabBox(
                     title = "Full Result",
                     width = "100%",
                     height = "100%",
                     id = "tabset1",
                     tabPanel(
                       title = "Plot",
                       status = "primary",
                       plotlyOutput("plotlogr", height = "500px", width = NULL),
                     ),
                     tabPanel(
                       title = "Data",
                       status = "primary",
                       dataTableOutput("predictDataTable")
                     )
                   ),
                   fluidRow(
                     column(width = 6,
                            box(title = "Key Metrics",
                                width = "100%",
                                status = "primary",
                                infoBoxOutput("choicestartBox", width = NULL),
                                infoBoxOutput("choiceendBox", width = NULL),
                                infoBoxOutput("choicerangeBox", width = NULL),
                            )
                     ),
                     column(width = 6,
                            box(title = "Forecast Period",
                                width = "100%", 
                                height = "100%",
                                status = "primary",
                                plotlyOutput("forecastlogr", height = "250px", width = NULL)
                            )
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "input.predictchoice == 'Realised Volatility' && input.checkchoice == true",
                   box(title = "Full Series",
                       width = "100%", 
                       height = "100%",
                       status = "primary",
                       plotlyOutput("plotvol", height = "500px", width = NULL),
                   ),
                 ),
                 width = 10
               )
             )
    ),
    tabPanel('Methods',
       h4(HTML('<b>Introduction</b>')),
       HTML('In this project, we have utilised two multivariate models as briefly described below. As default, future prediction of your chosen stock is generated using our optimal model. However, if you want to have a comparison between these two models, you can try our feature in the Prediction tab. 
            For future work, we would consider exploring different multivariate models.'),
       h5(HTML('<b>Vector Auto Regression (VAR)</b>')),
       paste('A Vector AutoRegression (VAR) model is a multivariate time series model that captures the linear interdependencies among multiple time series. It is a system of equations where each variable is regressed on its own lagged values and the lagged values of all other variables in the model. This structure captures the interdependencies and feedback effects between the variables. VAR models are powerful tools for analyzing how shocks propagate through a system and for making multivariate time series forecasts.'),
       HTML('<br>'),
       br(),
       h5(HTML('<b>Dynamic Conditional Correlation (DCC) GARCH</b>')),
       paste('The Dynamic Conditional Correlation (DCC) GARCH model extends the univariate GARCH model to a multivariate context, allowing for dynamic correlations between multiple time series. The DCC-GARCH model consists of two steps: modeling individual conditional variances and then modeling the conditional correlations. This approach provides a comprehensive framework for understanding the co-movements and volatility interactions among multiple time series'),
       HTML('<br>'),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  chosen_stock <- reactive ({
    
    stock_name_ex <- sub("_\\d+$", "", input$stid )
    time_id_ex <- as.numeric(sub("^stock_\\d+_", "", input$stid))
    
    cluster_ex <- eval_df %>% filter(stock_id == stock_name_ex & time_id == time_id_ex)
    
    cluster_of_chosen <- as.integer(cluster_ex$cluster)
    
    if (cluster_of_chosen == 1) {
      optimal <- "Vector Autogregression (VAR)"
      choices <- c("DCC-GARCH")
      predicted_data <- data.frame(
        seconds_in_bucket = seq(601, 630),
        log_return = unlist(forecasts_arma1[paste(stock_name_ex, time_id_ex, sep="_"), ]),
        source = "Prediction"
      )
    } else if (cluster_of_chosen == 3) {
      optimal <- "Vector Autogregression (VAR)"
      choices <- c("DCC-GARCH")
      predicted_data <- data.frame(
        seconds_in_bucket = seq(601, 630),
        log_return = unlist(forecasts_arma3[paste(stock_name_ex, time_id_ex, sep="_"), ]),
        source = "Prediction"
      )
    } else if (cluster_of_chosen == 2) {
      optimal <- "DCC-GARCH"
      choices <- c("VAR")
      predicted_data <- data.frame(
        seconds_in_bucket = seq(601, 630),
        log_return = unlist(forecasts_var2[paste(stock_name_ex, time_id_ex, sep="_"), ]),
        source = "Prediction"
      )
    }
    
    actual_data <- merge(data.frame(
      seconds_in_bucket = seq(1, 600)), 
      log_return_list[[paste(stock_name_ex, ".csv", sep="")]] %>% 
        filter(time_id == time_id_ex) %>%
        dplyr::select(seconds_in_bucket, log_return), 
      by = "seconds_in_bucket", all.x = TRUE) %>%
      mutate(source = "Log Return")
    actual_data$log_return[is.na(actual_data$log_return)] <- 0
    
    plotlogr <- rbind(actual_data, predicted_data)
    
    actual_vol <-  merge(data.frame(
      time_bucket = seq(1, 20)), 
      vol_list[[paste(stock_name_ex, ".csv", sep="")]] %>% 
        filter(time_id == time_id_ex) %>%
        dplyr::select(time_bucket, volatility), 
      by = "time_bucket", all.x = TRUE) %>%
      mutate(source = "Realised Volatility")
    
    
    predicted_vol <- data.frame(time_bucket = 21,
                                volatility = comp_vol(predicted_data$log_return),
                                source = "Prediction")
    
    plotvol <- rbind(actual_vol, predicted_vol)
    
    return(list(cluster_of_chosen = cluster_of_chosen,
                plotlogr = plotlogr, 
                plotvol = plotvol, 
                predicted_data = predicted_data))
  })
  
  output$plotlogr <- renderPlotly ({
    chosen_stock_list <- chosen_stock()
    
    if (input$moremodelchoice == "VAR") {
      plotlogr <- chosen_stock_list$plotlogr
      f1 <- plotlogr |> ggplot(aes(x = seconds_in_bucket, 
                                   y = log_return, 
                                   color = source,
                                   group = 1,
                                   text = paste("Seconds: ", seconds_in_bucket,
                                                  "<br>Log Return: ", signif(log_return, 3),
                                                  "<br>Source: ", source)
                                   )) +
        geom_line() +
        scale_color_manual(
          values = c("Log Return" = "grey", "Prediction" = "red")) +
        labs(title = paste0("Log return prediction for uploaded data in the next 30 seconds"),
             x = "Seconds",
             y = "Log Return",
             color = "Source") +
        theme_minimal()
      
      ggplotly(f1, tooltip = "text")
      
    } else if (input$moremodelchoice == "DCC-GARCH") {
      plotlogr <- chosen_stock_list$plotlogr
      f1 <- plotlogr |> ggplot(aes(x = seconds_in_bucket, 
                                   y = log_return, 
                                   color = source,
                                   group = 1,
                                   text = paste("Seconds: ", seconds_in_bucket,
                                                "<br>Log Return: ", signif(log_return, 3),
                                                "<br>Source: ", source)
                                   )) +
        geom_line() +
        scale_color_manual(
          values = c("Log Return" = "grey", "Prediction" = "red")) +
        labs(title = paste0("Log return prediction for uploaded data in the next 30 seconds"),
             x = "Seconds",
             y = "Log Return",
             color = "Source") +
        theme_minimal()
      
      ggplotly(f1, tooltip = "text")
    }
    
  })
  
  
  output$plotvol <- renderPlotly ({
    chosen_stock_list <- chosen_stock()
    
    if (input$moremodelchoice == "VAR") {
      plotvol <- chosen_stock_list$plotvol
      f3 <- ggplot(plotvol, aes(x = time_bucket, 
                                y = volatility, 
                                color = source, 
                                group = 1,
                                text = paste("Bucket: ", time_bucket,
                                             "<br>Realised Volatility: ", signif(volatility, 3),
                                             "<br>Source: ", source)
                                )) +
        geom_line() +
        geom_point() +
        geom_segment(data = plotvol[20:21,], aes(x = plotvol[20,]$time_bucket, xend = plotvol[21,]$time_bucket, y = plotvol[20,]$volatility, yend = plotvol[21,]$volatility), color = "red") +
        scale_color_manual(values = c("Volatility" = "grey", "Prediction" = "red")) +
        labs(title = "Volatility prediction for for uploaded data in the next 30 seconds",
             x = "Time Bucket",
             y = "Volatility",
             color = "Source") +
        theme_minimal()
      
      ggplotly(f3, tooltip = "text")
    } else if (input$moremodelchoice == "DCC-GARCH") {
      plotvol <- chosen_stock_list$plotvol
      f3 <- ggplot(plotvol, aes(x = time_bucket, 
                                y = volatility, 
                                color = source, 
                                group = 1,
                                text = paste("Bucket: ", time_bucket,
                                             "<br>Realised Volatility: ", signif(volatility, 3),
                                             "<br>Source: ", source)
                                )) +
        geom_line() +
        geom_point() +
        geom_segment(data = plotvol[20:21,], aes(x = plotvol[20,]$time_bucket, xend = plotvol[21,]$time_bucket, y = plotvol[20,]$volatility, yend = plotvol[21,]$volatility), color = "red") +
        scale_color_manual(values = c("Volatility" = "grey", "Prediction" = "red")) +
        labs(title = "Volatility prediction for for uploaded data in the next 30 seconds",
             x = "Time Bucket",
             y = "Volatility",
             color = "Source") +
        theme_minimal()
      
      ggplotly(f3, tooltip = "text")
    }
    
  })
  
  
  output$forecastlogr <- renderPlotly ({
    chosen_stock_list <- chosen_stock()
    
    if (input$moremodelchoice == "VAR") {
      predicted_data <- chosen_stock_list$predicted_data
      f5 <- predicted_data |> ggplot(aes(x = seconds_in_bucket, 
                                         y = log_return, 
                                         color = source,
                                         group = 1,
                                         text = paste("Seconds: ", seconds_in_bucket,
                                                      "<br>Log Return: ", signif(log_return, 3))
                                         )) +
        geom_line() +
        scale_color_manual(
          values = c("Prediction" = "red")) +
        labs(title = paste0("Forecast period for log return"),
             x = "Seconds",
             y = "Log Return",
             colour = "") +
        theme_minimal()
      
      ggplotly(f5, tooltip = "text")
    } else if (input$moremodelchoice == "DCC-GARCH") {
      predicted_data <- chosen_stock_list$predicted_data
      f5 <- predicted_data |> ggplot(aes(x = seconds_in_bucket, 
                                         y = log_return, 
                                         color = source,
                                         group = 1,
                                         text = paste("Seconds: ", seconds_in_bucket,
                                                      "<br>Log Return: ", signif(log_return, 3))
                                         )) +
        geom_line() +
        scale_color_manual(
          values = c("Prediction" = "red")) +
        labs(title = paste0("Forecast period for log return"),
             x = "Seconds",
             y = "Log Return",
             colour = "") +
        theme_minimal()
      
      ggplotly(f5, tooltip = "text")
    }
    
  })
  
  LRValue <- reactive ({
    chosen_stock_list <- chosen_stock()
    
    plotlogr <- chosen_stock_list$plotlogr
    start <- plotlogr |> filter(source == "Prediction") |> pull(log_return) |> first()
    end <- plotlogr |> filter(source == "Prediction") |> pull(log_return) |> last()
    
    quant <- quantile(plotlogr |> filter(source == "Prediction") |> pull(log_return), c(0.025, 0.975))
    q1 <- signif(quant[[1]], digits = 3)
    q2 <- signif(quant[[2]], digits = 3)
    
    return(list(start = start, 
                end = end, 
                q1 = q1, 
                q2 = q2))
  }) 
  
  output$choicestartBox <- renderInfoBox({
    LRValue_list <- LRValue()
    if (input$moremodelchoice == "VAR") {
      infoBox(
        title = "Forecast Start", 
        subtitle = "First value of the forecast period",
        value = signif(LRValue_list$start, digits = 3), 
        icon = icon("hourglass-start"),
        color = "teal"
      )
    } else if (input$moremodelchoice == "DCC-GARCH") {
      infoBox(
        title = "Forecast Start", 
        subtitle = "First value of the forecast period",
        value = signif(LRValue_list$start, digits = 3),
        icon = icon("hourglass-start"),
        color = "teal"
      )
    }
  })
  
  output$choiceendBox <- renderInfoBox({
    LRValue_list <- LRValue()
    if (input$moremodelchoice == "VAR") {
      infoBox(
        title = "Forecast End", 
        subtitle = "Last value of the forecast period",
        value = signif(LRValue_list$end, digits = 3), 
        icon = icon("hourglass-end"),
        color = "teal"
      )
    } else if (input$moremodelchoice == "DCC-GARCH") {
      infoBox(
        title = "Forecast End", 
        subtitle = "Last value of the forecast period",
        value = signif(LRValue_list$end, digits = 3), 
        icon = icon("hourglass-end"),
        color = "teal"
      )
    }
  })
  
  output$choicerangeBox <- renderInfoBox({
    LRValue_list <- LRValue()
    if (input$moremodelchoice == "VAR") {
      infoBox(
        title = "Forecast Range", 
        subtitle = "95% of forecast values are in this range",
        value = paste(LRValue_list$q1, "to", LRValue_list$q2), 
        icon = icon("up-right-and-down-left-from-center"),
        color = "teal"
      )
    } else if (input$moremodelchoice == "DCC-GARCH") {
      infoBox(
        title = "Forecast Range", 
        subtitle = "95% of forecast values are in this range",
        value = paste(LRValue_list$q1, "to", LRValue_list$q2), 
        icon = icon("up-right-and-down-left-from-center"),
        color = "teal"
      )
    }
  })
  
  
  optimal_model <- reactive ({
    stock_name_ex <- sub("_\\d+$", "", input$stid )
    time_id_ex <- as.numeric(sub("^stock_\\d+_", "", input$stid))
    
    cluster_ex <- eval_df %>% filter(stock_id == stock_name_ex & time_id == time_id_ex)
    
    cluster_of_chosen <- as.integer(cluster_ex$cluster)
    
    if (cluster_of_chosen == 1) {
      optimal <- "Vector Autogregression (VAR)"
      choices <- c("DCC-GARCH")
      predicted_data <- data.frame(
        seconds_in_bucket = seq(601, 630),
        log_return = unlist(forecasts_var1[paste(stock_name_ex, time_id_ex, sep="_"), ]),
        source = "Prediction"
      )
    } else if (cluster_of_chosen == 3) {
      optimal <- "Vector Autogregression (VAR)"
      choices <- c("DCC-GARCH")
      predicted_data <- data.frame(
        seconds_in_bucket = seq(601, 630),
        log_return = unlist(forecasts_var3[paste(stock_name_ex, time_id_ex, sep="_"), ]),
        source = "Prediction"
      )
    } else if (cluster_of_chosen == 2) {
      optimal <- "Dynamic Conditional Correlation (DCC) GARCH"
      choices <- c("VAR")
      predicted_data <- data.frame(
        seconds_in_bucket = seq(601, 630),
        log_return = unlist(forecasts_arma2[paste(stock_name_ex, time_id_ex, sep="_"), ]),
        source = "Prediction"
      )
    }
    
    actual_data <- merge(data.frame(
      seconds_in_bucket = seq(1, 600)), 
      log_return_list[[paste(stock_name_ex, ".csv", sep="")]] %>% 
        filter(time_id == time_id_ex) %>%
        dplyr::select(seconds_in_bucket, log_return), 
      by = "seconds_in_bucket", all.x = TRUE) %>%
      mutate(source = "Log Return")
    actual_data$log_return[is.na(actual_data$log_return)] <- 0
    
    plotlogr <- rbind(actual_data, predicted_data)
    
    actual_vol <-  merge(data.frame(
      time_bucket = seq(1, 20)), 
      vol_list[[paste(stock_name_ex, ".csv", sep="")]] %>% 
        filter(time_id == time_id_ex) %>%
        dplyr::select(time_bucket, volatility), 
      by = "time_bucket", all.x = TRUE) %>%
      mutate(source = "Realised Volatility")
    
    
    predicted_vol <- data.frame(time_bucket = 21,
                                volatility = comp_vol(predicted_data$log_return),
                                source = "Prediction")
    
    plotvol <- rbind(actual_vol, predicted_vol)
    
    return(list(cluster_of_chosen = cluster_of_chosen,
                choices = choices,
                optimal = optimal,
                plotlogr = plotlogr, 
                plotvol = plotvol, 
                predicted_data = predicted_data))
    
  })
  
  observe({
    optimal_model_list <- optimal_model()
    choices <- optimal_model_list$choices
    
    updateSelectInput(session, "moremodelchoice", choices = choices)
  })
  
  output$optimalModel <- renderText ({
    HTML(paste('The model used to generate prediction for this stock is', optimal_model()$optimal))
  })
  
  output$predictDataTableOptimal <- DT::renderDataTable ({
    data <- optimal_model()$predicted_data
    data$log_return <- signif(data$log_return, 3)
    DT::datatable(data)
  })
  
  output$predictDataTable <- DT::renderDataTable ({
    data <- chosen_stock()$predicted_data
    data$log_return <- signif(data$log_return, 3)
    DT::datatable(data)
  })
  
  
  output$plotlogroptimal <- renderPlotly ({
    optimal_model_list <- optimal_model()
    plotlogr <- optimal_model_list$plotlogr
    
    f1 <- plotlogr |> ggplot(aes(x = seconds_in_bucket, 
                                 y = log_return, 
                                 color = source,
                                 group = 1,
                                 text = paste("Seconds: ", seconds_in_bucket,
                                              "<br>Log Return: ", signif(log_return, 3),
                                              "<br>Source: ", source)
                                 )) +
      geom_line() +
      scale_color_manual(
        values = c("Log Return" = "grey", "Prediction" = "red")) +
      labs(title = paste0("Log return prediction for uploaded data in the next 30 seconds"),
           x = "Seconds",
           y = "Log Return",
           color = "Source") +
      theme_minimal()
    
    ggplotly(f1, tooltip = "text")
  })
  
  output$plotvoloptimal <- renderPlotly ({
    optimal_model_list <- optimal_model()
    plotvol <- optimal_model_list$plotvol
    
    f3 <- ggplot(plotvol, aes(x = time_bucket, 
                              y = volatility, 
                              color = source, 
                              group = 1,
                              text = paste("Bucket: ", time_bucket,
                                           "<br>Realised Volatility: ", signif(volatility, 3),
                                           "<br>Source: ", source)
                              )) +
      geom_line() +
      geom_point() +
      geom_segment(data = plotvol[20:21,], aes(x = plotvol[20,]$time_bucket, xend = plotvol[21,]$time_bucket, y = plotvol[20,]$volatility, yend = plotvol[21,]$volatility), color = "red") +
      scale_color_manual(values = c("Volatility" = "grey", "Prediction" = "red")) +
      labs(title = "Volatility prediction for for uploaded data in the next 30 seconds",
           x = "Time Bucket",
           y = "Volatility",
           color = "Source") +
      theme_minimal()
    
    ggplotly(f3, tooltip = "text")
  })
  
  output$forecastlogroptimal <- renderPlotly ({
    optimal_model_list <- optimal_model()
    predicted_data <- optimal_model_list$predicted_data
    
    f5 <- predicted_data |> ggplot(aes(x = seconds_in_bucket, 
                                       y = log_return, 
                                       color = source,
                                       group = 1,
                                       text = paste("Seconds: ", seconds_in_bucket,
                                                    "<br>Log Return: ", signif(log_return, 3))
                                       )) +
      geom_line() +
      scale_color_manual(
        values = c("Prediction" = "red")) +
      labs(title = paste0("Forecast period for log return"),
           x = "Seconds",
           y = "Log Return",
           colour = "") +
      theme_minimal()
    
    ggplotly(f5, tooltip = "text")
  })
  
  
  LRValueOptimal <- reactive ({
    optimal_model_list <- optimal_model()
    
    plotlogr <- optimal_model_list$plotlogr
    start <- plotlogr |> filter(source == "Prediction") |> pull(log_return) |> first()
    end <- plotlogr |> filter(source == "Prediction") |> pull(log_return) |> last()
    
    quant <- quantile(plotlogr |> filter(source == "Prediction") |> pull(log_return), c(0.025, 0.975))
    q1 <- signif(quant[[1]], digits = 3)
    q2 <- signif(quant[[2]], digits = 3)
    
    return(list(start = start, 
                end = end, 
                q1 = q1, 
                q2 = q2))
  })
  
  output$startBox <- renderInfoBox({
    LRValueOptimal_list <- LRValueOptimal()
    infoBox(
      title = "Forecast Start", 
      subtitle = "First value of the forecast period",
      value = signif(LRValueOptimal_list$start, digits = 3), 
      icon = icon("hourglass-start"),
      color = "teal",
    )
  })
  
  output$endBox <- renderInfoBox({
    LRValueOptimal_list <- LRValueOptimal()
    infoBox(
      title = "Forecast End", 
      subtitle = "Last value of the forecast period",
      value = signif(LRValueOptimal_list$end, digits = 3), 
      icon = icon("hourglass-end"),
      color = "teal"
    )
  })
  
  output$rangeBox <- renderInfoBox({
    LRValueOptimal_list <- LRValueOptimal()
    infoBox(
      title = "Forecast Range", 
      subtitle = "95% of forecast values are in this range",
      value = paste(LRValueOptimal_list$q1, "to", LRValueOptimal_list$q2), 
      icon = icon("up-right-and-down-left-from-center"),
      color = "teal"
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)