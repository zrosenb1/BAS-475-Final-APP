library(dplyr)
library(fpp3)
library(ggplot2)
library(ggeasy)
library(plotly)
library(ggpubr)
library(feasts)
library(seasonal)

DATA <- aus_production
DATA %>%
  features(DATA[,"Cement"], features = guerrero) %>%
  pull(lambda_guerrero) -> lambda 
DATA <- DATA %>% 
  mutate(Transformed_Cement = box_cox(DATA$Cement, lambda))
input <- list()
input$product <- "Cement"
input$addprod <- "Cement"
input$type <- "Seasonality"

library(shiny)

ui <- navbarPage("Individual Midterm",
                 tabPanel("Full Plot over Time",
                          sidebarLayout(
                            
                            sidebarPanel(
                              selectInput(
                                inputId = "product",
                                label = "Select Product to Plot",
                                choices = c( "Beer",
                                             "Tobacco",
                                             "Bricks",
                                             "Cement",
                                             "Electricity",
                                             "Gas"),
                                selected="Cement")
                              
                            ),
                            
                            mainPanel(
                              textOutput("intro"),
                              plotlyOutput("fplot"))
                          )
                 ),
                 
                 tabPanel("Decomposed Plots",
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                inputId = "type",
                                label = "Select Type of Plot",
                                choices = c("Seasonality","Autocorrelation", "Decomposition", "Transformation"),
                                selected="Autocorrelation")),
                            
                            mainPanel(
                              plotOutput("dplot"),
                              textOutput("interp")
                              
                            )
                        )
                 ),
                 
                 tabPanel("Forecasting Feature",
                          sidebarLayout(
                            
                            sidebarPanel(
                              selectInput(
                                inputId = "forecast",
                                label = "Select Type of Forecast",
                                choices = c( "Naive",
                                             "Seasonal Naive",
                                             "Mean",
                                             "Drift",
                                             "Holts",
                                             "Holts / Winters",
                                             "ARIMA",
                                             "Auto ARIMA"),
                                selected="Auto ARIMA")
                              
                            ),
                            
                            mainPanel(
                              verbatimTextOutput("addtext"),
                              plotOutput("addplot"),
                              tableOutput("addtab"))
                          )
                 )
)


server <- function(input, output) {
  
  output$intro <- renderText({
    
    paste("This is an app to show you how the Australian production of certain products has changed over the years. 
          This is ordered time sereis data that I am using. This first page includes a basic plot showing the total production of a product.
          The second tab gives you a choice of breaking down the Cement production data into different parts and reading some analytics about them.
          The final page provides different forecasts for the Cement production data.")
    
  })
  
  
  
  output$fplot <- renderPlotly({

      x <- as_tsibble(DATA[,c("Quarter",input$product)])
      autoplot(x) + labs(title= paste(colnames(x[,2]), 
                                      "Production"), y = "Total Count", x = "Date")
      

  })

  
  DF <- as_tsibble(DATA[,c("Quarter","Cement")])
  
  output$dplot <- renderPlot({
    if (input$type == "Seasonality") {
      gg_season(DF)   } 
    else if (input$type == "Autocorrelation") {
      DF %>%
        ACF(diff(Cement)) %>%
        autoplot()    } 
    else if (input$type == "Decomposition") {
      DF %>%
        model(STL()) %>%
        components() %>% 
        autoplot() } 
    else {
      DF %>%
        features(DF[,2], features = guerrero) %>%
        pull(lambda_guerrero) -> lambda 
      DF %>% 
        autoplot(box_cox(Cement, lambda = lambda))  + labs(title= "Transformed Cement Production", y = "Total Count", x = "Date")
    }
  })

  
 
  
  output$interp <- renderText({
    
    
    interp_S <- paste("This plot shows the seasonality of Australian Cement production. There is not significant seasonality, however if there was to be anything it would be slightly higher during quarter 2 and 3. ")
    
    interp_A <- paste("This plot shows the ACF plot on the different lag coefficiants.")
    
    interp_D <- paste("This plot shows a STL decomposition of the Australian Cement Production. There is a positive trend cycle which has the largest significance. The seasonality part of the decompositon has a small significance and does not show consistant variation which is why a STL decomp was used instead of additive. The remainder part of the Cement decomp is random and seems to be white noise.")
    
    interp_T <- paste("The full time series plot of Cement, as seen on the first tab, is linear but has some heteroskedasticity. A guerrero transformation with box-cox was used to plot a new transformed graph to see if the transformation was helpful. ")
    
    ifelse(input$type == "Seasonality", paste(interp_S),
           ifelse(input$type == "Autocorrelation", paste(interp_A),
                  ifelse(input$type == "Decomposition", paste(interp_D),
                         ifelse(input$type == "Transformation", paste(interp_T),"No Plot"))))
  })
  
  
  output$addplot <- renderPlot({
    
    DFA <- as_tsibble(DATA)
    
    
    if (input$forecast == "Naive") {
      DFA %>%
        model("Naive" = NAIVE(Transformed_Cement)) %>%
        forecast(h = 24) -> fit
      
      fit %>% autoplot(DFA, level = NULL) + labs(title= "Transformed_Cement Forecast", y = "Total Count",x = "Date")   } 
    else if (input$forecast == "Seasonal Naive") {
      DFA %>%
        model("Seasonal Naive" = SNAIVE(Transformed_Cement ~ lag(8))) %>%
        forecast(h = 24) -> fit
      
      fit %>% autoplot(DFA, level = NULL) + labs(title= "Transformed_Cement Forecast", y = "Total Count", x = "Date")   
      } 
    else if (input$forecast == "Mean") {
      DFA %>%
        model("Mean" = MEAN(Transformed_Cement)) %>%
        forecast(h = 24) -> fit
      
      fit %>% autoplot(DFA, level = NULL) + labs(title= "Transformed_Cement Forecast", y = "Total Count", x = "Date") 
    } 
    else if (input$forecast == "Drift") {
      DFA %>%
        model("Drift" = RW(Transformed_Cement ~ drift())) %>%
        forecast(h = 24) -> fit
      
      fit %>% autoplot(DFA, level = NULL) + labs(title= "Transformed_Cement Forecast", y = "Total Count", x = "Date") 
    } 
    else if (input$forecast == "Holts") {
      DFA %>%
        model("Holts" = ETS(Transformed_Cement ~ trend())) %>%
        forecast(h = 24) -> fit
      
      fit %>% autoplot(DFA, level = NULL) + labs(title= "Transformed_Cement Forecast", y = "Total Count", x = "Date") 
    } 
    else if (input$forecast == "Holts / Winters") {
      DFA %>%
        model("H/W" = ETS(Transformed_Cement ~ error("A") + trend("A") +
                            season("A"))) %>%
        forecast(h = 24) -> fit
      
      fit %>% autoplot(DFA, level = NULL) + labs(title= "Transformed_Cement Forecast", y = "Total Count", x = "Date") 
    } 
    else if (input$forecast == "ARIMA") {
      DFA %>%
        model("ARIMA" = ARIMA(Transformed_Cement ~ pdq(0,1,1) + PDQ(0,1,1))) %>%
        forecast(h = 24) -> fit
      
      fit %>% autoplot(DFA, level = NULL) + labs(title= "Transformed_Cement Forecast", y = "Total Count", x = "Date") 
    } 
    
    else {
      DFA %>%
        model("Auto" = ARIMA(Transformed_Cement, stepwise = FALSE)) %>%
        forecast(h = 24) -> fit
      
      fit %>% autoplot(DFA, level = NULL) + labs(title= "Transformed_Cement Forecast", y = "Total Count", x = "Date") 
    } 
  })
  
  
  output$addtext <- renderText({
    
    paste("This tab plots the forecast of your choice of the Transformed Cement producton data.",
          "\nAvailible forecasts are the Seasonal Naive, Naive, Mean, Drfit, Holts, Holts/Winters, manual chosen ARIMA, and Auto ARIMA.",
          "\nThe Seasonal Naive projects using the previous cycle's data and expecting the same trend.", 
          "\nThe Naive model uses the last point plotted and projects a horizontal line forward into time.",
          "\nThe Mean model uses the data mean to project a horizontal line from the mean.",
          "\nThe Drfit model takes the first and last points of the model to create a linear line between them",
          "\nThe Holts model is an exponential smoothing model that adds a trend component to the forecast.",
          "\nThe Holts / Winters model is an exponential smoothing modle that also includes a seasonality component to the forecast.",
          "\nThe Manual chosen Auto Regressive Integrated Moving Average model has paremeters I thought could be the best forecast.",
          "\nThe Auto ARIMA is a model that uses the best possible ARIMA paremters to best predict the forecast.")
    
  })
  
  output$addtab <- renderTable({
    DFA <- as_tsibble(DATA)
    
    
    fit <- DFA %>%
      model("Naive" = NAIVE(Transformed_Cement),
            "Seasonal Naive" = SNAIVE(Transformed_Cement ~ lag(8)),
            "Drift" = RW(Transformed_Cement ~ drift()),
            "Mean" = MEAN(Transformed_Cement),
            "Holts" = ETS(Transformed_Cement ~ trend()),
            "H/W" = ETS(Transformed_Cement ~ error("A") + trend("A") + season("A")),
            "ARIMA" = ARIMA(Transformed_Cement ~ pdq(0,1,1) + PDQ(0,1,1)),
            "Auto" = ARIMA(Transformed_Cement, stepwise = FALSE))
    
    
    tab <- accuracy(fit)
    
    as.data.frame(tab)[,c(1,4,5)] %>% 
      arrange(RMSE)

  })
  
}

shinyApp(ui = ui, server = server)