
#Framework Source: https://bookdown.org/paulcbauer/idv2/8-20-example-a-simple-regression-app.html



library(shiny)


bike_data <- read.csv("bike data.csv")
bike_data$Season <- as.factor(bike_data$Season)


str(bike_data)

ui <- fluidPage(
  titlePanel("Regression Model, Bike Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dependent", label = h3("Dependent"),
                  choices = list("Casual.Users" = "Casual.Users",
                                 "Total.Users" = "Total.Users",
                                 "Temperature.F" = "Temperature.F",
                                 "Season" = "Season"), selected = 1),
      
      selectInput("indepvar", label = h3("Explanatory variable"),
                  choices = list("Casual.Users" = "Casual.Users",
                                 "Total.Users" = "Total.Users",
                                 "Temperature.F" = "Temperature.F",
                                 "Season" = "Season"), selected = 1),
      
      selectInput("indepvar2", label = h3("Explanatory variable"),
                  choices = list("Casual.Users" = "Casual.Users",
                                 "Total.Users" = "Total.Users",
                                 "Temperature.F" = "Temperature.F",
                                 "Season" = "Season"), selected = 1)
      
      
),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                 
                  tabPanel("Model Summary", verbatimTextOutput("summary")) # Regression output
      )
    )  
))

# SERVER
server <- function(input, output) {
  
  # Regression output
 output$summary <- renderPrint({
    fit <- lm(bike_data[,input$dependent] ~ bike_data[,input$indepvar]+bike_data[,input$indepvar2])
    names(fit$coefficients) <- c("Intercept", input$indepvar, input$indepvar2)#Changes the names of coefficients based on the elements selected 
    summary(fit)
  })
  
  
  # Scatterplot output
 output$scatterplot <- renderPlot({
    plot(bike_data[,input$indepvar], bike_data[,input$dependent], main="Scatterplot",
         xlab=input$indepvar, ylab=input$dependent, pch=19)
    abline(lm(bike_data[,input$dependent] ~ bike_data[,input$indepvar]+bike_data[,input$indepvar2]), col="red")
    lines(lowess(bike_data[,input$indepvar],bike_data[,input$indepvar2],bike_data[,input$dependent]), col="blue")
  }, height=400)
  
  
}

shinyApp(ui = ui, server = server)

#Add more features to the dropdown menu and a 3rd variable option for the regression model. 
