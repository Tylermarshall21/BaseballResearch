# Package installation
library(tidyverse)
library(shiny)
library(shinybusy)
library(tidytext)
library(ggrepel)

# Load in data
pitching2018 <- readr::read_csv("data-raw/pitching2018.csv") %>%
  janitor::clean_names() %>%
  mutate(ERA = (p_earned_run/p_formatted_ip) *9) %>%
  select(last_name, first_name, year, player_age, p_game, p_formatted_ip, ERA) %>%
  mutate(player_name = paste0(first_name, " ", last_name))
pitching2019 <-readr::read_csv("data-raw/pitching2019.csv") %>%
  janitor::clean_names() %>%
  mutate(TRUE_ERA = round(((p_earned_run/p_formatted_ip) *9),3)) %>%
  select(last_name, first_name, year, player_age, p_game, p_formatted_ip, TRUE_ERA) %>%
  mutate(player_name = paste0(first_name, " ", last_name))
pitchingtotal <- inner_join(pitching2018, pitching2019, by = "player_name") %>%
  mutate(MLE = round(ERA,3))

xbar <- mean(pitchingtotal$MLE) #Average ERA of all pitchers in data for 2018
N <- length(pitchingtotal$MLE) #Number of pitchers in our data set
estimators <- pitchingtotal %>% #Calculate JS estimator
  mutate(
    sigma2 = round(sum((ERA - xbar)^2)/N,3),
    S  = sum((ERA-xbar)^2),
    bhat = 1 - (N - 3)/S,
    mu_i = round(xbar + bhat*(ERA - xbar),3),
    JS = round(sqrt(sigma2)*mu_i, 3),
    shrink = round(MLE - JS, 3)
  ) %>%
  select(player_name, TRUE_ERA, MLE, JS, sigma2, shrink)
#Set up data for comparing estimators plot
estimatorslong <- estimators %>%
  pivot_longer(cols = c(JS, MLE), names_to = "Estimator", values_to = "Prediction")


#Set up data for errors
errors <- estimators %>%
  mutate(
    JS_Error = (JS - TRUE_ERA), #Find each individual error for JS
    MLE_Error = (MLE - TRUE_ERA) #Find each individual error for MLE
  ) %>%
  select(player_name, TRUE_ERA, JS_Error, MLE_Error)
  
#Player names
player_choices <- as.list(c( as.character(pitchingtotal$player_name)))
names(player_choices) <- c(as.character(pitchingtotal$player_name))


# User interface
ui <- fluidPage(
  
  # Title panel
  titlePanel("Using the James-Stein Estimator to Predict Pitcher Performance"),
  #Needed to help explain James-Stein
  withMathJax(),
  
  # some things take time: this lets users know
  add_busy_spinner(spin = "fading-circle"),
  
  # Options for user 
  fluidRow(
    #Create radio button that allows user to choose estimator to look at
    column(4,radioButtons(inputId = "estimator",
                         label = "Choose estimator:",
                         choices = c("JS", "MLE"))
    ),
    #Create input that allows user to choose players to investigate
    column(4,selectInput(inputId = "player",
                         label = "Choose players:",
                         choices = player_choices,
                         selected = c("Max Scherzer", "Stephen Strasburg", "Jacob deGrom",
                                      "Blake Treinen", "Andrew Kittredge", "Kirby Yates",
                                      "Edwin Jackson"),
                         multiple = TRUE)
    ),

    #Create button that allows user to download table they curated through selection parameters
    column(4, 
           downloadButton(
             outputId = "downloadData",
             label = "Download:", 
             value = ""
           )
    )
  ),
  
  # Create a new row for the table/plots
  fluidRow(
    tabsetPanel(type = "tabs",
                tabPanel("James-Stein Overview", uiOutput("background")),
                tabPanel("Prediction Table", DT::dataTableOutput("exampletable")),
                tabPanel("Error Table", DT::dataTableOutput("errortable")),
                tabPanel("True ERA and Prediction", plotOutput("comparison")),
                tabPanel("Shrinkage Comparison", plotOutput("shrinkage"))
    )
  )
  
)


# Server
server <- function(input, output) {
  
  #Data for prediction table
  pitchingdata <- reactive({
    estimators %>%
      filter(player_name %in% input$player) %>%
      arrange(shrink)
  })
  #Data for ERA and Innings Pitched Plot
  pitchingtotals <- reactive({
    pitchingtotal %>%
      filter(player_name %in% input$player)
  })
  #Data for plot to compare predicted and TRUE ERA
  pitchingestimate <- reactive({
    estimatorslong %>%
      filter(Estimator %in% input$estimator) %>%
      filter(player_name %in% input$player)
  })

  #Data to investigate error for MLE and JS
  errordata <- reactive({
    errors %>%
      filter(player_name %in% input$player) %>%
      arrange(JS_Error)
  })



  output$background <- renderUI({
    
  withMathJax('The equation for estimating ERA comes from Computer Age of 
  Statistical Inference (CASI) page 94, and is shown below. 
  $$ \\hat{x_i}^{js} = \\sigma_0 \\hat{\\mu_i}^{js}$$ There are two parts of this 
  equation that we need to find. The first comes from page 93 of Computer Age of 
  Statistical Inference, and is shown here.
  $$ \\hat{\\mu_i}^{js} = \\bar{x} + (1 - \\frac{N-3}{S}) 
  (x_i - \\bar{x}) \\textrm{ where } S = \\sum_{i = 1}^{N} (x_i - \\bar{x})^2$$ 
  In this equation, x_i is the ERA of the pitcher during the 2018 season, xbar is the 
  average ERA of the 2018 season, and N is the number of pitchers in our data.
  We also need to calculate the variance for our data. In the example from 
  CASI, they are estimating batting average which follows a 
  binomial distribution, so they are using binomial variance. ERA does not 
  follow a binomial distribution, but instead can be estimated by a normal 
  distribution, so using the formula for variance of a normal distribution 
  we can find the variance. The equation for variance in this case is 
    $$ \\sigma^2_0 = (\\sum_{i = 1}^{n}(ERA - \\bar{x})^2)/N$$
  We can now take the square root of the variance to get the standard deviation.
  Since we have both parts of the equation, to get our predicted ERA for the 
  2019 season for each pitcher using the James-Stein estimator. 
  The predictions can be seen in the next tab of this app.')
  })
  
  #Plot Table of Data to Allow User to see
  output$exampletable <- DT::renderDataTable(DT::datatable(pitchingdata()))
  
  
  #Plot to compare predicted ERA with actual ERA
  output$comparison <- renderPlot({
    ggplot(data = pitchingestimate(), aes (x = TRUE_ERA, y = Prediction, label = player_name)) +
      geom_point() + geom_label_repel() + geom_abline(slope = 1, intercept = 0) +
      labs( title = "Predicted ERA and Actual ERA", 
            x = "True ERA", y = "Predicted ERA")
  })
  
  #Table to compare errors
  output$errortable <- DT::renderDataTable(DT::datatable(errordata()))
  
  
  #Plot to compare shrinkage by innings pitched and ERA
  output$shrinkage <- renderPlot({
    ggplot(data = pitchingdata(), aes(x = MLE, y = shrink, label = player_name)) + 
      geom_point() + geom_label_repel() + labs(title = "Shrinkage Based on ERA",
                         x = "2018 Season ERA", y = "Shrinkage")
  })
  
  #Allow user to download table as .csv file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("pitching_data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(pitchingdata(), file)
    }
  )
  
  
}

shinyApp(ui = ui, server = server)