library(shiny)
library(janitor)
library(tidyverse)

#

nescacwar2 <- read.csv("https://www.dropbox.com/s/0ponhwf58x2jmi1/nescacwar2.csv?dl=1")

summary1 <- nescacwar2

summary2 <- nescacwar2 %>%
  filter(fOWar != "NaN") %>%
  filter(wRC != "NaN") %>%
  group_by(School) %>%
  mutate(TeamfOWar= sum(fOWar)) %>%
  mutate(TeamwRC= sum(wRC)) %>%
  distinct(School, TeamfOWar, TeamwRC) %>%
  gather(key= "TeamStat", value= "Value", TeamfOWar, TeamwRC)



###################
## ui            ## 
###################


school_choices <- as.list(c( as.character(nescacwar2$School)))
names(school_choices) <- c(as.character(nescacwar2$School))

stat_choices <- as.list(c("TeamfOWar", "TeamwRC"))
names(stat_choices) <- c("TeamfOWar", "TeamwRC")

position_choices <- as.list(c("Infield", "Outfield", "Catcher", "Utility", "Designated Hitter"))
names(position_choices) <- c("Infield", "Outfield", "Catcher", "Utility", "Designated Hitter")

year_choices <- as.list(c("Senior", "Junior", "Sophomore", "First-Year"))
names(year_choices) <- c("Senior", "Junior", "Sophomore", "First-Year")




# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NESCAC War, wOBA and wRC"),
  
  # Sidebar with a slider input for years
  sidebarLayout(
    sidebarPanel(
      
      
      selectInput(inputId= "School",
                  label= "Choose school:",
                  choices= school_choices,
                  multiple=TRUE,
                  selected= c("Amherst")
      ),
      radioButtons(inputId= "Stat",
                  label= "Choose stat:",
                  choices= stat_choices,
                  selected= c("TeamfOWar")
      ),
      checkboxGroupInput(inputId= "Year",
                         label= "Choose Class Year(s):",
                         choices= year_choices,
                         selected= c("Senior", "Junior", "Sophomore", "First-Year")
      ),
      checkboxGroupInput(inputId= "Position",
                         label= "Choose Position(s):",
                         choices= position_choices,
                         selected= c("Infield", "Outfield", 
                                     "Utility", "Catcher", "Designated Hitter")
      ),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs"
                  , tabPanel("Plot of Individual Player's fOWAR", plotOutput(outputId = "plot1"))
                  , tabPanel("Plot of Individual Player's wOBA", plotOutput(outputId = "plot2"))
                  , tabPanel("Plot of Individual Player's wRC", plotOutput(outputId = "plot3"))
                  , tabPanel("Plot of Team Totals", plotOutput(outputId = "plot4"))
                  , tabPanel("Table of Selected Data", tableOutput(outputId = "table"))
                  , tabPanel("Table of Wrangled Dataset", tableOutput(outputId = "table2"))
                  
      )
    )
  )
)


###################
## server        
###################

server <- function(input, output) {
  
  data_use <- reactive({
    summary1 %>%
      filter(School %in% input$School) %>%
      filter(Year %in% input$Year) %>%
      filter(Position %in% input$Position)
    
  })
  data_use2 <- reactive({
    summary1 %>%
      filter(School %in% input$School) %>%
      filter(Year %in% input$Year) %>%
      filter(Position %in% input$Position) %>%
      filter(fOWar != "NaN") %>%
      filter(wRC != "NaN") %>%
      group_by(School) %>%
      mutate(TeamfOWar= sum(fOWar)) %>%
      mutate(TeamwRC= sum(wRC)) %>%
      distinct(School, TeamfOWar, TeamwRC) %>%
      gather(key= "TeamStat", value= "Value", TeamfOWar, TeamwRC) %>%
      filter(TeamStat %in%input$Stat)
    
    
  })

  
  
  output$plot1 <- renderPlot({
    ggplot(data = data_use(), aes(x=Name , y = fOWar,
                                   color = School))  +
      geom_text(label=data_use()$Name) +
      labs(x = "Name", y = "fOWar"
           , color = "School") + 
      #theme_minimal() + 
      scale_color_brewer(type="qual") + 
      theme(axis.text=element_text(size=10, angle=90),
            axis.title=element_text(size=16))#,face="bold")
  })
  output$plot2 <- renderPlot({
    ggplot(data = data_use(), aes(x = Name, y = wOBA
                                   , color = School)) +
      geom_text(label=data_use()$Name) +
      labs(x = "Name", y = "wOBA"
           , color = "School") + 
      #theme_minimal() + 
      scale_color_brewer(type="qual") + 
      theme(axis.text=element_text(size=10, angle=90),
            axis.title=element_text(size=16))#,face="bold")
  })
  output$plot3 <- renderPlot({
    ggplot(data = data_use(), aes(x = Name, y = wRC
                                   , color = School)) +
      geom_text(label=data_use()$Name) +
      labs(x = "Name", y = "wRC"
           , color = "School") + 
      #theme_minimal() + 
      scale_color_brewer(type="qual") + 
      theme(axis.text.x=element_text(size=10, angle=90),
            axis.title=element_text(size=16))#,face="bold")
  })
  output$plot4 <- renderPlot({
    ggplot(data = data_use2(), aes(x = School, y = data_use2()$Value
                                  , fill = School)) +
      geom_col() +
      labs(x = "School", y = "Team Total"
           , fill = "School") + 
      #theme_minimal() + 
      scale_color_brewer(type="qual") + 
      theme(axis.text.x=element_text(size=10, angle=90),
            axis.title=element_text(size=16))#,face="bold")
  })

  output$table <- renderTable({
    select(data_use(), Name, School, avg, obp, slg,  OPS,
          wOBA, wRAA, wSB, fOWar, wRC)
  })
  output$table2 <- renderTable({
    select(nescacwar2, Name, ab, r, h, Doubles, Triples, hr, rbi, bb, k, sb, cs, avg, obp, slg,
    hbp, sf, sh, tb, xbh, pa)
  })
  
  
  
}

###################
## run the app   ## 
###################
shinyApp(ui = ui, server = server)