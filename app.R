#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("src/functions.R")
ggplot2::theme_set(theme_grey(15))

papers <- read.csv("papers.csv", stringsAsFactors = FALSE) %>% 
  select(text=1)
src_df <- create_tidy_df(papers) 

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("n_words",
                     "Number of Words to Plot:",
                     min = 1,
                     max = 25,
                     value = 10)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
           tabPanel("Import"),
           tabPanel("Frequency", (
             plotOutput("freqPlot")
           )),
           tabPanel("Sentiment", (
             plotOutput("sentimentPlot")
           )),
           tabPanel("Word Cloud", (
             plotOutput("wordcloudPlot")
           )),
           tabPanel("Topic Modeling", (
             plotOutput("topicPlot")
           ))
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$freqPlot <- renderPlot({
      plot_word_freq(src_df, input$n_words)
   })
   
   output$sentimentPlot <- renderPlot({
     plot_sentiment(src_df, input$n_words)
   })
   
   output$wordcloudPlot <- renderPlot({
     plot_word_cloud(src_df)
   })
   
   output$topicPlot <- renderPlot({
     plot_topic_model(src_df, topics = 4, n_words = input$n_words)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

