library(shiny)
library(gapminder)
library(tidyverse)
View(gapminder)
countries <- distinct(gapminder[1])

ui <- shinyUI(fluidPage(
  
  titlePanel("Per Capita GDP Versus Life Expectancy"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("Year","Range of Years:", min = 1952, max = 2007, value = c(1952,2007), sep = ""),
      selectInput("Country", "Country", countries, selected = "United States")
    ),
    
    
    mainPanel(
      plotOutput("gapminderPlot")
    )
  )
))

server <- shinyServer(function(input, output) {
  View(reduced_df)
  output$gapminderPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    reduced_df <- filter(gapminder, country == input$Country[1], year >= input$Year[1] & year <= input$Year[2])
    # draw the plot with the input from above for year and for country
    ggplot(data = reduced_df, aes(log10(gdpPercap), lifeExp)) + geom_point() + geom_smooth() + ggtitle(paste("GDP Versus Life Expectancy for", input$Country, collapse=" ")) +
      xlab("GDP per Capita (Log)") + ylab("Life Expectancy") + geom_label(aes(label = year), data = reduced_df, alpha = 0.5, nudge_y = 0.5)
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

