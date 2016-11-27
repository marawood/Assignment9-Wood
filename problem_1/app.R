library(shiny)
library(babynames)
library(tidyverse)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1880, max = 2014, value = c(1900, 2000), sep = "", round = TRUE),
      textInput("nameInput", "Name", value = "Taylor"),
      radioButtons("sexID", "Sex",  choices = c("Female only", "Male only", "Both"), selected = "Both")),
    mainPanel(
      plotOutput("main_plot"),
      tableOutput("results")
    )
  ),
  titlePanel("Baby Names")
)
server <- function(input, output, session) {
  reduced_df <- reactive({
    sex_vec <- switch(input$sexID,
                      `Female only` = "F",
                      `Male only` = "M",
                      Both = c("F", "M")
    )
    babynames$year <-as.integer(babynames$year)
    filter(
      babynames, 
      name == input$nameInput, 
      year >= input$yearInput[1] & year <= input$yearInput[2], 
      sex %in% sex_vec 
    )
  })
  set_wide_df <- reactive({
    spread(reduced_df(), key = sex, value = n)
    
  })
  output$main_plot <- renderPlot({
    if (input$sexID == "Both"){
      ggplot(data = reduced_df(), 
             aes(year, n, colour = sex)) + 
        geom_line() + ggtitle(input$nameInput) + scale_color_manual(name = "Sex", values = c("red", "blue")) +xlab("Year") + ylab("Number")
      }
    else if (input$sexID == "Female only"){
      ggplot(data = reduced_df(), 
             aes(year, n, colour = sex)) + 
        geom_line() + ggtitle(input$nameInput) + scale_color_manual(name = "Sex", values = "red") +xlab("Year") + ylab("Number")
    }
    else {
      ggplot(data = reduced_df(), 
             aes(year, n, colour = sex)) + 
        geom_line() + ggtitle(input$nameInput) + scale_color_manual(name = "Sex", values = "blue") +xlab("Year") + ylab("Number")
    }
  })
  
  output$results <- renderTable({ 
      if (input$sexID == "Both"){
      dplyr::select(set_wide_df(), -prop)
      }
      else{
        dplyr::select(reduced_df(), -prop)
      }
    })
    
}
shinyApp(ui = ui, server = server)



