library(shiny)
library(shinythemes)
library(dplyr)
library(DT)
library(readxl)

load("movies.Rdata")

d_title_type <- sort(unique(movies$title_type))
d_genre <- sort(unique(movies$genre))
d_mpaa_rating <- sort(unique(movies$mpaa_rating))
d_studio <- sort(unique(movies$studio))
min_year <- min(movies$thtr_rel_year)
max_year <- max(movies$thtr_rel_year)

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 2,
                 selectizeInput(
                   inputId = "i_title_type",
                   label = "Title type:",
                   choices = d_title_type,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Begin typing title type...")
                 ),
                 
                 selectizeInput(
                   inputId = "i_genre",
                   label = "Genre:",
                   choices = d_genre,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Begin typing genre...")
                 ),
                 
                 selectizeInput(
                   inputId = "i_studio",
                   label = "Studio:",
                   choices = d_studio,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Begin typing studio...")
                 ),
                 
                 selectizeInput(
                   inputId = "i_mpaa_rating",
                   label = "MPAA rating:",
                   choices = d_mpaa_rating,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(placeholder = "Begin typing mpaa rating...")
                 ),
                 
                 sliderInput(
                   inputId = "i_year",
                   label = "Year", min = min_year, max = max_year,
                   value = c(1995, 2000)
                   
                 ),
                 
                 br(), br(),
                 
                 downloadButton("download", "Download results")
                 
    ),
    
    mainPanel(width = 10,
              DT::dataTableOutput(outputId = "mtable")
    )
  )
)


server <- function(input, output) {
  
  mtable <- reactive({
    filter(movies, (title_type %in% input$i_title_type) &
             (genre %in% input$i_genre) &
             (studio %in% input$i_studio) &
             (mpaa_rating %in% input$i_mpaa_rating) &
             (year >= input$year[1] & year <= input$year[2])) %>%
      select(title:thtr_rel_year) 
  })
  
  output$mtable <- DT::renderDataTable({
    DT::datatable(data = mtable(), options = list(pageLength = 10),
                  rownames = FALSE, class = 'display', escape = FALSE)
    
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "movie-results.csv"
    },
    content = function(con) {
      write.csv(mtable(), con)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)