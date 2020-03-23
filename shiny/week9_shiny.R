library(shiny)
library(tidyverse)
library(lubridate)
library(DT)

ui <- fluidPage(

    # Application title
    titlePanel("Week 9 Project"),

    # Sidebar with inputs 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "gender",
                        label = "Gender",
                        choices = c("Male", "Female", "All"),
                        selected = "All"
                        ),
            checkboxInput(inputId = "date",
                          label = "Include Participants Before August 1, 2017",
                          value = TRUE)
        ),

        # Show plot and table
        mainPanel(
           plotOutput("meanPlot"),
           dataTableOutput("dataTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    week9_tbl <- read_csv("data/week3.csv") %>%
        mutate(timeStart = ymd_hms(timeStart),
               timeEnd = ymd_hms(timeEnd),
               condition = factor(condition, 
                                  levels = c("A", "B", "C"), 
                                  labels = c("Block A", "Block B", "Control")),
               gender = factor(gender, 
                               levels = c("M", "F"), 
                               labels = c("Male", "Female")),
               mean1_5 = (q1 + q2 + q3 + q4 + q5)/5,
               mean6_10 = (q6 + q7 + q8 + q9 + q10)/5)

    output$meanPlot <- renderPlot({
        temp <- week9_tbl
        if(input$gender != "All"){
            temp <- subset(temp, gender == input$gender)
        }
        
        if(!input$date){
            temp <- subset(temp, timeStart > as.POSIXct("2017-07-31 23:59:59"))
        }
        
        ggplot(temp, aes(x = mean1_5, y  = mean6_10)) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE) +
            labs(x = "Q1-Q5 Mean", y = "Q6-Q10 Mean")
    })
    
    output$dataTable <- renderDataTable({
        datatable(week9_tbl) %>%
            formatDate(c("timeStart", "timeEnd"), method = "toUTCString")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# https://butts029.shinyapps.io/Week9Project2/ 