
## Olivia Coleman 
## 05/04/2022
## Working on a ER injuries Case Study Outlined in chapter 4 of Mastering Shiny by O'Reilly Media
## Made app improvements to UI by allowing user pick the number of the columns shown 
## and let the user look at all stories by using up and down arrows 

library(shiny)
library(vroom)
library(tidyverse)

#loading in main data set 
injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries

products <- vroom::vroom("neiss/products.tsv")
products

population <- vroom::vroom("neiss/population.tsv")
population

#creating my sub set of the products data 
prod_codes <- setNames(products$prod_code, products$title)

#function for limiting the number of elements in data frame to 5 
count_top <- function(df, var, n) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    #display user input to select data set and numbers of columns to display  
    fluidRow(
        column(8,selectInput("code", "Product",
                choices = setNames(products$prod_code, products$title),
                width = "100%")),
        column(2, numericInput("n", label = "Number of columns", value = 5, min = 1, max = 20, step = 1 )
    )),
    
    #display tables 
    fluidRow(
        column(4, tableOutput("diag")),
        column(4, tableOutput("body_part")),
        column(4, tableOutput("location"))
    ),
    
    #display user input for y - axis of graph 
    fluidRow(
        column(2, selectInput("y", "Y axis", c("rate", "count"))),
        column(12, plotOutput("age_sex"))
    ),
    
    #display random story 
    fluidRow(
        column(2, actionButton("story", "Tell me a story")),
        column(10, textOutput("narrative"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    selected <- reactive(injuries %>% filter(prod_code == input$code))
    
    ## modified code to change column names and number of entries 
    output$diag <- renderTable({
        diagTable <- count_top(selected(), diag, input$n - 1)
        colnames(diagTable) <- c("Diagnosis", "Count")
        diagTable
        }, width = "100%")
    
    output$body_part <- renderTable({
        bpTable <- count_top(selected(), body_part, input$n - 1)
        colnames(bpTable) <- c("Body Part", "Count")
        bpTable
        }, width = "100%")
    
    output$location <- renderTable({
        locTable <- count_top(selected(), location, input$n - 1 )
        colnames(locTable) <- c("Location", "Count") 
        locTable
        }, width = "100%")
    
    summary <- reactive({
        selected() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c("age", "sex")) %>%
            mutate(rate = n / population * 1e4)
    })
    
    #crate table based on user input 
    #changed the graph to bar plot to improve interpenetration 
    output$age_sex <- renderPlot({
        if (input$y == "count") {
            summary() %>%
                ggplot(aes(age, n, fill = sex)) +
                geom_col() +
                labs(y = "Estimated number of injuries")
        } else {
            summary() %>%
                ggplot(aes(age, rate, fill = sex)) +
                geom_col(na.rm = TRUE) +
                labs(y = "Injuries per 10,000 people")
        }
    }, res = 96)
    
    #select narrative to display 
    narrative_sample <- eventReactive(
        list(input$story, selected()),
        selected() %>% pull(narrative) %>% sample(1)
    )
    output$narrative <- renderText(narrative_sample())
}

# Run the application 
shinyApp(ui = ui, server = server)
