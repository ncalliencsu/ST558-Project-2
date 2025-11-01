library(shiny)
library(shinyalert)
library(tidyverse)
library(gtsummary)

load(file = "store_data.Rdata")
num_vars <- store_dat |> 
  select(where(is.numeric), -c(Row_ID, Year))
nvars <- colnames(num_vars)

cat_vars <- store_dat |> 
  select(where(is.character) | where(is.factor), -c(Customer_Name, STATEFIPS))
cvars <- colnames(cat_vars)

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Store Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Numeric Variables:"),

      selectizeInput(
        inputId = "num_var_1",
        label = "Num Variable 1",
        choices = nvars,
        selected = nvars[1]
      ),
      
      sliderInput("slider_nv_1", label = "Range for Num Variable 1",
                  min = 0 , max = 500,
                  value = 250),

      selectizeInput(
        inputId = "num_var_2",
        label = "Num Variable 2",
        choices = nvars,
        selected = nvars[2]
      ),

      sliderInput("slider_nv_2", label = "Range for Num Variable 2",
                  min = 0, max = 500,
                  value = 250),

      h2("Choose a Subset of the Data:"),

      selectizeInput(
        inputId = "cat_var_1",
        label = "Categorical Variable 1",
        choices = cvars,
        selected = cvars[1]
      ),
      
      selectizeInput(
        inputId = "cat_var_2",
        label = "Categorical Variable 2",
        choices = cvars,
        selected = cvars[2]
      ),
  
      actionButton("subset","Update")

      
    ), #sidebarPanel()
    
    mainPanel()
    
  ), #sidebarLayout
  
 
  
  
  
) #fluidPage
  





# Define server logic required to draw a histogram
server <- function(input, output, session) {
}


# Run the application
shinyApp(ui = ui, server = server)
