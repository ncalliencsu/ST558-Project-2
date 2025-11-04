library(bslib)
library(shiny)
library(shinyalert)
library(tidyverse)
library(gtsummary)
library(ggplot2)      # For plotting
library(DT)           # For interactive data tables
library(zipcodeR)

source("project2_2helper.R")


############################################################
#                        UI Definition
############################################################  
ui <- fluidPage(

  titlePanel("Store Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      
      #Numeric Variable Definition
      h2("Select Numeric Variables:"),

      selectizeInput(
        inputId = "num_var_1",
        label = "Num Variable 1",
        choices = c(NA, nvars),
        selected = NA
      ),
      
      #Only show this panel if a numeric variable is selected
      conditionalPanel(condition = "input.num_var_1 != 'NA'",
                       sliderInput("slider_nv_1", label = "Range for Num Variable 1",
                                   min = 0 , max = 25000,
                                   value = 250)),

      selectizeInput(
        inputId = "num_var_2",
        label = "Num Variable 2",
        choices = c(NA, nvars),
        selected = NA
      ),
      
      #Only show this panel if a numeric variable is selected
      conditionalPanel(condition = "input.num_var_2 != 'NA'",
                       sliderInput("slider_nv_2", label = "Range for Num Variable 2",
                                   min = -10000 , max = 10000,
                                   value = 40)),
      
      #Categorical Variable Definition
      h2("Select Categorical Variables:"),

      selectizeInput(
        inputId = "category",
        label = "Category",
        choices = cvars_1 ,
        selected = cvars_1[1]
      ),
      
      selectizeInput(
        inputId = "region",
        label = "Region",
        choices = cvars_2,
        selected = cvars_2[1]
      ),
      
      actionButton(inputId = "analyze",label = "Analyze Data")

      
    ), #sidebarPanel()
    
    #Main Panel Definition
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 h4("Description and Purpose of the App"), 

                 # Output area for Text
                 p( "Purpose of the App"),
                 p("Description of the Data"), 
                 p("Description of the App"), 
                                    

        ),
        
        tabPanel("Data Download",
                 dataTableOutput("DT_download"),
                 downloadButton("download", "Download Data"),

                ),
        
        tabPanel("Data Exploration",
              tabsetPanel(
                  tabPanel("Categorical Variables",
                               
              #Categorical Variable Selections
#             h2("Select Categorical Variable:"),

                  ui <- page_fillable(
  
                  layout_columns(
                    card(card_header(NULL),
                         
                         layout_columns(
                           card(card_header("Select Primary Categorical Variable")),
                           card(card_header("Select Secondary Categorical Variable")),
                           col_widths = c(6, 6)
                                        )
                         ),
                    
                    card(card_header("Numerical Summaries")),
                    card(card_header("Graphical Summaries")),
                    
                    col_widths = c(12, 12, 12)
                                )
                  
                                      ),
                
                    # Output area for Table
                    dataTableOutput("DT_exp_cat"),
                           
                    # Output area for plots
                    plotOutput("plot_exp_cat")
                          ),
                               
                  tabPanel("Numeric Variables",
                               
                           # num widgets go here  

                           # Output area for Table
                           dataTableOutput("DT_exp_num"),
                           
                           # Output area for plots
                           plotOutput("plot_exp_num")                               
                          ),
                             

                  # Output area for plots
                   plotOutput("plot"),
                    )
        ) # tabPanel Data Exploration
      ) #tabsetPanel under Main Panel
    ) #mainPanel
    
  ), #sidebarLayout
  
 
) #fluidPage
  
my_data <-readRDS(file = "store_data.RDS")


############################################################
#                      Server Definition
############################################################  

# Define server logic 
server <- function(input, output, session) {

# Update Slider Values based on Numeric Variable Selection
  
  observeEvent(input$num_var_1,{
    
    vals <-c(0,0,0)
    
    if(input$num_var_1 == "Sales") {
      vals = c(4,23000,55)
    } 
    else if(input$num_var_1 == "Quantity") {
      vals = c(1,14,3)
    }
    else if(input$num_var_1 == "Discount") {
      vals = c(0,100,20)
    } 
    else if(input$num_var_1 == "Profit") {
      vals = c(-10000,10000,9)
    }

    updateSliderInput(session, "slider_nv_1", 
                      value = vals[3],
                      min = vals[1],
                      max = vals[2],)
  })
  
  observeEvent(input$num_var_2,{
    
    vals <-c(0,0,0)
    
    if(input$num_var_2 == "Sales") {
      vals = c(4,23000,55)
    } 
    else if(input$num_var_2 == "Quantity") {
      vals = c(1,14,3)
    }
    else if(input$num_var_2 == "Discount") {
      vals = c(0,100,20)
    } 
    else if(input$num_var_2 == "Profit") {
      vals = c(-10000,10000,9)
    }
 
    updateSliderInput(session, "slider_nv_2", 
                      value = vals[3],
                      min = vals[1],
                      max = vals[2],)
  })
  #Create a reactiveValues() object on the server side to subset
  #your data appropriately and pass it to subsequent sections of 
  #the main panel.
  
  subsetted <- reactiveValues(data = NULL)
  
  observeEvent(input$analyze,{

# Subset Categorical Variables 

    if(input$category == "All"){
      cat_subset <- cvars_1[-1]
    } else if(input$category == "Furniture"){
      cat_subset <- cvars_1[2]
    } else if(input$category == "Office Supplies"){
      cat_subset <- cvars_1[3]
    } else {
      cat_subset <- cvars_1[4]
    }

    if(input$region == "All"){
      reg_subset <- cvars_2[-1]
    } else if(input$region == "East"){
      reg_subset <- cvars_2[2]
    } else if(input$region == "Central"){
      reg_subset <- cvars_2[3]
    } else if(input$region == "South"){
      reg_subset <- cvars_2[4]
    } else {
      reg_subset <- cvars_2[5]
    }
    num_vars <- c(input$num_var_1, input$num_var_2)
    
    subset1 <- my_data |>
      filter(

    #Determine which elements of cvars[] are present in xxx_subset.
    #Return a logical vector of the same length as cvars[], with TRUE
    #for elements found in xxx_subset and FALSE otherwise. The filter() 
    #function from dplyr filters rows in a data frame based on conditions 
    #applied to its columns.
    
      Category %in% cat_subset,
      Region %in% reg_subset,

     #conditionally filter the data frame for positive Sales, positive
     # Quantity, Discount and Profit.
     
      ) %>%
      {if("Sales" %in% num_vars) filter(., Sales > 0) else .} %>%
      {if("Quantity" %in% num_vars) filter(., Quantity > 0) else .} %>%
      {if("Discount" %in% num_vars) filter(., Discount) else .} %>%
      {if("Profit" %in% num_vars) filter(., Profit) else .}
    
    #***You now need to update the data_subset reactive value object***#
    #the data argument should be updated to be the subsetted_data

    subsetted$data <- subset1

  })
  


  
  #Create a renderDataTable() object to display the data
  #          output$DTO_download <- renderDataTable()
  
  }


# Run the application
shinyApp(ui = ui, server = server)
