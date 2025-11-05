library(bslib)
library(shiny)
library(shinyalert)
library(tidyverse)
library(gtsummary)
library(ggplot2)      # For plotting
library(DT)           # For interactive data tables
library(zipcodeR)

source("project2_2helper.R")


########################################################################################################
#                        UI Definition
##########################################################################################################  
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
        id = "main_tabs",
        tabPanel("About",
                 h4("Description and Purpose of the App"), 

                 # Output area for Text
                 p("Purpose of the App"),
                 p("Description of the Data"), 
                 p("Description of the App"), 
                                    

        ),
        
        tabPanel("Data Download",
                 
                 card(card_header("Data Table"),
                      
                 dataTableOutput("DT_download"),
                 ), 
                 downloadButton("downloadData", "Download Data"),

                ),
        
        tabPanel("Data Exploration", 
                tabsetPanel(
                  id = "data_exp",
                  

###########################Categorical Variable Analysis###############################################

                  tabPanel("Categorical Variable Analysis",

                  ui <- page_fillable(
                    # Numeric Summaries
                     card(
                      card_header("Data Summary"),
                      layout_sidebar(
                        sidebar = sidebar(
                          bg = "lightgrey",
                          
                          selectizeInput(
                            inputId = "cat_sum_choice",
                            label = "Select Summary Type",
                            choices = c(NA, cat_summaries) ,
                            selected = NULL
                          ),
                          
                          #Dynamic UI for Categorical Numerical Summaries Selection
                          uiOutput(outputId = "cat_sum_select_controls"),
 
                          actionButton(inputId = "cat_sum_display",label = "Display Summary"),
                          ),
                          
                          #Dynamic UI for Categorical Numerical Summaries Output
                          uiOutput("DT_cat_sum")
                        
                        ), #layout sidebar

                    ), #card

                  # Graphical Summaries
                    card(
                      card_header("Graphical Summary"),
                      layout_sidebar(
                        sidebar = sidebar(
                          bg = "lightgrey",
                          
                          selectizeInput(
                            inputId = "cat_plot_choice",
                            label = "Select Chart Type",
                            choices = c(NA, cat_charts) ,
                            selected = NULL
                            ),
                          
                          #Dynamic UI for Categorical Graphical Summary Selection
                          uiOutput("cat_graph_select_controls"),
                          
                          actionButton(inputId = "cat_plot_display",label = "Display Plot")
                          ), #sidebar
                       
                          "Main",
                          plotOutput("plot_exp_cat")
                        ), #layout
                      ),
                  )  #page fillable
                  
                  ), #tabPanel Categorical Variables


####################Numeric Variable Analysis##################


                  ) #tabsetPanel Data Exploration
              
        ) # tabPanel Data Exploration
      ) #tabsetPanel Main Panel
    ) #mainPanel
    
  ), #sidebarLayout
  
 
) # ui end (fluidpage)
  
my_data <-readRDS(file = "store_data.RDS")


############################################################
#                      Server Definition
############################################################  

# Define server logic 
server <- function(input, output, session) {

# Update Slider Values on Main Sidebar
# based on Numeric Variable Selection
  
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
  
  #Analyze Button Executes the Code in this Block
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
  
  #Create a renderDataTable() object to display Data
  #on Download Data Panel
  output$DT_download <- renderDataTable(subsetted$data)
  
  #Download Handler for the Data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("store_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(subsetted$data, file)
    }
  )
  
  # Update Categorical Variable Menus on Data
  # Exploration Tab for Summary Data 
  output$cat_sum_select_controls <- renderUI({ 
    if (input$cat_sum_choice == "1-Way Contingency Table") { 
      
      tagList( 
        selectizeInput(inputId = "cvar_out_1",  label = "Select Summary Variable",
                       choices = cvars, selected = NULL), 
      ) 
    } 
    
    else if (input$cat_sum_choice == "2-Way Contingency Table") { 
      
      tagList( 
        selectizeInput(inputId = "cvar_out_1",  label = "Select Summary Variable",
                       choices = cvars, selected = NULL),  
        #          observeEvent({})
        
        selectizeInput(inputId = "cvar_out_2", label = "Select Group Variable",
                       choices = cvars , selected = NULL
        ), 
      ) 
    } 
  }) 
  
  
  # Update Categorical Variable Menus on Data
  # Exploration Tab for Graphical Summaries 
  output$cat_graph_select_controls <- renderUI({ 
    if (input$cat_plot_choice == "Simple Bar Plot") { 
      
      tagList( 
        selectizeInput(inputId = "cvar_out_3",  label = "Select Summary Variable",
                       choices = cvars,selected = NULL), 
      ) 
    } 
    
    else if (input$cat_plot_choice == "Grouped Bar Plot") { 
      
      tagList( 
        selectizeInput(inputId = "cvar_out_3",  label = "Select Summary Variable",
                       choices = cvars,selected = NULL),  
        #          observeEvent({})
        
        selectizeInput(inputId = "cvar_out_4", label = "Select Group Variable",
                       choices = cvars , selected = NULL
        ), 
      ) 
    } 
  }) 
  

        
    #Create Categorical Data Summary Table
        observeEvent(input$cat_sum_display,{
          
          output$DT_cat_sum <- renderTable({
            if (input$cat_sum_choice == "1-Way Contingency Table") {
              table(subsetted$data[[input$cvar_out_1]])
            
              } else if (input$cat_sum_choice == "2-Way Contingency Table") {
              table(subsetted$data[[input$cvar_out_1]], subsetted$data[[input$cvar_out_2]])
            }
          })
        
        })
        
        
 

  
  } #server end 


# Run the application
shinyApp(ui = ui, server = server)


