library(shiny)
library(shinyalert)
library(tidyverse)
library(gtsummary)
library(ggplot2)      # For plotting
library(DT)           # For interactive data tables
library(zipcodeR)

source("project2_helper.R")


# Define UI for application 
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
        inputId = "sub_category",
        label = "Sub Category",
        choices = cvars_2,
        selected = cvars_2[1]
      ),
  
      selectizeInput(
        inputId = "month",
        label = "Month",
        choices = cvars_3,
        selected = cvars_3[1]
      ),
      
      selectizeInput(
        inputId = "region",
        label = "Region",
        choices = cvars_4,
        selected = cvars_4[1]
      ),
      
      selectizeInput(
        inputId = "state",
        label = "State",
        choices = cvars_5,
        selected = cvars_5[1]
      ),
      
      selectizeInput(
        inputId = "county",
        label = "County",
        choices = NA,
        selected = NA
      ),
      
      actionButton(inputId = "analyze",label = "Analyze Data")

      
    ), #sidebarPanel()
    
    #Main Panel
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
#                h4(""),
                 dataTableOutput("DTO_download"),
                 downloadButton("download", "Download Data")
        ),
        
        tabPanel("Data Exploration",
#                h4(""),
                 dataTableOutput("DTO_explore"),

                 # Output area for plots
                 uiOutput("barplot"),
        )
      )
    )
    
  ), #sidebarLayout
  
 
) #fluidPage
  
data <-readRDS(file = "store_data.RDS")


# Define server logic 
server <- function(input, output, session) {

  #################################################
  ##Numeric Variables
  #This code makes sure the select boxes update so they can't select the same variable in both!
  #first, update the first selection
  # observeEvent(input$num_var_1, {
  #   nv1 <- input$num_var_1
  #   nv2 <- input$num_var_2
  #   choices <- nvars
  #   if (nv1 != nv2){
  #     choices <- choices[-which(choices == nv1)]
  #     updateSelectizeInput(session,
  #                          "num_var_2",
  #                          choices = choices,
  #                          selected = nv2)
  #   }
  # })
  # #now, update the second selection
  # observeEvent(input$num_var_2, {
  #   nv1 <- input$num_var_1
  #   nv2 <- input$num_var_2
  #   choices <- nvars
  #   if (nv1 != nv2){
  #     choices <- choices[-which(choices == nv2)]
  #     updateSelectizeInput(session,
  #                          "num_var_1",
  #                          choices = choices,
  #                          selected = nv1)
  #   }
  # })
  
  # filter the selections for Sub-Category based on Category

  observeEvent(input$category, {
    cv1 <- input$category
    if (cv1 == cvars_1[2]) { #Furniture
      choices <- furniture 
      updateSelectizeInput(session,
                           "sub_category",
                           choices = choices,
                           selected = furniture[1])

    } else if (cv1 == cvars_1[3]) { #Office Supplies
      choices <- office_supplies
      updateSelectizeInput(session,
                           "sub_category",
                           choices = choices,
                           selected = office_supplies[1])

    } else if (cv1 == cvars_1[4]) { #Technology
      choices <- technology
      updateSelectizeInput(session,
                           "sub_category",
                           choices = choices,
                           selected = technology[1])

     } else {choices <- cvars_2 #All
      updateSelectizeInput(session,
                           "sub_category",
                           choices = choices,
                           selected = cvars_2[1])}
  })


  # filter the selections for States based on Region  
  
  observeEvent(input$region, {
    cv4 <- input$region
    if (cv4 == cvars_4[2]) {
      choices <- east #East
      updateSelectizeInput(session,
                           "state",
                           choices = choices,
                           selected = east[1])
      
    } else if (cv4 == cvars_4[3]) {
      choices <- central #Central
      updateSelectizeInput(session,
                           "state",
                           choices = choices,
                           selected = central[1])
      
    } else if (cv4 == cvars_4[4]) {
      choices <- south #South
      updateSelectizeInput(session,
                           "state",
                           choices = choices,
                           selected = south[1])
      
    } else if (cv4 == cvars_4[5]) {
      choices <- west  #West
      updateSelectizeInput(session,
                           "state",
                           choices = choices,
                           selected = west[1])
      
    } else {choices <- cvars_5 #All
      updateSelectizeInput(session,
                         "state",
                         choices = choices,
                         selected = cvars_5[1])}
  })
  
 
 # filter the selections for County based on State
  
  observeEvent(input$state, {
    cv5 <- input$state
    if(cv5 == cvars_5[9]){
      updateSelectizeInput(session,
                         "county",
                         choices = NA,
                         selected = NA)
      
      } else if (cv5 != cvars_5[1]) {
      st_abb <- state.abb[match(cv5, state.name)]
      counties <- search_state(st_abb) |> select(county)
      updateSelectizeInput(session,
                           "county",
                           choices = c("All",counties),
                           selected = "All" )

    } else {                         #All
    updateSelectizeInput(session,
                         "county",
                         choices = NA,
                         selected = NA)}

  })

#  data_subset <- reactiveValues(data = NULL)
  
  observeEvent(input$analyze,{

    #Numeric Variables
 
    
#  Categorical Variables
    if(input$category == "All"){
      cat_subset <- cvars_1[-1]
    } else if(input$category == "Furniture"){
      cat_subset <- cvars_1[2]
    } else if(input$category == "Office Supplies"){
      cat_subset <- cvars_1[3]
    } else {
      cat_subset <- cvars_1[4]
    }

    if(input$sub_category & input$category == "All"){
     sub_cat_subset <- cvars_2[-1]
    } else if(input$category == "Furniture" & input$sub_category == "All"){
     sub_cat_subset <- furniture[-1]
    } else if(input$category == "Furniture" & input$sub_category == "Furnishings"){
       sub_cat_subset <- furniture[2]
    } else if(input$category == "Furniture" & input$sub_category == "Bookcases"){
      sub_cat_subset <- furniture[3]
    } else if(input$category == "Furniture" & input$sub_category == "Chairs"){
        sub_cat_subset <- furniture[4]
    } else if (input$category == "Furniture" & input$sub_category == "Tables") {
      }

  })
  #Create a renderDataTable() object to display the data
#          output$DTO_download <- renderDataTable()
  
  }


# Run the application
shinyApp(ui = ui, server = server)
