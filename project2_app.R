library(bslib)        # For building UI's easier
library(DT)           # For interactive data tables
library(dplyr)        # Renaming variables instead of rename in Base R
library(ggplot2)      # For making nice versatile plots
library(gt)           # For making formatted display tables
library(gtsummary)    # For making presentation ready numerical summaries
library(janitor)      # For examining and cleaning dirty data
library(plotly)       # For interactive ggplots
library(shiny)        # For building Shiny Apps
library(shinyalert)   # For creating popup messages in Shiny
library(shinycssloaders)   #For loaders
library(shinydashboard) # For hte box function
library(tidyverse)    # For making working with data easy
library(tigris)       # For making Chloropeth Map
library(zipcodeR)     # For working with Zip Code data

source("project2_helper.R")


##########################################################################################################
#                        UI Definition
##########################################################################################################  
ui <- fluidPage(

  titlePanel("Store Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      
      #Reduce Width of Sidebar
      width = 3,
      
      #Numeric Variable Definition
      h2("Select Numeric Variables:"),

      selectizeInput(
        inputId = "num_var_1",
        label = "Num Variable 1",
        choices = c("All", nvars),
        selected = "All"
      ),
      
      #Only show this panel if a numeric variable is selected
      conditionalPanel(condition = "input.num_var_1 != 'All'",
                       sliderInput("slider_nv_1", label = "Range for Num Variable 1",
                                   min = 0 , max = 25000,
                                   value = 250)),

      selectizeInput(
        inputId = "num_var_2",
        label = "Num Variable 2",
        choices = c("All", nvars),
        selected = "All"
      ),
      
      #Only show this panel if a numeric variable is selected
      conditionalPanel(condition = "input.num_var_2 != 'All'",
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
      
      #
      actionButton(inputId = "subset_data",label = "Subset Data")

      
    ), #sidebarPanel()
    
    #Main Panel Definition
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("About",
                 h4("US Superstore App"), 

                 frow1 <- fluidRow(
                    box(title = h6(strong("Purpose of the App"),),
                      status = "primary",
                      solidHeader = F,
                      collapsible = F,
                      width = 12,
                      fluidRow(column(width = 8, textOutput( "purpose" )),
                              column(width = 4, align = "center",
                                     img(src="https://storage.googleapis.com/kaggle-datasets-images/422303/805812/e6738158f953eeab9dceb6fac01d6ce5/dataset-cover.jpg?t=2019-11-21-09-14-22", width=400))))),
                 
                 p(),
                strong("Description of the Data"),
                 p("The dataset contains data on orders placed and shipped for the calendar years 2014 - 2018."), 
                 p("The categorical variables available for analysis are:"),
                 tags$ol(
                   tags$li("Category (Furniture, Office Supplies, Technology)"),
                   tags$li("Sub Category",
                     tags$ol(
                       tags$li("Accessories, Appliances, Art, Binders, Bookcases, Chairs"), 
                       tags$li("Copiers, Envelopes, Fasteners, Furnishings, Labels, Machines"), 
                       tags$li("Paper, Phones, Storage, Tables"),
                       )),
                   tags$li("Month, Year"),
                   tags$li("Segment (Consumer, Corporate, Home Office)"),
                   tags$li("Region (East, West, South, Central State")
                 ),
                 p("The continuous variables are Sales, Quantity, Discount and Profit."),

                 strong("Description of the App"),
                 p("The sidebar on this page is for selecting variables to subset the dataset. You may select two (2) 
                   numeric variables and two (2) categorical variables to create a subset. If you select numeric 
                   variable(s), you will see a slider appear that will allow you to filter the numeric variable(s) up 
                   to the level(s) of the slider selected. If you select a categorical variable(s), the data will be 
                   filtered by the selected variable(s). When you are ready to subset the data, press the 'Subset 
                   Data' button."),
                 
                 p("The Data Download panel allows you to view the data and provides a means for downloading the data 
                   in csv format. The Data Exporation panel has as sub-panel for categorical variable analysis and a 
                   sub-panel for numeric variable analysis. Each sub-panel provides a panel for summary data and a 
                   panel for graphical output.  You can select appropriate variables using pickers available on each 
                   sub-panel. Action buttons are provided for displaying data of interest.
                   
                   Note that the Chloropeth Map on the Numeric Analysis panel uses the entire dataset to render the plot. The subsetted data is not used for this function.")
                 
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
                  

###########################Categorical Variable Analysis UI Definition###############################################

                  tabPanel("Categorical Variable Analysis",

                  ui <- page_fillable(
                    # Categorical Variable Numeric Summaries
                     card(
                      card_header("Data Summary"),
                      layout_sidebar(
                        sidebar = sidebar(width = 300,
                          bg = "lightgrey",
                          
                          selectizeInput(
                            inputId = "cs_choice",
                            label = "Select Summary Type",
                            choices = c(NA, cat_summaries) ,
                            selected = NULL
                          ),
                          
                          #Dynamic UI for Categorical Numerical Summaries Selection
                          uiOutput(outputId = "cs_select"),
 
                          actionButton(inputId = "cs_display",label = "Display Summary"),
                          ),
                          
                          #Dynamic UI for Categorical Numerical Summaries Output
                          #uiOutput("DT_cat_sum")
                          gt_output("cs_table") 
                        
                        ), #layout sidebar

                    ), #card

                  # Categorical Variable Graphical Summaries
                    card(
                      card_header("Graphical Summary"),
                      layout_sidebar(
                        sidebar = sidebar(width = 300,
                          bg = "lightgrey",
                          
                          selectizeInput(
                            inputId = "cg_choice",
                            label = "Select Chart Type",
                            choices = c(NA, cat_charts) ,
                            selected = NULL
                            ),
                          
                          #Dynamic UI for Categorical Graphical Summary Selection
                          uiOutput("cg_select"),
                          
                          actionButton(inputId = "cg_display",label = "Display Plot")
                          ), #sidebar
                       
                          plotOutput("cg_plot")
                        
                        ), #layout
                      ),
                  )  #page fillable
                  
                  ), #tabPanel Categorical Variables


####################Numeric Variable Analysis UI Definition####################################################################

                  tabPanel("Numeric Variable Analysis",

                  ui <- page_fillable(
                    # Numeric Variable Summaries
                     card(
                      card_header("Data Summary"),
                      layout_sidebar(
                        sidebar = sidebar(width = 300,
                          bg = "lightgrey",
                          
                          selectizeInput(
                            inputId = "nv_out_1",
                            label = "Select Summary Variable",
                            choices = c(NA, nvars),
                            selected = NULL
                          ),
                          
                          selectizeInput(
                            inputId = "cv_out_5",
                            label = "Select Categorical Variable",
                            choices = c(NA, cvars),
                            selected = NULL
                          ),
 
                          actionButton(inputId = "ns_display",label = "Display Summary"),
                          ),
                          
                          #Dynamic UI for Numerical Summaries Output
                           textOutput("ns_caption"),
                           tableOutput("ns_table") 
#                          gt_output("ns_table")
                        
                        ), #layout sidebar

                    ), #card

                  # Numeric Variable Graphical Summaries
                    card(
                      card_header("Graphical Summary"),
                      layout_sidebar(
                        sidebar = sidebar(width = 300,
                          bg = "lightgrey",
                          
                          selectizeInput(
                            inputId = "ng_choice",
                            label = "Select Chart Type",
                            choices = c(NA, num_charts) ,
                            selected = NULL
                            ),
                          
                          #Dynamic UI for Numeric Graphical Summary Selection
                          uiOutput("ng_select"),

                          actionButton(inputId = "ng_display",label = "Display Plot")
                          ), #sidebar
                       
                          withSpinner(
                          plotlyOutput("ng_plot")
                          )
                        
                        ), #layout
                      ),
                  )  #page fillable
                  
                  ), #tabPanel Numeric Variables
                  ) #tabsetPanel Data Exploration
              
        ) # tabPanel Data Exploration
      ) #tabsetPanel Main Panel
    ) #mainPanel
    
  ), #sidebarLayout
  
 
) # ui end (fluidpage)
  
my_data <-readRDS(file = "store_data.RDS")


##########################################################################################################
#                      Server Definition
##########################################################################################################

# Define server logic 
server <- function(input, output, session) {

  
# About Page Purpose Text and Image  
  output$purpose <- renderText("The online sector, referred to as “clicks,” has been slowly eating up market share in the past two decades. E-commerce platform allows people to buy products from books, toys, clothes, and shoes to food, furniture, and other household items. The purpose of this app is to analyze sales data from an unidentified online US superstore that sold many of these types of items.")
  

# Update Slider Values on Main Sidebar
# based on Numeric Variable Selection
  
  observeEvent(input$num_var_1,{
    
    vals <-c(0,0,0)
    
    if(input$num_var_1 == "Sales") {
      vals = c(0,23000,55)
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
      vals = c(0,23000,55)
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
  
  #Subset Data Button Executes the Code in this Block
  observeEvent(input$subset_data,{

# Subset Numeric and Categorical Variables 

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
    num_var_1 <- input$num_var_1
    num_var_2 <- input$num_var_2
    
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
      {if("Sales" %in% num_var_1) filter(., Sales <= input$slider_nv_1) else .} %>%
      {if("Sales" %in% num_var_2) filter(., Sales <= input$slider_nv_2) else .} %>%
      
      {if("Quantity" %in% num_var_1) filter(., Quantity <= input$slider_nv_1) else .} %>%
      {if("Quantity" %in% num_var_2) filter(., Quantity <= input$slider_nv_2) else .} %>%

      {if("Discount" %in% num_var_1) filter(., Discount <= input$slider_nv_1) else .} %>%
      {if("Discount" %in% num_var_2) filter(., Discount <= input$slider_nv_2) else .} %>%
      
      {if("Profit" %in% num_var_1) filter(., Profit <= input$slider_nv_1) else .} %>%
      {if("Profit" %in% num_var_2) filter(., Profit <= input$slider_nv_2) else .}      
    
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
  output$cs_select <- renderUI({ 
    if (input$cs_choice == "1-Way Contingency Table") { 
      
      tagList( 
        selectizeInput(inputId = "cv_out_1",  label = "Select Summary Variable",
                       choices = cvars, selected = NULL), 
      ) 
    } 
    
    else if (input$cs_choice == "2-Way Contingency Table") { 
      
      tagList( 
        selectizeInput(inputId = "cv_out_1",  label = "Select Summary Variable",
                       choices = cvars, selected = NULL),  

        
        selectizeInput(inputId = "cv_out_2", label = "Select Group Variable",
                       choices = cvars , selected = NULL
        ), 
      ) 
    } 
  }) 
  
  
  # Update Categorical Variable Menus on Data Exploration Tab for 
  # Graphical Summaries based on Plot Type Selected 
  output$cg_select <- renderUI({ 
    if (input$cg_choice == "Simple Bar Plot") { 
      
      tagList( 
        selectizeInput(inputId = "cv_out_3",  label = "Select Summary Variable",
                       choices = cvars,selected = NULL), 
      ) 
    } 
    
    else if (input$cg_choice == "Grouped Bar Plot") { 
      
      tagList( 
        selectizeInput(inputId = "cv_out_3",  label = "Select Summary Variable",
                       choices = cvars,selected = NULL),  

        
        selectizeInput(inputId = "cv_out_4", label = "Select Group Variable",
                       choices = cvars , selected = NULL
        ), 
      ) 
    } 
  }) 
  

    #Create Categorical Data Summary Table
    output$cs_table <- render_gt({

      validate(
        need(!is.null(subsetted$data), "Please select your variables, subset, and click the 'Analyze Data' button."
             ))
      
      if (isolate(input$cs_choice) == "1-Way Contingency Table") {
        
        ivar1 <- isolate(input$cv_out_1)
        
        subsetted$data %>%
          tabyl(!!sym(ivar1)) %>%
          gt(rowname_col = ivar1) %>%
          tab_header(
            title = paste("1-Way Contingency Table for", ivar1)
          )
        
      } else if (isolate(input$cs_choice) == "2-Way Contingency Table") {

          ivar1 <- isolate(input$cv_out_1)
          ivar2 <- isolate(input$cv_out_2)
          
          validate(
          need(ivar1 != ivar2, "Please select a different grouping variable."
                  )
           )
        

        ctab <- subsetted$data %>%
          tabyl(!!sym(ivar1), !!sym(ivar2))
        gt(ctab, rowname_col = ivar1) %>%
          tab_spanner(
            columns = names(ctab)[-1], # all columns except the first
            label = ivar2
          ) %>%
          tab_stubhead(
            label = ivar1
          ) %>%
          tab_header(
            title = paste("2-Way Contingency Table for", ivar1, "and", ivar2)
          )
      }
    })    %>% bindEvent(input$cs_display)

      #Create Categorical Graphical Summary

      output$cg_plot <- renderPlot({input$cg_display
        
        validate(
          need(!is.null(subsetted$data), "Please select your variables, subset, and click the 'Analyze Data' button."
          ))
        
        if (isolate(input$cg_choice) == "Simple Bar Plot") {
          
          ivar3 <- isolate(input$cv_out_3)

                ggplot(data = subsetted$data, aes(x = !!sym(ivar3))) +
                    geom_bar() +
                      geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
                        labs(
                          title = paste("Count of Orders by", ivar3),
                            x = ivar3,
                            y = paste("Count by", ivar3)
            )
          

        } else if (isolate(input$cg_choice) == "Grouped Bar Plot") {
          
          ivar3 <- isolate(input$cv_out_3)
          ivar4 <- isolate(input$cv_out_4)
          
          validate(
            need(ivar3 != ivar4, "Please select a different grouping variable."
            )
          )

              ggplot(data = subsetted$data, aes(x = !!sym(ivar3), fill = !!sym(ivar4))) +
                      geom_bar(position = "dodge") +
                        scale_fill_discrete(name = ivar4) +
                          labs(
                            title = paste("Count of Orders by", ivar4),
                              x = ivar3,
                              y = paste("Count by", ivar4)
            )
          
          
        }
      }) %>% bindEvent(input$cg_display)


#    Create Numeric Data Summary Table
      
      #Generate Caption for Summary Table
      output$ns_table <- renderTable({ 
        
     
        validate(
          need(!is.null(subsetted$data), "Please select your variables, subset, and click the 'Analyze Data' button."
          ))
        
        output$ns_caption <- renderText({
          paste("Summary Statistics for ", input$nv_out_1, " by ", input$cv_out_5)
        }) %>% bindEvent(input$ns_display)
        
        ivar1 <- isolate(input$nv_out_1)
        ivar5 <- isolate(input$cv_out_5)
        
        subsetted$data |>
                select(!!sym(ivar1), !!sym(ivar5)) |>
                group_by(!!sym(ivar5)) |>
                summarise(min = min(!!sym(ivar1)),
                q1 = quantile(!!sym(ivar1), 0.25),
                median = median(!!sym(ivar1)),
                mean = mean(!!sym(ivar1)),
                q3 = quantile(!!sym(ivar1), 0.75),
                max = max(!!sym(ivar1))) 
        
          }) %>% bindEvent(input$ns_display)
      

    # Update Numeric Variable Menus on Data Exploration Tab for 
    # Graphical Summaries based on Type of Plot Selected 
    output$ng_select <- renderUI({ 
      if (input$ng_choice == "Box Plot") { 
        
        tagList( 
          selectizeInput(inputId = "cv_out_6",  label = "Select X-Axis Variable",
                         choices = cvars,selected = NULL), 
          selectizeInput(inputId = "nv_out_2", label = "Select Y-Axis Variable",
                         choices = nvars, selected = NULL)
        ) 
      } 
      
      else if (input$ng_choice == "Histogram") { 
        
        tagList( 
          selectizeInput(inputId = "nv_out_2",  label = "Select X-Axis Variable",
                         choices = nvars,selected = NULL),  
          selectizeInput(inputId = "cv_out_6", label = "Select Fill Variable",
                         choices = cvars, selected = NULL), 
        ) 
      } 
      
      else if (input$ng_choice == "Scatter Plot") { 
        
        tagList( 
          selectizeInput(inputId = "cv_out_6",  label = "Select X-Axis Variable",
                         choices = c("Order_Date", "Month", "Year"), selected = "Year"),  
          selectizeInput(inputId = "nv_out_2", label = "Select Y-Axis Variable",
                         choices = nvars, selected = NULL), 
          selectizeInput(inputId = "cv_out_7", label = "Select Shading Variable",
                         choices = cvars, selected = NULL), 
          
          checkboxInput(inputId = "facet_wrap", label = "Facet Wrap"),
          selectizeInput(inputId = "cv_out_8", label = "Select Wrap Variable",
                         choices = cvars, selected = NULL)
          
        ) 
      } 
      
      else if (input$ng_choice == "Chloropeth Map") { 
        
        tagList( 
          selectizeInput(inputId = "nv_out_2", label = "Select Gradient Variable",
                         choices = nvars, selected = NULL), 
         ) 
      } 
      
      
    }) 
          
          
    #Create Numeric Graphical Summary
    
      output$ng_plot <- renderPlotly({
        
        validate(
          need(!is.null(subsetted$data), "Please select your variables, subset, and click the 'Analyze Data' button."
          ))
        
        if (isolate(input$ng_choice) == "Box Plot") {
          
          ivar2 <- input$nv_out_2
          ivar6 <- input$cv_out_6
          
          bplot <- ggplot(data = subsetted$data, 
                      aes(x = !!sym(ivar6), y = !!sym(ivar2), 
                        fill = !!sym(ivar6))) + geom_boxplot() + 
                          labs(title = paste("Boxplot of ", ivar2, "by" , ivar6), 
                                x = ivar6 , y = ivar2) 
          
          ggplotly(bplot)

        } else if (isolate(input$ng_choice) == "Histogram") {
          
          ivar2 <- input$nv_out_2
          ivar6 <- input$cv_out_6          
          
          if (ivar2 %in% c("Sales", "Profit")) {
            hplot <- ggplot(data = subsetted$data, 
              aes(x = log(!!sym(ivar2)), fill = !!sym(ivar6))) + 
                geom_histogram(aes(y = ..density..), color = 'black', position = "identity") +
                  labs(title = paste("Distribution of", ivar2, "by", ivar6))

        } else {
            hplot <- ggplot(data = subsetted$data, 
              aes(x =  (!!sym(ivar2)), fill = !!sym(ivar6))) + 
                geom_histogram(aes(y = ..density..), color = 'black', position = "identity") +
                  labs(title = paste("Distribution of ", ivar2, "by", ivar6))  
            
          }
                             
          
          ggplotly(hplot)

        } else if (isolate(input$ng_choice) == "Scatter Plot") {
          
          ivar2 <- input$nv_out_2
          ivar6 <- input$cv_out_6     
          ivar7 <- input$cv_out_7
          ivar8 <- input$cv_out_8
          
            if (input$facet_wrap == TRUE) {
                # Generate scatter plot with facet_wrap
                scplot_fw <- ggplot(data = subsetted$data,
                              aes(x = !!sym(ivar6), y = !!sym(ivar2), color = !!sym(ivar7))) + 
                                geom_point() + geom_jitter(width = 0.5, alpha = 0.3) + 
                                  labs(title = paste("Scatterplot of ", ivar2, " by ", 
                                     ivar7, " and ", ivar8," over ",ivar6,"s", sep = "")) +
                                      facet_wrap(~ get(ivar8), scales = "free_y") +
                                        theme(axis.text.x = element_text(angle = 90, vjust=.5, hjust=1))
                
                ggplotly(scplot_fw)
                
              } else {
                # Generate regular scatter plot
                scplot <- ggplot(data = subsetted$data,
                                 aes(x = !!sym(ivar6), y = !!sym(ivar2), color = !!sym(ivar7))) + 
                                  geom_point() + geom_jitter(width = 0.5, alpha = 0.3) + 
                                    labs(title = paste("Scatterplot of ", ivar2, " by ", 
                                     ivar7, " over ",ivar6,"s", sep = ""))
                
                ggplotly(scplot)
              }
          
        } else if (isolate(input$ng_choice) == "Chloropeth Map"){
          
          ivar2 <- input$nv_out_2

          FIPS_fill = aggregate(my_data[[ivar2]], list(my_data$STATEFIPS), FUN = sum) 
            colnames(FIPS_fill) <- c("STATEFP", ivar2)
          
          states <- states(cb = TRUE, class = "sf") |>
            filter(!as.numeric(STATEFP) %in% c(2, 15, 60, 66, 69, 72, 78)) |> # lower 48 and DC only
            left_join(FIPS_fill, by = "STATEFP")
          
                chlorplot <- states |>
                        ggplot(aes(fill = !!sym(ivar2))) +
                          geom_sf() + 
                           theme_test() +
                            scale_fill_gradientn(ivar2, colours = rev(scales::hue_pal()(5))) +
                              labs(title = paste("Gradient Map of Store", ivar2)) 
                
                ggplotly(chlorplot)
            }
          
        }) %>% bindEvent(input$ng_display)
      
  } #server end


# Run the application
shinyApp(ui = ui, server = server)


