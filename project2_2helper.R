# info about the data set

#All Categorical Variables
cvars <- c(
  "County",
  "Category",
  "Order_Date",
  "Customer_ID",
  "Region",
  "Segment",
  "State",
  "Sub_Category"
)

# Categories
cvars_1 <- c(
  "All", 
  "Furniture", 
  "Office Supplies", 
  "Technology"
  )


#Regions
cvars_2 <- c(
  "All",
  "East",
  "Central",
  "South",
  "West"
)


#All Numeric Variables
nvars <- c(
  "Sales",
  "Quantity",
  "Discount",
  "Profit"
)

#Chart Types
cat_charts <- c(
  "Simple Bar Plot",
  "Grouped Bar Plot"
)

num_charts <- c(
  "Box Plot",
  "Histogram",
  "Scatterplot",
  "Chloropeth Map"
)

#Summary Types
cat_summaries <- c(
  "1-Way Contingency Table",
  "2-Way Contingency Table"
)

num_summaries <- c(
  
  
)



#Variable Choices by Chart Types
observeEvent(input$charts,{
  
 
  if(input$cat_plot_choice == "Simple Bar Plot") {
    x_choices = where(is.character(subsetted$data))
  } 
  else if(input$cat_plot_choice == "Grouped Bar Plot") {
    x_choices = where(is.character(subsetted$data))
    y_choices = where(is.character(subsetted$data))
    
  }
  else if(input$cat_plot_choice == "Boxplot") {
    x_choices = where(is.character(subsetted$data))
    y_choices = where(is.numeric(subsetted$data))
  } 
  else if(input$cat_plot_choice == "Histogram") {
    x_choices = where(is.character(subsetted$data))
    fill_choices = where(is.character(subsetted$data))
  }
  else if(input$cat_plot_choice == "Scatterplot") {
    x_choices = where(is.character(subsetted$data))
    y_choices = where(is.numeric(subsetted$data))
    color = where(is.character(subsetted$data))
    facet_wrap = checkbox
  
  }
  else {#Chloropeth Map
    choice_1 = subsetted$data |> select(Sales, Profit)
  }
  
  updateSliderInput(session, "slider_nv_2", 
                    value = vals[3],
                    min = vals[1],
                    max = vals[2],)
})