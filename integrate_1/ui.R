library(shiny)

# Define UI for application
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Group A - Amazon Product Reviews"),
    
    # Sidebar layout with a product search input and table output
    sidebarLayout(
        sidebarPanel(
            textInput("txtSearch", "Search product:", ),
            selectInput("selectBrand", "Brand", c('All')),
            actionButton("btnSearch", "Search"),
            textInput(inputId = "ProductSearch",label = "Product Full Name: ",
                      value = "LG LT700P Refrigerator Water Filter, Filters up to 300 Gallons of Water, Compatible with Select LG Multi-Door Refrigerators with SlimSpace Plus Ice System"),
            actionButton("btnSearch2", "Search Product Review")
        ),
        
        # Show a table output of searched product
        mainPanel(
            dataTableOutput("tblProducts"),
            plotOutput("good_review"),
            plotOutput("bad_review")
        )
    )
))
