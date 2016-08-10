shinyUI(fluidPage(
    titlePanel("dlstats: Download stats of R packages"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("bioc_pkg_input", label = h3("BioC packages"), value = "ggtree"),
            actionButton("bioc_go", "Go!")
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Trend",
                         plotOutput("bioc_trendplot")
                         ),
                tabPanel("Data",
                         DT::dataTableOutput('tbl')
                         )
                
            )
        )
    )
))

