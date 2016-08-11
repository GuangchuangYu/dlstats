shinyUI(fluidPage(
    titlePanel("dlstats: Download stats of R packages"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("cran_pkg_input", label = h3("CRAN packages"), value = "dlstats, emojifont, rvcheck"),
            actionButton("cran_go", "Go!")
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Trend",
                         plotOutput("cran_trendplot")
                         ),
                tabPanel("Data",
                         DT::dataTableOutput('tbl')
                         )
                
            )
        )
    )
))

