library("shiny")
library("dlstats")
library("ggplot2")

shinyServer(function(input, output, session) {
    cran_pkg_stats <- eventReactive(input$cran_go, {
        pkgs <- gsub("\\s*", "", unlist(strsplit(input$cran_pkg_input, split=',')))
        cran_stats(pkgs)
    })    

    output$cran_trendplot <- renderPlot({
        ggplot(cran_pkg_stats(), aes(end, downloads, color=package)) + geom_line() + xlab(NULL) + ylab(NULL)
    })
  
    output$tbl <- DT::renderDataTable(cran_pkg_stats(), options = list(lengthChange = FALSE))
})




