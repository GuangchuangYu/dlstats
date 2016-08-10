library("shiny")
library("dlstats")
library("ggplot2")
library("DT")

shinyServer(function(input, output, session) {
    bioc_pkg_stats <- eventReactive(input$bioc_go, {
        pkgs <- gsub("\\s*", "", unlist(strsplit(input$bioc_pkg_input, split=',')))
        bioc_stats(pkgs)
    })
  
    output$bioc_trendplot <- renderPlot({
        ggplot(bioc_pkg_stats(), aes(end, Nb_of_downloads, color=package)) + geom_line() + xlab(NULL) + ylab(NULL)
    })

    output$tbl <- DT::renderDataTable(bioc_pkg_stats(), options = list(lengthChange = FALSE))
})




