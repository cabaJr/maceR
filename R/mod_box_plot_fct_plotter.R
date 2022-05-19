plot_actogram <- function(env, plot_type){
  if ("total" %in% input$stdActogram){
      shinyjs::show(id = "spin1_1", anim = FALSE)
      # if(Custom_tables$cacheKeys[1,2] == Annotate$cacheKeys[1,2]){
      #   output$actogram1 <- renderCachedPlot({plot1}, cacheKeyExpr = Custom_tables$table1)
      # }else{
      #plot actogram and store it
      Annotate$plot_actogram(env, "total")
      plot1 <- Annotate$Actograms$acto1[[1]]
      output$actogram1 <- renderCachedPlot({plot1}, cacheKeyExpr = Custom_tables$table1)
    }
    shinyjs::show(id = "actogram1", anim = FALSE)
    shinyjs::show(id = "actogram_1", anim = FALSE)
    # }
    if ("sex" %in% input$stdActogram){
      shinyjs::show(id = "spin1_2", anim = FALSE)
      #plot actogram and store it
      Annotate$plot_actogram(env, "sex")
      plot2 <- Annotate$Actograms$acto2[[1]]
      output$actogram2 <- renderCachedPlot({plot2}, cacheKeyExpr = Custom_tables$table1)
      shinyjs::show(id = "actogram2", anim = FALSE)
      shinyjs::show(id = "actogram_2", anim = FALSE)
    }
    if ("genotype" %in% input$stdActogram){
      shinyjs::show(id = "spin1_3", anim = FALSE)
      #plot actogram and store it
      Annotate$plot_actogram(env, "genotype")
      plot3 <- Annotate$Actograms$acto3[[1]]
      output$actogram3 <- renderCachedPlot({plot3}, cacheKeyExpr = Custom_tables$table1)
      shinyjs::show(id = "actogram3", anim = FALSE)
      shinyjs::show(id = "actogram_3", anim = FALSE)
    }
    if ("cabinet" %in% input$stdActogram){
      shinyjs::show(id = "spin1_4", anim = FALSE)
      #plot actogram and store it
      Annotate$plot_actogram(env, "cabinet")
      plot4 <- Annotate$Actograms$acto4[[1]]
      output$actogram4 <-renderCachedPlot({plot4}, cacheKeyExpr = Custom_tables$table1)
      shinyjs::show(id = "actogram4", anim = FALSE)
      shinyjs::show(id = "actogram_4", anim = FALSE)
    }
}