shinyServer(function(input, output,session) {
  dataxy = reactive({
      XYcurves_ClinVarMutations[XYcurves_ClinVarMutations[,1]==input$Genes,]
    })

  dataXYCG = reactive({
      XYCG0<-XYCG[XYCG[,1]==input$Genes,]
      dataXYCG=XYCG0[!is.na(XYCG0[,3]),]
    })
    
    output$table = DT::renderDataTable({
      MitoFates_1[MitoFates_1[,1]==input$Genes,]
    })
    

################ Plot 2
    output$plotxy2 = renderPlotly({
      pp<-ggplot(dataXYCG(), aes(y=dataXYCG()[,3], x=dataXYCG()[,2]))+
        geom_line() + 
        geom_point(aes(color=dataXYCG()[,7], text=sprintf("Protein.Change: %s<br>NT.Change: %s <br> Annotation: %s", dataXYCG()[,5], dataXYCG()[,6], dataXYCG()[,11])),show.legend = FALSE)+
        xlab("amino acid")+
        ylab("iMTSscore")
      ggplotly(pp,tooltip="text")
    })
    output$CVM = renderTable({
      message('clicked')
      d <- event_data("plotly_click")
      if (!is.null(d)) dataXYCG()[dataXYCG()[,2]==d$x,1:3]
    })
     output$GADV = renderTable({
      message('clicked')
      d <- event_data("plotly_click")
      if (!is.null(d)) dataXYCG()[dataXYCG()[,2]==d$x,c(1,11:13)]
    })  

})
#####    

