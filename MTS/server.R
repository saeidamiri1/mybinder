
shinyServer(function(input, output,session) {


  
  ###Title of Module###
  output$genetitle <- renderText({
    paste(input$Genes, " | ", Gene_index[Gene_index[,"Genes"]==input$Genes, "Header"])
    #input$Genes
    
  })
  
  
  

  ###All mutations###
  output$all_mutations = DT::renderDataTable({
   
    DT::datatable(XYC, options = list(order = list(3,'desc'), pageLength = 1000))
    
  })
  
  
  
  ###legend###
  
  output$legend <- renderImage({
    list(src = paste("./WWW/", toString(structure_legend()), sep=""),
         width = 252.4,
         height = 356.4,
         alt = "This is alternate text")
    
  })
  
  
   ###index###
  
  datalen = reactive({
    len <- Gene_index[Gene_index[,"Genes"]==input$Genes,]
    datalen = len[,"Length"]
   
  })
  
  
   ###XY curves to plot on plotly###
  
  dataXY = reactive({
    XY0<-XY_1[XY_1[,1]==input$Genes,]
    dataXY=XY0
    
  })
  
  
  ### XYCG to plot clinvar or GnomAD mutations onto plotly###
  
  dataXYCG = reactive({

  
  if (input$database == "ClinVar"){
    XYCG0<-XYC[XYC[,"Genes"]==input$Genes,]
    dataXYCG=XYCG0[!is.na(XYCG0[,"value"]),]

  }

  else{
    XYCG0<-XYG[XYG[,"Genes"]==input$Genes,]
    dataXYCG=XYCG0[!is.na(XYCG0[,"value"]),]
  }
  
})
  
  
  ### Reactive MPC for Plotly ###
  
  dataMPC = reactive({
    MPC0<-MPC_1[MPC_1[,"genename"]==input$Genes,]
   # print(MPC0)
    dataMPC=MPC0[!is.na(MPC0[,"MPC_score"]),]
    #dataMPC=dataMPC[dataMPC[,2]<100,]
    
    })
  
  
  ### Cleavage Site Predictions
  dataMitoF = reactive({
    
    placeholder<-MitoFates_proc[MitoFates_proc[,"Gene"]==input$Genes,]
    
    if (input$Nterm == "Yes"){
      placeholder$color <- "black"
      placeholder$alpha <- 1
    }
    else {
      
      placeholder$color <- "white"
      placeholder$alpha <- 0

    }
    dataMitoF <- placeholder
    
  })
  
  ### N-terminomics
  dataN_term = reactive({


    placeholder<-N_term_filt[N_term_filt[,"Gene.symbol"]==input$Genes,]


    if (input$Nterm == "Yes"){
      #placeholder$color <- "blue"
      placeholder$alpha <- 1
    }
    else {

      placeholder$alpha <- 0

    }
    
    if (placeholder[,"Cleavage.site"]==0){
      placeholder$alpha <- 0
    }
    
  dataN_term <- placeholder

  
  })

  
  
  dataTP = reactive({
    dataTP<-TargetP[TargetP[,"Gene"]==input$Genes,]
    
    placeholder<-TargetP[TargetP[,"Gene"]==input$Genes,]
    
    if (input$Nterm == "Yes"){

      placeholder$alpha <- 1
    }
    else {
      
      placeholder$alpha <- 0
      
    }
    dataTP <- placeholder
    
  })
    
  
  
  ### Reactive functions to plot mutations onto NGL Viewer Structure ###
  
  Pathogenic = reactive({
    if (input$database == "ClinVar") {
      uncert <- XYC
    }
    else{
      uncert <- XYG
    }
    placeholder1<-uncert[uncert[,"Genes"]==input$Genes,]
    placeholder2=placeholder1[!is.na(placeholder1[,3]),]
    x <- ""
    for (n in 1:nrow(placeholder2)){
      aa <- placeholder2[n, "amini.acid"]
      clinsig <- placeholder2[n,"label"]

      
      if(toString(clinsig)=="Pathogenic"){
        
        x = paste(x, " or ", aa, sep = "")
      }
      
    }
  if(x==""){
    x = "0"
  }  
  Pathogenic=x
    
  })
  
  
  Benign = reactive({
    if (input$database == "ClinVar") {
      uncert <- XYC
    }
    else{
      uncert <- XYG
    }
    placeholder1<-uncert[uncert[,"Genes"]==input$Genes,]
    placeholder2=placeholder1[!is.na(placeholder1[,3]),]
    
    x <- ""
    for (n in 1:nrow(placeholder2)){
      aa <- placeholder2[n, "amini.acid"]
      clinsig <- placeholder2[n,"label"]
      
      
      if(toString(clinsig)=="Benign"){
        
        x = paste(x, " or ", aa, sep = "")
      }
      
    }
    if(x==""){
      x = "0"
    }  
    Benign=x
    
  })
  
  
  
  
  Uncertain = reactive({
    if (input$database == "ClinVar") {
      uncert <- XYC
    }
    else{
      uncert <- XYG
    }
    placeholder1<-uncert[uncert[,"Genes"]==input$Genes,]
    placeholder2=placeholder1[!is.na(placeholder1[,3]),]
    
    x <- ""
    for (n in 1:nrow(placeholder2)){
      aa <- placeholder2[n, "amini.acid"]
      clinsig <- placeholder2[n,"label"]
      
      
      if(toString(clinsig)=="Uncertain"){
        
        x = paste(x, " or ", aa, sep = "")
      }
      
    }
    if(x==""){
      x = "0"
    }  
    Uncertain=x
    
  })
  
  
  ### Trying to map interactions onto NGL viewer ###
  
  totalaa = reactive({

  placeholder <- XYcurves[XYcurves[,"Genes"]==input$Genes,]
  num_aa = ncol(placeholder)
    x <- ""
    for (aa in 1:num_aa){
      
      x = paste(x, " or ", toString(aa), sep = "")
      
    }
  
    totalaa=x

  })
  
  
  ###Displaying AlphaFold or iMTS scores###
  
  structure_file = reactive({

    if (input$colouring == "AlphaFold"){
      structure_file<-"PDB_original/"
  }

    else {
      structure_file<-"PDB_extracted/"

    }
  })
  
  structure_col = reactive({

    if (input$colouring == "AlphaFold"){
      structure_col<-"RdYlBu"
    }
    
    else {
      structure_col<-"Blues"
      
    }
  })
  
  structure_dom = reactive({

    if (input$colouring == "AlphaFold"){
      structure_dom<-""
    }
    
    else {
      structure_dom<-"0,6"
      
    }
  })
  
  
  structure_legend = reactive({
    
    if (input$colouring == "AlphaFold"){
      structure_legend<-"AFconfidence.png"
    }
    
    else {
      structure_legend<-"iMTS_Final.png"
      
    }
  })
  

  
  
  
  ###Mitofates prediction table###
  
datamutations = reactive({
  if (input$database == "ClinVar"){
    datamutations <- ClinVarMutations[ClinVarMutations[,"genename"]==input$Genes,]
  }
  
  else{
    datamutations <- GnomADVariants[GnomADVariants[,"genename"]==input$Genes,]
  }
  
})
  

    output$MitoFates = DT::renderDataTable({
      MitoFates_proc[MitoFates_proc[,"Gene"]==input$Genes,]
    })
    
    output$mutations = DT::renderDataTable({
      # ClinVarMutations[ClinVarMutations[,"genename"]==input$Genes,]
      datamutations()
    })
    
    
    output$N_terminomics = DT::renderDataTable({
      # ClinVarMutations[ClinVarMutations[,"genename"]==input$Genes,]
      N_term_filt[N_term_filt[,"Gene.symbol"]==input$Genes,]
    })

    
    output$TargetP = DT::renderDataTable({
      # ClinVarMutations[ClinVarMutations[,"genename"]==input$Genes,]
      TargetP[TargetP[,"Gene"]==input$Genes,]
    })
    
    
    output$DeepMito = DT::renderDataTable({
      # ClinVarMutations[ClinVarMutations[,"genename"]==input$Genes,]
      DeepMito_proc[DeepMito_proc[,"Genes"]==input$Genes,]
    })

    
  ###Download button###
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(datamutations(), con)
      }
    )
    

################ Plot 2

    
    output$plotxy2 = renderPlotly({
      pp<-ggplot(dataXY(), aes(y=dataXY()[,2], x=dataXY()[,3]))+
        geom_line() + 

        #geom_point(data=dataXYCG(),aes(x=dataXYCG()[,2], y=dataXYCG()[,3], color=dataXYCG()[,"label"], text=sprintf("Protein.Change: %s<br>NT.Change: %s <br> Annotation: %s", dataXYCG()[,"HGVSp_VEP"], dataXYCG()[,"HGVSc_VEP"], dataXYCG()[,"clinvar_trait"])),show.legend = FALSE)+
        geom_point(data=dataXYCG(),aes(x=dataXYCG()[,"amini.acid"], y=dataXYCG()[,"value"], color=dataXYCG()[,"label"], text=sprintf("Protein.Change: %s<br>NT.Change: %s", dataXYCG()[,"HGVSp_VEP"], dataXYCG()[,"HGVSc_VEP"])),show.legend = FALSE)+
        scale_color_manual(values = c("Benign" = "green4", "Pathogenic" = "red", "Uncertain" = "black"))+
        
        xlab("amino acid")+
        coord_cartesian(xlim = c(1,as.integer(datalen())), ylim = c(-2,6))+
        
        
        ylab("iMTSscore") +
        
        geom_vline(xintercept=dataMitoF()[,"MPP"], colour= toString(dataMitoF()[,"color"]), linetype="dashed", alpha = dataMitoF()[,"alpha"])+
        annotate("text", x = dataMitoF()[,"MPP"], y = min(dataXYCG()[,3]), label = "MitoFates" , fontface = 'italic', alpha = dataMitoF()[,"alpha"])+
        
        geom_vline(xintercept=strtoi(dataN_term()[,"Cleavage.site"]), colour= "blue", linetype="dashed", alpha = dataN_term()[,"alpha"])+
        annotate("text", x = strtoi(dataN_term()[,"Cleavage.site"]), y = min(dataXYCG()[,3]) + 0.5, label = "Other" , fontface = 'italic', alpha = dataN_term()[,"alpha"])+
        
        geom_vline(xintercept=dataTP()[,"CS"], colour="green", linetype="dashed", alpha = dataTP()[,"alpha"])+
        annotate("text", x = dataTP()[,"CS"], y = min(dataXYCG()[,3]), label = "TargetP" , fontface = 'italic', alpha = dataTP()[,"alpha"])
        
        #geom_col(data=dataMPC(), aes(x=dataMPC()[,"aapos"],y=dataMPC()[,"MPC_score"]), position = "dodge", alpha = 0.02)
        #geom_col(data=dataXYCG(), aes(x=dataXYCG()[,"amini.acid"],y=dataXYCG()[,"MPC_score"]), position = "dodge", alpha = 0.02)
      
        
      ggplotly(pp,tooltip="text")
        
      
    })
    
    
    ####Unsure if this is necessary
    
    output$CVM = renderTable({
      message('clicked')
      d <- event_data("plotly_click")
      if (!is.null(d)) ClinVarMutations[ClinVarMutations[,"genename"]==input$Genes,]
    })
     output$GADV = renderTable({
      message('clicked')
      d <- event_data("plotly_click")
      if (!is.null(d)) GnomADVariants[GnomADVariants[,"genename"]==input$Genes,]
    })  


     
     ####### NGLViewer
     
       output$structure <- renderNGLVieweR({
         NGLVieweR(paste("Q:/Jerry/MTS_new/MTS/PDBs/",toString(structure_file()),toString(input$Genes),".pdb", sep = "")) %>%
         # NGLVieweR("R:/Jerry/MTS -20210823T190740Z-001/MTS/dataset/PDB/PINK1_Bfactor.pdb") %>%
           addRepresentation(input$type,
                             param = list(
                               name = "cartoon", colorScheme = "bfactor", colorScale = toString(structure_col()), colorDomain = structure_dom())
           ) %>%
           
           ######ADDING CLINVAR AND GNOMAD########
           
           addRepresentation("ball+stick",

                            param = list(

                              name = "clinvar",
                              color = "red",

                              probeRadius = 0.01,
                              labelType = "res",
                              sele = Pathogenic()
                            )

                             )%>%


           addRepresentation("ball+stick",

                             param = list(

                               name = "benign",
                               color = "green",

                               probeRadius = 0.01,
                               labelType = "res",
                               sele = Benign()
                             )

           )%>%


           addRepresentation("ball+stick",

                             param = list(

                               name = "Uncertain",
                               color = "black",

                               probeRadius = 0.01,
                               labelType = "res",
                               sele = Uncertain()
                             )

           )%>%
           
           
           
           
           
           

           stageParameters(backgroundColor = "white") %>%
           setQuality("high") %>%
           setFocus(0) #%>%
         #setSpin(TRUE)
         
         
         
       })
       
       
           
         #Save click selections
         sele <- reactiveValues()

         observe({
           sele$aa <- str_extract(input$structure_selection, "(?<=[\\[])(.*?)(?=\\])")
           sele$aa_bond <- str_extract(input$structure_selection, "(?<=[\\]])(.*?)(?=[:space:])")
           sele$resi <- str_extract(input$structure_selection, "(?<=[]])(.*?)(?=[:])")
           sele$fileName <- str_extract(input$structure_selection, "(?<=[(])(.*?)(?=[.])")
           sele$resiChain <- str_extract(input$structure_selection, "(?<=[]])(.*?)(?=[.])")
         })

         output$selection = renderPrint({

           #Full selection
           print(input$structure_selection)
           #Amino Acid
           print(sele$aa)
           #Bond
           print(sele$aa_bond)
           #Residue number + ChainNAme
           print(sele$resiChain)
           #Residue number
           print(sele$resi)
           #PDB name
           print(sele$fileName)
           #SelAround
           print(input$structure_selAround)

         })
         
        

         observeEvent(sele$resi, {

           #Remove any selections
           NGLVieweR_proxy("structure") %>% removeSelection("label")
           NGLVieweR_proxy("structure") %>% removeSelection("contact")

           #Add label and contacts
           NGLVieweR_proxy("structure") %>%

             addSelection("label",
                          param = list(
                            name = "label",
                            sele = sele$aa_bond,
                            labelType = "format",
                            labelFormat = "[%(resname)s]%(resno)s", # or enter custom text
                            labelGrouping = "residue", # or "atom" (eg. sele = "20:A.CB")
                            color = "black",
                            xOffset = 1,
                            fixedSize = TRUE,
                            radiusType = 1,
                            radiusSize = 1.5
                          )

             ) %>%


             addSelection("contact",
                          param = list(
                            name = "contact",
                            sele = "*", #Select all residues
                            filterSele =
                              list(sele$resi, # Show bonds between selected residue
                                   "*"),      # and all other residues
                            labelVisible = TRUE,
                            labelFixedSize = FALSE,
                            labelUnit = "angstrom", # "", "angstrom", "nm"
                            labelSize = 2
                            # hydrogenBond=TRUE,
                            # weakHydrogenBond=FALSE,
                            # waterHydrogenBond=FALSE,
                            # backboneHydrogenBond=TRUE,
                            # hydrophobic=FALSE,
                            # halogenBond=TRUE,
                            # ionicInteraction=TRUE,
                            # metalCoordination=TRUE,
                            # cationPi=TRUE,
                            # piStacking=TRUE,
                            # maxHydrophobicDist= 4.0,
                            # maxHbondDist= 3.5,
                            # maxHbondSulfurDist= 4.1,
                            # maxHbondAccAngle= 45,
                            # maxHbondDonAngle= 45,
                            # maxHbondAccPlaneAngle= 90,
                            # maxHbondDonPlaneAngle= 30,
                            # maxPiStackingDist= 5.5,
                            # maxPiStackingOffset= 2.0,
                            # maxPiStackingAngle= 30,
                            # maxCationPiDist= 6.0,
                            # maxCationPiOffset= 2.0,
                            # maxIonicDist= 5.0,
                            # maxHalogenBondDist= 3.5,
                            # maxHalogenBondAngle= 30,
                            # maxMetalDist= 3.0,
                            # refineSaltBridges= TRUE,
                            # masterModelIndex= -1,
                            # lineOfSightDistFactor= 1)
                          )
         )

         })
         
        
         
         ###Communication between plotly and ngl###
         
         clk = reactive({
           
           clk <- event_data("plotly_click")
           
         })
         
         # observe({
         #   
         #   clk <- event_data("plotly_click")
         #   #(clk$x)
         #   
         # })
         
         # output$click <-renderPrint({
         #   clk <- event_data("plotly_click")
         #   
         # })
  
         observeEvent(clk(), {
                    
           
           #Remove any selections
           NGLVieweR_proxy("structure") %>% removeSelection("label")
           NGLVieweR_proxy("structure") %>% removeSelection("contact")
           
           #Add label and contacts
           NGLVieweR_proxy("structure") %>%
             
             addSelection("label",
                          param = list(
                            name = "label",
                            sele = toString(clk()[,'x']),
                            labelType = "format",
                            labelFormat = "[%(resname)s]%(resno)s", # or enter custom text
                            labelGrouping = "residue", # or "atom" (eg. sele = "20:A.CB")
                            color = "black",
                            xOffset = 1,
                            fixedSize = TRUE,
                            radiusType = 1,
                            radiusSize = 1.5
                          )
                          
             ) %>%
             
             
             addSelection("contact",
                          param = list(
                            name = "contact",
                            sele = "*", #Select all residues
                            filterSele =
                              list(toString(clk()[,'x']), # Show bonds between selected residue
                                   "*"),      # and all other residues
                            labelVisible = TRUE,
                            labelFixedSize = FALSE,
                            labelUnit = "angstrom", # "", "angstrom", "nm"
                            labelSize = 2
                            # hydrogenBond=TRUE,
                            # weakHydrogenBond=FALSE,
                            # waterHydrogenBond=FALSE,
                            # backboneHydrogenBond=TRUE,
                            # hydrophobic=FALSE,
                            # halogenBond=TRUE,
                            # ionicInteraction=TRUE,
                            # metalCoordination=TRUE,
                            # cationPi=TRUE,
                            # piStacking=TRUE,
                            # maxHydrophobicDist= 4.0,
                            # maxHbondDist= 3.5,
                            # maxHbondSulfurDist= 4.1,
                            # maxHbondAccAngle= 45,
                            # maxHbondDonAngle= 45,
                            # maxHbondAccPlaneAngle= 90,
                            # maxHbondDonPlaneAngle= 30,
                            # maxPiStackingDist= 5.5,
                            # maxPiStackingOffset= 2.0,
                            # maxPiStackingAngle= 30,
                            # maxCationPiDist= 6.0,
                            # maxCationPiOffset= 2.0,
                            # maxIonicDist= 5.0,
                            # maxHalogenBondDist= 3.5,
                            # maxHalogenBondAngle= 30,
                            # maxMetalDist= 3.0,
                            # refineSaltBridges= TRUE,
                            # masterModelIndex= -1,
                            # lineOfSightDistFactor= 1)
                          )
             )%>%
           
           updateZoomMove(
             center = toString(clk()[,'x']), 
             zoom = toString(clk()[,'x']),
             duration = 0
           )

           
         })

})

#####    

