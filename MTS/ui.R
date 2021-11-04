fluidPage(
  titlePanel(
    title = 
      tags$a(
        href="https://saeidamiri1.github.io/", target = "_blank",
        tags$div(style = "float:right",
                 tags$img(style = "width:150px", src="mcgill.png"),
                 tags$img(style = "width:50px", src="neuro.png")),
        tags$div(style = "float:center",
                 tags$img(style = "width:400px", src="MTSViewer.png")
        
      )
    ),
    windowTitle = "MTSviewer"
  ), tags$head(tags$style('h3 {font-family: Helvetica; font-weight: Bold;}')), 
  tags$head(tags$style('h1 {font-family: Helvetica; font-weight: Bold;}')),
  
  tabsetPanel(
    id = "mainTabsetPanel",
    tabPanel("Home",br(), br(),
             #tags$h4("MTSmutDB v1.0"),
             tags$img(style = "width:800px", src="GA.png"),
             tags$div(style="line-height:25%;", br())),
 
     tabPanel("Models",
           sidebarLayout(
             sidebarPanel(tags$head(
      tags$style(HTML("
      .selectize-input { font-size: 11px; line-height: 11px;} 
                 .selectize-dropdown { font-size: 13px; line-height: 13px; }
                 .form-group, .selectize-control {margin-bottom:-0px;max-height: 100px !important;}
                 .box-body {
          padding-bottom: 0px;
      }
    "))
    ),
    
    selectizeInput('Genes',' Choose Genes', 
                       choices = na.omit(unique(XYcurves_ClinVarMutations[,1])),
                       selected = 'PINK1'), width=1, 
      selectInput("database", "Database", c("ClinVar", "GnomAD")),
      selectInput("colouring", "Colouring", c("iMTS","AlphaFold")),
      selectInput("type", "Type", c("cartoon","ball+stick", "backbone")),
      selectInput("Nterm","Cleavage sites?", c("Yes","No"))

             ),
    
               mainPanel(div(
               width = 40,
               
                       fluidRow(h1(textOutput("genetitle")),
                         
                       column(11, title = "structure", NGLVieweROutput("structure")),
                       
                       column(1, imageOutput("legend"))
                       
                       ),
               
                       fluidRow(h3("iMLP curves"),                      
                       title = "Plot_text",plotlyOutput('plotxy2'), verbatimTextOutput("click")),hr(),
                       
                       fluidRow(box(h3("Mutation List"), h5(DT::dataTableOutput('mutations')))),
               
                       fluidRow(
                        box(h5(downloadButton('downloadData', 'Download')), h3("Cleavage Site Predictions"), DT::dataTableOutput('MitoFates'), dataTableOutput('TargetP'), solidHeader = T, status = 'warning')), 
                        br(),
                      
                       fluidRow(h3("Other Predictions"), box(h5(DT::dataTableOutput('N_terminomics')))),
               
                       fluidRow(box(h3("DeepMito"), h5(DT::dataTableOutput('DeepMito'))))
               
               
                        #tabsetPanel(tabPanel("ClinVarMutations",tags$div(style="line-height:50%;", br()), tableOutput("CVM")),
                        #tabPanel("GnomADVariants",tags$div(style="line-height:50%;", br()), tableOutput("GADV"))
               ),class = "span25",tags$div(style="line-height:100%;", hr())))
    

           ),
    
    tabPanel("Mutation list", 
             
             fluidRow(title = "All mutations", DT::dataTableOutput('all_mutations'))
             
             ),
    
    tabPanel("FAQ", 
             
             fluidRow(title = "Questions", faq::faq(data=faqdf, elementId = "faq", faqtitle = "Frequently Asked Questions")))
 
  )
        
  
)
#)

