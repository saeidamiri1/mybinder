fluidPage(
  titlePanel(
    title = tags$div(
      "MPTmmmmm",
      tags$a(
        href="https://saeidamiri1.github.io/", target = "_blank",
        tags$div(style = "float:right",
                 tags$img(style = "width:150px", src="mcgill.png"),
                 tags$img(style = "width:50px", src="neuro.png")
        )
      )
    ),
    windowTitle = "MNI"
  ), 
  tabsetPanel(
    id = "mainTabsetPanel",
    tabPanel("Home",
             tags$h4("Title....."),
             tags$p(),
             "A short description.",
             tags$p(),
             "This app was developed by ... at Neuro MNI",
             tags$div(style="line-height:25%;", br())),
  tabPanel("Models",
           sidebarLayout(
             sidebarPanel(tags$head(
      tags$style(HTML("
      .selectize-input { font-size: 11px; line-height: 11px;} 
                 .selectize-dropdown { font-size: 13px; line-height: 13px; }
                 .form-group, .selectize-control {margin-bottom:-10px;max-height: 100px !important;}
                 .box-body {
          padding-bottom: 0px;
      }
    "))
    ), selectizeInput('Genes',' Choose Genes', 
                       choices = na.omit(unique(XYcurves_ClinVarMutations[,1])),
                       selected = 'PINK1'),width = 1
             ),
               mainPanel(div(
               width = 25,
                       fluidRow(                      
                       title = "Plot_text",plotlyOutput('plotxy2')),hr(),
                       fluidRow(
                         box(h5('Text 3:  MitoFates ??? '),DT::dataTableOutput('table'), solidHeader = T, status = 'warning')
                       ), br(), h5('Text 4:  adddddd ??? '), 
                        tabsetPanel(tabPanel("ClinVarMutations",tags$div(style="line-height:50%;", br()),helpText(" Here add text related ClinVarMutations "), tableOutput("CVM")),
                        tabPanel("GnomADVariants",tags$div(style="line-height:50%;", br()),helpText(" Here add text related to GnomADVariants"), tableOutput("GADV"))
               ),class = "span25",tags$div(style="line-height:100%;", hr())))
           )
  )
)
)
