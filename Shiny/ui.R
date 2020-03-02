library(shiny)
library(shinythemes)
library(plotly)
library(DT)
        
#setwd("/Users/JAPicache/Box Sync/R_Scripts&Data/20171218JAP_iceberg/Shiny/CCScompendium_Shiny")

#plotlyInput
all <- read.csv("data/allMASTER.csv", header = TRUE)
super.col <- c("#A5142A", "#F42813", "#DF7F12", "#C9A544", "#E3CD10", 
               "#5DE310", "#1C9E30", "#10C890", "#23D1E3", "#3B9DE0", 
               "#072DC8", "#6B33EC", "#BAA2F0", "#E471EC", "#D12196", "#C8C8C8")
sym <- c("circle", "square", "diamond", "cross", "triangle-up")
sources <- as.character(levels(all$Source))
supclasses <- as.character(levels(all$Super.Class))
classes <- as.character(levels(all$Class))
adducts <- as.character(levels(all$Ion.Species))

#trendsInput
trends <- list.files(path = "data/ClassTrends_R", 
                     pattern = "*.csv", full.names = TRUE)
curves <- lapply(trends, read.csv)
trendnams <- list.files(path = "data/ClassTrends_R", 
                        pattern = "*.csv", full.names = FALSE)
trendnams <- strsplit(trendnams, '.csv')
names(curves) <- trendnams
for(i in seq_along(curves)){
  curves[[i]] <- curves[[i]][, -1]
}
choice <- as.list(names(curves))
names(choice) <- names(curves)

##colors = blue, purple, orange, green, pink, light blue, yellow, teal, red, grey
linecols <- c("#0049FF", "#A65CFF", "#FF911C", "#029900", "#FF63D7", 
              "#4CBBFF", "#FFEE20", "#68E7A7", "#FF0404", "#9E9E9E")
fillcols <- c("rgba(111, 152, 255, 0.2)", "rgba(203, 161, 254, 0.23)","rgba(255, 163, 64, 0.2)", 
              "rgba(85, 214, 83, 0.2)", "rgba(255, 159, 230, 0.2)", "rgba(156, 217, 255, 0.2)", 
              "rgba(255, 242, 89, 0.2)", "rgba(127, 247, 187, 0.2)","rgba(255, 90, 90, 0.2)",
              "rgba(203, 203, 203, 0.2)")

####################################################################################################################

#ui
ui <- fluidPage(theme = shinytheme("simplex"), withMathJax(),
                titlePanel("Unified CCS Compendium"),
                
                mainPanel(
                  navlistPanel(widths = c(2, 10),
                    tabPanel("Home",
                             helpText(h3(strong(HTML("Welcome to the Unified CCS Compendium.</br><br>")))),
                             div(imageOutput("homefig"), align = "center"),
                             helpText(h4(HTML("<p align = 'justify'><span style = 'margin-left: 3em'>The Unified Compendium is a repository of > 3800 experimentally acquired CCS values obtained from traceable molecular standards and measured with drift tube-mass spectrometers.
                                                  Represented in the Compendium are 14 structurally-based chemical super classes, consisting of a total of 80 classes and 157 subclasses.
                                                  Using this large data set, regression fitting and predictive statistics have been performed to describe mass-CCS correlations specific to each chemical ontology.
                                                  These structural trends provide a rapid and effective filtering method in the traditional untargeted workflow for identification of unknown biochemical species.
                                                  The predictive abilities of this Compendium will improve in specificity and expand across more chemical classes as data from the IM-MS community is contributed.
                                                  Inclusion criteria and instructions for data submission to the Compendium can be found in the 'Data Submission Guidelines and Tools' page on the menu to the left.
                                              <br><br>
                                              Please use the following for citation purposes: <br>
                                              J. A. Picache, B. S. Rose, A. Balinski, K. L. Leaptrot, S. D. Sherrod, J. C. May and J. A. McLean, <i>Chem. Sci.</i>, 2019, <b>10</b>, 983–993.</span></p>"))),
                             helpText(div(h4(HTML("<br>Use the menu on the left to navigate this interactive tool."), style = "color:red"))),
                             
                             helpText(div(h5(HTML("<br><br>Launched: 13 August 2018
                                                  <br>Updated: 25 April 2019"), style = "color:grey")))
                    ),
                    
                    tabPanel("Interactive Compendium", 
                             helpText(h4(strong("Use the options below to navigate the interactive compendium."))),
                             br(),
                             fluidRow(
                               column(width = 3, offset = 0,
                                      selectInput("pol", "Polarity:",
                                                  c("Both" = ".",
                                                    "Positive" = "pos",
                                                    "Negative" = "neg")),
                                      selectInput('supclass', 'Super Class:', c(All = '.', supclasses))
                                      ),
                               column(width = 3, offset = 0,
                                      selectInput('adduct', 'Ion Species:', c(All = '.', adducts)),
                                      selectInput('class', 'Class:', c(All = '.', classes))
                                      ),
                               column(width = 3, offset = 0,
                                      selectInput('source', 'Source:', c(All = '.', sources))
                                      ),
                               column(width = 3,
                                      helpText(div(h5(HTML("<font color = 'red'><b>Legend: Charge State</b></br>
                                                           <br><span style = 'margin-left: 3.7em'> &#9711; &#177;1 </span></br>
                                                           <br><span style = 'margin-left: 3.7em'> &#9634; &#177;2 </span></br>
                                                           <br><span style = 'margin-left: 3.7em'> &#9674; &#177;3 </span></br>
                                                           <br><span style = 'margin-left: 3.7em'> &#43; +4 </span></br>
                                                           <br><span style = 'margin-left: 3.7em'> &#9651; >+4 </span></br></font>")))))
                               ),
                             hr(),
                             plotlyOutput('allPlot', width = "1250px", height = "825px")
                    ),
                    
                    tabPanel("Compound Table",
                             helpText(div(h5(strong("To download a copy of the full Compendium data set, click the links below."))), style = "color:black"),
                             downloadLink('alldata', "CCS Compendium CSV"),
                             br(), uiOutput("pcdl"),
                             downloadLink('ref', "Data Source References"),
                             br(), br(),
                             helpText(div(h5("*Peak number is the number of observed mobility peaks in order of size (smallest to largest CCS/z).")), style = "color:black"),
                             helpText(div(h5("**Measurements from Reference 6 were acquired on the Agilent Reference System and are the most accurate measurements to date.")), style = "color:black"),
                             hr(),
                             DT::dataTableOutput("table"), width = 8),
                    
                    tabPanel("Class Specific Regression Trends",
                             helpText(div(h4(strong("To visualize the trends, choose up to 10 classes below and click Show Plot."))), style = "color:black"),
                             helpText(div(h5(em(HTML("Each class plot includes a fitted nonlinear regression curve (solid center line),
                                                as well as 99% confidence and predictive intervals (inner and outer dashed lines, respectively).
                                                <br><br>
                                                To remove an individual plot, click on the class name and press delete. Click Show Plot to refresh.")), style = "color:black"))),
                             selectizeInput('trendclass', 'Classes:', choice, multiple = TRUE, options = list(maxItems = 12)),
                             actionButton('action', "Show Plot"),
                             hr(),
                             plotlyOutput('trendPlot', width = "1250px", height = "825px")
                    ),
                    
                    tabPanel("Class and Subclass Regression Equations",
                             DT::dataTableOutput("fitTable")
                    ),
                   
                    tabPanel("Data Submission Guidelines and Tools",
                             HTML("<div style='height: 140px;'>"), div(imageOutput("subfig"), align = "center"), HTML("</div>"),
                             br(),
                             helpText(h4(HTML("<p align = 'justify'><span style = 'margin-left: 3em'>The unified CCS Compendium is anticipated to be a collaborative effort of the IM-MS community;
                                      and the authors would like to invite contributions to this open-access repository for quality-controlled CCS measurements.
                                      Contributions towards the unified CCS Compendium will improve informatics tools within the Compendium to aid in IM-MS based multi-omic analyte identification workflows.
                                      <br><br>For consistency, please follow the guidelines below. These guidelines are aimed at standardizing the data submission process and will expedite data quality assessment.</span></p>"))),
                             hr(),
                             column(width = 4,
                                    helpText(div(h4(HTML("<u>Peak Annotation Guidelines:</u>"))), style = "color:black"),
                                    downloadLink('peakannotate', "Peak Annotation Guidelines")
                                    ),
                             column(width = 4,
                                    helpText(div(h4(HTML("<u>Single Field Tools:</u>"))), style = "color:black"),
                                    downloadLink('singleguide', "Single Field Guidelines"), br(),
                                    downloadLink('singledata', "SI_SingleField_DataFormat.xlsx")
                                    ),
                             column(width = 4,
                                    helpText(div(h4(HTML("<u>Stepped Field Tools:</u>"))), style = "color:black"),
                                    downloadLink('steppedguide', "Stepped Field Guidelines"), br(),
                                    downloadLink('steppeddata', "SI_SteppedField_ScaleAndDataFormat.xlsx")
                                    )
                    )
                    
                  )
                )
        )

#runApp
#shinyApp(ui, server)