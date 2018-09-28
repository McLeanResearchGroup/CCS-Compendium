library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(pdftools)

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
adducts <- as.character(levels(all$Adduct))

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

#fitTable
ft <- read.csv("data/regeq.csv", header = TRUE)

eq <- c('<img src = "http://quicklatex.com/cache3/22/ql_81062506f2cd83637777e4c562ec6722_l3.png"></img>', #Azoles
        '<img src = "http://quicklatex.com/cache3/37/ql_fcb4318d5a05c209d237d5a7124ff237_l3.png"></img>', #Benzene and substituted derivatives
        '<img src = "http://quicklatex.com/cache3/b3/ql_c0b831211948b883465cb5656af8fbb3_l3.png"></img>', #Carboxylic acids and derivatives
        '<img src = "http://quicklatex.com/cache3/6b/ql_5eb3ac234c6fa316159dc24e217e1d6b_l3.png"></img>', #Diazines
        '<img src = "http://quicklatex.com/cache3/19/ql_260f8b5f1cea0f00a875dcdb7e0f9719_l3.png"></img>', #Fatty acyls
        '<img src = "http://quicklatex.com/cache3/37/ql_42b3d40039f262212131cc5af9ec4637_l3.png"></img>', #Glycerophospholipids
        '<img src = "http://quicklatex.com/cache3/c6/ql_f27501280413c0ec44ff7075885731c6_l3.png"></img>', #Homogeneous transition metal compounds
        '<img src = "http://quicklatex.com/cache3/83/ql_81c75300afd61a0c95a71408fe184983_l3.png"></img>', #Naphthofurans
        '<img src = "http://quicklatex.com/cache3/81/ql_ca1fd309c6ee6f37b111ddcf2fa2ad81_l3.png"></img>', #Organofluorides
        '<img src = "http://quicklatex.com/cache3/9c/ql_7d3fdd43c6a05167ebce991e8181ea9c_l3.png"></img>', #Organooxygen compounds
        '<img src = "http://quicklatex.com/cache3/51/ql_26698cc691418e860e8526eb4e70b851_l3.png"></img>', #Peptidomimetics
        '<img src = "http://quicklatex.com/cache3/27/ql_ca0a96465c0cd1c0051c3c809bce1627_l3.png"></img>', #Proteins
        '<img src = "http://quicklatex.com/cache3/c1/ql_56fb901e426b135860b22685efe4bec1_l3.png"></img>', #Pteridines and derivatives
        '<img src = "http://quicklatex.com/cache3/4b/ql_9930ab424cd16b64d59e9bd7a4a57e4b_l3.png"></img>', #Pyridines and derivatives
        '<img src = "http://quicklatex.com/cache3/f2/ql_f1103e288c90ec6e9cc1494380b22af2_l3.png"></img>', #Tryptic peptides
        '<img src = "http://quicklatex.com/cache3/ff/ql_878fcb94810be61b8493842684af5aff_l3.png"></img>', #Imidazopyrimidines
        '<img src = "http://quicklatex.com/cache3/98/ql_249b6ce3c8df0bde309168b98b16be98_l3.png"></img>', #Isoflavonoids
        '<img src = "http://quicklatex.com/cache3/13/ql_1053025ca38b3b63e82d666b28b8be13_l3.png"></img>', #Macrolides and analogues
        '<img src = "http://quicklatex.com/cache3/b3/ql_d4e098c88ac33915be3bd549758587b3_l3.png"></img>', #Organonitrogen compounds
        '<img src = "http://quicklatex.com/cache3/6b/ql_ddfc8e2336e1116f60853ed04c64d46b_l3.png"></img>', #Prenol lipids
        '<img src = "http://quicklatex.com/cache3/30/ql_3a53ee6a48add984274e43ea01940230_l3.png"></img>', #Purine nucleotides
        '<img src = "http://quicklatex.com/cache3/f1/ql_06b741df5fe59b89b3afa8c9d74beff1_l3.png"></img>', #Pyrenes
        '<img src = "http://quicklatex.com/cache3/0b/ql_6146fa5cfbb480da4acdea71a3c1330b_l3.png"></img>', #Pyrimidine nucleotides
        '<img src = "http://quicklatex.com/cache3/56/ql_863780f56e78d2947fa07822cf4f2456_l3.png"></img>', #Quinolines and derivatives
        '<img src = "http://quicklatex.com/cache3/fd/ql_98659bfd3be0ef1c62ebcfc0274ca3fd_l3.png"></img>', #Organooxygen compounds/ Alcohols and polyols
        '<img src = "http://quicklatex.com/cache3/db/ql_ad046820f9c32bd21b3239b720348fdb_l3.png"></img>', #Carboxylic acids and derivatives/ Amino acids, peptides, and analogues
        '<img src = "http://quicklatex.com/cache3/d6/ql_dbd57ee06fae7b76143c78f9b30277d6_l3.png"></img>', #Flavonoids/ Flavonoid glycosides
        '<img src = "http://quicklatex.com/cache3/d0/ql_2770c5b8ba43afb82674d5aaec083dd0_l3.png"></img>', #Glycerophospholipids/ Glycerophosphates
        '<img src = "http://quicklatex.com/cache3/6d/ql_11f14ff77f76b58752b5afa3752ca06d_l3.png"></img>', #Glycerophospholipids/ Glycerophosphocholines
        '<img src = "http://quicklatex.com/cache3/b5/ql_2a312fdcd53027ecf62a9ecd7c352db5_l3.png"></img>', #Glycerophospholipids/ Glycerophosphoethanolamines
        '<img src = "http://quicklatex.com/cache3/81/ql_ca1fd309c6ee6f37b111ddcf2fa2ad81_l3.png"></img>', #Organofluorides/ Phosphazene and phosphazene derivatives
        '<img src = "http://quicklatex.com/cache3/95/ql_e4455f650618a88c51ee56f152497a95_l3.png"></img>', #Pyrimidine nucleotides/ Pyrimidine ribonucleotides
        '<img src = "http://quicklatex.com/cache3/6b/ql_5eb3ac234c6fa316159dc24e217e1d6b_l3.png"></img>', #Diazines/ Pyrimidines and pyrimidine derivatives
        '<img src = "http://quicklatex.com/cache3/a3/ql_15062458967a66323e405c8b649e42a3_l3.png"></img>', #Organonitrogen compounds/ Quaternary ammonium salts
        '<img src = "http://quicklatex.com/cache3/b0/ql_4f4745f1a579c17c479b7d1d3c6eefb0_l3.png"></img>', #5'-deoxyribonucleosides/ 5'-deoxy-5'-thionucleosides
        '<img src = "http://quicklatex.com/cache3/82/ql_7332d9b3e8babe45607cb2f025497b82_l3.png"></img>', #Organonitrogen compounds/ Amines
        '<img src = "http://quicklatex.com/cache3/31/ql_aafcb441036d66bf5643de5f944f9c31_l3.png"></img>', #Benzene and substituted derivatives/ Benzenesulfonamides
        '<img src = "http://quicklatex.com/cache3/63/ql_236a3c66142a2a90e0786fdb3e6ad363_l3.png"></img>', #Steroids and steroid derivatives/ Bile acids, alcohols and derivatives
        '<img src = "http://quicklatex.com/cache3/1b/ql_18d9906ca21282f4932147ff43d2751b_l3.png"></img>', #Benzene and substituted derivatives/ Biphenyls and derivatives
        '<img src = "http://quicklatex.com/cache3/81/ql_df932fd3ddf6201ac42ee44698125681_l3.png"></img>', #Organooxygen compounds/ Carbohydrates and carbohydrate conjugates
        '<img src = "http://quicklatex.com/cache3/f4/ql_56d54dda8d73218d71c92766e5a292f4_l3.png"></img>', #Organooxygen compounds/ Carbonyl compounds
        '<img src = "http://quicklatex.com/cache3/e4/ql_030af2eaf774679f81201f0322b4e9e4_l3.png"></img>', #Glycerophospholipids/ Glycerophosphoserines
        '<img src = "http://quicklatex.com/cache3/79/ql_064747b4d9ff328aaa833bf28f4d3679_l3.png"></img>', #Indoles and derivatives/ Indoles
        '<img src = "http://quicklatex.com/cache3/47/ql_9d8629bd27a1c8f30aecbd4506f61847_l3.png"></img>', #Indoles and derivatives/ Indolyl carboxylic acids and derivatives
        '<img src = "http://quicklatex.com/cache3/d6/ql_4f1d3c384d52b6ccd8b8b5c62f2cf3d6_l3.png"></img>', #Sphingolipids/ Phosphosphingolipids
        '<img src = "http://quicklatex.com/cache3/84/ql_577fb705b93d2ba71a93f452380d2e84_l3.png"></img>', #Purine nucleotides/ Purine deoxyribonucleotides
        '<img src = "http://quicklatex.com/cache3/2f/ql_25ab82ca2bc7df336a4180e1ecdfdf2f_l3.png"></img>', #Purine nucleotides/ Purine nucleotide sugars
        '<img src = "http://quicklatex.com/cache3/ff/ql_878fcb94810be61b8493842684af5aff_l3.png"></img>' #Imidazopyrimidines/ Purines and purine derivatives
        )

####################################################################################################################

#ui
ui <- fluidPage(theme = shinytheme("simplex"),
                titlePanel("United CCS Compendium"),
                
                mainPanel(
                  navlistPanel(widths = c(2, 10),
                    tabPanel("Home",
                             helpText(h3(strong(HTML("Welcome to the Unified CCS Compendium.</br><br>")))),
                             div(imageOutput("homefig"), align = "center"),
                             helpText(h4(HTML("<p align = 'justify'><span style = 'margin-left: 3em'>The Compendium is a repository of > 3800 experimentally acquired CCS values obtained from traceable molecular standards and measured with drift tube-mass spectrometers.
                                                  Represented in the Compendium are 14 structurally-based chemical super classes, consisting of a total of 80 classes and 157 subclasses.
                                                  Using this large data set, regression fitting and predictive statistics have been performed to describe mass-CCS correlations specific to each chemical ontology.
                                                  These structural trends provide a rapid and effective filtering method in the traditional untargeted workflow for identification of unknown biochemical species.
                                                  The predictive abilities of this Compendium will improve in specificity and expand across more chemical classes as data from the IM-MS community is contributed.
                                                  Inclusion criteria and instructions for data submission to the Compendium can be found in the 'Data Submission Guidelines and Tools' page on the menu to the left.</span></p>"))),
                             helpText(div(h4(HTML("<br>Use the menu on the left to navigate this interactive tool."), style = "color:red")))
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
                                      selectInput('adduct', 'Adduct:', c(All = '.', adducts)),
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
                             helpText(div(h5(strong("To download a copy of the full Compendium data set, click the link: "))), style = "color:black"), downloadLink('alldata', "Download the Data Set"),
                             hr(),
                             DT::dataTableOutput("table"), width = 8),
                    
                    tabPanel("Class Specific Regression Trends",
                             helpText(div(h4(strong("To visualize the trends, choose up to 10 classes blow and click Show Plot."))), style = "color:black"),
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
                             column(width = 5,
                                    helpText(div(h4(HTML("<u>Single Field Tools:</u>"))), style = "color:black"),
                                    downloadLink('singleguide', "Single Field Guidelines"), br(),
                                    downloadLink('singledata', "SI_SingleField_DataFormat.xlsx")
                                    ),
                             column(width = 5,
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