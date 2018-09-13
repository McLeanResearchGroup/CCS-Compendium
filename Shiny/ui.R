library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(shinyjs)

#setwd("/Users/JAPicache/Box Sync/R_Scripts&Data/20171218JAP_iceberg/Shiny/CCScompendium")

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

eq <- c('<img src = "http://quicklatex.com/cache3/06/ql_ae458daad4ef14cd4ff0c17127959f06_l3.png"></img>', #Azoles
        '<img src = "http://quicklatex.com/cache3/8c/ql_bb56749792d869b421bec1a97848058c_l3.png"></img>', #Benzene and substituted derivatives
        '<img src = "http://quicklatex.com/cache3/9e/ql_54baf21751eb7e8554d35a9b5305429e_l3.png"></img>', #Carboxylic acids and derivatives
        '<img src = "http://quicklatex.com/cache3/59/ql_809ac155d88fc7aff01993793a0b0f59_l3.png"></img>', #Fatty acyls
        '<img src = "http://quicklatex.com/cache3/ea/ql_cb481962e7f7eca49cf5521c7c67ecea_l3.png"></img>', #Glycerophospholipids
        '<img src = "http://quicklatex.com/cache3/52/ql_54836268a28b1f714d40bb813e074852_l3.png"></img>', #Homogeneous transition metal compounds
        '<img src = "http://quicklatex.com/cache3/79/ql_770f2891843ac5028f7bbd9efd825879_l3.png"></img>', #Naphthofurans
        '<img src = "http://quicklatex.com/cache3/8b/ql_65dcbcc3d604befd34e9d3fc6a56cd8b_l3.png"></img>', #Organofluorides
        '<img src = "http://quicklatex.com/cache3/e0/ql_8fea573c2c466fb3233ba45f6fe66ce0_l3.png"></img>', #Organooxygen compounds
        '<img src = "http://quicklatex.com/cache3/d6/ql_035bb8d3561459a73f93842076485bd6_l3.png"></img>', #Peptidomimetics
        '<img src = "http://quicklatex.com/cache3/5d/ql_58e8c207afee2de504b11e01e874805d_l3.png"></img>', #Proteins
        '<img src = "http://quicklatex.com/cache3/bf/ql_8b760c02d8760eca9f78fdb9a9d9cdbf_l3.png"></img>', #Pteridines and derivatives
        '<img src = "http://quicklatex.com/cache3/9b/ql_0d77ab17ace3717d8599123e581cd39b_l3.png"></img>', #Pyridines and derivatives
        '<img src = "http://quicklatex.com/cache3/da/ql_8b67ed605b60392004dfe38311ac39da_l3.png"></img>', #Tryptic peptides
        '<img src = "http://quicklatex.com/cache3/cf/ql_c77bbf302deaf5c9c06e6637cbf20acf_l3.png"></img>', #Diazines
        '<img src = "http://quicklatex.com/cache3/ff/ql_878fcb94810be61b8493842684af5aff_l3.png"></img>', #Imidazopyrimidines
        '<img src = "http://quicklatex.com/cache3/8f/ql_b958388f383171e85db26d142d56208f_l3.png"></img>', #Isoflavonoids
        '<img src = "http://quicklatex.com/cache3/8c/ql_675e403a48341cb23dda6205220b0d8c_l3.png"></img>', #Macrolides and analogues
        '<img src = "http://quicklatex.com/cache3/ca/ql_1d455767112c8fe92c7a4f280a266dca_l3.png"></img>', #Organonitrogen compounds
        '<img src = "http://quicklatex.com/cache3/6f/ql_c684abd7908643c4c8e9ed69bd89306f_l3.png"></img>', #Prenol lipids
        '<img src = "http://quicklatex.com/cache3/47/ql_c6ea8219b7655d54dccbb604e1b58647_l3.png"></img>', #Purine nucleotides
        '<img src = "http://quicklatex.com/cache3/24/ql_774f1529d350f886cbdd4ffb2d76c824_l3.png"></img>', #Pyrenes
        '<img src = "http://quicklatex.com/cache3/c1/ql_08416591b7a411cbf6cc5f41ae647dc1_l3.png"></img>', #Pyrimidine nucleotides
        '<img src = "http://quicklatex.com/cache3/c2/ql_722caad11b38ade41f95b695396935c2_l3.png"></img>', #Quinolines and derivatives
        '<img src = "http://quicklatex.com/cache3/e8/ql_bd8f85d499a763c0324e5fce91aefde8_l3.png"></img>', #Organooxygen compounds/ Alcohols and polyols
        '<img src = "http://quicklatex.com/cache3/2d/ql_1ba41308a8d95fb2766e7978beb56f2d_l3.png"></img>', #Carboxylic acids and derivatives/ Amino acids, peptides, and analogues
        '<img src = "http://quicklatex.com/cache3/5c/ql_cb5a131363a36001d8c3b3af85f7a95c_l3.png"></img>', #Flavonoids/ Flavonoid glycosides
        '<img src = "http://quicklatex.com/cache3/ec/ql_9ec2bce4ee9b97ea76336d3fa6b494ec_l3.png"></img>', #Glycerophospholipids/ Glycerophosphates
        '<img src = "http://quicklatex.com/cache3/81/ql_ca1fd309c6ee6f37b111ddcf2fa2ad81_l3.png"></img>', #Organofluorides/ Phosphazene and phosphazene derivatives
        '<img src = "http://quicklatex.com/cache3/7b/ql_7cc47fe226ff0d1331569d7f18be267b_l3.png"></img>', #Pyrimidine nucleotides/ Pyrimidine ribonucleotides
        '<img src = "http://quicklatex.com/cache3/b0/ql_4f4745f1a579c17c479b7d1d3c6eefb0_l3.png"></img>', #5'-deoxyribonucleosides/ 5'-deoxy-5'-thionucleosides
        '<img src = "http://quicklatex.com/cache3/41/ql_80c1ff4b7f9dcd186c4ac9fb0596c341_l3.png"></img>', #Organonitrogen compounds/ Amines
        '<img src = "http://quicklatex.com/cache3/8a/ql_06e9ed9d107014fff45367abecf4bb8a_l3.png"></img>', #Benzene and substituted derivatives/ Benzenesulfonamides
        '<img src = "http://quicklatex.com/cache3/99/ql_d1106d7e964d5f4b8ef58c998485a699_l3.png"></img>', #Steroids and steroid derivatives/ Bile acids, alcohols and derivatives
        '<img src = "http://quicklatex.com/cache3/4d/ql_f435b359f38a68069b832895fcb3854d_l3.png"></img>', #Benzene and substituted derivatives/ Biphenyls and derivatives
        '<img src = "http://quicklatex.com/cache3/e3/ql_454c05dc7d83caa05464f966aaf719e3_l3.png"></img>', #Organooxygen compounds/ Carbohydrates and carbohydrate conjugates
        '<img src = "http://quicklatex.com/cache3/0f/ql_c4616f685b81480f06aa6b10e530e60f_l3.png"></img>', #Organooxygen compounds/ Carbonyl compounds
        '<img src = "http://quicklatex.com/cache3/a7/ql_51cedf737227dec0f92a821e83d668a7_l3.png"></img>', #Glycerophospholipids/ Glycerophosphoserines
        '<img src = "http://quicklatex.com/cache3/27/ql_2a05b5c07b516c8542328d7554b8f727_l3.png"></img>', #Indoles and derivatives/ Indoles
        '<img src = "http://quicklatex.com/cache3/a4/ql_9446eedfb1996a05c5af342670d65fa4_l3.png"></img>', #Indoles and derivatives/ Indolyl carboxylic acids and derivatives
        '<img src = "http://quicklatex.com/cache3/40/ql_e0eab67bde5cf447bb9f0ff667e78440_l3.png"></img>', #Sphingolipids/ Phosphosphingolipids
        '<img src = "http://quicklatex.com/cache3/40/ql_e0eab67bde5cf447bb9f0ff667e78440_l3.png"></img>', #Purine nucleotides/ Purine deoxyribonucleotides
        '<img src = "http://quicklatex.com/cache3/ff/ql_878fcb94810be61b8493842684af5aff_l3.png"></img>', #Imidazopyrimidines/ Purines and purine derivatives
        '<img src = "http://quicklatex.com/cache3/cf/ql_c77bbf302deaf5c9c06e6637cbf20acf_l3.png"></img>', #Diazines/ Pyrimidines and pyrimidine derivatives
        '<img src = "http://quicklatex.com/cache3/84/ql_38c48d2820bf35604d70cbc0144e9584_l3.png"></img>' #Organonitrogen compounds/ Quaternary ammonium salts
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
                                                  The predictive abilities of this Compendium will improve in specificity and expand across more chemical classes as data from the IM-MS community is contributed.</span></p>"))),
                             # Inclusion criteria and instructions for data submission to the Compendium are provided.
                             helpText(div(h4(HTML("<br>Use the menu on the left to navigate this interactive tool."), style = "color:red")))
                    ),
                    
                    tabPanel("Interactive Compendium", 
                             helpText(h4(strong("Use the options below to navigate the interactive compendium."))),
                             br(),
                             fluidRow(
                               # column(width = 3,
                               #        helpText(div(h5(HTML("<font color = 'red'><b>Legend: Charge State</b></br>
                               #                            <br><span style = 'margin-left:2.5em'> &#9711; &#177;1 </span></br>
                               #                            <br><span style = 'margin-left:2.5em'> &#9634; &#177;2 </span></br>
                               #                            <br><span style = 'margin-left:2.5em'> &#9674; &#177;3 </span></br>
                               #                            <br><span style = 'margin-left:2.5em'> &#43; +4 </span></br>
                               #                            <br><span style = 'margin-left:2.5em'> &#9651; >+4 </span></br></font>"))))),
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
                    
                    tabPanel("Compound Table", DT::dataTableOutput("table"), width = 8),
                    
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
                    )
                    
                  )
                )
        )

#runApp
#shinyApp(ui, server)