library(shiny)
library(plotly)
library(DT)

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

#ui
ui <- fluidPage(
  titlePanel(h1(strong("United CCS Compendium"))),
  sidebarPanel(width = 3,
    helpText(div(h4(strong("Use the options below to navigate the corresponding tabs to the right."))), style = "color:red"),
    helpText(div(h4(em("Compendium Navigation:"))), style = "color:black"),
    selectInput("pol", "Polarity:",
                c("Both" = ".",
                  "Positive" = "+",
                  "Negative" = "-")
    ),
    selectInput('adduct', 'Adduct:', c(All = '.', adducts)),
    selectInput('supclass', 'Super Class:', c(All = '.', supclasses)),
    selectInput('class', 'Class:', c(All = '.', classes)),
    selectInput('source', 'Source:', c(All = '.', sources)),
      
    helpText(div(h4(em("Class Trend Plots:"))), style = "color:black"),
    helpText("Choose up to 10 classes and click Show Plot to visualize their
               representative trends."),
    selectizeInput('trendclass', 'Classes:', choice, multiple = TRUE, options = list(maxItems = 10)), 
    actionButton("action", "Show Plot"), 
    helpText("Each class plot includes a fitted nonlinear regression curve, 
             as well as 99% confidence and predictive intervals (dashed lines).")
    ),
  
  mainPanel(
    h4(textOutput("Use the options below to navigate the respective tabs to the right.")),
    tabsetPanel(type = "tabs",
                tabPanel("Compendium", plotlyOutput("allPlot", width = "1500px", height = "1100px")),
                tabPanel("Compound Table", DT::dataTableOutput("table")),
                tabPanel("Class Specific Trends", plotlyOutput("trendPlot", width = "1500px", height = "1100px"))
    )
  )
)


#runApp
shinyApp(ui, server)
