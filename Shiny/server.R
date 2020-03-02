library(shiny)
library(shinythemes)
library(plotly)
library(DT)

setwd("/Users/JAPicache/Box Sync/R_Scripts&Data/20171218JAP_iceberg/Shiny/CCScompendium_Shiny")

#plotlyInput
all <- read.csv("data/allMASTER.csv", header = TRUE)
allclean <- all[,c("Compound", "Neutral.Formula", "CAS", "InChi", "InChiKey", "Theoretical.mz", "Ion.Species", "Ion.Species.Agilent", "Charge", "CCS", "SD", "RSD", "CCS.z", "Peak.N", "Kingdom", "Super.Class", "Class", "Subclass", "Sources","Shape", "N.Rep")]

classtree <- read.csv("data/classlist.csv", header = TRUE)
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

#fitTable
ft <- read.csv("data/regeq.csv", header = TRUE)

####################################################################################################################

#server
server <- function(input, output, session) {

#home  
  output$homefig <- renderImage({
    
    list(src = "data/abstractfig.png",
         width = "160", height = "350", align = "center")
    
  }, deleteFile = FALSE)
    
#compendium  
  sourceType <- reactive({
    input$variable
  })
  
  output$caption <- renderText({
    sourceType()
  })
  
  output$allPlot <- renderPlotly({
    c <- plot_ly() %>% 
      layout(
        title = "<b>Interactive Compendium</b>",
        titlefont = list(family = "Arial", size = 30, color = "#000000"),
        autosize = TRUE,
        xaxis = list(title = "<b>m/z</b>",
                     titlefont = list(family = "Arial", size = 20, color = "#000000"),
                     range = c(0, 3350), zeroline = FALSE, showline = TRUE, linewidth = 10,
                     ticks = "outside", ticklen = 10, tickwidth = 10,
                     tickfont = list(family = "Arial", size = 20, color = "#000000"),
                     showgrid = FALSE),
        yaxis = list(title = "<b>CCS/z</b>",
                     titlefont = list(family = "Arial", size = 20, color = "#000000"),
                     range = c(50, 500), zeroline = FALSE, showline = TRUE, linewidth = 10,
                     ticks = "outside", ticklen = 10, tickwidth = 10,
                     tickfont = list(family = "Arial", tick0 = 0, size = 20, color = "#000000"),
                     showgrid = FALSE),
        margin = list(t = 100, l = 100, b = 100))

      data <- allclean

      if(input$adduct != "."){
        data <- filter(data, Ion.Species == input$adduct)
      }
      if(input$supclass != "."){
        data <- filter(data, Super.Class == input$supclass)
      }
      if(input$class != "."){
        data <- filter(data, Class == input$class)
      }
      if(input$source != "."){
        data <- filter(data, Sources == input$source)
      }
      if(input$pol == "pos"){
        data <- filter(data, Charge > 0)
      }
      if(input$pol == "neg"){
        data <- filter(data, Charge < 0)
      }
      
      validate(
        need(nrow(data) > 0, 'No data meet your criteria.')
      )

      c <- c %>% 
        add_trace(data, x= ~data$Theoretical.mz, x0 = 0, y= ~data$CCS.z, y0 = 0,
                  type = "scatter", mode = "markers", showlegend = T,
                  color = ~data$Super.Class, colors = ~super.col,
                  marker = list(size = 12, opacity = 0.6), symbol = ~data$Shape, symbols = ~sym,
                  text = ~paste("Name: ", data$Compound,
                                "<br> Neutral Formula: ", data$Neutral.Formula,
                                "<br>InChi Key: ", data$InChiKey,
                                "<br><i>m/z</i>:", data$Theoretical.mz,
                                "<br>Charge Species: ", data$Ion.Species,
                                "<br>CCS/z:",data$CCS.z,"+/-", data$SD,
                                "<br>Peak Number: ", data$Peak.N,
                                "<br>Class/Subclass: ", data$Class,"/",data$Subclass,
                                "<br>Source References: ", data$Sources
                                ),
                  hoverlabel = list(font = list(family = "Arial", size = 14, color = "#000000"))
                  )
      return(c)
  })
  
#table
  output$alldata <- downloadHandler(
    filename = function() {
      paste0("UnifiedCCSCompendium_FullDataSet_",Sys.Date(),".csv")
    },
    content = function(file) {
      write.csv(allclean, file, row.names = FALSE, fileEncoding = "UTF-8")
    })
  
  output$pcdl <- renderUI({
      tagList(a("CCS Compendium PCDL", href="https://github.com/McLeanResearchGroup/CCS-Compendium/tree/master/PCDL", target="_blank"))
    })
  
  output$ref <- downloadHandler(
    filename = function() {
      paste0("UnifiedCCSCompendium_References_",Sys.Date(),".pdf")
    },
    content = function(file) {
      file.copy("data/References.pdf", file)
    })
  
  output$table <- DT::renderDataTable({
    datatable(data.frame(allclean[, c("Compound", "Neutral.Formula", "CAS", "InChi", "InChiKey", "Theoretical.mz", "Ion.Species", "Charge", "CCS", "SD", "RSD", "CCS.z", "Peak.N", "N.Rep", "Kingdom", "Super.Class", "Class", "Subclass", "Sources")]),
              colnames = c("Compound", "Neutral Formula", "CAS", "InChi", "InChi Key", "Theoretical m/z", "Ion Species", "Charge State", "CCS", " CCS SD", "CCS RSD", "CCS/z", "Peak Number", "Replicates (N)", "Kingdom", "Super Class", "Class", "Subclass", "Source(s)"),
              options = list(pageLength = 10, autoWidth = TRUE, columnDefs = list(list(width = '100px'))))
  })

#trends   
  dataVals <- eventReactive(input$action, {
    as.list(input$trendclass)
  })
  
  output$trendPlot <- renderPlotly({
    #baseplot
    p <- plot_ly() %>%
      layout(xaxis = list(title = "<b>m/z</b>", ticks = "outside", tickwidth = 6,
                          range = c(0, 3350), linewidth = 6, showline = TRUE, showgrid = FALSE, 
                          zeroline = FALSE),
             yaxis = list(title = "<b>CCS/z</b>", ticks = "outside", tickwidth = 6,
                          range = c(50, 500), linewidth = 6, showline = TRUE, showgrid = FALSE, 
                          zeroline  = FALSE))
    #layer trendlines
    # datalist <- datasetInput()
    if(!is.null(dataVals())){
      for(i in c(1:length(dataVals()))){
        name <- dataVals()[[i]]
        dataset <- curves[[name]]
        linecol <- as.character(classtree[[which(classtree[, 2] == name)[1], 4]])
        shadecol <- as.character(classtree[[which(classtree[, 2] == name)[1], 5]])
        p <- p %>% 
          add_trace(data = dataset, x = dataset[,5], y = dataset[,6],
                    name = name, type = 'scatter', mode = 'lines',
                    line = list(color = linecol, width = 3), legendgroup = name, 
                    showlegend = TRUE, inherit = FALSE) %>%
          add_trace(data = dataset, x = dataset[,5], y = dataset[,9],
                    name = name, type = 'scatter', mode = 'lines', fill = "tonexty", fillcolor = shadecol,
                    line = list(color = linecol, width = 3, dash = 'dash'), 
                    legendgroup = name, showlegend = F, inherit = F) %>%
          add_trace(data = dataset, x = dataset[,5], y =  dataset[,10],
                    name = name, type = 'scatter', mode = 'lines', fill = "tonexty", fillcolor = shadecol,
                    line = list(color = linecol, width = 3, dash = 'dash'), 
                    legendgroup = name, showlegend = F,  inherit = F) %>%
          add_trace(data = dataset, x = dataset[,5], y = dataset[,7],
                    name = name, type = 'scatter', mode = 'lines',
                    line = list(color = linecol, width = 3, dash = 'dash'), 
                    legendgroup = name, showlegend = F,  inherit = F) %>%
          add_trace(data = dataset, x = dataset[,5], y = dataset[,8],
                    name = name, type = 'scatter', mode = 'lines',
                    line = list(color = linecol, width = 3, dash = 'dash'), 
                    legendgroup = name, showlegend = F,  inherit = F)
      }
    }
    return(p)
  })

#eq
  output$fitTable <- DT::renderDataTable({
   datatable(data.frame(ft[, 1:3],
                         eq = c(HTML("y=19.69 &bull; x<sup>0.375</sup>"),
                                HTML("y=19.16 &bull; x<sup>0.381</sup>"),
                                HTML("y=0.61 &bull; x<sup>0.7380</sup>+96.07"),
                                HTML("y=0.61 &bull; x<sup>0.738</sup>+96.07"),
                                HTML("y=15.32 &bull; x<sup>0.4255</sup>"),
                                HTML("y=17.71 &bull; x<sup>0.398</sup>"),
                                HTML("y=281.33+ <sup>-126.02</sup>&frasl;<sub>1+10^<sub>308.51&bull;-0.3108</sup></sub>"),
                                HTML("y=176.22+ <sup>-27.85</sup>&frasl;<sub>1+10^<sub>272.34&bull;-0.0294</sup></sub>"),
                                HTML("y=63.39+ <sup>162.76</sup>&frasl;<sub>1+10^<sub>192.89&bull;0.0042</sup></sub>"),
                                HTML("y=126.23+ <sup>68.81</sup>&frasl;<sub>1+10^<sub>293.54&bull;0.0043</sup></sub>"),
                                HTML("y=238.44+ <sup>-188.26</sup>&frasl;<sub>1+10^<sub>172.50&bull;-0.0009</sup></sub>"),
                                HTML("y=26.52 &bull; x<sup>0.320</sup>"),
                                HTML("y=-105.48+ <sup>637.19</sup>&frasl;<sub>1+10^<sub>548.19&bull;0.0006</sup></sub>"),
                                HTML("y=69.54 &bull; x<sup>0.1334</sup>"),
                                HTML("y=-115.04+ <sup>649.37</sup>&frasl;<sub>1+10^<sub>530.92&bull;0.0006</sup></sub>"),
                                HTML("y=-1217.89 &bull; x<sup>-0.4503</sup>+262.06"),
                                HTML("y=-1217.89 &bull; x<sup>-0.450</sup>+262.06"),
                                HTML("y=9.97 &bull; x<sup>0.5048</sup>"),
                                HTML("y=6.36 &bull; x<sup>0.5822</sup>"),
                                HTML("y=206.73 &bull; x<sup>0.1549</sup>-320.07"),
                                HTML("y=4.45 &bull; x<sup>0.6514</sup>"),
                                HTML("y=18.11 &bull; x<sup>0.402</sup>"),
                                HTML("y=10.51 &bull; x<sup>0.4941</sup>"),
                                HTML("y=14.69 &bull; x<sup>0.433</sup>"),
                                HTML("y=7.62 &bull; x<sup>0.550</sup>"),
                                HTML("y=9.65 &bull; x<sup>0.5067</sup>"),
                                HTML("y=10.60 &bull; x<sup>0.4953</sup>"),
                                HTML("y=45.54+ <sup>272.81</sup>&frasl;<sub>1+10^<sub>345.27&bull;0.0019</sup></sub>"),
                                HTML("y=9.40 &bull; x<sup>0.5096</sup>"),
                                HTML("y=7.88 &bull; x<sup>0.5423</sup>"),
                                HTML("y=24.93 &bull; x<sup>0.393</sup>+-57.28"),
                                HTML("y=18.53 &bull; x<sup>0.3869</sup>"),
                                HTML("y=18.53 &bull; x<sup>0.387</sup>"),
                                HTML("y=127.43+ <sup>32.04</sup>&frasl;<sub>1+10^<sub>195.59&bull;0.0204</sup></sub>"),
                                HTML("y=17.68 &bull; x<sup>0.3961</sup>"),
                                HTML("y=41.54 &bull; x<sup>0.2385</sup>"),
                                HTML("y=18.13 &bull; x<sup>0.393</sup>"),
                                HTML("y=8.71 &bull; x<sup>0.527</sup>"),
                                HTML("y=196.71 &bull; x<sup>-0.074</sup>"),
                                HTML("y=6.49 &bull; x<sup>0.561</sup>"),
                                HTML("y=16.93 &bull; x<sup>0.401</sup>"),
                                HTML("y=6.37 &bull; x<sup>0.5326</sup>"),
                                HTML("y=6.37 &bull; x<sup>0.533</sup>"),
                                HTML("y=94.33+ <sup>151.81</sup>&frasl;<sub>1+10^<sub>262.58&bull;0.0044</sup></sub>"),
                                HTML("y=-29.20+ <sup>413.90</sup>&frasl;<sub>1+10^<sub>270.42&bull;0.0017</sup></sub>"),
                                HTML("y=26.74+ <sup>349.62</sup>&frasl;<sub>1+10^<sub>335.14&bull;0.0020</sup></sub>"),
                                HTML("y=29.78 &bull; x<sup>0.2956</sup>"),
                                HTML("y=-26.62+ <sup>496.70</sup>&frasl;<sub>1+10^<sub>567.65&bull;0.0008</sup></sub>"),
                                HTML("y=119.84+ <sup>32.26</sup>&frasl;<sub>1+10^<sub>175.73&bull;0.0590</sup></sub>"),
                                HTML("y=177.27+ <sup>187.55</sup>&frasl;<sub>1+10^<sub>930.00&bull;0.0053</sup></sub>"),
                                HTML("y=177.27+ <sup>187.55</sup>&frasl;<sub>1+10^<sub>930.00&bull;0.0053</sup></sub>"),
                                HTML("y=10.63 &bull; x<sup>0.4820</sup>"),
                                HTML("y=149.53+ <sup>229.81</sup>&frasl;<sub>1+10^<sub>869.97&bull;0.0026</sup></sub>"),
                                HTML("y=25.10 &bull; x<sup>0.324</sup>"),
                                HTML("y=140.28+ <sup>-26.99</sup>&frasl;<sub>1+10^<sub>135.30&bull;-0.0363</sup></sub>"),
                                HTML("y=19.68 &bull; x<sup>0.380</sup>"),
                                HTML("y=0.20 &bull; x<sup>0.991</sup>+76.78"),
                                HTML("y=0.42 &bull; x<sup>0.919</sup>+93.91"),
                                HTML("y=-342648.87 &bull; x<sup>-1.275</sup>+285.94"),
                                HTML("y=204.29+ <sup>-127.12</sup>&frasl;<sub>1+10^<sub>191.50&bull;-0.0047</sup></sub>"),
                                HTML("y=23.36 &bull; x<sup>0.345</sup>"),
                                HTML("y=12.78 &bull; x<sup>0.4482</sup>"),
                                HTML("y=32.31 &bull; x<sup>0.2823</sup>"),
                                HTML("y=4.14 &bull; x<sup>0.6180</sup>"),
                                HTML("y=11.47 &bull; x<sup>0.4576</sup>"),
                                HTML("y=15.61 &bull; x<sup>0.408</sup>"),
                                HTML("y=23.55 &bull; x<sup>0.337</sup>"),
                                HTML("y=18.45 &bull; x<sup>0.3834</sup>"),
                                HTML("y=38.88 &bull; x<sup>0.2415</sup>"),
                                HTML("y=30.29 &bull; x<sup>0.292</sup>"),
                                HTML("y=9.72 &bull; x<sup>0.503</sup>"),
                                HTML("y=31.44 &bull; x<sup>0.2893</sup>"),
                                HTML("y=9.02 &bull; x<sup>0.4957</sup>"),
                                HTML("y=12.55 &bull; x<sup>0.4427</sup>"),
                                HTML("y=152.74+ <sup>88.26</sup>&frasl;<sub>1+10^<sub>505.58&bull;0.0043</sup></sub>"),
                                HTML("y=25.59 &bull; x<sup>0.3237</sup>"),
                                HTML("y=7.48 &bull; x<sup>0.5543</sup>"),
                                HTML("y=7.48 &bull; x<sup>0.554</sup>"),
                                HTML("y=1.19 &bull; x<sup>0.8461</sup>"),
                                HTML("y=280.68+ <sup>28.45</sup>&frasl;<sub>1+10^<sub>788.85&bull;0.0146</sup></sub>"),
                                HTML("y=11.40 &bull; x<sup>0.488</sup>"),
                                HTML("y=202.83+ <sup>17.11</sup>&frasl;<sub>1+10^<sub>505.62&bull;0.1890</sup></sub>"),
                                HTML("y=1.43 &bull; x<sup>0.8366</sup>"),
                                HTML("y=31.06 &bull; x<sup>0.3119</sup>"),
                                HTML("y=20.02 &bull; x<sup>0.387</sup>")
                                ),
                         ft[, 4:5]),
              colnames = c("Class", "Subclass", "Fit Type", "Regression Equation", "AICc", "Standard Error"),
              options = list(pageLength = 25, autowidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 2:5))),
              escape = FALSE)
  })
  
#guidelines

  output$subfig <- renderImage({

    list(src = "data/submissionfig.png",
         width = "600", height = "150", align = "center")

  }, deleteFile = FALSE)

  output$peakannotate <- downloadHandler(
    filename = function() {
      paste0("PeakAnnotationGuidelines_",Sys.Date(),".pdf")
    },
    content = function(file) {
      file.copy("data/PeakAnnotationGuidelines.pdf", file)
    }
  )
  
  output$singleguide <- downloadHandler(
    filename = function() {
      paste0("SingleFieldGuidelines_",Sys.Date(),".pdf")
    },
    content = function(file) {
      file.copy("data/SingleFieldGuidelines.pdf", file)
    }
  )

  output$singledata <- downloadHandler(
    filename = function() {
      paste0("SI_SingleField_DataFormat_",Sys.Date(),".xlsx")
    },
    content = function(file) {
      file.copy("data/SI_SingleField_DataFormat_online.xlsx", file)
    }
  )

  output$steppedguide <- downloadHandler(
    filename = function() {
      paste0("SteppedFieldGuidelines_",Sys.Date(),".pdf")
    },
    content = function(file) {
      file.copy("data/SteppedFieldGuidelines.pdf", file)
    }
  )

  output$steppeddata <- downloadHandler(
    filename = function() {
      paste0("SI_SteppedField_ScaleAndDataFormat_",Sys.Date(),".xlsx")
    },
    content = function(file) {
      file.copy("data/SI_SteppedField_ScaleAndDataFormat_online.xlsx", file)
    }
  )

}

