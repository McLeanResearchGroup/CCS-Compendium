library(shiny)
library(shinythemes)
library(plotly)
library(DT)

#setwd("/Users/JAPicache/Box Sync/R_Scripts&Data/20171218JAP_iceberg/Shiny/CCScompendium_Shiny")

#plotlyInput
all <- read.csv("data/allMASTER.csv", header = TRUE)
allclean <- all[,c("Compound", "Neutral.Formula", "CAS", "InChi", "InChiKey", "mz", "Ion.Species", "Ion.Species.Agilent", "Charge", "CCS", "SD", "RSD", "CCS.z", "Peak.N", "Kingdom", "Super.Class", "Class", "Subclass", "Sources","Shape", "N.Rep")]

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

eq <- c(#classes
  '<img src = "https://quicklatex.com/cache3/29/ql_64be5fb2660901784aec26e398620829_l3.png"></img>', #(5'->5')-dinucleotides
  '<img src = "https://quicklatex.com/cache3/09/ql_5f273559d3da063d1afad977ce9ee809_l3.png"></img>', #Alkali metal salts
  '<img src = "https://quicklatex.com/cache3/0d/ql_c5b92f1c6dbb1cb628e68f29c1f8290d_l3.png"></img>', #Azoles
  '<img src = "https://quicklatex.com/cache3/fe/ql_c2ec4507569d190ff3da6eccde24befe_l3.png"></img>', #Benzene and substituted derivatives
  '<img src = "https://quicklatex.com/cache3/92/ql_499b90815e2731bf1024ac50429f0a92_l3.png"></img>', #Carboxylic acids and derivatives
  '<img src = "https://quicklatex.com/cache3/a5/ql_b1a6bd2d21e67ba1e94a46d09a778aa5_l3.png"></img>', #Diazines
  '<img src = "https://quicklatex.com/cache3/fc/ql_4981a4dac8277e8466c039a946545ffc_l3.png"></img>', #Fatty acyls
  '<img src = "https://quicklatex.com/cache3/d9/ql_4eeab06750e4dae6dcd97c28420172d9_l3.png"></img>', #Flavonoids
  '<img src = "https://quicklatex.com/cache3/cd/ql_8db9e7981fe6f69e327fd06ede56e2cd_l3.png"></img>', #Glycerophospholipids
  '<img src = "https://quicklatex.com/cache3/1d/ql_a0acdfc360cc19ed1744f3bad78acc1d_l3.png"></img>', #Imidazopyrimidines
  '<img src = "https://quicklatex.com/cache3/f6/ql_1a7b22564cc45df4787c66e17c9debf6_l3.png"></img>', #Isoflavonoids
  '<img src = "https://quicklatex.com/cache3/13/ql_f4b36d53c7b09e749371de91fd3cad13_l3.png"></img>', #Naphthalenes
  '<img src = "https://quicklatex.com/cache3/f8/ql_95041fdbf3f6ea9f8fb47ce04a5ac5f8_l3.png"></img>', #Organofluorides
  '<img src = "https://quicklatex.com/cache3/62/ql_c1be82faeb03f6f3eb1bb2ff23064262_l3.png"></img>', #Organonitrogen compounds
  '<img src = "https://quicklatex.com/cache3/2b/ql_c6b8c5a0d04ed10d95e9621ad37a722b_l3.png"></img>', #Organooxygen compounds
  '<img src = "https://quicklatex.com/cache3/d0/ql_732ef7344945fa1b9da5bb162d9279d0_l3.png"></img>', #Peptidomimetics
  '<img src = "https://quicklatex.com/cache3/c2/ql_71dc7b1a1c8f7ccb1f9392ddabdeefc2_l3.png"></img>', #Polypeptides
  '<img src = "https://quicklatex.com/cache3/7b/ql_abf8a575e7c7543ee726260a1218be7b_l3.png"></img>', #Prenol lipids
  '<img src = "https://quicklatex.com/cache3/95/ql_5d098ce7d7585168800ebf2d76636895_l3.png"></img>', #Proteins
  '<img src = "https://quicklatex.com/cache3/f8/ql_c0899e2494468e8dc5e62e3fc71409f8_l3.png"></img>', #Pteridines and derivatives
  '<img src = "https://quicklatex.com/cache3/70/ql_e421db66e142c34b420c6b19bfc13970_l3.png"></img>', #Purine nucleotides
  '<img src = "https://quicklatex.com/cache3/3f/ql_1b44df96a11a620342ad8a3cbe9f9e3f_l3.png"></img>', #Pyrenes
  '<img src = "https://quicklatex.com/cache3/37/ql_9b8e7212b22f48828e10b34c2154fc37_l3.png"></img>', #Pyridines and derivatives
  '<img src = "https://quicklatex.com/cache3/88/ql_c036df89ae63fd5ec0557c7d1b516c88_l3.png"></img>', #Pyrimidine nucleotides
  '<img src = "https://quicklatex.com/cache3/87/ql_f9f344fa542dd565a0eab0ea1b89fb87_l3.png"></img>', #Quinolines and derivatives
  #subclasses
  '<img src = "https://quicklatex.com/cache3/48/ql_4037988c5929ee001b751342f54ebc48_l3.png"></img>', #Organooxygen compounds/ Alcohols and polyols
  '<img src = "https://quicklatex.com/cache3/09/ql_5f273559d3da063d1afad977ce9ee809_l3.png"></img>', #Alkali metal salts/ Alkali metal iodides
  '<img src = "https://quicklatex.com/cache3/a5/ql_f205c68b4f806e3ca046f360431b28a5_l3.png"></img>', #Organonitrogen compounds/ Amines
  '<img src = "https://quicklatex.com/cache3/a6/ql_5a2b50a7e133d1fe9d9143831e9565a6_l3.png"></img>', #Carboxylic acids and derivatives/ Amino acids, peptides, and analogues
  '<img src = "https://quicklatex.com/cache3/ed/ql_6300e7188c217c37af783df0497e9aed_l3.png"></img>', #Benzene and substituted derivatives/ Anilides
  '<img src = "https://quicklatex.com/cache3/e1/ql_48bf2231d1310c1a1f8d423fb1c848e1_l3.png"></img>', #Phenols/ Benzenediols
  '<img src = "https://quicklatex.com/cache3/29/ql_7e2caeb3d180da0f1f8343626f611429_l3.png"></img>', #Benzene and substituted derivatives/ Benzenesulfonamides
  '<img src = "https://quicklatex.com/cache3/19/ql_6c298065185f6215058b4c9c9dc4e819_l3.png"></img>', #Steroids and steroid derivatives/ Bile acids, alcohols and derivatives
  '<img src = "https://quicklatex.com/cache3/95/ql_6a9e5c661093d145ff2c3ba838983e95_l3.png"></img>', #Benzene and substituted derivatives/ Biphenyls and derivatives
  '<img src = "https://quicklatex.com/cache3/c1/ql_48470f7e27443f0b06b31405de27b4c1_l3.png"></img>', #Organooxygen compounds/ Carbohydrates and carbohydrate conjugates
  '<img src = "https://quicklatex.com/cache3/cb/ql_07ba42d2a557545ec569752ef3405fcb_l3.png"></img>', #Organooxygen compounds/ Carbonyl compounds
  '<img src = "https://quicklatex.com/cache3/f1/ql_8ffded55a709516c7d8f73dc46d9ebf1_l3.png"></img>', #Peptidomimetics/ Depsipeptides
  '<img src = "https://quicklatex.com/cache3/53/ql_3b610dfbffd14ac19ae9514cc7f34d53_l3.png"></img>', #Benzene and substituted derivatives/ Diphenylethers
  '<img src = "https://quicklatex.com/cache3/f5/ql_ab2cd75a2e789846a0da81617805eff5_l3.png"></img>', #Fatty Acyls/ Fatty acids and conjugates
  '<img src = "https://quicklatex.com/cache3/80/ql_e41513552ba1ba2f399ad7bc5c711780_l3.png"></img>', #Glycerophospholipids/ Glycerophosphates
  '<img src = "https://quicklatex.com/cache3/1b/ql_cfb10593cc201ca42c5cd4410f9aa21b_l3.png"></img>', #Glycerophospholipids/ Glycerophosphocholines
  '<img src = "https://quicklatex.com/cache3/38/ql_406c88a608aa2b1c4989422402989b38_l3.png"></img>', #Glycerophospholipids/ Glycerophosphoethanolamines
  '<img src = "https://quicklatex.com/cache3/e1/ql_882c697487be427b191e7a54665379e1_l3.png"></img>', #Glycerophospholipids/ Glycerophosphoserines
  '<img src = "https://quicklatex.com/cache3/51/ql_eeed477b1f87e52d34423cfd8c1e0a51_l3.png"></img>', #Sphingolipids/ Glycosphingolipids
  '<img src = "https://quicklatex.com/cache3/df/ql_f2daab48ec9875a2e44161b64584a3df_l3.png"></img>', #Azoles/ Imidazoles
  '<img src = "https://quicklatex.com/cache3/41/ql_c08d41cc6712ab9a48ca01de82c67f41_l3.png"></img>', #Indoles and derivatives/ Indoles
  '<img src = "https://quicklatex.com/cache3/69/ql_cf0e7032dbf5ff7614971d324da6d569_l3.png"></img>', #Indoles and derivatives/ Indolyl carboxylic acids and derivatives
  '<img src = "https://quicklatex.com/cache3/f8/ql_95041fdbf3f6ea9f8fb47ce04a5ac5f8_l3.png"></img>', #Organofluorides/ Phosphazene and phosphazene derivatives
  '<img src = "https://quicklatex.com/cache3/75/ql_8b8505933200efc1d38c4b2684c49075_l3.png"></img>', #Sphingolipids/ Phosphosphingolipids
  '<img src = "https://quicklatex.com/cache3/ac/ql_7173e2cd4b5d7e90f1e891b49e36aeac_l3.png"></img>', #Purine nucleotides/ Purine deoxyribonucleotides
  '<img src = "https://quicklatex.com/cache3/f5/ql_5b1c02a5871579e128ca899fd215a2f5_l3.png"></img>', #Purine nucleotides/ Purine nucleotide sugars
  '<img src = "https://quicklatex.com/cache3/44/ql_d86bac4300df18903c29be99ad602544_l3.png"></img>', #Imidazopyrimidines/ Purines and purine derivatives
  '<img src = "https://quicklatex.com/cache3/f8/ql_3e00f56f00c93b289d9938401c19f4f8_l3.png"></img>', #Pyridines and derivatives/ Pyridinecarboxylic acids and derivatives
  '<img src = "https://quicklatex.com/cache3/09/ql_31114a0f003c3d5970421afb25bf2509_l3.png"></img>', #Pyrimidine nucleotides/ Pyrimidine nucleotide sugars
  '<img src = "https://quicklatex.com/cache3/d5/ql_e1bfd3faabb36377aa0fcd863c93bcd5_l3.png"></img>', #Pyrimidine nucleotides/ Pyrimidine ribonucleotides
  '<img src = "https://quicklatex.com/cache3/a5/ql_b1a6bd2d21e67ba1e94a46d09a778aa5_l3.png"></img>', #Diazines/ Pyrimidines and pyrimidine derivatives
  '<img src = "https://quicklatex.com/cache3/22/ql_2f307450e1da1f4c9eb575e5171c4022_l3.png"></img>', #Organonitrogen compounds/ Quaternary ammonium salts
  '<img src = "https://quicklatex.com/cache3/80/ql_41b7120b8378845b03396261c4087880_l3.png"></img>', #Quinolines and derivatives/ Quinoline carboxylic acids
  '<img src = "https://quicklatex.com/cache3/17/ql_e573784932064b4135c95235f573ef17_l3.png"></img>' #Indoles and derivatives/ Tryptamines and derivatives     
  )

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
        add_trace(data, x= ~data$mz, x0 = 0, y= ~data$CCS.z, y0 = 0,
                  type = "scatter", mode = "markers", showlegend = T,
                  color = ~data$Super.Class, colors = ~super.col,
                  marker = list(size = 12, opacity = 0.6), symbol = ~data$Shape, symbols = ~sym,
                  text = ~paste("Name: ", data$Compound,
                                "<br> Neutral Formula: ", data$Neutral.Formula,
                                "<br>InChi Key: ", data$InChiKey,
                                "<br><i>m/z</i>:", data$mz,
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
    datatable(data.frame(allclean[, c("Compound", "Neutral.Formula", "CAS", "InChi", "InChiKey", "mz", "Ion.Species", "Charge", "CCS", "SD", "RSD", "CCS.z", "Peak.N", "N.Rep", "Kingdom", "Super.Class", "Class", "Subclass", "Sources")]),
              colnames = c("Compound", "Neutral Formula", "CAS", "InChi", "InChi Key", "m/z", "Ion Species", "Charge State", "CCS", " CCS SD", "CCS RSD", "CCS/z", "Peak Number", "Replicates (N)", "Kingdom", "Super Class", "Class", "Subclass", "Source(s)"),
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
    datatable(data.frame(ft[, 1:3], eq = eq, ft[, 5:6]),
              colnames = c("Class", "Subclass", "Fit Type", "Regression Equation", "Standard Error", "Standard Deviation"),
              options = list(pageLength = 25, autowidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 2:5))),
              escape = FALSE)
  })
  
#guidelines

  output$subfig <- renderImage({

    list(src = "data/submissionfig.png",
         width = "600", height = "150", align = "center")

  }, deleteFile = FALSE)

  output$pea <- downloadHandler(
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

