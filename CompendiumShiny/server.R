library(shiny)
library(plotly)
library(DT)


#plotlyInput
all <- read.csv("allMASTER.csv", header = TRUE)
super.col <- c("#A5142A", "#F42813", "#DF7F12", "#C9A544", "#E3CD10", 
               "#5DE310", "#1C9E30", "#10C890", "#23D1E3", "#3B9DE0", 
               "#072DC8", "#6B33EC", "#BAA2F0", "#E471EC", "#D12196", "#C8C8C8")
sym <- c("circle", "square", "diamond", "cross", "triangle-up")
sources <- as.character(levels(all$Source))
supclasses <- as.character(levels(all$Super.Class))
classes <- as.character(levels(all$Class))
adducts <- as.character(levels(all$Adduct))

#trendsInput
trends <- list.files(path = "ClassTrends_R", 
                     pattern = "*.csv", full.names = TRUE)
curves <- lapply(trends, read.csv)
trendnams <- list.files(path = "ClassTrends_R", 
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


#server
server <- function(input, output, session) {
    
  #compendium  
  sourceType <- reactive({
    input$variable
  })
  
  output$caption <- renderText({
    sourceType()
  })
  
  output$allPlot <- renderPlotly({
    pdf(NULL)
    plot_ly(all, x= ~all$mz, x0 = 0, y= ~all$CCS.z, y0 = 0,
            type = "scatter", mode = "markers", showlegend = T,
            color = ~Super.Class, colors = ~super.col,
            marker = list(size = 10, opacity = 0.6), symbol = ~Shape, symbols = ~sym,
            text = ~paste("Name: ", Compound,
                          "<br>Formula", Formula,
                          "<br>CAS: ", CAS,
                          "<br>m/z:", mz,
                          "<br>Charge Species: ", Adduct,
                          "<br>CCS/z:",CCS,"+/-", RSD,"%",
                          "<br>Class/Subclass: ", Class,"/",Subclass,
                          "<br>Source: ", Source,
                          "<br>DOI: ", DOI),
            hoverlabel = list(font = list(family = "Arial", size = 14, color = "#000000")),
            transforms = list(
              if(input$pol == "+"){
                list(type = 'filter', target = ~all$Charge, operation = '>', value = 0)
                } else{},
              if(input$pol == "-"){
                list( type = 'filter', target = ~all$Charge, operation = '<', value = 0)
                } else{},
              if(input$adduct != "."){
                list(type = 'filter', target = ~all$Adduct, operation = '=', value = input$adduct)
                } else{},
              if(input$supclass != "."){
                list(type = 'filter', target = ~all$Super.Class, operation = '=', value = input$supclass)
                } else{},
              if(input$class != "."){
                list(type = 'filter', target = ~all$Class, operation = '=', value = input$class)
                } else{},
              if(input$source != "."){
                list(type = 'filter', target = ~all$Source, operation = '=', value = input$source
                ) } else{} )
    ) %>%
      layout(
        title = "<b>Interactive Compendium (n>3800)</b>",
        titlefont = list(family = "Arial", size = 30, color = "#000000"),
        autosize = TRUE,
        xaxis = list(title = "<b>m/z</b>",
                     titlefont = list(family = "Arial", size = 20, color = "#000000"),
                     zeroline = FALSE, showline = TRUE, linewidth = 10,
                     ticks = "outside", ticklen = 10, tickwidth = 10,
                     tickfont = list(family = "Arial", size = 20, color = "#000000"),
                     showgrid = FALSE),
        yaxis = list(title = "<b>CCS/z</b>",
                     titlefont = list(family = "Arial", size = 20, color = "#000000"),
                     zeroline = FALSE, showline = TRUE, linewidth = 10,
                     ticks = "outside", ticklen = 10, tickwidth = 10,
                     tickfont = list(family = "Arial", tick0 = 0, size = 20, color = "#000000"),
                     showgrid = FALSE),
        margin = list(t = 100, l = 100, b = 100))
  })
  
#table  
  output$table <- DT::renderDataTable({
    all[,1:15]
  },
  options = list(pageLength = 25))
  
#trends   
  dataVals <- eventReactive(input$action, {
    as.list(input$trendclass)
  })
  
  output$trendPlot <- renderPlotly({
    #baseplot
    p <- plot_ly() %>%
      layout(xaxis = list(title = "<b>m/z</b>", ticks = "outside", tickwidth = 6, 
                          linewidth = 6, showline = TRUE, showgrid = FALSE, 
                          zeroline = FALSE),
             yaxis = list(title = "<b>CCS/z</b>", ticks = "outside", tickwidth = 6, 
                          linewidth = 6, showline = TRUE, showgrid = FALSE, 
                          zeroline  = FALSE))
    
    #layer trendlines
    # datalist <- datasetInput()
    if(!is.null(dataVals())){
      for(i in seq_along(dataVals())){
        dataset <- curves[[dataVals()[[i]]]]
        linecol <- linecols[[i]]
        shadecol <- fillcols[[i]]
        p <- p %>% 
          add_trace(data = dataset, x = dataset[,5], y = dataset[,6],
                    name = dataVals()[[i]], type = 'scatter', mode = 'lines',
                    line = list(color = linecol, width = 3), legendgroup = "b", 
                    showlegend = T, inherit = F) %>%
          add_trace(data = dataset, x = dataset[,5], y = dataset[,9],
                    type = 'scatter', mode = 'lines', fill = "tonexty", fillcolor = shadecol,
                    line = list(color = linecol, width = 3, dash = 'dash'), 
                    legendgroup = "b", showlegend = F, inherit = F) %>%
          add_trace(data = dataset, x = dataset[,5], y =  dataset[,10],
                    type = 'scatter', mode = 'lines', fill = "tonexty", fillcolor = shadecol,
                    line = list(color = linecol, width = 3, dash = 'dash'), 
                    legendgroup = "b", showlegend = F,  inherit = F) %>%
          add_trace(data = dataset, x = dataset[,5], y = dataset[,7],
                    type = 'scatter', mode = 'lines',
                    line = list(color = linecol, width = 3, dash = 'dash'), 
                    legendgroup = "b", showlegend = F,  inherit = F) %>%
          add_trace(data = dataset, x = dataset[,5], y = dataset[,8],
                    type = 'scatter', mode = 'lines',
                    line = list(color = linecol, width = 3, dash = 'dash'), 
                    legendgroup = "b", showlegend = F,  inherit = F)
      }
    }
    return(p)
  })
  
}


#runApp
shinyApp(ui, server)
