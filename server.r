### IMPORTED LIBRARIES ########################################################

library(shiny)
library(jsonlite)
library(curl)
library(ggplot2)
library(ggdendro)



### GLOBAL VARIABLES ##########################################################

FIXER_URL = "http://api.fixer.io/"
BASE = "USD"
SIMTABLE_DEFAULT = data.frame(Code = c(), Currency = c(), Distance = c())



### FUNCIONS ##################################################################

GetCurrNames = function() {
  # Currency list and abbreviations
  
  CTBL = read.csv("currencies.csv", stringsAsFactors = FALSE)
  return(CTBL)
}



GetRates = function(date = "latest") {
  # Get exchange rates from any day since 1999
  # Rates are against EUR by default
  
  url = paste0(FIXER_URL, date, "?base=", BASE) 
  ratelist = fromJSON(url)
  
  df = data.frame(Label = names(ratelist$rates),
                  Rate = unlist(ratelist$rates),
                  stringsAsFactors = FALSE,
                  row.names = NULL)
  df = rbind(df, 
             data.frame(Label = c(BASE),
                        Rate = c(1))
  )
  df = df[order(df$Label), ]
  
  return( list(Exchange = df, 
               Date = ratelist$date)
  )
}



DistFunction = function(x, y) {
  
  # Cosine similarity
  sim = sum(x * y)/sqrt(sum(x^2) * sum(y^2))
  
  # Correct floating point inaccuracies
  if (sim > 1) {
    sim = 1
  }
  if (sim < -1) {
    sim = -1
  }
  
  # Return distance
  return( acos(sim)/pi )
}



GetDist = function(exc0, exc1) {
  # Requires exchange rates at the same base
  
  # Inner join of data frames created
  df = merge(exc0$Exchange, exc1$Exchange, by = "Label", all = FALSE)
  names(df)[2:3] = c("Rate0", "Rate1")
  
  # Get the long names of the currencies
  if ( is.null(nrow(ALL_CLIST)) ) {
    ALL_CLIST = GetCurrNames()
  }
  LN = sapply(df$Label, function(x){
                                        if (length(ALL_CLIST[ALL_CLIST$Code == x, "Long_Name"]) == 1){ 
                                          ALL_CLIST[ALL_CLIST$Code == x, "Long_Name"]
                                        } else {
                                          "na"
                                        } 
                                      })
  CD = data.frame(Label = df$Label,
                  Long_Name = unlist(LN),
                  row.names = NULL)
  
  # All to all conversion rates
  m0 = sapply(df$Rate0, function(x) {df$Rate0/x})
  m1 = sapply(df$Rate1, function(x) {df$Rate1/x})
  
  # Vectors of currency rate changes between time points
  # Normalized to the data at the second time point
  Diff = (m1 - m0)/m1
  
  # Obtain cosine distances between currencies
  Dist = apply(Diff, 2, function(x) {apply(Diff, 2, function(y) {DistFunction(x, y)})})
  #rownames(Dist) = df$Currency
  colnames(Dist) = df$Label
  
  return( list(Distance = Dist,
               Difference = Diff,
               CodeDict = CD)
  )
}



GetClosest = function(dist, curr, n=5) {
  # Returns the most similar currencies
  
  toplist = dist$Distance[order(dist$Distance[, curr]), curr]
  toplist_names = dist$CodeDict[order(dist$Distance[, curr]), ]
  
  
  return(data.frame(Label = toplist_names$Label[1:(1 + n)],
                    Currency = toplist_names$Long_Name[1:(1 + n)],
                    Distance = toplist[1:(1 + n)],
                    row.names = NULL)
  )
}



GetMomentum = function(dist) {
  # Determines the Momentum and Weakness of the currencies
  # Momentum: the sum of changes relative to other currencies
  # Weakness: the number of currencies to which the actual currencies is getting weaker
  
  Label = dist$CodeDict$Label
  Momentum = apply(dist$Difference, 2, function(x){ sum(x) })
  Weakness = apply(dist$Difference, 2, function(x){ sum(x < 0) })
  Label = Label[order(Momentum, decreasing = T)]
  Weakness = Weakness[order(Momentum, decreasing = T)]
  Momentum = Momentum[order(Momentum, decreasing = T)]
  return(data.frame(label = Label, momentum = Momentum, weakness = Weakness))
}



### MAIN ######################################################################

ALL_CLIST = GetCurrNames()

shinyServer(function(input, output, session){
  
  DIST = reactiveVal()
  MOMENTUM = reactiveVal()
  WARNING = reactiveVal()
  SIMTABLE = reactiveVal(value = SIMTABLE_DEFAULT)
  
  ### Fetch data ###
  observeEvent(input$SEND_DATERANGE, {
    d0 = GetRates(input$DATERANGE[1])
    d1 = GetRates(input$DATERANGE[2])
    
    Warn = c()
    if (input$DATERANGE[1] != d0$Date) {
      Warn = c(Warn, paste0("WARNING. No data found on date ", input$DATERANGE[1], ": ", d0$Date, " used instead."))
    }
    if (input$DATERANGE[2] != d1$Date) {
      Warn = c(Warn, paste0("WARNING. No data found on date ", input$DATERANGE[2], ": ", d1$Date, " used instead."))
    }
    if (d0$Date == d1$Date) {
      Warn = c(Warn, "ERROR. Starting and ending dates are identical.")
    }
    WARNING(Warn)
    
    if (d0$Date != d1$Date) {
      D = GetDist(d0, d1)
      updated_list = sapply(1:nrow(D$CodeDict), function(x) {paste(D$CodeDict[x, "Label"], D$CodeDict[x, "Long_Name"], sep = " - ")})
      updateSelectInput(session, inputId = "SELECTCURR", choices = updated_list)
      DIST(D)
      MOMENTUM(GetMomentum(D))
    } else {
      updateSelectInput(session, inputId = "SELECTCURR", choices = c("none"))
      DIST(NULL)
      MOMENTUM(NULL)
    }
    
    SIMTABLE(SIMTABLE_DEFAULT)
  })
  
  ### Clustogram output ###
  output$CLUSTOGRAM = renderPlot({
    D = DIST()
    M = MOMENTUM()
    if (is.null(D)) {
      plot(1, type="n", axes=F, xlab="", ylab="")
    } else {
      hc = hclust(as.dist(D$Distance), method = "average")
      #plot(as.dendrogram(hc))
      den = dendro_data(hc, type = "rectangle")
      den$labels = merge(den$labels, M, by = "label", sort = F)
      ggplot() + 
        geom_segment(data = segment(den), aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_text(data = label(den), aes(x, y, label = label, color = momentum), size = 6, hjust = 1, angle = 90, nudge_y = -0.02) +
        scale_y_continuous(expand = c(0.2, 0), limits = c(-0.09, NA)) +
        scale_color_gradient2(low = "blue", mid = "green", high = "red", midpoint = 0) +
        theme(axis.line.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size = 15),
              panel.background = element_rect(fill = "white"),
              panel.grid = element_blank()
        )
    }
  })
  
  ### Warnings occured during data fetching ###
  output$CLUSTOGRAM_MSG = renderUI({
    Warn = WARNING()
    if (is.null(Warn)) {
      HTML("")
    } else {
      HTML(paste(Warn, collapse = "<br>"))
    }
  })
  
  ### Fetch most similar currencies ###
  observeEvent(input$SEND_SELECTCURR, {
    D = DIST()
    M = MOMENTUM()
    if (is.null(D)) {
      SIMTABLE(SIMTABLE_DEFAULT)
    } else {
      #currid = D$CodeDict[D$CodeDict$Long_Name == input$SELECTCURR, "Code"]
      currid = unlist(strsplit(input$SELECTCURR,  " - ", fixed = TRUE))[1]
      N = min(input$NUMCURR, nrow(D$CodeDict))
      Closest = GetClosest(D, currid, N)
      names(M) = c("Label", "Momentum", "Weakness")
      SIMTABLE(merge(Closest, M, by = "Label", sort = F))
    }
  })
  
  ### Show the most similar currencies ###
  output$TABLE_CLOSEST = renderTable({
    SIMTABLE()
  })
  
})