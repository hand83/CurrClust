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
  RATELIST = fromJSON(url)
  
  DF = data.frame(label = names(RATELIST$rates),
                  rate = unlist(RATELIST$rates),
                  stringsAsFactors = FALSE,
                  row.names = NULL)
  DF = rbind(DF, 
             data.frame(label = c(BASE),
                        rate = c(1))
  )
  DF = DF[order(DF$label), ]
  
  return( list(Exchange = DF, 
               Date = RATELIST$date)
  )
}



GetMergedData = function(EXC0, EXC1) {
  # Creates data with conversion rates at two time points
  # Requires exchange rates with the same base
  
  # Inner join of data frames created
  MERGED = merge(EXC0$Exchange, EXC1$Exchange, by = "label", all = FALSE)
  names(MERGED)[2:3] = c("rate0", "rate1")
  
  # Get long names
  LN = sapply(MERGED$label, function(x){
                                        if (length(ALL_CLIST[ALL_CLIST$Code == x, "Long_Name"]) == 1){ 
                                          ALL_CLIST[ALL_CLIST$Code == x, "Long_Name"]
                                        } else {
                                          "na"
                                        } 
                                      })

  MERGED$Long_Name = LN
  MERGED = MERGED[order(MERGED$label), ]
  return(MERGED)
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



GetDist = function(EXC) {
  # Calculates the distance matrix
  # Requires the data frame with conversion rates at two time points
  
  # All to all conversion rates
  M0 = sapply(EXC$rate0, function(x) {EXC$rate0/x})
  M1 = sapply(EXC$rate1, function(x) {EXC$rate1/x})
  
  # Vectors of currency rate changes between time points
  # Normalized to the data at the second time point
  DIFF = (M1 - M0)/M1
  
  # Obtain cosine distances between currencies
  DIST = apply(DIFF, 2, function(x) {apply(DIFF, 2, function(y) {DistFunction(x, y)})})
  #rownames(Dist) = df$Currency
  colnames(DIST) = EXC$label
  rownames(DIST) = EXC$label
  
  return(DIST)
}



ValueChanges = function(EXC) {
  # calculates the change in the value of the currency
  # assumes that the average value changes of all currencies equals to one
  # Requires the data frame with conversion rates at two time points
  
  # All to all conversion rates
  M1 = sapply(EXC$rate1, function(x) {EXC$rate1/x})
  M0 = sapply(EXC$rate0, function(x) {EXC$rate0/x})

  # Ratio of conversion rates at the two time points
  R01 = M0/M1
  n = dim(EXC)[1]
  
  # Calculate the value changes of each currency from the averaged ratios
  vchange = apply(R01, 2, function(x) {n/sum(x)})
  vrank = as.integer(rank(-1 * vchange))
  
  # Returns percent change in the values
  return(data.frame(label = EXC$label, index = 100 * (vchange - 1), rank = vrank, stringsAsFactors = F))
}



GetClosest = function(DIST, VCH, EXC, curr, n=5) {
  # Returns the most similar currencies
  # Requires the distance matrix
  # Requires the value changes data frame
  # Requires the data frame with conversion rates at two time points
  # Requires a selected currency in 3 letter code format
  
  ord = order(DIST[, curr])
  toplist_names = EXC[ord, c("label", "Long_Name")]
  toplist_dist = DIST[ord, curr]
  toplist_vch = VCH[ord, c("index", "rank")]
  
  
  return(data.frame(Label = toplist_names$label[1:(1 + n)],
                    Currency = toplist_names$Long_Name[1:(1 + n)],
                    Distance = toplist_dist[1:(1 + n)],
                    Index = toplist_vch$index[1:(1 + n)],
                    Rank = toplist_vch$rank[1:(1 + n)],
                    row.names = NULL,
                    stringsAsFactors = F)
  )
}







### MAIN ######################################################################

ALL_CLIST = GetCurrNames()

shinyServer(function(input, output, session){
  
  DATA = reactiveVal()
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
      E = GetMergedData(d0, d1)
      D = GetDist(E)
      VCH = ValueChanges(E)
      #updated_list = apply(E[, c("label", "Long_Name")], 1, function(x) {paste0(x, collapse = " - ")})
      updated_list = sapply(1:nrow(E), function(x) {paste(E[x, "label"], E[x, "Long_Name"], sep = " - ")})
      updated_list = unlist(updated_list)
      updateSelectInput(session, inputId = "SELECTCURR", choices = updated_list)
      DATA(list(EXC = E, DIST = D, VALCH = VCH))
    } else {
      updateSelectInput(session, inputId = "SELECTCURR", choices = c("none"))
      DATA(NULL)
    }
    
    SIMTABLE(SIMTABLE_DEFAULT)
  })
  
  ### Clustogram output ###
  output$CLUSTOGRAM = renderPlot({
    D = DATA()
    if (is.null(D)) {
      plot(1, type="n", axes=F, xlab="", ylab="")
    } else {
      hc = hclust(as.dist(D$DIST), method = "average")
      #plot(as.dendrogram(hc))
      den = dendro_data(hc, type = "rectangle")
      den$labels = merge(den$labels, D$VALCH, by = "label", sort = F)
      ggplot() + 
        geom_segment(data = segment(den), aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_text(data = label(den), aes(x, y, label = label, color = index), size = 5, hjust = 1, angle = 90, nudge_y = -0.02) +
        scale_y_continuous(expand = c(0.2, 0), limits = c(-0.07, NA)) +
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
    D = DATA()
    if (is.null(D)) {
      SIMTABLE(SIMTABLE_DEFAULT)
    } else {
      #currid = D$CodeDict[D$CodeDict$Long_Name == input$SELECTCURR, "Code"]
      currid = unlist(strsplit(input$SELECTCURR,  " - ", fixed = TRUE))[1]
      N = min(input$NUMCURR, nrow(D$CodeDict))
      Closest = GetClosest(D$DIST, D$VALCH, D$EXC, currid, N)
      SIMTABLE(Closest)
    }
  })
  
  ### Show the most similar currencies ###
  output$TABLE_CLOSEST = renderTable({
    SIMTABLE()
  })
  
})