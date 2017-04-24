### IMPORTED LIBRARIES ########################################################

library(shiny)
library(jsonlite)
library(curl)



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
  
  df = data.frame(
    names(ratelist$rates), 
    unlist(ratelist$rates), 
    stringsAsFactors = FALSE, 
    row.names = NULL)
  names(df) = c("Currency", "Rate")
  df = rbind(df, 
             data.frame(
               Currency = c(BASE), 
               Rate = c(1))
  )
  df = df[order(df$Currency), ]
  
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
  df = merge(exc0$Exchange, exc1$Exchange, by = "Currency", all = FALSE)
  names(df)[2:3] = c("Rate0", "Rate1")
  
  # Get the long names of the currencies
  if ( is.null(nrow(ALL_CLIST)) ) {
    ALL_CLIST = GetCurrNames()
  }
  LN = sapply(df$Currency, function(x){ALL_CLIST[ALL_CLIST$Code == x, "Long_Name"]})
  CD = data.frame(Code = df$Currency,
                  Long_Name = unlist(LN),
                  row.names = NULL)
  
  # All to all conversion rates
  m0 = sapply(df$Rate0, function(x) {df$Rate0/x})
  m1 = sapply(df$Rate1, function(x) {df$Rate1/x})
  
  # Vectors of currency rate changes between time points
  Diff = m1 - m0
  
  # Obtain cosine distances between currencies
  Dist = apply(Diff, 1, function(x) {apply(Diff, 1, function(y) {DistFunction(x, y)})})
  rownames(Dist) = df$Currency
  colnames(Dist) = df$Currency
  
  return( list(Distance = Dist,
               CodeDict = CD)
  )
}



GetClosest = function(dist, curr, n) {
  # Returns the most similar currencies
  
  toplist = dist$Distance[order(dist$Distance[, curr]), curr]
  
  
  return(data.frame(Code = names(toplist)[2:(1 + n)],
                    Currency = sapply(names(toplist)[2:(1 + n)], function(x) {dist$CodeDict[dist$CodeDict$Code == x, "Long_Name"]}),
                    Distance = toplist[2:(1 + n)],
                    row.names = NULL)
  )
}



### MAIN ######################################################################

ALL_CLIST = GetCurrNames()

shinyServer(function(input, output, session){
  
  DIST = reactiveVal()
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
      updateSelectInput(session, inputId = "SELECTCURR", choices = D$CodeDict$Long_Name)
      DIST(D)
    } else {
      D = NULL
      updateSelectInput(session, inputId = "SELECTCURR", choices = c("none"))
      DIST(D)
    }
    
    SIMTABLE(SIMTABLE_DEFAULT)
  })
  
  ### Clustogram output ###
  output$CLUSTOGRAM = renderPlot({
    D = DIST()
    if (is.null(D)) {
      plot(1, type="n", axes=F, xlab="", ylab="")
    } else {
      hc = hclust(as.dist(D$Distance), method = "average")
      plot(as.dendrogram(hc))
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
    if (is.null(D)) {
      SIMTABLE(SIMTABLE_DEFAULT)
    } else {
      currid = D$CodeDict[D$CodeDict$Long_Name == input$SELECTCURR, "Code"]
      N = min(input$NUMCURR, nrow(D$CodeDict))
      SIMTABLE(GetClosest(D, currid, N))
    }
  })
  
  ### Show the most similar currencies ###
  output$TABLE_CLOSEST = renderTable({
    SIMTABLE()
  })
  
})