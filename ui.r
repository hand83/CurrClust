### IMPORTED LIBRARIES ########################################################

library(shiny)



### GLOBAL VARIABLES ##########################################################

APPNAME = "CurrClust"
TITLE = "Currency Clusterer"
FOOTNOTE = paste(APPNAME, "2017")
Description_l1 = paste("CurrClust calculates the distance between currencies based on cosine similarity.",
                       "The calculation does not rely on any particular base currency, each currency is compared to all the other currencies.", 
                       sep = " ")
Description_l2 = paste("An index is calculated for each currency. This indicates the per-cent changes in the value of the given currency between the time points.",
                       "Positive values indicate that the selected currency is getting stronger relative to the rest of currencies, while negative values indicate the opposite of that.",
                       "Zero or almost zero value indicates that the selected currency changes minimally to the rest of currencies or the changes compared to other currencies are rather balanced.",
                       "The index is based on the assumption that the net currency changes is be zero for the given set; weakening or strenghtening happens in a balanced way.",
                       sep = " ")
Description_l3 = paste("The rank is the position of a currency on the toplist of the strength index.", 
                       sep = " ")
Description_l4 = "How to use:"
Description_l5 = paste("Pick two dates to select a time interval you are interested in.",
                       "CurrClust will calculate the changes in exchange rates between these time points and draws a histogram based on the similarities of the directions of the changes.",
                       "After that, you can select a specific currrencies to view the most similar currencies to that.",
                       "The resulting table shows the Distance, Index and Rank values.",
                       sep = " ")
Description_l6 = paste("CurrClust uses the freely available data of European Central Bank, which include only a limited number of currencies.",
                       "The data are accessed via the fixer.io API.",
                       sep = " ")


### MAIN ######################################################################

shinyUI(fluidPage(
  
  ### head ###
  withTags({
    head(
      title(APPNAME),
      style("
            body {
            background-color: #f8f8f8;
            }
          
            .header {
              text-align: center;
              padding: 10px 5px 20px;
            }

            .description {
              padding: 0px 5px 0px;
              font-size: 110%;
              text-align: justify;
              margin: 0px;
            }

            p {
            margin-top: 0px;
            margin-bottom: 5px;
            color: gray;
            }

            .footer {
              text-align: center;
              padding: 10px;
            }

            .daterange {
              padding: 5px 0px 20px;

            }

            .selectcurr{
              padding: 20px 0px 5px;
            }

            .MostSimilarOutput {
              background-color: #ffffff;
              border-radius: 5px;
              padding: 5px;
            }

            .ClustogramOutput {
              background-color: #ffffff;
              border-radius: 5px;
              padding: 5px;
            }

            #CLUSTOGRAM_MSG {
              font-size: 75%;
              color: #f74b42;
            }

            ")
    )
  }),


  
  ### header ###
  withTags({
    div(
      class = "header",
      h3(TITLE)
    )
  }),

  withTags({
    hr()
  }),

  sidebarLayout(
    
    ### sidebar ###
    sidebarPanel(width = 3,
      
      withTags({
        div(
          class = "daterange",
          dateRangeInput(inputId = "DATERANGE",
                         label = "Time interval:",
                         min = "1999-01-01",
                         max = as.POSIXct(Sys.Date(), format='%Y-%m-%d'),
                         start = as.POSIXct(Sys.Date()-7, format='%Y-%m-%d'),
                         end = as.POSIXct(Sys.Date(), format='%Y-%m-%d'),
                         weekstart = 1),
          actionButton(inputId = "SEND_DATERANGE", "submit")
          )
      }),
      
      br(),
      
      withTags({
        div(
          class = "selectcurr",
          selectInput(inputId = "SELECTCURR",
                      label = "Select currency to which you want to get the most similar currencies:",
                      choices = c("none")),
          numericInput(inputId = "NUMCURR",
                       label = "Number of most similar currencies:",
                       value = 5,
                       min = 0,
                       step = 1),
          actionButton(inputId = "SEND_SELECTCURR", "submit")
        )
      })
    ),
    
    
    
    ### main panel ###
    mainPanel(width = 9,
      
      column(width = 5, align = "center",
        withTags({
          div(
            class = "MostSimilarOutput",
            h4(align = "center", "Most similar currencies"),
            tableOutput("TABLE_CLOSEST")
          )
        })  
      ),
      
      column(width = 7, align = "center",
        withTags({
          div(
            class = "ClustogramOutput",
            h4("Distance tree"),
            plotOutput("CLUSTOGRAM"),
            htmlOutput("CLUSTOGRAM_MSG")
          )
        })
      )
    )
  ),
  
  withTags({
    hr()
  }),
  
  ### description
  withTags({
    div(
      class = "description",
      p(Description_l1),
      p(Description_l2),
      p(Description_l3),
      p(Description_l4),
      p(Description_l5),
      p(Description_l6),
      p("Reference to the data sources:"),
      a("fixer.io", href = "http://fixer.io/"),
      br(),
      a("ECB", href = "http://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html")
      
    )
  }),

  withTags({
    hr()
  }),
  
  ### footer ###
  withTags({
    div(
      class = "footer",
      p(FOOTNOTE)
    )
  })
 
))


