### IMPORTED LIBRARIES ########################################################

library(shiny)



### GLOBAL VARIABLES ##########################################################

APPNAME = "CurrClust"
TITLE = "Currency Clusterer"
FOOTNOTE = paste(APPNAME, "2017")



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
      
      column(width = 4, align = "center",
        withTags({
          div(
            class = "MostSimilarOutput",
            h4(align = "center", "Most similar currencies"),
            tableOutput("TABLE_CLOSEST")
          )
        })  
      ),
      
      column(width = 8, align = "center",
        withTags({
          div(
            class = "ClustogramOutput",
            h4("Similarity relations"),
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
      p("Pick a time interval to get the distances between currencies. The changes in currency exchange rates between the time points are converted into a distance measure which is base independent. In other words, the relationships are not shown against any particular currency but on the whole currency landscape. If you pick up a currency, you will get the most similar currencies with the distance values."),
      p("Data sources:"),
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


