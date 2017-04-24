# CurrClust
Currency Clusterer Shiny Web Application

This is a Shiny web application written in RStudio.
The app collects currency exchange data from the fixer.io API.

Input options: 

  input dates
    Data are downloaded at the dates.
    The app checks if data is collected from different dates indeed.
    Warnings are displayed if the fetched data corresponds to a different date than selected.
    Error is displayed if the data is fetched for the same date. Distance cannot be calculated in this case. (division by zero)
    The app calculates a distance matrix based on the currency exchange data. (Details in ClustCurr_Description.docx)
    
  currency selection and length of list
    You can pick a currency to list those currencies which are the most similar to it.
    The length of the list cannot be longer than the number of the available currencies.
  
A working version of the App is available at:
https://hand83.shinyapps.io/currclust/
