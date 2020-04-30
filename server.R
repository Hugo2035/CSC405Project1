function(input, output) {
  
  rqrd_Pkg = c('shiny','plotly','plyr','tidyverse','ggmap','DT')
  for(p in rqrd_Pkg){
    if(!require(p,character.only = TRUE)) 
    install.packages(p);
    library(p,character.only = TRUE)
  }
  
  file_link = "https://www.uscis.gov/sites/default/files/USCIS/Data/Employment-based/H-1B/h1b_datahubexport-All.zip"
  temp <- tempfile()
  download.file(file_link, temp)
  temp_2 = unzip(temp)
  
  # temp_2 = c("h1b_datahubexport-2009.csv","h1b_datahubexport-2010.csv","h1b_datahubexport-2011.csv"
  # ,"h1b_datahubexport-2012.csv","h1b_datahubexport-2013.csv","h1b_datahubexport-2014.csv"
  # ,"h1b_datahubexport-2015.csv","h1b_datahubexport-2016.csv","h1b_datahubexport-2017.csv"
  # ,"h1b_datahubexport-2018.csv","h1b_datahubexport-2019.csv")
  
  #Load the data 
  Data <- purrr::map_dfr(temp_2, readr::read_csv)
  columns <-c("Initial Approvals", "Initial Denials", "Continuing Approvals", "Continuing Denials")
  Data[, columns] <- purrr::map(columns, function(x) as.numeric(Data[[x]]))
  
  # Data Cleaning and transformation
  colnames(Data) <- c("Year","Employer","Initial_Approvals","Initial_Denials"    
                      ,"Continuing_Approvals","Continuing_Denials","NAICS","Tax_ID"              
                      ,"State", "City","ZIP")
  Data[is.na(Data)] <- 0
  
  #cleaning Employer field
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  drop_words <- function (x) gsub("INC|LLC|L L C|LLP|CORPORATION|CORP", "", x) 
  Data$Employer <- trim(drop_words(Data$Employer))
  
  Data <- cbind(Data[1:6],apply(Data[7:11],2,as.factor))
  Data$Employer <- as.factor(Data$Employer)
  
  ## Top Industries with most approvals/denials -- NAICS ----
  Dept <- read.csv("NAICS.csv")
  #Dept <- read_csv("NAICS.csv")
  colnames(Dept) <- c("NAICS","Dept_Name")
  Dept$NAICS <- as.factor(Dept$NAICS)
  
  c <- left_join(Data, Dept)
  
  Dept_Data <- c %>%
    group_by(Year, Dept_Name) %>%
    summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
              C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
    mutate(Denial_Rate = round(Denials/(Approvals+Denials)*100, digits=2))
  
  #Preparing cities data
  coords_cities <- read.csv("City_Coordinates.csv")
  #coords_cities <- read_csv("City_Coordinates.csv")
  
  #Creating all Output objects -------
  output$table1 <- DT::renderDataTable({
    Data
  }, filter='top', 
  options = list(pageLength = 10, scrollX=TRUE, autoWidth = TRUE, columnDefs = list(list(width = '200px', targets = 2))))
  
  ## Download Buttons ----
  output$downloadData <- downloadHandler(
    filename = 'Download.csv',
    content = function(file) {
      write.csv(Data[input[["table1_rows_all"]],], file, row.names = FALSE)
    }
  )
  
  #plotly chart of total approvals and denials:
  output$plot1 <- renderPlotly({
    Data %>%
      group_by(Year) %>%
      summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
                C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
      plot_ly(x = ~Year, y = ~Approvals, type = "scatter", mode = "lines", color = I('dark green'), name = "Approvals") %>%
      add_trace(x = ~Year, y = ~Denials, type = "scatter", mode = "lines", color = I('red'), name = "Denials") %>%
      layout(title = "H-1B Visas by Year",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Count"))
    
  })
  
  
  #Which Industries/departments are doing well ---
  #No. of approvals plot
  output$dept_approval <- renderPlotly({
    plot_ly(Dept_Data, x = ~Year, y=~Approvals, color =~Dept_Name, type='scatter', mode = 'line') %>%
      layout(title = "H-1B Visas Approvals by Department",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Count"))
  })
  
  #Denial rate plot
  output$dept_denial <- renderPlotly({
    plot_ly(Dept_Data, x = ~Year, y=~Denial_Rate, color =~Dept_Name, type='scatter', mode = 'line') %>%
      layout(title = "H-1B Visas Denial Rate by Department",
             xaxis = list(title = "Year"),
             yaxis = list(range = c(0,50), title = "% Denials"))
  })
  
  #Geographic analysis --- 
  #Plotting top 10 states with max approvals in last 2 years using plotly
  output$states <- renderPlotly({
    Data %>%
      filter(Year %in% input$Year) %>%
      group_by(State) %>%
      summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
                C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
      arrange(desc(Approvals)) %>%
      top_n(10, Approvals) %>%
      plot_ly(x= ~(factor(State, levels=unique(State))[order(Approvals, decreasing = TRUE)]), 
              y=~Approvals, type='bar') %>%
      layout(title = "Top 10 states with highest approvals in the selected Years",
             xaxis = list(title = "State"),
             yaxis = list(title = "Approvals"))
  })
  
  # geo styling for plot_geo
  g <- list(
    scope = 'usa',
    showland = TRUE,
    landcolor = toRGB('light gray'),
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  
  # Plot of top cities
  output$cities <- renderPlotly({
    Data %>%
      filter(Year %in% input$Year) %>%
      group_by(City) %>%
      summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
                C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
      arrange(desc(Approvals)) %>%
      top_n(50, Approvals) %>%
      left_join(coords_cities, by="City") %>%
      plot_geo(lat = ~lat, lon = ~lon, color = ~Approvals, size=~(Approvals)) %>%
      add_markers(hovertext = ~(paste("City:", City, "\nNo. of Approvals:", Approvals))) %>%
      layout(title = 'Top cities with H-1B Visa approvals in the selected Years', geo=g)
  })
}
