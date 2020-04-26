

# CSC405/Projects/H-1B")
# Author: Hugues Mbumba 
# Create better plotly plots allowing user inputs for years
# NAISC, Cities csv files from Github
# More in-depth analysis

#Loading dependent packages  ----
rqrd_Pkg = c('shiny','data.table','plotly','plyr','tidyverse','wordcloud2','ggmap')
for(p in rqrd_Pkg){
  if(!require(p,character.only = TRUE)) 
  install.packages(p);
  library(p,character.only = TRUE)
}

##Download the Data from Web ----
file_link = "https://www.uscis.gov/sites/default/files/USCIS/Data/Employment-based/H-1B/h1b_datahubexport-All.zip"
temp <- tempfile()
download.file(file_link, temp)
temp_2 = unzip(temp)

# Data Cleaning and transformation ----
Data <- ldply(temp_2, fread)
columns <-c("Initial Approvals", "Initial Denials", "Continuing Approvals", "Continuing Denials")
Data[, columns] <- lapply(columns, function(x) as.numeric(Data[[x]]))

colnames(Data) <- c("Year","Employer","Initial_Approvals","Initial_Denials"
,"Continuing_Approvals","Continuing_Denials","NAICS","Tax_ID"  
,"State", "City","ZIP")

#apply(Data, 2, function(x){sum(is.na(x))})  #check missing values
Data[is.na(Data)] <- 0  #Replace missing values

### Is there a decline in approvals----
a <- Data %>%
  group_by(Year) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
  C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  mutate(Denial_Rate = Denials/(Approvals+Denials)*100)

#plotly chart:
plot_ly(a , x = ~Year, y = ~Approvals, type = "scatter", mode = "lines", color = I('dark green'), name = "Approvals") %>%
  add_trace(x = ~Year, y = ~Denials, type = "scatter", mode = "lines", color = I('red'), name = "Denials") %>%
  layout(title = "H-1B Visas",
 xaxis = list(title = "Year"),
 yaxis = list (title = "Count"))


### Top Employers, Industries with most accepts, most denials? -----

#Got the following details from (USCIS) 
## Top Industries with most approvals/denials -- NAICS -
Dept <- read.csv("NAICS.csv")
colnames(Dept) <- c("NAICS","Dept_Name")
#Dept$NAICS <- as.factor(Dept$NAICS)

# ----
c <- left_join(Data, Dept)
c <- c %>%
  group_by(Year, Dept_Name) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
  C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  mutate(Denial_Rate = round(Denials/(Approvals+Denials)*100, digits=2))

                          
# Use geocode from ggmap to get lat,lon coordinates
# get_coords <- function(City){
#   coords <- geocode(City, source = 'dsk')
#   df <- cbind(City, coords)
#   return(df)
# }
# 
# coords_cities <- get_coords(cities$City)
# write.csv(coords_cities, file='City_Coordinates.csv', row.names=FALSE)

#Preparing cities data
cities <- Data %>%
  filter(Year > 2017) %>%
  group_by(City) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
            C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  arrange(desc(Approvals)) %>%
  top_n(50, Approvals)

coords_cities <- read.csv("City_Coordinates.csv")

cities <- left_join(cities, coords_cities, by="City")


# geo styling
g <- list(
  scope = 'north-america',
  showland = TRUE,
  landcolor = toRGB("grey83"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white"),
  showlakes = TRUE,
  lakecolor = toRGB("white"),
  showsubunits = TRUE,
  showcountries = TRUE,
  resolution = 50,
  projection = list(
    type = 'conic conformal',
    rotation = list(lon = -100)
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(-140, -55),
    dtick = 5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(15, 70),
    dtick = 5
  )
)

g <- list(
  scope = 'usa',
  showland = TRUE,
  landcolor = toRGB('light gray'),
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

Data %>%
  filter(Year > 2017) %>%
  group_by(City) %>%
  summarize(Approvals = sum(Initial_Approvals), Denials = sum(Initial_Denials),
            C_Approvals = sum(Continuing_Approvals), C_Denials = sum(Continuing_Denials)) %>%
  arrange(desc(Approvals)) %>%
  top_n(50, Approvals) %>%
  left_join(coords_cities, by="City") %>%
  plot_geo(lat = ~lat, lon = ~lon, color = ~Approvals, size=~(Approvals)) %>%
  add_markers(hovertext = ~(paste("City:", City, "\nNo. of Approvals:", Approvals))) %>%
  layout(title = 'Top cities with H-1B Approvals in 2019 & 2020', geo=g)


