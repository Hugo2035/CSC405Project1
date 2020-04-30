library(plotly)
navbarPage("H-1B Visas Analysis",
           tabPanel("All Data",
                    downloadButton('downloadData', 'Download'),
                    DT::dataTableOutput("table1")), 
           tabPanel("Approvals vs Declines",
                    sidebarLayout(sidebarPanel(
                      helpText("The total number of approvals and denials in the following graph, we realize that since 2015, approvals are dwindling.", 
                               "On the other hand, denials are surging.")),
                      mainPanel(plotlyOutput("plot1"))
                    )),
           tabPanel("Geographic",
                    sidebarLayout(sidebarPanel(
                      checkboxGroupInput(inputId = "Year", label = "Select Years",
                                         choices = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
                                         selected = c(2018,2019))
                    ),
                    mainPanel(
                      plotlyOutput("states"),
                      plotlyOutput("cities"))
                    )),
           tabPanel("Departments",
                    fluidRow(
                      helpText("Department wise approvals over the years.",
                               " ",
                               "Note: You can select which particular deparment you wish to look at by clicking or double clicking on the department name in the plot legends."),
                      plotlyOutput("dept_approval"),
                      helpText("\n
                                 Denial Rate is the percentage of denials over all the applications.
                                 \n As we can see all the deparments are witnessing more denials since 2016."),
                      plotlyOutput("dept_denial"))
           )
)