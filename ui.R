source("data.R")

my_ui <- dashboardPage(
  
  dashboardHeader(
    title = span(tagList("COVID-19 Dashboard"))
  ),
  
  dashboardSidebar(
    width = 230,
    sidebarMenu(
      menuItem("Global Statistics", tabName = "GlobalMenu", icon = icon('globe')),
      menuItem("US Statistics", tabName = "USMenu", icon = icon('flag-usa')),
      menuItem("About", tabName = "Devs", icon = icon('address-card'))
    )
  ),
  
  dashboardBody(tags$head(tags$style(HTML('
      .img-local {
      }
      
      .small-box .img-local {
        position: absolute;
        top: auto;
        bottom: 5px;
        right: 5px;
        z-index: 0;
        font-size: 70px;
        color: rgba(0, 0, 0, 0.15);
      }
      
      .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
        background-color: #13212e;
      }
      
      .content-wrapper {
        background-color: #202324;
      }
    '))),
                
                tabItems(
                  tabItem(tabName = "GlobalMenu", 
                          fluidRow(
                            box(
                              title = "Global Map of Coronavirus",
                              id = 'tab6',
                              height = 'auto',
                              width = 12,
                              solidHeader = TRUE,
                              leafletOutput('globalmap', height = "500px")
                            )
                          ), 
                          
                          fluidRow(
                            box(
                              title = "Filters",
                              width = 6,
                              selectizeInput(
                                inputId = "region",
                                label = paste(
                                  "Select countries: (remove selection with backspace)"
                                ),
                                multiple = T,
                                choices = unique(df$region)
                              ),
                              
                              dateRangeInput(
                                "globaldate",
                                label = "Select date range:",
                                format = "m/dd/yyyy",
                                start = min(df$date),
                                end = max(df$date),
                                min = as.character(min(df$date)),
                                max = as.character(max(df$date))
                              ),
                              
                              verbatimTextOutput("globalsummary")
                            ),
                            
                            box(title = "Global Datatable",
                                width = 6,
                                dataTableOutput("globaltable"))
                          ), 
                          
                          fluidRow(box(
                            title = "Top 5 Countries by Cases",
                            solidHeader = TRUE,
                            width = 6,
                            plotOutput("confirmed_bar")
                          ),
                          
                          box(
                            title = "Top 5 Countries by Deaths",
                            solidHeader = TRUE,
                            width = 6,
                            plotOutput("deaths_bar")
                          ))
                  ),
                  
                  tabItem(tabName = "USMenu",
                          fluidRow(
                            box(
                              title = "US Map of Coronavirus",
                              id = 'tab6',
                              height = 'auto',
                              width = 12,
                              solidHeader = TRUE,
                              leafletOutput('nationalmap', height = "500px")
                            )
                          ),
                          
                          fluidRow(
                            box(
                              title = "Filters",
                              width = 6,
                              selectizeInput(
                                inputId = "state",
                                label = paste(
                                  "Select territories/states: (remove selection with backspace)"
                                ),
                                multiple = T,
                                choices = unique(df_state$state)
                              ),
                              
                              dateRangeInput(
                                "nationaldate",
                                label = "Select date range:",
                                format = "m/dd/yyyy",
                                start = min(df$date),
                                end = max(df$date),
                                min = as.character(min(df$date)),
                                max = as.character(max(df$date))
                              ),
                              
                              verbatimTextOutput("nationalsummary")
                            ),
                            
                            box(title = "US Datatable",
                                width = 6,
                                dataTableOutput("nationaltable"))
                          ), 
                          
                          fluidRow(box(
                            title = "Trendline in Confirmed Cases",
                            id = 'tab2',
                            height = 'auto',
                            width = 6,
                            solidHeader = TRUE, 
                            plotOutput("confirmed_plot")
                          ),
                          
                          box(
                            title = "New US Cases",
                            id = 'tab2',
                            height = 'auto',
                            width = 6,
                            solidHeader = TRUE, 
                            dataTableOutput("new_cases")
                          ))
                  ),
                  
                  tabItem(tabName = "Devs",
                          userDescription(
                            type = 2,
                            title = "Tony Su",
                            subtitle = "Data Analyst",
                            backgroundImage = "https://media.giphy.com/media/uQOj9dKyrg0JG/giphy.gif",
                            image = "https://media.giphy.com/media/xT1R9MyOQLEB2WY6Oc/giphy.gif"
                          ),
                          
                          userBox(
                            "Contacts:",
                            a("LinkedIn | ", href="https://www.linkedin.com/in/su-tony/", target="_blank"),
                            a("Github | ", href="https://github.com/su-tony", target="_blank"),
                            a("Email", href="mailto:stony00@yahoo.com", target="_blank"),
                            closable = TRUE,
                            width = 12,
                            background = "navy"
                          ),
                          
                          box(title = "Coronavirus Data Source",
                              id = 'link',
                              a(paste("New data updated daily", sep = '\n'), href="https://github.com/CSSEGISandData/COVID-19", target="_blank")     
                          ),
                          
                          box(title = "Icon Data Source",
                              id = 'link',
                              paste0("Source for icons: "),
                              a("Flaticon | ", href="https://www.flaticon.com/", target="_blank"),
                              a("Font Awesome", href="https://fontawesome.com/v6.0", target="_blank")
                          )
                  )
                )
  ),
  
  title = "Coronavirus Dashboard"
)