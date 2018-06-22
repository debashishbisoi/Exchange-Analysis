#Dashborad example : https://gallery.shinyapps.io/087-crandash/
#Icons : https://fontawesome.com/icons?d=gallery


library(ggplot2)
library(plotly)
library(ggthemes)
library(dplyr)
library(DT)
library(ggthemes)
library(stringr)
library(shiny)
library(shinydashboard)

ExchangePrice <<- read.csv(file = "ExchangePrice.csv", header = TRUE, stringsAsFactors = FALSE)


names(ExchangePrice) <<- c("Provider", "Service.Name", "PDP", "Billed.By", "Country.exceptions", "Special.comments" ,
                          "Sntd.User.Charge..monthly.",  "Exchange.Currency", "Site.License.Charge",
                          "China.Price", "China.Currency", "Site.License.Charge...China", "USD.Price",
                          "EUR.Price", "GBP.Price")
ExchangePrice <<- ExchangePrice[, c("Provider", "Service.Name", "PDP", "Billed.By", "Special.comments","USD.Price" )]

DelayTime <<- read.csv(file = "ExchangeDelayTime.csv", header = TRUE, stringsAsFactors = FALSE)

names(DelayTime) <<- c("Exchange.Name", "Service.Name", "Exchange.Code", "Official.Delay.Time", "PDP.Code", "T1.Entitlement.Code.RT.DL")


Actperm <<- read.csv(file = "20180613_ActivePermissionByUser.csv", header = F, stringsAsFactors = F)

Actperm <<- Actperm[!(duplicated(Actperm)), ]


names(Actperm) <<- c("SiteName", "VService", "Login", "Login Instance", "UName", "UGroup", "UCostCtr", "Email", "UAccessPos", 
                    "Type", "Global_Name", "Description", "Activation")

Actperm <<- Actperm[-1,]

Actperm <<- Actperm[, c("Login", "UName", "UGroup", "UCostCtr", "Type", "Global_Name", "Description", "Activation")]

UserList <<- read.csv(file = "UserList.csv", header = T, stringsAsFactors = FALSE, na.strings = '')
names(UserList) <<- c("site", "login", "name", "user_group", "department" , "access_pos", "deactivation", "ae", "uadeactivation")

PermissionList <<- read.csv(file = "PermissionList.csv", header = T, stringsAsFactors = FALSE, na.strings = '')


names(PermissionList)  <- c ("DacsId", "Name", "Location", "Department", "Type", "PDPCode", "Description", "ActivationDate", "Deactivation", "AccessType")
PermissionList$ActivationDate <- as.Date(PermissionList$ActivationDate)
PermissionList$Deactivation <- as.Date(PermissionList$Deactivation)

SiteSubscription <<- read.csv(file = "SiteSubscription.csv", header = T, stringsAsFactors = FALSE, na.strings = '')
names(SiteSubscription) <<- c("Site", "PDP", "Descrition")


Usage <<- read.csv(file = "1yearusage.csv", header = TRUE, stringsAsFactors = FALSE)

Usage <<- Usage[, c("Type", "Login", "UName", "UGroup", "UCostCtr", "Global_Name", "Description", "Item", "ItemCount", "RequestType")]

Usage$ISUsed <<- "Used"

Actperm$Status <<- "Permissioned"

SiteSubscription$UserPermission <<- Actperm$Status[match(SiteSubscription$PDP, Actperm$Global_Name)]
SiteSubscription[which(is.na(SiteSubscription$UserPermission)), "UserPermission"] <<- "Not Permissionned"


UserCount <<- Actperm %>%
  group_by(Global_Name)%>%
  summarise(user_count = n())

SiteSubscription$Numberofusers <<- UserCount$user_count[match(SiteSubscription$PDP, UserCount$Global_Name)]
SiteSubscription$ExchangePrice <<- ExchangePrice$USD.Price[match(SiteSubscription$PDP, ExchangePrice$PDP)]
SiteSubscription$TotalCost <<- SiteSubscription$Numberofusers * SiteSubscription$ExchangePrice
SiteSubscription$Is_Used <<- Usage$ISUsed[match(SiteSubscription$PDP, Usage$Global_Name)]
SiteSubscription$BilledBy <<- ExchangePrice$Billed.By[match(SiteSubscription$PDP, ExchangePrice$PDP)]
SiteSubscription[which(is.na(SiteSubscription$Is_Used)), "Is_Used"] <<- "Not Used"
SiteSubscription[which(is.na(SiteSubscription$ExchangePrice)), "ExchangePrice" ] <<- "NA"
SiteSubscription[which(is.na(SiteSubscription$BilledBy)), "BilledBy"] <<- "Direct Billed"


SiteSubscription$PDP <<- as.factor(SiteSubscription$PDP)
SiteSubscription$UserPermission <<- as.factor(SiteSubscription$UserPermission)
SiteSubscription$Is_Used <<- as.factor(SiteSubscription$Is_Used)
SiteSubscription$Numberofusers <<- as.numeric(SiteSubscription$Numberofusers)

ui <- shinyUI(
  dashboardPage(title = "DACS Report Module", skin = "green",
                dashboardHeader(title = "DACS Report Dashboard"),
                dashboardSidebar(
                  #sliderInput("bins", "Number of bins in chart",1 , 100, 50),
                  sidebarMenu(
                    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                    menuItem("User Permission", tabName = "ActivePermission", icon = icon("file") ),
                    menuItem("Site Report", tabName = "SiteReport"),
                    menuSubItem("Exchange Detail", tabName = "ExchangeDetails"),
                    # menuSubItem("Exchange Time", tabName = "ExchangeTime"),
                    menuItem("Usage Details", tabName = "Usage", icon = icon("file"))
                    
                  )),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "dashboard",
                            fluidRow(
                              infoBox("Site", value = "SMO_CA", color = "green"),
                              infoBox("Reuters Servers", value = "rcotrep01,rcotrep02,stktrep01,stktrep02", 
                                      color = "fuchsia", fill = T, href = "http://10.65.78.42:8080/dacs/"),
                              infoBoxOutput("ActiveUsers")
                              
                            ),
                            fluidRow(
                              box(collapsible = TRUE, title = "Exchange Distribution across User Groups", solidHeader = T, 
                                  status = "primary", background = "aqua",width = NULL,collapsed = TRUE,
                                  plotlyOutput("histogram", height = "500px"))
                              
                            ),
                            fluidPage(
                              # box(collapsible = TRUE, title = "Data Analysis CHart", solidHeader = T, 
                              #     status = "primary", background = "aqua",width = NULL,collapsed = TRUE,
                              #     plotlyOutput("plot", height = "500px")),
                              # title = "Diamonds Explorer",
                              plotlyOutput("plot"),
                              hr(),
                            fluidRow(
                              column(3,
                                     h4("Exchange Analysis Chart"),
                                     sliderInput('sampleSize', 'Sample Size', 
                                                 min=1, max=nrow(SiteSubscription),
                                                 value=min(1, nrow(SiteSubscription)), 
                                                 step=10, round=0),
                                     br(),
                                     checkboxInput('jitter', 'Jitter'),
                                     checkboxInput('smooth', 'Smooth')
                              ),
                              column(4, offset = 1,
                                     selectInput('x', 'X', names(SiteSubscription)),
                                     selectInput('y', 'Y', names(SiteSubscription), names(SiteSubscription)[[2]]),
                                     selectInput('color', 'Color', c('None', names(SiteSubscription)))
                              ),
                              column(4,
                                     selectInput('facet_row', 'Facet Row',
                                                 c(None='.', names(SiteSubscription[sapply(SiteSubscription, is.factor)]))),
                                     selectInput('facet_col', 'Facet Column',
                                                 c(None='.', names(SiteSubscription[sapply(SiteSubscription, is.factor)])))
                              )
                              
                            ))
                            
                            ),
                    tabItem(tabName = "ActivePermission",
                            h1("Reuters User Permissions"),
                            fluidRow(
                              box(DT::dataTableOutput("table1"), width = 12)
                            )
                            
                    ),
                    tabItem(tabName = "SiteReport",
                            h2("Site Level Subscription"),
                            fluidRow(
                              box(DT::dataTableOutput("site"), width = 12)
                            )

                    ),
                    
                    tabItem(tabName = "ExchangeDetails" , h2("ExchangeDetails"),
                            fluidRow(
                              box(title = "Exchange Cost", solidHeader = T, DT::dataTableOutput("ExchangePrice"))
                              
                              ,
                              box(title = "Exchange Delay Time", solidHeader = T,DT::dataTableOutput("DelayTime"))
                            )),
                    
                    tabItem(tabName = "Usage", h2("User Usage Report"),
                            fluidRow(
                              box(DT::dataTableOutput("Usage"), width = 12)
                            ))
                    
                  )
                  
                  
                )
  )
)


server <- shinyServer(function(input, output){
  
  dataset <- reactive({
    SiteSubscription[sample(nrow(SiteSubscription), input$sampleSize),]
  })
  
  output$histogram <- renderPlotly({
    p <-ggplot(subset(Actperm, Type == "EXCHANGE"), aes(x = Global_Name, fill = UGroup,label = Description))+
      geom_histogram(binwidth = 5, stat = "count")+
      ggtitle("Exchange Distribution")+ labs(x = "Exchange" , y = "Total Count", fill = "User Group")+
      theme_economist(dkpanel = TRUE, base_family="Verdana", stata = TRUE) +
      theme(legend.title = element_text(size=12, color = "firebrick"),legend.text = element_text(size=10))
    p <- plotly_build(p)
    p <- ggplotly(p)
    
    # ggplotly(p)
    # print(p)
  })
  

  
  output$plot <- renderPlotly({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y), label = TotalCost) + geom_point()
    # p<- plotly_build(p)
    
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$jitter)
      p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()
    
    p<- ggplotly(p)
    # print(p)
    
  })
  
  output$ActiveUsers <- renderInfoBox({
    
    x <-   UserList %>%
      select(site, login, name, user_group, department, access_pos, deactivation, ae, uadeactivation) %>%
      filter(access_pos ==1 & !user_group == '_SYSTEM_' & is.na(uadeactivation == '')) %>%
      count()
    x1 <- as.data.frame(x)
    infoBox("Active Users", x1, icon = icon("users"))
  })
  
  
  output$table1 <- DT::renderDataTable({
    
    table1 <- PermissionList %>%
      select(DacsId, Name, Location, Department, Type, PDPCode, Description, ActivationDate, Deactivation, AccessType)%>%
      filter(!Name == " ")
    
    
    datatable(table1, 
              colnames = c("DacsID","User", "Location", "Department","Type", 
                           "PDP", "Name", "Activation Date", "Deactivation Date", "Access Type")
              ,filter = 'top',style = 'bootstrap',rownames = FALSE,extensions = "Buttons",
              class = 'container-fluid',
              options = list(pageLength = 10, autoWidth = TRUE,scrollX = TRUE,scrollY = 400,
                             columnDefs = list(list(width = '100px', targets = "_all")),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"),dom = "Blfrtip"
                             , buttons = 
                               list("copy", list(
                                 extend = "collection"
                                 , buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                 , text = "Download"
                               ) ), lengthMenu = list( c(10, 20, -1) # declare values
                                                       , c(10, 20, "All") # declare titles
                               )
                             
              )
    )
    
    
  })
  
  output$site <- DT::renderDataTable({
    


    datatable(SiteSubscription, colnames = c("Site", "PDP", "Name", "UserPermission", "Numberofusers", "ExchangePrice", "TotalCost", "Is_Used", "BilledBy")
              ,filter = 'top',style = 'bootstrap',rownames = FALSE,extensions = "Buttons",
              class = 'container-fluid',
              options = list(pageLength = 10, autoWidth = TRUE,scrollX = TRUE,scrollY = 400,
                             columnDefs = list(list(className = 'dt-center', width = '100px', targets = "_all")),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"),dom = "Blfrtip"
                             , buttons = 
                               list("copy", list(
                                 extend = "collection"
                                 , buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                 , text = "Download"
                               ) ), lengthMenu = list( c(10, 20, -1) # declare values
                                                       , c(10, 20, "All") # declare titles
                               )
                             
              )
    )
    
  })
  
  output$ExchangePrice <- DT::renderDataTable({
    datatable(ExchangePrice,
              filter = 'top',style = 'bootstrap',rownames = FALSE,
              class = 'container-fluid',
              options = list(pageLength = 10, autoWidth = TRUE,scrollX = TRUE,scrollY = 400,
                             columnDefs = list(list(width = '70px', targets = "_all")),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"),dom = "Blfrtip"
                             , buttons = 
                               list("copy", list(
                                 extend = "collection"
                                 , buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                 , text = "Download"
                               ) ), lengthMenu = list( c(10, 20, -1) # declare values
                                                       , c(10, 20, "All") # declare titles
                               )
                             
              ))
  })
  
  output$DelayTime <- DT::renderDataTable({
    datatable(DelayTime,
              filter = 'top',style = 'bootstrap',rownames = FALSE,
              class = 'container-fluid',
              options = list(pageLength = 10, autoWidth = TRUE,scrollX = TRUE,scrollY = 400,
                             columnDefs = list(list(width = '70px', targets = "_all")),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"),dom = "Blfrtip"
                             , buttons = 
                               list("copy", list(
                                 extend = "collection"
                                 , buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                 , text = "Download"
                               ) ), lengthMenu = list( c(10, 20, -1) # declare values
                                                       , c(10, 20, "All") # declare titles
                               )
                             
              ))
  })
  
  
  output$Usage <- DT::renderDataTable({
    Usage <- read.csv(file = "1yearusage.csv", header = TRUE, stringsAsFactors = FALSE)
    
    names(Usage)
    
    Usage <- Usage[, c("Type", "Login", "UName", "UGroup", "UCostCtr", "Global_Name", "Description", "Item", "ItemCount", "RequestType")]
    
    
    datatable(Usage, colnames = c("Type", "DacsId", "User", "Group", "CostCtr", "PDP", "Name", "Item", "ItemCount", "RequestType"),
              filter = 'top',style = 'bootstrap',rownames = FALSE,
              class = 'container-fluid',extensions = 'Buttons',
              options = list(pageLength = 10, autoWidth = TRUE,scrollX = TRUE,scrollY = 400,
                             columnDefs = list(list(width = '70px', targets = "_all")),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"),dom = "Blfrtip"
                             , buttons = 
                               list("copy", list(
                                 extend = "collection"
                                 , buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                 , text = "Download"
                               ) ), lengthMenu = list( c(10, 20, -1) # declare values
                                                       , c(10, 20,"All") # declare titles
                               )
                             
              ))
    
  })
  
  
  
})



shinyApp(ui = ui, server = server)



