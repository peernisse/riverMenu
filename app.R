

library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)


# Ap UI begin-----
ui <- dashboardPage(

   #HEader begin---------
   dashboardHeader(
           title="Trip Menu Planner"
   ),#End db header
   
   #Sidebar begin------------
   dashboardSidebar(
           
           sidebarMenu(
                   menuItem("Home", tabName = "home", icon = icon("home"))
                   
           ),#End sidebar menu
           sidebarMenu(
                   menuItem("Menu Planner", tabName = "menuplanner", icon = icon("pencil"))
                   
           )#End sidebar menu
           
   ),#End db sidebar
   
   #Pages begin----------------
   dashboardBody(
           tabItems(
                   # Home tab content----------------
             tabItem(tabName = "home",
                   fluidRow(
                           
                       column(width = 6,
                                h1("River Trip Menu Planner"),
                                hr(),
                                h2('Description'),
                                p("This menu planning app is based on years of river trip experience, both
                            commercial and private. It was initially develped in MS Access and has 
                           had a lot of ingrediants and pre-designed meals loaded for the user to choose 
                           from. There is also the option to add new ingrediants and create new meals."),
                                h2('Instructions'),
                                p('Put instruction statement here...'),
                                tags$ul(
                                        tags$li('Bullet 1')
                                )
                              
                              ),#End column 1
                       
                       column(width = 6,
                              
                              tags$img(src = 'tammi.jpg',width='500px',height='400px')
                              
                              
                              
                              )#End column 2
                           
                   ),#End fluid row
                   
                   fluidRow(
                     column(width = 12,style="float: left;padding-top:10px;",
                            
                            tags$img(src = 'kayak.jpg',width='350px',height='300px'),
                            tags$img(src = 'cataractsign.jpg',width='350px',height='300px'),
                            tags$img(src = 'riverNest.jpg',width='350px',height='300px')
                            
                            
                            )
                     
                     
                     
                   )
                  ),#End tab item Home
                   
                   #Planner tab content------------
                   tabItem(tabName = "menuplanner",
                           
                           h1('Build Trip Menu'),
                           hr(),
                           fluidRow(
                             column(width=6,
                                    textInput('tripName',"Trip Name")),
                             column(width=6,
                                    p("Placeholder")
                                    
                                    )
                           )
                           
                           
                           )#End tab item planner
                )#End tab items
           
           
   )#End dashboard body
   
)#end fluid page

# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
   
}#End server function

# Run the application 
shinyApp(ui = ui, server = server)

