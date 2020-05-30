

library(shiny)
library(shinyFiles)
library(tidyverse)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(googlesheets)


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
                   
           ),#End sidebar menu
           
           sidebarMenu(
             menuItem("View Data Tables", tabName = "viewdfs", icon = icon("glasses"))
             
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
                             textInput('tripName',"Trip Name")
                           ),
                           fluidRow(
                             column(width=6,
                                    
                                    uiOutput('lumeal')
                                    
                                    
                                    
                                    ),#End column 1
                             
                             
                             #shinySaveButton('projsave',label='Save Menu',title='Save Project'),
                                    
                             column(width=6,
                                    
                                    uiOutput('lumtype')
                           )#End column 2
                        
                           )#End fluid row  
                           
                        ),#End tab item planner
             
             
              tabItem(tabName = 'viewdfs',
                      
                        h1('Data Tables'),
                        
                        fluidRow(
                          
                          box(title = 'Ingredient List',
                              DTOutput('LU_INGREDIENTS'),collapsible = TRUE,collapsed = TRUE,
                              width = 12
                          ),#End box
                          box(title = 'Meal List',
                              DTOutput('LU_MEAL'),collapsible = TRUE,collapsed = TRUE,
                              width = 12
                          ),#End box
                          box(title = 'Meal TYpe Lookup',
                              DTOutput('LU_MEAL_TYPE'),collapsible = TRUE,collapsed = TRUE,
                              width = 12
                          ),#End box
                          box(title = 'Menu Meal and Ingredient Cross-Reference',
                              DTOutput('XREF_INGREDIENT'),collapsible = TRUE,collapsed = TRUE,
                              width = 12
                          )#End box
                        
                        )#End fluid row
                      
                        
                        
                      
                      
                      
                      )#End tab item viewdfs
             
                )#End tab items
           
           
   )#End dashboard body
   
)#end fluid page

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  #Import Data-------------------------------------------------------
  #Register google sheet with the data frames
  gs<-gs_url('https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing')
   
  #Read in data frames---------
  output$LU_INGREDIENTS<-renderDT({gs_read(gs,'LU_INGREDIENTS')})
  output$LU_MEAL<-renderDT({gs_read(gs,'LU_MEAL')})
  output$LU_MEAL_TYPE<-renderDT({gs_read(gs,'LU_MEAL_TYPE')})
  output$XREF_INGREDIENT<-renderDT({gs_read(gs,'XREF_INGREDIENT')})
  
  #Lookup values----------
  mls<-gs_read(gs,'LU_MEAL') %>% pull(MEAL_NAME) %>% sort()
  mtypes<-gs_read(gs,'LU_MEAL_TYPE') %>% pull(MEAL_TYPE) %>% sort()
  
  #Dropdowns----------------
  
  #Lookup meal name
  output$lumeal<-renderUI({
    
    pickerInput('choosemeal',label='Select a Meal',
                choices = mls
                )#End picker input
    
  })#End output lumeanl
  
  #Lookup meal type
  
  output$lumtype<-renderUI({
    pickerInput(
      'choosemealtype',label = 'Select Meal Type',
      choices = mtypes
    )
    
  })#End lu meal type
  
  
  
  
}#End server function

# Run the application 
shinyApp(ui = ui, server = server)

