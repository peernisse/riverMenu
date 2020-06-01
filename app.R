

library(shiny)
library(shinyFiles)
library(tidyverse)
library(plyr)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(googlesheets)
library(rhandsontable)

#Testing
# gs<-gs_url('https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing')
# 
# LU_INGREDIENTS<-gs_read(gs,'LU_INGREDIENTS')
# LU_MEAL<-gs_read(gs,'LU_MEAL')
# LU_MEAL_TYPE<-gs_read(gs,'LU_MEAL_TYPE')
# XREF_INGREDIENT<-gs_read(gs,'XREF_INGREDIENT')
####



# Ap UI begin-----
ui <- dashboardPage(
    #links----------------
    
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
     
     tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
     ),
     
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
                             column(width = 12,
                               h2('Step 1: Create Trip'),
                               p('This will create a working directory on your C: drive 
                                 called "RiverMenu" for your trip data. You can thereby work on your menu 
                                 and return to it later.'),
                               textInput('tripName',"Trip Name", width = '90%'),
                               actionButton('createtrip','Create Trip',style="float:left;
                                            background-color:#007bff;color:white;"
                                            )
                               
                               )#End column
                           ),#End fluid row
                           hr(),
                           p('Below are the choices to build the menu on a by-meal
                              basis. Select "Add Meal" after each entry to add it to your menu.'),
                           fluidRow(
                             column(width=6,
                                    
                                    pickerInput('riverday',label = 'River Day',
                                                selected = '--Select River Day--',
                                                choices = c('--Select River Day--',seq(1:30))
                                                ),
                                    uiOutput('lumtype')
                                    
                                    
                                    
                                    
                                    ),#End column 1
                             
                             
                             #shinySaveButton('projsave',label='Save Menu',title='Save Project'),
                                    
                             column(width=6,
                                    
                                    pickerInput('nopeople',
                                                label = 'Number of People',
                                                
                                                selected = '--Select Number of People--',
                                                choices = c('--Select Number of People--',seq(1:30))),
                                    uiOutput('lumeal')
                                    
                              )#End column 2
                        
                           ),#End fluid row 
                           
                           fluidRow(
                             column(width =12,
                                    actionButton('commit',label='Add Meal',
                                                 style = "background-color:#007bff;color:white;",
                                                 icon = icon('download')
                                                 )#End button
                                    )#End column
                             
                           ),#End fluid row
                           
                           hr(),
                           
                           fluidRow(
                             column(width = 12,
                                        actionButton('save','Save Progress',
                                                     style = "background-color:#007bff;color:white;margin-bottom:12px;",
                                                     icon = icon('pencil')
                                        )#End button
                                    )#End column
                             
                             
                          ),#End fluid row
                           
                           fluidRow(
                            box(
                               title = 'Meals added to the Menu',
                               DTOutput('menulist'),width = 12
                               
                               
                             )#End box
                             
                             
                           ),#End fluid row
                          
                          p('The table below presents the ingredients and quantitites for each meal.
                               Select meals in the table above to filter this table.'),
                           
                           fluidRow(
                             
                             
                                 box(
                                   title = 'Selected Meal and Ingredients',
                                   DT::dataTableOutput('ingView'),
                                   width = 12
                                   
                                 )#End box
                             
                             
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
    
    pickerInput('choosemeal',label='Meal Name',
                selected = '--Select Meal--',
                choices = c('--Select Meal--',mls)
                )#End picker input
    
  })#End output lumeanl
  
  #Lookup meal type
  
  output$lumtype<-renderUI({
    pickerInput(
      'choosemealtype',label = 'Meal Type',
      selected = '--Select Meal Type--',
      choices = c('--Select Meal Type--',mtypes)
    )
    
  })#End lu meal type
  
  
  #Growing menu dataframe--------------
  
  # D <- "RIVER_DAY,NO_PEOPLE,MEAL_TYPE,MEAL\n'','','',''"
  # write(D, file = "data.csv")
  
  data <- reactiveValues(
    file = data.frame()
  )
  
  #Output menu table-----------------
   output$menulist<-renderDT(
     data$file,editable = TRUE, 
     style = "bootstrap", rownames = FALSE
   )
 
  
  
   #Surface the ingredients table filtered by selection from the meal table-----------
  
  #Create join of menu item and ingredients---------
  
  
  
  viewMenuIngredients<-gs_read(gs,'XREF_INGREDIENT') %>% 
    inner_join(gs_read(gs,'LU_MEAL')) %>% 
    inner_join(gs_read(gs,'LU_INGREDIENTS')) %>% 
    select(MEAL_NAME,INGREDIENT,INGREDIENT_DESCRIPTION,SERVING_SIZE_DESCRIPTION,
           SERVING_SIZE_FACTOR) %>% 
    arrange(MEAL_NAME,INGREDIENT)
  
  #Output the filtered table
   output$ingView<-DT::renderDataTable({
     
       sel <- input$menulist_rows_selected
       
       lookup<-data$file[sel,] %>% 
         mutate(NO_PEOPLE = as.numeric(NO_PEOPLE))
       
       ings<-viewMenuIngredients %>% 
         filter(MEAL_NAME %in% lookup$MEAL) %>% 
         left_join(lookup, by = c('MEAL_NAME' = 'MEAL')) %>% 
         as.data.frame(.) %>% 
         mutate(QUANTITY = round_any(SERVING_SIZE_FACTOR*NO_PEOPLE,1,ceiling)) %>% 
         select(MEAL_NAME,INGREDIENT,INGREDIENT_DESCRIPTION,SERVING_SIZE_DESCRIPTION,
                NO_PEOPLE,SERVING_SIZE_FACTOR,QUANTITY)
       
    
   })#End output ingView
  
  
  
  
  #Buttons observe and actions-------------------
 
   #Button to add meals to the menu-----------
   addData <- observeEvent(
     input$commit,
     {
       newLine <- data.frame(
         RIVER_DAY = input$riverday,
         NO_PEOPLE = as.numeric(input$nopeople),
         MEAL_TYPE = input$choosemealtype,
         MEAL = input$choosemeal,
         stringsAsFactors = FALSE
         
         )
       
       print(newLine)
       
       
       data$file <- bind_rows(data$file,newLine)
     }
   )
   
   #Button to save progress of the menu--------------
   saveData<-observeEvent(input$save,{
     
     
     
   })
   
   #Button to create a local directory to store the menu----------------------
   #CReate reactive value to store the local filepath for the session
   activePath <- reactiveVal()
   
   #Observe create trip button and make directories accordingly and set the path.
   #Then load the base data as csv files so the user can tweak as needed
   mkDir<-observeEvent(input$createtrip,{
     
     if(input$tripName == ""){
       showNotification('Select a Trip Name')
     } else
       
      if(input$tripName != "" & dir.exists(paste0('./RiverMenus/',input$tripName))){
        
        showNotification(paste0('./RiverMenus/',input$tripName,' already exists.'))
        
      } else
     
     if(input$tripName != "" & !dir.exists(paste0('./RiverMenus/',input$tripName))){

       #if(!dir.exists('./RiverMenus')){dir.create('./RiverMenus')}
       dir.create(paste0('./RiverMenus/',input$tripName))
       showNotification(paste0('./RiverMenus/',input$tripName,' has been created.'))
       activePath(paste0('./RiverMenus/',input$tripName))
       #print(activePath())
     }
   })
  
  
}#End server function

# Run the application 
shinyApp(ui = ui, server = server)

