

library(shiny)
library(shinyFiles)
library(tidyverse)
library(openxlsx)
library(plyr)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(googlesheets)
library(rhandsontable)
library(markdown)

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
           
           sidebarMenu(id = 'pages',
                   menuItem("Home", tabName = "home", icon = icon("home")),
                   menuItem("Menu Planner", tabName = "menuplanner", icon = icon("pencil")),
                   menuItem("Create New Meal", tabName = "newmeal", icon = icon("check")),
                   menuItem("Shopping List", tabName = "shoppingList", icon = icon("shopping-basket")),
                   menuItem("River Menu", tabName = "riverMenu", icon = icon("shopping-basket")),
                   
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
                           had a lot of ingredients and pre-designed meals loaded for the user to choose 
                           from. There is also the option to add new ingredients and create new meals."),
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
                            tags$img(src = 'riverNest.jpg',width='375px',height='300px')
                            
                            
                            )
                     
                     
                     
                   )
                  ),#End tab item Home
                   
                   #Planner tab content------------
                   tabItem(tabName = "menuplanner",
                           
                           h1('Build Trip Menu'),
                           hr(),
                           fluidRow(
                             column(width = 12,
                               h2('Step 1: Create Trip or Load an Existing Trip in Progress'),
                               h3('Create New Trip'),
                               p('This will create an empty Excel file 
                                  in a local directory for your trip data. You can thereby work on your menu 
                                 and return to it later by using the "Choose Existing Menu File" 
                                 to pick up where you left off.'),
                               textInput('tripName',"Trip Name", width = '90%'),
                               downloadButton('createtrip','Create Trip',style="float:left;
                                            background-color:#007bff;color:white;"
                                            )#End button
                                                    
                               )#End column
                           ),#End fluid row
                           #hr(),
                           fluidRow(
                             column(width = 12,
                                      #h2('Load an Existing Trip File'),
                                      h3('Upload a Previously Saved Menu in Progress'),
                                      fileInput("menuFile", buttonLabel = 'Choose File',
                                                label='Upload Existing Menu File',
                                                placeholder = 'Choose Existing Menu File',
                                                accept='.xlsx'
                                                )#End file input
                                
                                    )#End column
                           ),#End fluid row
                           hr(),
                           h2('Step 2: Select Trip Information and Meals'),
                           p('Below are the choices to build the menu on a by-meal
                              basis. Select "Add Meal to Menu" after each entry to add it to your menu.'),
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
                                    
                                    #Meal list table-------------
                                    uiOutput('lumeal')
                                    
                              )#End column 2
                        
                           ),#End fluid row 
                           
                           fluidRow(
                             column(width =12,
                                    actionButton('commit',label='Add Meal to Menu',
                                                 style = "background-color:#007bff;color:white;",
                                                 icon = icon('plus')
                                                 )#End button
                                    )#End column
                             
                           ),#End fluid row
                           
                           hr(),
                           
                           h2('Step 3: Review Meals/Ingredients'),
                           p("Your menu will appear below. Click a meal to see the ingredients 
                             and quantities involved. You can edit any meal and its ingredients/quantities."),
                           #Menu buttons-----------------------
                           fluidRow(
                             column(width = 12,
                                        downloadButton('save','Save Progress',
                                                     style = "padding-right:10px; float:left;background-color:#007bff;color:white;margin-bottom:12px;",
                                                     icon = icon('pencil')
                                        ),#End button
                                      
                                        actionButton('deleteMenuItem','Delete Menu Item',
                                                     style = "padding-right:10px;float:left;background-color:#dc3545;color:white;margin-bottom:12px;",
                                                      icon = icon('remove')
                                        ),#End button
                                    
                                        actionButton('addNewMeal','Create New Meal',
                                                     style = "padding-right:10px;float:left;background-color:#28a745;color:white;margin-bottom:12px;",
                                                     icon = icon('pencil'))
                                    
                                    )#End column
                             
                             
                          ),#End fluid row
                           
                           fluidRow(
                            box(
                               title = 'Meals added to the Menu',
                               DTOutput('menulist'),width = 12
                               
                               
                             )#End box
                             
                             
                           ),#End fluid row
                          
                          p('The table below presents the ingredients and quantitites for each meal.
                               Select/Deselect meals in the table above to filter this table.'),
                           
                           fluidRow(
                             
                             
                                 box(
                                   title = 'Selected Meal and Ingredients',
                                   DT::dataTableOutput('ingView'),
                                   #rHandsontableOutput('hot'),
                                   width = 12
                                   
                                 )#End box
                             
                             
                           ),#End fluid row
                          
                          fluidRow(
                            
                            
                            
                          ),#end fluid row
                          
                          hr(),
                          
                          h2('Step 4: Export Menu and Shopping List'),
                          p("Two reports are available: A menu to take on the river 
                            and a shopping list. The river menu gives a daily meal list 
                            with the ingredients for that meal. The shopping list gives 
                            a summary of all ingredients and quantities to buy."),
                          
                          #Menu and shopping list buttons---------------------
                          fluidRow(
                            actionButton('viewMenu','Preview Menu Report',
                                         style = "float:left;background-color:#007bff;
                                         color:white;margin-bottom:12px;margin-left:12px;",
                                         icon = icon('list-alt')),#End action button,
                            actionButton('viewShop','Preview Shopping List',
                                         style = "float:left;background-color:#007bff;
                                         color:white;margin-bottom:12px;margin-left:12px;",
                                         icon = icon('shopping-basket'))#End action button,
                            
                            
                            
                          )#End fluid row,
                           
                        ),#End tab item planner
              
             #Add new meal tab-------------------------
             tabItem(tabName = 'newmeal',
                      h1('Create New Meal'),
                      fluidRow(
                        column(width = 6,
                           textInput('newmeal',label = 'New Meal Name'),
                           textInput('newmealdesc',label = 'New Meal Description'),
                           uiOutput('lumtype2')
                               
                        ),#End left column
                        
                        column(width = 6,
                              p('Other side of 2 column form')
                               
                               )#End right column
                        
                        
                        
                      ),#End fluid row
                     
                     fluidRow(
                       column(width = 12,
                              
                              actionButton('returnMenu','Return to Menu Planner',
                                           style = "float:left;background-color:#007bff;color:white;margin-bottom:12px;",
                                           icon = icon('arrow-left'))
                              
                              )#End column
                     )#End fluid row
                      
                      
                  ),#End tab item new meal
             
             #Shopping list tab--------------------------------
             tabItem(tabName = 'shoppingList',
                     h1("Shopping List"),
                     
                     
                     p("Below is a preview of your current shopping list."),
                     
                     
                       
                       fluidRow(
                         column(width = 12,
                                
                                actionButton('returnMenu2','Return to Menu Planner',
                                             style = "float:left;background-color:#007bff;color:white;margin-bottom:12px;",
                                             icon = icon('arrow-left')),
                                downloadButton('sreport','Generate Shop List',
                                               style = "padding-right:15px; float:left;background-color:#28a745;color:white;margin-bottom:12px;",
                                               icon = icon('pencil')
                                ),#End button
                                DTOutput('sList')
                                
                         )#End column
                       )#End fluid row
                     
                     ),#End shopping list page 
             
             #View menu tab--------------------------------
             tabItem(tabName = 'riverMenu',
                     h1("River Menu"),
                     
                     fluidRow(
                       column(width = 12,
                              
                              actionButton('returnMenu3','Return to Menu Planner',
                                           style = "padding-right:15px; float:left;background-color:#007bff;color:white;margin-bottom:12px;",
                                           icon = icon('arrow-left')),
                              downloadButton('report','Generate Report',
                                             style = "padding-right:15px; float:left;background-color:#28a745;color:white;margin-bottom:12px;",
                                             icon = icon('pencil')
                              ),#End button
                              DTOutput('menuPlan'),
                              
                       )#End column
                     )#End fluid row
                     
                     
                     
             ),#End shopping list page 
             
             #View data frames tab------------------
             #This is for development only
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
  #Import Data from google sheets-------------------------------------------------------
  #Register google sheet with the data frames
  gs<-gs_url('https://docs.google.com/spreadsheets/d/1qbWU0Ix6VrUumYObYyddZ1NvCTEjVk18VeWxbvrw5iY/edit?usp=sharing')
   
  #Read in data frames for the first time when create trip is pushed---------
  
  
  
  LU_INGREDIENTS<-gs_read(gs,'LU_INGREDIENTS')
  LU_MEAL<-gs_read(gs,'LU_MEAL')

  LU_MEAL_TYPE<-gs_read(gs,'LU_MEAL_TYPE')
  XREF_INGREDIENT<-gs_read(gs,'XREF_INGREDIENT')
  
  #Push dfs out to the viewer
  output$LU_INGREDIENTS<-renderDT(LU_INGREDIENTS)
  output$LU_MEAL<-renderDT(LU_MEAL)
  output$LU_MEAL_TYPE<-renderDT(LU_MEAL_TYPE)
  output$XREF_INGREDIENT<-renderDT(XREF_INGREDIENT)
  
  #Lookup values----------
  mls<-LU_MEAL %>% pull(MEAL_NAME) %>% sort()
  mtypes<-LU_MEAL_TYPE %>% pull(MEAL_TYPE)
  
  
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
  
  #Choose meal type for new meal
  output$lumtype2<-renderUI({
    pickerInput(
      'choosemealtype2',label = 'Meal Type',
      selected = '--Select Meal Type--',
      choices = c('--Select Meal Type--',mtypes)
    )
    
  })#End lu meal type
  
  
  #'Growing menu dataframe--------------
  
  # D <- "RIVER_DAY,NO_PEOPLE,MEAL_TYPE,MEAL\n'','','',''"
  # write(D, file = "data.csv")
  #Import data from existing file-------------------
  inFile<-observeEvent(input$menuFile,{
    
    infile<-input$menuFile
    updateTextInput(session,'tripName',value = gsub('.xlsx','',infile$name))
    
    data$file<-read.xlsx(infile$datapath,sheet = 'Menu') %>% 
      select(RIVER_DAY,MEAL_TYPE,MEAL_NAME,NO_PEOPLE) %>% 
      unique(.)
    
    data$revMenu<-read.xlsx(infile$datapath,sheet = 'Menu') %>% 
      select(RIVER_DAY,MEAL_NAME,INGREDIENT,INGREDIENT_DESCRIPTION,SERVING_SIZE_DESCRIPTION,
             NO_PEOPLE,SERVING_SIZE_FACTOR,QUANTITY,REVISED) %>% 
      filter(REVISED == "Revised")
    
    
    #These are reading from the project file now, not google sheets
    # LU_INGREDIENTS<-read.xlsx(infile$datapath,sheet = 'LU_INGREDIENTS')
    # LU_MEAL<-read.xlsx(infile$datapath,sheet = 'LU_MEAL')
    # LU_MEAL_TYPE<-read.xlsx(infile$datapath,sheet = 'LU_MEAL_TYPE')
    # XREF_INGREDIENT<-read.xlsx(infile$datapath,sheet = 'XREF_INGREDIENT')
    # 
    # viewMenuIngredients<-XREF_INGREDIENT %>%
    #   inner_join(LU_MEAL) %>%
    #   inner_join(LU_INGREDIENTS) %>%
    #   select(MEAL_NAME,INGREDIENT,INGREDIENT_DESCRIPTION,SERVING_SIZE_DESCRIPTION,
    #          SERVING_SIZE_FACTOR) %>%
    #   arrange(MEAL_NAME,INGREDIENT)
    
    
        }
    )
  
 
  #Define reactive menu data frame controlled by user input, uploads, or CRUD ops---------------
  data <- reactiveValues(
    file = data.frame(),
    revMenu = data.frame()
    
  )
  
  #Output menu table-----------------
   output$menulist<-renderDT(
     data$file,editable = TRUE, 
     style = "bootstrap", rownames = FALSE
   )
 
  
 
   #Surface the ingredients table filtered by selection from the meal table-----------
  
  #Create join of menu item and ingredients---------
  #TODO:Make one of these from the google sheets source if no file has been uploaded
  #TODO: but make a new one that reads from the uploaded project file instead
  

  viewMenuIngredients<-XREF_INGREDIENT %>%
    inner_join(LU_MEAL) %>%
    inner_join(LU_INGREDIENTS) %>%
    select(MEAL_NAME,INGREDIENT,INGREDIENT_DESCRIPTION,SERVING_SIZE_DESCRIPTION,
           SERVING_SIZE_FACTOR) %>%
    arrange(MEAL_NAME,INGREDIENT)
  
  
  
  
  ########HEEEEEEEEEEEEEEEEEEEEERRRRRRRRRRRRRRRRRRRRREEEEEEEEEEEEEEE------------
  # viewMenuIngredients<-gs_read(gs,'XREF_INGREDIENT') %>%
  #   inner_join(gs_read(gs,'LU_MEAL')) %>%
  #   inner_join(gs_read(gs,'LU_INGREDIENTS')) %>%
  #   select(MEAL_NAME,INGREDIENT,INGREDIENT_DESCRIPTION,SERVING_SIZE_DESCRIPTION,
  #          SERVING_SIZE_FACTOR) %>%
  #   arrange(MEAL_NAME,INGREDIENT)
  
  #Output the filtered ingredients table to be viewed in the app--------------------
  #Make ingredient df from selected meal rows
  
      #Create reactive objects from rows selected
      sel<-reactiveValues(
        
        sel2 = vector(),
        lookup = data.frame(),
        ings = data.frame()
        
        )
    
  
      ingredients<-observeEvent( input$menulist_rows_selected, {
        
        
        sel$sel2<-input$menulist_rows_selected
        
        sel$lookup<-data$file[sel$sel2,] %>%
                    mutate(NO_PEOPLE = as.numeric(NO_PEOPLE))
        
        sel$ings<-viewMenuIngredients %>%
                     filter(MEAL_NAME %in% sel$lookup$MEAL_NAME) %>%
                     left_join(sel$lookup, by = c('MEAL_NAME' = 'MEAL_NAME')) %>%
                     as.data.frame(.) %>%
                     mutate(QUANTITY = round_any(SERVING_SIZE_FACTOR*NO_PEOPLE,1,ceiling),
                            REVISED = '') %>%
                     select(RIVER_DAY,MEAL_NAME,INGREDIENT,INGREDIENT_DESCRIPTION,SERVING_SIZE_DESCRIPTION,
                            NO_PEOPLE,SERVING_SIZE_FACTOR,QUANTITY,REVISED)
        
        if(nrow(data$revMenu) > 0){
          print("Revs detected")
          hold<-sel$ings %>% anti_join(data$revMenu,by = c('RIVER_DAY' = 'RIVER_DAY','MEAL_NAME' = 'MEAL_NAME',
                                                           'INGREDIENT' = 'INGREDIENT'))
          str(data$revMenu)
          str(hold)
          
          data$revMenu2<-data$revMenu %>% 
            filter(RIVER_DAY %in% sel$lookup$RIVER_DAY, MEAL_NAME %in% sel$lookup$MEAL_NAME)
          
          sel$ings<-bind_rows(hold,data$revMenu2) %>% 
            arrange(MEAL_NAME)
          
        
          
        }
        
      })
  
    
   
    #Ouput ingredients table based on rows selected
    output$ingView<-DT::renderDT(sel$ings,rownames = F, editable = T )
    
    proxy<-dataTableProxy('ingView')

    observeEvent( input$ingView_cell_edit,{

      info = input$ingView_cell_edit
      str(info)
      i = info$row
      j = info$col + 1  # column index offset by 1
      v = info$value
      sel$ings[i, j] <<- DT::coerceValue(v, sel$ings[i, j])
      sel$ings[i,9] <-"Revised"
      replaceData(proxy, sel$ings, resetPaging = FALSE, rownames = FALSE)
      
      str(data$file)
      str(sel$ings)

    })
  
  
  
  
  # 
  # 
  #Output Preview shopping list table-----------------
  output$sList<-DT::renderDataTable({
   
    
    shp1<-readWorkbook(myMenu(),sheet = 'ShopList')
   #  lookup<-data$file
   #  
   # shp1<-viewMenuIngredients %>% 
   #   filter(MEAL_NAME %in% lookup$MEAL_NAME) %>% 
   #   left_join(lookup, by = c('MEAL_NAME' = 'MEAL_NAME')) %>% 
   #   as.data.frame(.) %>% 
   #   mutate(QUANTITY1 = round_any(SERVING_SIZE_FACTOR*NO_PEOPLE,1,ceiling)) %>% 
   #   select(INGREDIENT,INGREDIENT_DESCRIPTION,QUANTITY1) %>% 
   #   dplyr::group_by(INGREDIENT,INGREDIENT_DESCRIPTION) %>% 
   #   dplyr::summarize(
   #     QUANTITY = sum(QUANTITY1),
   #     MEAL_COUNT = length(INGREDIENT)
   #     ) %>% 
   #   arrange(INGREDIENT)
   # 
   # print(class(shp1))
   
   return(shp1)
   
   
  })#End output sList
  
  
    #Output shopping list report from RMD file----------------------
    output$sreport<-downloadHandler(
      
      
      filename = paste(input$tripName,"_shopList", ".html", sep = ""),
      content = function(file){
        
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        
        tempReport <- file.path(tempdir(), "shopList.Rmd")
        file.copy("shopList.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(data = readWorkbook(myMenu(),sheet = 'ShopList'),
                       title = paste(input$tripName," Shop List")
        )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        
      }
      
    )#end download handler  
    
    
    
    
    
    
    
  #Output menu plan table------------------------------
    output$menuPlan<-renderDT({
      
      readWorkbook(myMenu(),sheet = 'Menu') %>% 
        select(RIVER_DAY,MEAL_TYPE,NO_PEOPLE,INGREDIENT,QUANTITY)
      
     
      
    })#end render menuPlan
  
  
    
  #Output menu plan report from RMD file----------------------
    output$report<-downloadHandler(
      
      
      filename = paste(input$tripName,"_menu", ".html", sep = ""),
      content = function(file){
        
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        
        tempReport <- file.path(tempdir(), "riverMenu.Rmd")
        file.copy("riverMenu.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(data = readWorkbook(myMenu(),sheet = 'Menu'),
                       title = paste(input$tripName," Menu")
                      )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        
      }
      
    )#end download handler
    
    
    
   #Keep a running workbook of the menu to be saved whenever------------------
   myMenu<-reactive({
     
     myMenuOut<-createWorkbook()
     
     addWorksheet(myMenuOut,'Menu')
     addWorksheet(myMenuOut,'ShopList')
     addWorksheet(myMenuOut,'LU_INGREDIENTS')
     addWorksheet(myMenuOut,'LU_MEAL')
     addWorksheet(myMenuOut,'LU_MEAL_TYPE')
     addWorksheet(myMenuOut,'XREF_INGREDIENT')
     
     #This df is based on root menu values
     #Need to be able to rip and replace any changes made in sel$ings
     mmo<-viewMenuIngredients %>%
       #filter(MEAL_NAME %in% lookup$MEAL) %>%
       left_join(data$file, by = c('MEAL_NAME' = 'MEAL_NAME')) %>%
       as.data.frame(.) %>%
       mutate(QUANTITY = round_any(SERVING_SIZE_FACTOR*NO_PEOPLE,1,ceiling),
              REVISED = '') %>%
       select(RIVER_DAY,MEAL_TYPE,MEAL_NAME,INGREDIENT,INGREDIENT_DESCRIPTION,SERVING_SIZE_DESCRIPTION,
              NO_PEOPLE,SERVING_SIZE_FACTOR,QUANTITY,REVISED) %>%
       filter(!is.na(QUANTITY))
     
     
     if(nrow(sel$ings) > 0){
       
       revised<-sel$ings %>% 
         left_join(data$file, by = c('RIVER_DAY' = 'RIVER_DAY','MEAL_NAME' = 'MEAL_NAME','NO_PEOPLE' = 'NO_PEOPLE')) %>%
         as.data.frame(.)
       
       revd<-anti_join(mmo,revised)
       mmo<-bind_rows(revd,revised)
       
     }
     
     
     
     # mmo<-viewMenuIngredients %>%
     #   #filter(MEAL_NAME %in% lookup$MEAL) %>%
     #   left_join(data$file, by = c('MEAL_NAME' = 'MEAL_NAME')) %>%
     #   left_join(sel$ings, by = c('MEAL_NAME' = 'MEAL_NAME','INGREDIENT' = 'INGREDIENT'))
     
     #Set meal type order
     mmo$MEAL_TYPE<-factor(mmo$MEAL_TYPE,levels = mtypes)
     
     mmo<-mmo %>% 
       arrange(RIVER_DAY,MEAL_TYPE)
     
     #Create shopping list
     shpo<-mmo %>% 
       select(INGREDIENT,INGREDIENT_DESCRIPTION,QUANTITY) %>% 
       dplyr::group_by(INGREDIENT,INGREDIENT_DESCRIPTION) %>% 
       dplyr::summarize(
         QUANTITY = sum(QUANTITY),
         MEAL_COUNT = length(INGREDIENT)
       ) %>% 
       arrange(INGREDIENT)
       
     
     writeData(myMenuOut, sheet = 'Menu',mmo)
     writeData(myMenuOut, sheet = 'ShopList', shpo)
     writeData(myMenuOut,'LU_INGREDIENTS',LU_INGREDIENTS)
     writeData(myMenuOut,'LU_MEAL',LU_MEAL)
     writeData(myMenuOut,'LU_MEAL_TYPE',LU_MEAL_TYPE)
     writeData(myMenuOut,'XREF_INGREDIENT',XREF_INGREDIENT)
     
     
     
     return(myMenuOut)
     
   })
  
    
  #Buttons observe and actions-------------------
 
   #Observe meal type selection to filter meal name choices------
  observe({
    
    # if(input$choosemealtype == '--Select Meal Type--')
    #   return()
    
    mls<-LU_MEAL %>% 
      filter(MEAL_TYPE %in% input$choosemealtype) %>% 
      pull(MEAL_NAME) %>% 
      sort()
    prompt<-ifelse(
      input$choosemealtype == '--Select Meal Type--',
      '--Select Meal Type First--',
      paste0('--Select ',input$choosemealtype,'--')
      
    )#End ifelse
    
    updatePickerInput(session,'choosemeal',label='Meal Name',
                      selected = prompt,
                      choices = c(prompt,mls)
                      )
    
    
    
    
  })
  
   #Button to add meals to the menu-----------
   addData <- observeEvent(
     input$commit,
     {
       newLine <- data.frame(
         RIVER_DAY = input$riverday,
         MEAL_TYPE = input$choosemealtype,
         MEAL_NAME = input$choosemeal,
         NO_PEOPLE = as.numeric(input$nopeople),
         
         stringsAsFactors = FALSE
         
         )
       
       print(newLine)
       
       
       data$file <- bind_rows(data$file,newLine) %>% 
         mutate(MEAL_TYPE = factor(MEAL_TYPE,levels = mtypes)) %>% 
         arrange(RIVER_DAY,MEAL_TYPE)
     }
   )
   
   
   #Button to delete selected records from the menu----------
   removeData<-observeEvent(input$deleteMenuItem,{
     
     if (!is.null(input$menulist_rows_selected)) {
       
       data$file <- data$file[-as.numeric(input$menulist_rows_selected),]
     }
     
   })
   
  
  #Button to navigate to create new meal page------------------
  createMeal<-observeEvent(input$addNewMeal,{
    newtab <- switch(input$pages,
                     "menuplanner" = "newmeal",
                     "newmeal" = "menuplanner"
    )

    print(newtab)
    
    updateTabItems(session, "pages", newtab)
    
  })#end createMeal
  
  #Button to navigate back to menu from create new meal page------------------
  returnMenu<-observeEvent(input$returnMenu,{
    newtab <- switch(input$pages,
                     "newmeal" = "menuplanner",
                     "menuplanner" = "newmeal"
                     
    )
    
    print(newtab)
    
    updateTabItems(session, "pages", newtab)
    
  })#end returnMenu
  
  
  #Button to preview shopping list--------------------
  
  shopList<-observeEvent(input$viewShop,{
    newtab <- switch(input$pages,
                     "menuplanner" = "shoppingList",
                     "shoppingList" = "menuplanner"
    )
    
    print(newtab)
    
    updateTabItems(session, "pages", newtab)
    
  })#end view shop list
  
  #Button to navigate back to menu from view shopping list page------------------
  returnMenu2<-observeEvent(input$returnMenu2,{
    newtab <- switch(input$pages,
                     "shoppingList" = "menuplanner",
                     "menuplanner" = "shoppingList"
                     
    )
    
    print(newtab)
    
    updateTabItems(session, "pages", newtab)
    
  })#end returnMenu
  
  #Button to preview menu report------------------------
  menuReport<-observeEvent(input$viewMenu,{
    newtab <- switch(input$pages,
                     "menuplanner" = "riverMenu",
                     "riverMenu" = "menuplanner"
    )
    
    print(newtab)
    
    updateTabItems(session, "pages", newtab)
    
  })#end view shop list
  
  
  #Button to return to menu planner from menu preview---------------------
  
  returnMenu3<-observeEvent(input$returnMenu3,{
    newtab <- switch(input$pages,
                     "riverMenu" = "menuplanner",
                     "menuplanner" = "riverMenu"
                     
    )
    
    print(newtab)
    
    updateTabItems(session, "pages", newtab)
    
  })#end returnMenu
  
  
  
   #Button to save progress of the menu--------------
   
   
   output$save<-downloadHandler(
     filename = function() {
       
         paste(input$tripName, ".xlsx", sep = "")
       
       
       
     },
     
     content = function(file) {
       saveWorkbook(myMenu(), file, overwrite = TRUE)
     }
   )
   
   #Button to create a local directory to store the menu----------------------
   #CReate reactive value to store the local filepath for the session
   #activePath <- reactiveVal()
   
   #Create empty menu data frame and current list of LU values  to use in the trip creation file download
   #It downloads an excel file with headers and no data
  #Empty data frame for first tab creation
  tripDF<-data.frame(
    MEAL_NAME = '',
    INGREDIENT = '',
    INGREDIENT_DESCRIPTION = '',
    SERVING_SIZE_DESCRIPTION = '',
    SERVING_SIZE_FACTOR = '',
    stringsAsFactors = FALSE
  )
  
  #Make workbook
  tripWorkbook<-createWorkbook()
  addWorksheet(tripWorkbook,'Menu')
  addWorksheet(tripWorkbook,'LU_INGREDIENTS')
  addWorksheet(tripWorkbook,'LU_MEAL')
  addWorksheet(tripWorkbook,'LU_MEAL_TYPE')
  addWorksheet(tripWorkbook,'XREF_INGREDIENT')
  
  writeData(tripWorkbook,'Menu',tripDF)
  writeData(tripWorkbook,'LU_INGREDIENTS',LU_INGREDIENTS)
  writeData(tripWorkbook,'LU_MEAL',LU_MEAL)
  writeData(tripWorkbook,'LU_MEAL_TYPE',LU_MEAL_TYPE)
  writeData(tripWorkbook,'XREF_INGREDIENT',XREF_INGREDIENT)
  
  
  
   
   #Observe create trip button and make directories accordingly and set the path.
   #Then load the base data as csv files so the user can tweak as needed
   output$createtrip<-downloadHandler(
     
     #if(input$tripName == "") return(NULL),
       
     
       filename = function() {
         paste(input$tripName, ".xlsx", sep = "")
       },
       
       content = function(file) {
         saveWorkbook(tripWorkbook, file)
       }
    
   )#End download handler
   
  
}#End server function

# Run the application 
shinyApp(ui = ui, server = server)

