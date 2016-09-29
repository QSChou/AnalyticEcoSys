library(plotly)
library(shiny)
library(shinydashboard)


dashboardPage(
  dashboardHeader(
    title = "EDC Analysis"
  ),
  dashboardSidebar(
    conditionalPanel(
      condition="input.PlotTabs == `get_data`",
      headerPanel("Get Data Selection"),
    
      uiOutput("ChoiceData"),
      selectInput("data_source", "Data Source",c("EDW", "DataLake","Lab+"),selected = "Teradata"),

      checkboxInput("TypeUserPass","User Name & Passward",FALSE),
      conditionalPanel(
        condition="input.TypeUserPass==true"
        ,textInput("UserName","UserName:")
        ,passwordInput("Passward","Passward:")
        ,verbatimTextOutput("value")),
    
      uiOutput("daterangeui"),
      actionButton("connDB","connDB"),
      checkboxInput("Selection","Select Criteria",FALSE),
      conditionalPanel(
        condition="input.Selection==true",
        #actionButton("refreshCrit","refreshCrit"),
        checkboxInput("fixProd","Fixed Prod criteria",FALSE),
          uiOutput("produi"),
        checkboxInput("fixOpra","Fixed Opra criteria",FALSE),
          uiOutput("opraui"),
        checkboxInput("fixLine","Fixed Line criteria",FALSE),
          uiOutput("lineui"),
        checkboxInput("fixEquip","Fixed Equip criteria",FALSE),
          uiOutput("eqpui"),
        verbatimTextOutput("selCrit"),
        textInput("table_name", "Table Name",value = "Enter Table Name")),
        actionButton("getData","getData")),
    conditionalPanel(
      condition="input.PlotTabs == `PCA`",
      headerPanel("PCA Analysis"),
      checkboxInput("SelVars","Select PCA Variants (X)",FALSE),
      conditionalPanel(
        condition="input.SelVars==true",
        uiOutput("edcmXSelector")
      ),
      checkboxInput("SelColor","Select PCA Colored Factor (Y)",FALSE),
        conditionalPanel(
        condition="input.SelColor==true",
        uiOutput("edcYSelector")),
      actionButton("RunPCA","RunPCA")
      ,uiOutput("edcsXSelector")
    )
  ),
  dashboardBody(   
    tabsetPanel(
      id = "PlotTabs",
      tabPanel(title = "Get Data",value="get_data",
               h4("Get Data"),  
               #verbatimTextOutput("DataMeta"),
               #verbatimTextOutput("dfSql"),
               verbatimTextOutput("data_summary"),
               dataTableOutput('dfTbl'),
               dataTableOutput('meta_list'),
               dataTableOutput('numdatbl')
               #verbatimTextOutput("equip_list")
      ),
      tabPanel(title = "Data Summary",value="Data Summary",
      h4("Data Summary"),                       
      verbatimTextOutput("edc_num_summary"),
      verbatimTextOutput("edc_meta_summary")
      ),
    tabPanel(title = "PCA",value="PCA",
             fluidRow(
               column(6, 
                      #verbatimTextOutput("hover"),
                      plotlyOutput("pca_score")
               ),     
               column(6, 
                      #verbatimTextOutput("hover"),
                      plotlyOutput("pc1_loading")
               )      
             ),
             fluidRow(
                column(6,
                  #verbatimTextOutput("hover"),
                  plotlyOutput("pca_loading")
                 ),
                 column(6,
                   #verbatimTextOutput("brush"),
                   conditionalPanel(
                     condition="is.null(input.edcsX)==false",
                     plotlyOutput("singleVarDtl"))
                 )
              )
            )
  )))
