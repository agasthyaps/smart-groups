ui <- dashboardPage(
  
  dashboardHeader(title="Smart Groups V 0.1"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Groups",tabName = "groups",icon=icon("users"))
    ),
    fileInput("file1","Choose CSV File",
              accept=c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    tags$hr(),
    checkboxInput("header","Header",TRUE),
    textInput(inputId = "url",label = "Make sure the sheet is 'Published'!",
              value = "",
              placeholder = "paste url here"),
    textInput(inputId="min.size",label="Min group size",value="3"),
    textInput(inputId="max.size",label="Max group size",value="5")
    
  ),
  dashboardBody(
    # tabItems(
    #   tabItem(tabName = "groups",
    #           fluidRow(
    #             tabBox(
    #               tabPanel(
    #                 "Group Assignments",
    #                 DTOutput("sheet")
    #               ),
    #               tabPanel(
    #                 plotOutput("clusters"),
    #                 title="Principal Components Cluster Plot",
    #                 p("The sum of the percentages on the x and y axis is the percent of variance in the entire dataset that can be explained with the first two principal components.",
    #                   "This plot excludes students' skills. This way, you can see how closely related students are to each other,",
    #                   "and manually edit groups based on skills as you see fit.")
    #               ),
    #               tabPanel(
    #                 plotlyOutput("skills"),
    #                 title="Skills Distribution"
    #               )
    #             ),
    #             box(title = "Information",
    #                 status = "warning",
    #                 p("In order to enable Google Sheets functionality,",
    #                   "click File > 'Publish to the web...'."), 
    #                 p("Then find the",
    #                   "sharing link and paste that into the url field.",
    #                   "For any sheet, you only need to do this once."))
    #             
    #           ),
    #           fluidRow(
    #             box(
    #               downloadButton(outputId = "download",label="Download csv")
    #             )
    #           )
    #   )
    # )
    fluidRow(
      
      column(6, style = 'padding:25px;',
             h3(icon("users"),"Groups"),
                 DTOutput("sheet")
             ),
      column(6, style = 'padding:25px;',
             fluidRow(
               h3(icon("signal"),"Skills Distribution "),
               plotlyOutput("skills")
             ),
             fluidRow(
               h3(icon("sitemap"),"Principal Components Cluster Plot "),
                   plotlyOutput("clusters")
             ))
    )
  )
)