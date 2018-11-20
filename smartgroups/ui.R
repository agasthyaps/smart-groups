ui <- dashboardPage(
  
  dashboardHeader(title="Smart Groups V 0.1"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Groups",tabName = "groups",icon=icon("users"))
    )

    # textInput(inputId = "url",label = "Make sure the sheet is 'Published'!",
    #           value = "",
    #           placeholder = "paste url here"),
    
  ),
  dashboardBody(
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: none}")),
    fluidRow(
      box(
        sliderInput(inputId="group.size",label=htmlOutput("slider"),min=1,max=25,value="4",step=1),
        fileInput("file1","Choose CSV File",
                  accept=c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        )
      )
      
    ),
  fluidRow(      
      column(6, style = 'padding:25px;',
             h3(icon("users"),"Groups"),
              DTOutput("sheet"),
             fluidRow(
                 downloadButton(outputId = "download",label="Download csv")
             )
             ),
      column(6, style = 'padding:25px;',
             fluidRow(
               h3(icon("signal"),"Skills Distribution "),
               plotlyOutput("skills")
             )
             # take out cluster plot for now
             # ,fluidRow(
             #   h3(icon("sitemap"),"Principal Components Cluster Plot "),
             #       plotlyOutput("clusters")
             # )
             )
      )
    )
  )