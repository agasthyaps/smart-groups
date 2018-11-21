ui <- dashboardPage(
  
  dashboardHeader(title="Smart Groups V 0.1"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Group Creation",tabName = "groups",icon=icon("users"))
      ,menuItem("Student Facing",tabName="students")
    )

    # textInput(inputId = "url",label = "Make sure the sheet is 'Published'!",
    #           value = "",
    #           placeholder = "paste url here"),
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "groups",
              fluidRow(
                  box(
                    title="Instructions"
                    ,status = "success"
                    ,p("Welcome to the new version of Smart Groups. To start: use the slider (right) to choose a group size. 
                       Note that this setting deterimines the number of people in each group, give or take one person.")
                    ,p("Next, upload the .csv file you've downloaded from google sheets. This action will populate a data table with group assignments (below).")
                    ,p("You may want to filter the table to see who has been placed into a group using a recommendation algorithm by filtering on the column 'alternate grouping strategy'
                       You can click the icon to the left of any name to expand the row and see their survey answers.")
                    ,p("The skills distribution plot will update according to your filter settings on the datatable if you need to investigate further.")
                    ,p("Finally, click the download button.")
                  )
                  ,box(
                    sliderInput(inputId="group.size",label=htmlOutput("slider"),min=1,max=25,value="4",step=1)
                    ,tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: none}"))
                    ,fileInput("file1","Choose CSV File",
                               accept=c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                    )
                  )
              )
              ,fluidRow(
                box(
                  title=h3(icon("users")," Groups"),
                  status = "success",
                  solidHeader = T,
                  DTOutput("sheet")
                  ,tags$hr()
                  ,downloadButton(outputId = "download",label="Download Groups (csv)",class="butt")
                  ,tags$head(tags$style(".butt{background-color:#009200;} .butt{color: white;}"))
                )
                ,tabBox(
                  title = "About the Groups",
                  side = "right",
                  id = "info"
                  ,tabPanel(
                    h4("General Info")
                    ,htmlOutput("generalinfo")
                  )
                  ,tabPanel(
                    h4(icon("signal"),"Skills Distribution")
                    ,"This chart reflects any filtering done on the datatable. Refer to the filters you have set if you get confused."
                    ,plotlyOutput("skills")
                  )
                )
              )
      ), #end grouping tab
      tabItem(tabName = "students",
              fluidRow(
                box(
                  title="Instructions"
                  ,status="success"
                  ,solidHeader = T
                  ,p("Here is where you can generate a report to share groups with the class. Note that you only need to upload a .csv if you downloaded AND EDITED the .csv from the previous tab.")
                  ,p("You can switch back and forth between the 'live' results and uploaded results by toggling the 'use this file' switch.")
                  ,p("By default, the app assumes you will be using live results.")
                  ,downloadButton("report",label="Generate Report",class="butt")
                )
                ,box(
                  fileInput("file2","Upload Groups (if You Need To)",
                             accept=c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  )
                  ,materialSwitch("usefile",
                                 label="Use this file",
                                 status = "success")
                )
              )
              ,fluidRow(
                box(
                  title = "Group Specific Charts"
                  ,status = "warning"
                  ,solidHeader = T
                  ,uiOutput('grouppick')
                  ,htmlOutput("peopleingroup")
                  ,plotOutput("spider")
                )
              )
        
      ) # end students tab
    ) # end tabItems 
    )# end dashboardBody
  ) # end dashboardPage