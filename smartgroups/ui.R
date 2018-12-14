ui <- dashboardPagePlus(
  # header = argonDashHeader("SmartGroups or whatever"),
  header = dashboardHeaderPlus(title ="SG"),
  title="Smart Groups V 0.2",
  skin = "blue-light",
  sidebar_background = "light",
  # sidebar_collapsed = T,
  # collapse_sidebar = T,
  # bs4DashNavbar(title="Smart Groups V 0.1"),
  sidebar = dashboardSidebar(
    # id = "sb"
    # ,title = "Smart Groups V 0.1"
    # ,skin = "light"
    # ,side="left"
    # ,vertical = F
    sidebarMenu(
      menuItem("Group Creation",
                         tabName = "groups"
                         ,icon=icon("users")
                         )
      ,menuItem("Student Facing",
                          tabName="students"
                          ,icon =icon("atom")
                          )
    )
  )
  # ,bs4DashNavbar()
  ,body = dashboardBody(
    
    tags$head(tags$style(HTML('
                              .content-wrapper,.right-side {
                              background-color: #F6FBFC;
                              }
                              .main-sidebar{
                              box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                              }'
                              ))),
    
    
    setShadow(class = "box"),
    tabItems(
      tabItem(tabName = "groups",
              fluidRow(
                column(
                  width = 6,
                  boxPlus(
                    title="Instructions"
                    ,status = "success"
                    ,p("Welcome to the newest version of Smart Groups. To start: use the slider (right) to choose a group size. 
                       Note that this setting deterimines the number of people in each group, give or take one person.")
                    ,p("Next, upload the .csv file you've downloaded from google sheets. This action will populate a data table with group assignments (below).")
                    ,p("You may want to filter the table to see who has been placed into a group using a recommendation algorithm by filtering on the column 'alternate grouping strategy'
                       You can click the icon to the left of any name to expand the row and see their survey answers.")
                    ,p("The skills distribution plot will update according to your filter settings on the datatable if you need to investigate further.")
                    ,p("Finally, click the download button.")
                    ,width = 12
                    ,closable = T
                    ,collapsible = T
                    )
                  ,boxPlus(
                    title=h3(icon("users")," Groups"),
                    status = "success",
                    solidHeader = T,
                    # gradientColor = "teal",
                    DTOutput("sheet")
                    # ,tags$hr()
                    ,footer = downloadButton(outputId = "download",label="Download Groups (csv)",class="butt")
                    ,tags$head(tags$style(".butt{background-color:#009200;} .butt{color: white;} .butt{align: center;}"))
                    ,width = 12
                    ,collapsible = F
                    ,closable = F
                  )
                )
                ,column(
                  width = 6
                  ,fluidRow(
                    boxPlus(
                      title = "File Upload"
                      ,fileInput("file1","Choose CSV File",
                                 accept=c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                      )
                      ,width = 6
                      ,collapsible = T
                      ,closable = F
                    )
                    ,boxPlus(
                      closable = F
                      ,title = htmlOutput("slider")
                      ,sliderInput(inputId="group.size",label= "", min=1,max=25,value="4",step=1)
                      ,tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: none}"))
                      ,width = 6
                      ,collapsible = T
                    ) 
                  )
                  ,flipBox(
                    id = 1,
                    main_img = "https://image.flaticon.com/icons/svg/235/235186.svg",
                    back_title = "Skills Distribution",
                    front_btn_text = "Skills Distribution",
                    back_btn_text = "General Info",
                    h3("General Info"),
                    uiOutput("row1"),
                    uiOutput("row2"),
                    back_content = tagList(
                      "This chart reflects any filtering done on the datatable. Refer to the filters you have set if you get confused.",
                      plotlyOutput("skills")
                    )
                  )
                )
              )
              # ,fluidRow(
              # 
              # 
              # )
      ), #END GROUPUNG TAB
      
      # BEGIN STUDENT FACING TAB
      tabItem(tabName = "students",
              fluidRow(
                column(
                  width = 6,
                  boxPlus(
                    title="Instructions"
                    ,status="success"
                    ,solidHeader = T
                    ,p("Here is where you can generate a report to share groups with the class. Note that you only need to upload a .csv if you downloaded AND EDITED the .csv from the previous tab.")
                    ,p("You can switch back and forth between the 'live' results and uploaded results by toggling the 'use this file' switch.")
                    ,p("By default, the app assumes you will be using live results.")
                    ,collapsible = T
                    ,closable = T
                    ,width = 12
                  )
                  ,boxPlus(
                    title = "Group Specific Charts"
                    ,status = "warning"
                    ,solidHeader = T
                    ,uiOutput('grouppick')
                    ,htmlOutput("peopleingroup")
                    ,plotOutput("spider")
                    # ,gradientColor = "teal"
                    ,collapsible = F
                    ,closable = F
                    ,footer = downloadButton("report",label="Generate Full Report",class="butt")
                    ,width = 12
                  )
                )
                ,column(
                  width = 6,
                  boxPlus(
                    fileInput("file2","Upload Groups (if You Need To)",
                              accept=c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    )
                    ,materialSwitch("usefile",
                                    label="Use this file",
                                    status = "success")
                    ,closable = F
                  )
                )
              )

        
      ) # end students tab
    ) # end tabItems 
    )# end dashboardBody
  ) # end dashboardPage