ui <- dashboardPage(
  
  dashboardHeader(title="SmartGroups V 0.2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Random groups"
               
               ,fileInput("roster","Upload Class Roster",
                         accept=c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
               ),
               # tags$hr(),
               # checkboxInput("header","Header",TRUE),
               textInput(inputId="min.size",label="Min group size",value="3"),
               textInput(inputId="max.size",label="Max group size",value="5"),
               menuSubItem("Create Groups", tabName = "random"),
               selected = T
               ),
      
      menuItem("Question-based groups"
               
               ,fileInput("questions","Upload Answers",
                         accept=c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
               ),
               tags$hr(),
               # checkboxInput("header","Header",TRUE),
               textInput(inputId="qmin.size",label="Min group size",value="3"),
               textInput(inputId="qmax.size",label="Max group size",value="5"),
               radioButtons("grouptype", "Group type:",
                            c("Similar" = "sim",
                              "Mixed" = "mix")),
               menuSubItem("Create Groups", tabName = "qbgroups")
               )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "random"
              ,box(title = "Making Random Groups",
                  p("Upload a class roster (right) and define group size limits, then click 'create groups'."),
                  p("The table below are randomly generated group assignments."),
                  p("DEV NOTE: the group assignments will be pipelined to a database where we can keep track of past groups in order to keep groups novel."))
              ,DT::dataTableOutput("randresult")
              ),
      tabItem(tabName="qbgroups"
              ,box(title = "Making Question-Based Groups",
                  p("Upload a class roster (right) and define group size limits and group type, then click 'create groups'."),
                  p("The table below are groups created based on student answers."))
              ,DT::dataTableOutput("qbresult")
              )
    )
  )
)
