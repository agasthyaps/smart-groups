shinyServer(function(input, output) {
   
  roster <- reactive({
    inFile <- input$roster
    rost <- read.csv(inFile$datapath, header = T, stringsAsFactors = F)
    rost
  })
  
  questions <- reactive({
    inFile <- input$questions
    quest <- read.csv(inFile$datapath, header=T, stringsAsFactors = F)
    quest
  })
  
  randomGroups = function(df, max, min){
    max <- as.numeric(max)
    min <- as.numeric(min)
    rows <- nrow(df)
    sweet.spot = floor(mean(c(max,min)))
    n.groups <- floor(rows/sweet.spot)
    rem <- rows %% sweet.spot
    df2 <- data.table::copy(df)
    df$assignment <- rep(0,rows)
    
    for(i in seq(n.groups)){
      if(nrow(df2)>=sweet.spot){
        groupinds <- as.numeric(sample(rownames(df2),sweet.spot))
        group <- df2[groupinds,]
        df2 <- df2[!rownames(df2) %in% groupinds,]
        df[groupinds,]$assignment <- i
      }
    }
    if(nrow(df2)>0){
      for(i in seq_along(rownames(df2))){
        df[rownames(df2)[i],]$assignment <- i
      }
    }
    df
  }
  
  checkMC = function(df){
    if(grepl("[MC]",names(df)[3])){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  
  MCgroups = function(df, max, min){
    max <- as.numeric(max)
    min <- as.numeric(min)
    rows <- nrow(df)
    sweet.spot = floor(mean(c(max,min)))
    n.groups <- floor(rows/sweet.spot)
    
    df$assignment <- rep(0,nrow(df))
    by.answers <- split(df,as.factor(df[[3]]))
    g <- seq(n.groups)
    used <- c()
    
    for(i in seq_along(by.answers)){
      current <- by.answers[[i]]
      # print(current)
      cur.rows <- nrow(current)
      # if you're the only one, sorry buddy
      if(cur.rows<min){
        for(row in nrow(current)){
          df[row,]$assignment <- sample(1:n.groups,1)
        }
      }
      
      # if you're in a group that's exactly equal to max, min or ideal size, then you're all in a group
      else if(cur.rows==max | cur.rows == min | cur.rows == sweet.spot){
        df[rownames(current),]$assignment <- g[i]
        used <- c(used,g[i])
      }
      # else{
      #   print(current)
      # }
    }
    g <- g[!g %in% used]
    # print(g)
    
    # now we most likely have a huge group
    unassigned <- df[df$assignment == 0,]
    # print(unassigned)
    if(nrow(unassigned)==0){
      return(df)
    }
    # print(unassigned)
    leftover.groups <- floor(nrow(unassigned)/sweet.spot)
    
    for(i in seq(leftover.groups)){
      inds <- as.numeric(sample(rownames(unassigned),sweet.spot))
      unassigned <- unassigned[!rownames(unassigned) %in% inds,]
      df[inds,]$assignment <- g[i]
    }
    
    if(nrow(unassigned)==0){
      return(df)
    }
    
    
    if(nrow(unassigned)>=min){
      df[rownames(unassigned),]$assignment <- g[1]
    }
    else{
      smalls <- c()
      for(i in unique(df$assignment)){
        if(nrow(df[df$assignment == i,])<max){
          smalls <- c(smalls,i)
        }
      }
      # print(smalls)
      for(i in seq(nrow(unassigned))){
        df[rownames(unassigned[i,]),]$assignment <- smalls[i]
      }
      
    }
    df
  }
  
  SAgroups = function(df,max,min){
    
  }
  
  random <- reactive({
    randomGroups(roster(),input$max.size,input$min.size)
  })
  
  quest <- reactive({
    if(input$grouptype=="mix"){
      randomGroups(questions(),input$qmax.size,input$qmin.size)
    }
    else{
      MCgroups(questions(),input$qmax.size,input$qmin.size)
    }
  })
  output$randresult <- DT::renderDataTable({
    if(is.null(input$roster)){
      return(NULL)
    }
    DT::datatable(random(),
              extensions = 'Buttons',
              options = list(
                buttons = c('excel','csv','pdf')
              ))
  })
  
  output$qbresult <- renderDT({
    if(is_null(input$questions)) return(NULL)
    DT::datatable(quest(),
                  extensions = 'Buttons',
                  options = list(
                    buttons = c('excel','csv','pdf')
                  ))
  })
  
  output$downloadrand <- downloadHandler(
    filename = function(){
      paste("RandomGroups-",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write.csv(random(),file)
    }
  )
  
  output$downloadquest <- downloadHandler(
    filename = function(){
      paste("RandomGroups-",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write.csv(quest(),file)
    }
  )
  
  output$frame <- renderUI({
    HTML("<iframe src='https://docs.google.com/forms/d/e/1FAIpQLScdbifDbdkRgTXUi5W6pJPtAw6B_1WjkRnojhsl1Lmtpsryew/viewform?embedded=true' width='640' height='698' frameborder='0' marginheight='0' marginwidth='0'>Loading...</iframe>")
  })
  
  # for question groups:
  # first check if they want similar or mixed. if mixed, just do random groups
  # if similar, then check if MC. if MC then do MC groups
  # if not, then do cosinesimilarity for short answers
})
