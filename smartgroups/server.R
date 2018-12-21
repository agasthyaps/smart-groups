server <- function(input,output,session){
  options(expressions = 5e5)
  
  ##### FUNCTIONS #####
  
  # raw table
  og_t <- reactive({
    # read the url, create dfs
    inFile <- input$file1
    s <- read.csv(inFile$datapath, header = T)
    s[,1:ncol(s)] <- lapply(s[,1:ncol(s)],function(x) as.character(x))
    table <- s
    
    # rename skill columns
    skill.names <- vector()
    for(i in 8:ncol(table)){
      skill.names <- c(skill.names,gsub("\\.","",regmatches(names(table)[i],regexpr("\\w+",names(table[i])),invert=T)[[1]][2]))
    }
    names(table) <- c("time","name","shortanswer","interest1","interest2","choice","program",skill.names)
    # used to be time name interest choice graded shortanswer program skillnames
    rownames(table) <- table$name
    table
  })
  
  table <- reactive({
    table <- concat_partners(og_t())
    table <- table[,-c(5)]
    rownames(table) <- table$name
    table
  })
  
  second_choice <- reactive({
    sc <- og_t()[,c(2,5)]
    rownames(sc) <- sc$name
    sc
  })
  
  #this will be used to bring back people who were merged from concat_partners
  original_names_table <- reactive({
    table <- og_t()
    # want name interest program shortanswer skillnames
    table <- table[,c(2,4,7,3,8:ncol(table))]
    skill_cols <- names(table[,c(8:ncol(table))])
    # label encode skill cols:
    for(i in skill_cols){
      table[,i] <- label_encode(table[,i])
    }
    
    # now we are missing a "skills" column (all the persons skills concatted)
    table$skills <- ""
    for(i in 1:nrow(table)){
      tempskills <- ""
      for(j in 8:ncol(table)){
        if(table[i,names(table)[j]] == 1){
          tempskills <- paste(tempskills,names(table)[j],sep= " • ")
          # tempskills <- substr(tempskills,2,nchar(tempskills))
        }
      }
      table$skills[i] <- tempskills
    }
    
    # add "alternate grouping" as blank
    # add assignment as blank
    table$alternate_grouping_strategy <- ""
    table$assignment <- 0
    # table$choice <- og_t()$choice
    #re order to match final groups
    table <- table[,c("name","interest1","program","skills",
                      "shortanswer","alternate_grouping_strategy","assignment",skill_cols)]
    # rownames(table) <- table$name
    
    table
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste("groups-",Sys.Date(),".csv",sep="")
    },
    content = function(file){
      write.csv(groups(),file)
    }
  )
  
  # find partners and concat
  concat_partners <- function(df){
    skips <- c()
    for(i in seq(nrow(df))){
      if(df[i,]$choice != ""){
        if(df[i,]$choice %in% df$name){
          friend_pos = rownames(df[df$name == df[i,]$choice,])
          if(df[friend_pos,]$choice == df[i,]$name){
            df[i,]$name <- paste(df[i,]$name,"@",df[friend_pos,]$name,sep="")
            # print(df[i,]$name)
            skips <- c(skips,friend_pos)
            
          }
        }
      }
    }
    df[!rownames(df) %in% skips,]
  }
  
  # label encode everything
  label_encode <- function(column){
    newCol <- column %>% as.factor() %>% as.numeric() - 1
    newCol[is.na(newCol)] <- 0
    newCol
  }
  
  string_to_tokens <- function(answers){
    # string to tokens
    answer_words <- answers %>% unnest_tokens(word,text)
    
    # tokens to word count
    word_count <- answer_words %>%
      anti_join(stop_words) %>%
      count(document,word,sort = T) %>%
      ungroup()
    word_count
  }
  
  do_lda <- function(df,columns=c(2,3)){
    # column nums should be in this format: c(document, text)
    
    answers <- df[columns]
    names(answers) <- c("document","text")
    
    word_count <- string_to_tokens(answers)
    
    # now see if we lost any answers due to the anti_join
    # for example, an answer of "I want to be the very best" 
    # would disappear. we can't allow that
    
    doc_lengths <- word_count %>% group_by(document) %>% 
      summarize(freq = sum(n))
    
    left.out <- setdiff(answers$document,doc_lengths$document)
    if(length(left.out > 0)){
      inds <- match(left.out,answers$document)
      answers$text[inds] <- sapply(answers$text[inds],function(x) paste(x,"azkaban",sep=" "))
      word_count <- string_to_tokens(answers)
    }
    
    # now change to dtm
    answer_dtm <- word_count %>%
      cast_dtm(document,word,n)
    
    # actual lda
    control <- list(burnin = 500, iter = 1000, keep = 100, seed = 46)
    answer_lda <-  LDA(answer_dtm, k = 9, 
                       method="Gibbs", control=control)
    
    topic.list <- as.data.frame(topics(answer_lda,1))
    names(topic.list) <- "shortanswer"
    topic.list
    
  }
  
  # cleaned up; columns are ("name","interest1","program","skills"..."shortanswer")
  # everything is label encoded
  top <- reactive({
    topic.list <- do_lda(table())
  })
  
  le <- reactive({
    cl.le <- table()[,c("interest1","program")]
    # label encoding
    cl.le$interest1 <- label_encode(table()$interest1)
    # cl.le$graded <- label_encode(table()$graded)
    cl.le$program <- label_encode(table()$program)
    cl.le
  })
  
  skills <- reactive({
    sk <- table()[,c(8:ncol(table()))]
    # label encode skills
    skill.cols <- colnames(sk)
    for(i in skill.cols){
      sk[,i] <- label_encode(sk[,i])
    }
    sk
  })
  
  clean <- reactive({
    cl <- table()[,c("time","name")]
    cl$shortanswer <- top()$shortanswer
    cl$interest1 <- le()$interest1
    # cl$graded <- le()$graded
    cl$program <- le()$program
    cl <- merge(cl,skills(),by=0)
    last <- ncol(cl)
    # print(names(cl))
    cl <- cl[,c(3,5:last,4)]
    # print(names(cl))
    cl <- split(cl,as.factor(cl$interest1))
    cl <- bind_rows(cl)
    cl$alternate_grouping_strategy <- ""
    # print(rownames(cl))
    cl
  })

  # takes a single df
  assign_groups <- function(df,assignment.list){
    avail_inds <- rownames(df)
    unique.assign <- unique(assignment.list)
    for(a in seq_along(unique.assign)){
      size <- sum(assignment.list == unique.assign[a])
      if(a == length(unique.assign)){
        temp <- df[avail_inds,]
      }
      else{
        temp <- until_its_good(df[avail_inds,],size)  
      }
      used_inds <- rownames(temp)
      rm(temp)
      avail_inds <- setdiff(avail_inds,used_inds)
      df[row.names(df) %in% used_inds,]$assignment <- unique.assign[a]
    }
    df$assignment
  }
  
  # calculates group memberships, returns table
  # function takes in 's', a list of dfs
  make_groups <- function(s, group_size){
    
    group_size <- as.numeric(group_size)
    cur_group <- 1
    
    for(group in seq_along(s)){
      s[[group]]$assignment <- 0
      cur.df <- s[[group]]
      num_groups <- floor(nrow(cur.df)/group_size)
      assignments <- c()
      
      if(nrow(cur.df)>=group_size){
        
        for(g in cur_group:(cur_group+(num_groups-1))){
          assignments <- c(assignments,rep(cur_group,group_size))
          cur_group <- cur_group+1
        }
        rem <- nrow(cur.df) %% group_size
        assignments <- c(assignments,rep(0,rem))
        
        # only shuffle if num_groups (ie the number of groups within this interest) > 1
        if(num_groups > 1){
          s[[group]]$assignment <- assign_groups(s[[group]],assignments)
        }
        else{
          s[[group]]$assignment <- assignments
        }
      }
    }
    
    final.df <- s
    cl <- clean()
    rownames(cl) <- cl$name
    for(i in seq_along(final.df)){
      rownames(final.df[[i]]) <- final.df[[i]]$name
      r <- rownames(final.df[[i]])
      final.df[[i]]$shortanswer <- cl[r,]$shortanswer
      final.df[[i]]$interest1 <- table()[r,]$interest1
      final.df[[i]]$program <- table()[r,]$program
      # final.df[[i]]$graded <- table()[r,]$graded
    }

    final.df <- bind_rows(final.df)
    # print(names(final.df))
    
    
    ra <- c()
    for(i in 1:nrow(final.df)){
      if(final.df$assignment[i] == 0){
        ra <- c(ra,"unassigned")
      }
      else{
        ra <- c(ra,final.df$alternate_grouping_strategy[i])
      }
    }
    
    final.df$alternate_grouping_strategy <- ra
    unassigned <- final.df[final.df$assignment == 0,]
    
    # TAKEN OUT FOR JB. 
    # for(i in 1:nrow(unassigned)){
    #   pool <- final.df[final.df$interest1 == unassigned[i,]$interest1[1],]
    #   for(g in unique(pool$assignment)){
    #     # below, add "+1" to input$group.size (ouside paren). had to remove for JB.
    #     if(nrow(pool[pool$assignment == g,]) < as.numeric(input$group.size) && g != 0){
    #       final.df[rownames(unassigned[i,]),]$assignment <- pool[pool$assignment == g,]$assignment[1]
    #       # final.df[rownames(unassigned[i,]),]$alternate_grouping_strategy <- "randomly assigned"
    #     }
    #   }
    # }
    
    
    rownames(final.df) <- final.df$name
    #TOOK THIS OUT FOR JB
    # unassigned <- final.df[final.df$assignment == 0,]
    # if(nrow(unassigned) > 0){
    #   assigned <- final.df[final.df$assignment != 0,]
    #   assigned <- split(assigned,as.factor(assigned$assignment))
    #   for(i in 1:nrow(unassigned)){
    #     for(j in seq_along(assigned)){
    #       # below, add "+1" to input$group.size (inside paren). had to remove for JB.
    #       if(unassigned[i,]$shortanswer %in% assigned[[j]]$shortanswer && nrow(assigned[[j]]) < (input$group.size)){
    #         unassigned[i,]$assignment <- assigned[[j]]$assignment[1]
    #         unassigned[i,]$alternate_grouping_strategy <- "recommended"
    #         # print("recommended someone")
    #         next()
    #       }
    #     }
    #   }
    #   for(i in 1:nrow(unassigned)){
    #     r <- unassigned[i,]$name
    #     final.df[r,]$assignment <- unassigned[i,]$assignment
    #     final.df[r,]$alternate_grouping_strategy <- unassigned[i,]$alternate_grouping_strategy
    #   }
    # }
    
    for(i in 1:nrow(final.df)){
      r <- rownames(final.df[i,])
      final.df[r,]$shortanswer <- table()[r,]$shortanswer
    }
    
    final.df$assignment <- as.character(final.df$assignment)
    final.df$interest1 <- as.character(final.df$interest1)
    final.df$alternate_grouping_strategy <- as.character(final.df$alternate_grouping_strategy)
    
    # CHANGED THIS

    # final.df$shortanswer <- t()$lda
    # print(names(final.df))
    
    final.df
  }
  
  # helper function, shuffles groups until there is an 
  # appropriate diversity of skills (group size as limiting factor)
  until_its_good <- function(df,size){
    # important!!! (because now there is a shortanswer and assignment column as the last columns)
    last.skill <- ncol(df)-3
    skills.table <- df[,6:last.skill]
    # skills.table[is.na(skills.table)] <- 0
    samp <- skills.table[sample(nrow(df),size),]
    # print(samp)
    sums <- colSums(samp)
    if(sum(sums > size)>0){
      until_its_good(df,size)
    }
    else{
      df[rownames(samp),]
    }
  }
  
  # make sure that none of the interest groups are  
  # smaller than the minimum size
  # (I'm sure there's a cleaner way to do this)
  # takes a list of dfs
  at_least_min <- function(wholedf,splitby){
    df <- split(wholedf,as.factor(splitby))
    drop <- vector()
    for(i in seq_along(df)){
      if(nrow(df[[i]]) < as.numeric(input$group.size)-1){
        
        small <- df[[i]]
        drop <- c(drop,i)
        
        # now put each person into another group that talks about the same stuff.
        for(row in seq(nrow(small))){
          # don't look at the df we're already on
          for(j in seq_along(df)){
            if(j == i){
              next()
            }
            if(small[row,]$shortanswer %in% df[[j]]$shortanswer){
              name <- small[row,]$name
              df[[j]] <- rbind(df[[j]],small[row,])
              df[[j]][df[[j]]$name == name,]$alternate_grouping_strategy <- "recommended"
              break
            }
          }
        }
      }
    }
    df <- df[-drop]
    df
  }

  groups <- reactive({
    if(is_null(input$file1)) return(NULL)
    # break up into separate dfs based on interest
    # interest.dfs <- split(clean(),as.factor(clean()$interest))
    # print(interest.dfs)
    interest.dfs <- at_least_min(clean(),clean()$interest1)
    final.groups <- make_groups(interest.dfs,input$group.size)
    
    # get a list of skills so we can display in expanded row
    allskills <- list()
    final.groups$skills <- ""
    for(i in 1:nrow(final.groups)){
      tempskills <- ""
      for(j in 8:ncol(final.groups)){
        if(final.groups[i,names(final.groups)[j]] == 1){
          tempskills <- paste(tempskills,names(final.groups)[j],sep= " • ")
          # tempskills <- substr(tempskills,2,nchar(tempskills))
        }
      }
      final.groups$skills[i] <- tempskills
    }
    
    
    final.groups <- final.groups[,c(1,2,3,4,ncol(final.groups),ncol(final.groups)-3,ncol(final.groups)-2,ncol(final.groups)-1,5:(ncol(final.groups)-4))]
    # print(c(1,2,3,4,ncol(final.groups),ncol(final.groups)-3,ncol(final.groups)-2,ncol(final.groups)-1,5:(ncol(final.groups)-4)))
    
    # responses <- cbind(responses,second_choice()$interest2)
    
    # now bring back people who were concatted
    responses <- original_names_table()
    for(i in seq(nrow(final.groups))){
      name <- final.groups$name[i]
      if(regexpr("@",name,fixed=T) > 0){
        partners <- strsplit(name,"@")
        # get assignment
        responses[responses$name == partners[[1]][2],]$assignment <- final.groups$assignment[i]
        responses[responses$name == partners[[1]][1],]$assignment <- final.groups$assignment[i]
  
        #get grouping strat
        responses[responses$name == partners[[1]][2],]$alternate_grouping_strategy <- final.groups$alternate_grouping_strategy[i]
        responses[responses$name == partners[[1]][1],]$alternate_grouping_strategy <- final.groups$alternate_grouping_strategy[i]
        
        # # signal that these people were partners
        # responses[responses$name == partners[[1]][2],]$name <- paste(partners[[1]][2],"*",sep="")
        # responses[responses$name == partners[[1]][1],]$name <- paste(partners[[1]][1],"*",sep="")
      }
      else{
        responses[responses$name == name,]$assignment <- final.groups$assignment[i]
        responses[responses$name == name,]$alternate_grouping_strategy <- final.groups$alternate_grouping_strategy[i]
      }
    }
    
    responses$assignment <- as.factor(responses$assignment)
    responses$interest1 <- as.factor(responses$interest1)
    responses$alternate_grouping_strategy <- as.factor(responses$alternate_grouping_strategy)
    # print(responses[c(1:5),])
    responses
    })
  
  
  ##### OUTPUT #####
  
  #### TABLES
  
  # options for main DT
  opts <- reactive({
    if(is_null(input$file1)){
      list()
    }
    else{
      list(
        columnDefs = list(
          list(visible = F, targets = c(0,2,3,4,7,8,9,12:(ncol(groups())+4))
          )
          ,list(orderable = T, className = 'details-control', targets = 1)
        )
      ) 
    }
  })
  output$sheet <- renderDT({
    if(is_null(input$file1)) return(NULL)
    # print(second_choice()$interest2)
    df <- cbind(second_choice(), groups())
    df <- cbind(og_t()$choice,df)
    df <- cbind(' ' = '&oplus;',df)
    print(names(df))
    # df <- cbind(df,og_t()$choice)
    # names(df)[ncol(df)] <- "Group Assignment"
    df
    },
    escape = -2,
    # server= FALSE,
    filter = 'top',
    # rownames = FALSE,
    selection = 'none',
    options = opts
    ,callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
                  var format = function(d) {
                  return '<div style=\"background-color:#add8e6; padding: .5em;\"><strong>Program: </strong> ' +
                  d[7] +'<br>'+
                  '<strong>Short Answer: </strong> ' + d[9] + '<br>'+
                  '<strong>Skills: </strong> ' + d[8] + '<br>' + '<strong>Requested Partner: </strong>' + d[2] + '<br>' +
                  '<strong>Secondary Interest: </strong>' + d[4] + '<br>' +
                  '</div>';
                  };
                  table.on('click', 'td.details-control', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  td.html('&oplus;');
                  } else {
                  row.child(format(row.data())).show();
                  td.html('&CircleMinus;');
                  }
                  });"
    )
  )
  
  geninfo <- reactive({
      if(is_null(input$file1)) return(NULL)
      df <- groups()
      geninf <- list(
        "Number of students" = nrow(df),
        "Number of groups" = length(unique(df$assignment)),
        "Most popular interest" = as.data.frame(df %>% group_by(interest1) %>%
                                                  summarise(n = n()) %>%
                                                  filter(n==max(n)))[[1]],
        "num of pop interest" = as.data.frame(df %>% group_by(interest1) %>%
                                                summarise(n = n()) %>%
                                                filter(n==max(n)))[[2]],
        "Least popular interest" = as.data.frame(df %>% group_by(interest1) %>%
                                                   summarise(n = n()) %>%
                                                   filter(n==min(n)))[[1]],
        "num of least pop" = as.data.frame(df %>% group_by(interest1) %>%
                                             summarise(n = n()) %>%
                                             filter(n==min(n)))[[2]],
        "byrec" = nrow(df[df$alternate_grouping_strategy == "recommended",])
      )
      geninf
  })
  
  output$row1 <- renderUI({
    if(is_null(input$file1)) return(NULL)
    fluidRow(
      bs4ValueBox(
        subtitle = "Number of groups",
        value = h3(geninfo()[[2]]),
        status = "success",
        width = 6
      )
      ,bs4ValueBox(
        subtitle = "Number of students grouped by Recommender",
        value = h3(geninfo()[[7]]),
        status = "success",
        width = 6
      )
    )
  })
  
  output$row2 <- renderUI({
    if(is_null(input$file1)) return(NULL)
    fluidRow(
      bs4ValueBox(
        subtitle = paste("Most popular interest  (", geninfo()[[4]], " students)", sep = ""),
        value = h4(geninfo()[[3]]),
        width = 4
        # gradientColor = "warning"
      )
      ,bs4ValueBox(
        subtitle = paste("Least popular interest (", geninfo()[[6]], " students)", sep = ""),
        value = h4(geninfo()[[5]]),
        width = 4
        # gradientColor = "warning"
      )
      ,bs4ValueBox(
        subtitle = "Total number of students",
        value = h3(geninfo()[[1]]),
        width = 4
        # gradientColor = "warning"
      )
    )
  })
  
  #### CHARTS
  
  #SKILLS PLOT
  output$skills <- renderPlotly({
    if(is_null(input$file1)) return(NULL)
    rows = input$sheet_rows_all
    df <- groups()[rows,]
    df <- df %>% select(assignment,c(9:(ncol(df)))) %>% 
      group_by(assignment) %>% 
      summarise_all(funs(sum))
    df <- melt(df,"assignment")
    p <- ggplot(df,
           aes(assignment,value)) +
      geom_col(aes(fill=variable),
               position=position_dodge()) +
      xlab("Group") +
      ylab("Number of People in Group with Skill")
    ggplotly(p)
  })
  

  #### TEXT STUFF
  
  #SLIDER TEXT
  output$slider <- renderText({
    paste("<h5>Number of People per Group: <strong>",input$group.size,"</strong></h5>",sep=" ")
  })
  
  ################## STUDENT TAB ################
  
  student_sheet <- reactive({
    if(input$usefile == T){
      if(is_null(input$file2)) return(NULL)
      inFile <- input$file2
      ss <- read.csv(inFile$datapath, header = T,stringsAsFactors = F)
      ss <- ss[,c(2,8:ncol(ss))]
    }
    else{
      if(is_null(input$file1)) return(NULL)
      ss <- groups()[,c(1,7:ncol(groups()))]
    }
    ss$assignment <- as.numeric(ss$assignment)
    ss
  })
  
  chosen <- reactive({
    picked <- student_sheet()[student_sheet()$assignment == input$pickedgroup,]
    picked
  })
  
  # People in Group
  output$peopleingroup <- renderText({
    gm <- "<strong>Group Members:</strong><br>"
    n <- paste(chosen()$name,collapse=", ")
    paste(gm,n)
  })
  
  # Spider plot
  output$spider <- renderPlot({
    if(is_null(student_sheet())) return(NULL)
    cur_group <- chosen()
    rownames(cur_group) <- cur_group$names
    cur_group_data <- cur_group[,-c(1,2)]
    # print(cur_group_data)
    
    totals <- as.data.frame(colSums(cur_group_data))
    totals <- as.data.frame(transpose(totals))
    names(totals) <- names(cur_group_data)
    nmax <- rep(input$group.size,ncol(totals))
    nmin <- rep(0,ncol(totals))
    
    d <- rbind(nmax,nmin,totals)
    p <- radarchart(d  , axistype=1 ,
                    
                    #custom polygon
                    pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,
                    
                    #custom the grid
                    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,7,1), cglwd=0.8,
                    
                    #custom labels
                    vlcex=0.8,
                    title = paste("Skill Distribution for Group",input$pickedgroup)
    )
    p
  })
  
  # GROUP SELECTION
  output$grouppick <- renderUI({
    g <- sort(unique(student_sheet()$assignment))
    # print(g)
    pickerInput(inputId = 'pickedgroup',
                label = 'Select Group',
                choices = g,
                multiple = F)
  })
  
  # generate report
  output$report <- downloadHandler(
    filename = "StudentGroupsReport.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "studentgroupsreport.Rmd")
      file.copy("studentgroupsreport.Rmd",tempReport,overwrite = TRUE)
      
      params <- list(groups = student_sheet(),
                     size = input$group.size)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}