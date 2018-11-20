server <- function(input,output,session){
  options(expressions = 5e5)
  
  # raw table
  t <- reactive({
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
    names(table) <- c("time","name","interest","group.names","graded","lda","program",skill.names)
    rownames(table) <- table$name
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
  
  do_lda <- function(df,columns=c(2,6)){
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
    names(topic.list) <- "topics"
    topic.list
    
  }
  
  # cleaned up; columns are ("name","interest","graded","program","skills"..."topics")
  # everything is label encoded
  top <- reactive({
    topic.list <- do_lda(t())
  })
  
  le <- reactive({
    cl.le <- t()[,c("interest","graded","program")]
    # label encoding
    cl.le$interest <- label_encode(t()$interest)
    cl.le$graded <- label_encode(t()$graded)
    cl.le$program <- label_encode(t()$program)
    cl.le
  })
  
  skills <- reactive({
    sk <- t()[,c(8:ncol(t()))]
    # label encode skills
    skill.cols <- colnames(sk)
    for(i in skill.cols){
      sk[,i] <- label_encode(sk[,i])
    }
    sk
  })
  
  clean <- reactive({
    cl <- t()[,c("time","name")]
    cl$topics <- top()$topics
    cl$interest <- le()$interest
    cl$graded <- le()$graded
    cl$program <- le()$program
    cl <- merge(cl,skills(),by=0)
    last <- ncol(cl)
    cl <- cl[,c(3,5:last,4)]
    cl <- split(cl,as.factor(cl$interest))
    cl <- bind_rows(cl)
    cl$alternate_grouping_strategy <- ""
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
        # only shuffle if num_groups > 1
        if(num_groups > 1){
          s[[group]]$assignment <- assign_groups(s[[group]],assignments)
        }
        else{
          s[[group]]$assignment <- assignments
        }
      }
    }
    
    final.df <- s
    for(i in seq_along(final.df)){
      rownames(final.df[[i]]) <- final.df[[i]]$name
      r <- rownames(final.df[[i]])
      final.df[[i]]$topics <- t()[r,]$lda
      final.df[[i]]$interest <- t()[r,]$interest
      final.df[[i]]$program <- t()[r,]$program
      final.df[[i]]$graded <- t()[r,]$graded
    }
    final.df <- bind_rows(final.df)
    ra <- c()
    for(i in 1:nrow(final.df)){
      if(final.df$assignment[i] == 0){
        ra <- c(ra,"randomly assigned")
      }
      else{
        ra <- c(ra,final.df$alternate_grouping_strategy[i])
      }
    }
    final.df$alternate_grouping_strategy <- ra
    unassigned <- final.df[final.df$assignment == 0,]
    
    for(i in 1:nrow(unassigned)){
      pool <- final.df[final.df$interest == unassigned[i,]$interest[1],]
      for(g in unique(pool$assignment)){
        if(nrow(pool[pool$assignment == g,]) < as.numeric(input$group.size)+1 && g != 0){
          final.df[rownames(unassigned[i,]),]$assignment <- pool[pool$assignment == g,]$assignment[1]
        }
      }
    }
    unassigned <- final.df[final.df$assignment == 0,]
    assigned <- final.df[final.df$assignment != 0,]
    if(nrow(unassigned) > 0){
      # unassigned <- at_least_min(unassigned,final.df$interest)
      # print(unassigned)
    }
    
    final.df$assignment <- as.factor(final.df$assignment)
    final.df$interest <- as.factor(final.df$interest)
    final.df$alternate_grouping_strategy <- as.factor(final.df$alternate_grouping_strategy)
    final.df
  }
  
  # helper function, shuffles groups until there is an 
  # appropriate diversity of skills (group size as limiting factor)
  until_its_good <- function(df,size){
    # important!!! (because now there is a topics and assignment column as the last columns)
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
            if(small[row,]$topics %in% df[[j]]$topics){
              name <- small[row,]$name
              df[[j]] <- rbind(df[[j]],small[row,])
              df[[j]][df[[j]]$name == name,]$alternate_grouping_strategy <- "recommended"
              # print(df[[j]][df[[j]]$name == name,]$alternate_grouping_strategy)
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
    interest.dfs <- at_least_min(clean(),clean()$interest)
    final.groups <- make_groups(interest.dfs,input$group.size)
    
    # get a list of skills so we can display in expanded row
    allskills <- list()
    final.groups$skills <- ""
    for(i in 1:nrow(final.groups)){
      tempskills <- ""
      for(j in 8:15){
        if(final.groups[i,names(final.groups)[j]] == 1){
          tempskills <- paste(tempskills,names(final.groups)[j],sep= " • ")
          # tempskills <- substr(tempskills,2,nchar(tempskills))
        }
      }
      final.groups$skills[i] <- tempskills
    }
    
    
    final.groups <- final.groups[,c(1,2,3,4,ncol(final.groups),ncol(final.groups)-3,ncol(final.groups)-2,ncol(final.groups)-1,5:(ncol(final.groups)-4))]
    # print(c(1,2,3,4,ncol(final.groups),ncol(final.groups)-3,ncol(final.groups)-2,ncol(final.groups)-1,5:(ncol(final.groups)-4)))
    # print(names(final.groups))
    final.groups
    })
  
  # options for DT
  opts <- reactive({
    if(is_null(input$file1)){
      list()
    }
    else{
      list(
        columnDefs = list(
          list(visible = F, targets = c(0,4,5,6,7,10:(ncol(groups())+1))
          ),
          list(orderable = T, className = 'details-control', targets = 1)
        )
      ) 
    }
  })
  
  output$sheet <- renderDT({
    if(is_null(input$file1)) return(NULL)
    df <- cbind(' ' = '&oplus;',groups())
    # names(df)[ncol(df)] <- "Group Assignment"
    df
    },
    escape = -2,
    # server= FALSE,
    filter = 'top',
    # rownames = FALSE,
    selection = 'none',
    options = opts,
    callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
                  var format = function(d) {
                  return '<div style=\"background-color:#add8e6; padding: .5em;\"><strong>Taking for a grade?:</strong> ' +
                  d[4] +'<br>'+ '<strong>Program:</strong> ' + d[5] +'<br>'+ '<strong>Short Answer:</strong> ' + d[7] + '<br>'+ '<strong>Skills:</strong> ' + d[6] +'</div>';
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
  
  # just because its cool 
  # output$clusters <- renderPlotly({
  #   if(is_null(input$file1)) return(NULL)
  #   rows = input$sheet_rows_all
  #   data <- clean()[rows,-c(4,5:(ncol(clean())-1))]
  #   rownames(data) <- data$name
  #   data <- data[-c(1)]
  #   nclus <- floor(nrow(data)/as.numeric(input$group.size))
  #   clus_results <- kmeans(data,nclus)
  #   p <- autoplot(clus_results,data=data,frame=TRUE, label=TRUE,label.size=5)
  #   # print(clus_results)
  #   # ggplotly(p, tooltip = c("name"),layerData = 3)
  #   p
  # })
  
  
  
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


  output$slider <- renderText({
    paste("<h4>Number of People per Group (+/- 1): <strong>",input$group.size,"</strong></h4>",sep=" ")
  })
}