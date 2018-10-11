server <- function(input,output,session){
  options(expressions = 5e5)
  # raw table
  t <- reactive({
    # read the url, create dfs
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      s <- (gs_url(input$url))
      table <- as.data.frame(gs_read(s))
    }
    else{
      s <- read.csv(inFile$datapath, header = input$header)
      s[,1:ncol(s)] <- lapply(s[,1:ncol(s)],function(x) as.character(x))
      table <- s
    }
    
    # rename skill columns
    skill.names <- vector()
    for(i in seq(ncol(table)-7)){
      skill.names <- c(skill.names,paste("skill",as.character(i),sep="."))
    }
    names(table) <- c("time","name","interest","group.names","graded","lda","program",skill.names)
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
    newCol <- column %>% as.factor() %>% as.numeric()
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
  do_lda <- function(df,columns=c(1,6)){
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
    # cl.lda <- t()
    # # add lda topics to df
    # rownames(cl.lda) <- cl.lda$time
    # cl.lda <- merge(cl.lda,topic.list,by=0)
    # rownames(cl.lda) <- 1:nrow(cl.lda)
    # cl.lda
  })
  le <- reactive({
    cl.le <- t()[,c("time","interest","graded","program")]
    # label encoding
    cl.le$interest <- label_encode(t()$interest)
    cl.le$graded <- label_encode(t()$graded)
    cl.le$program <- label_encode(t()$program)
    rownames(cl.le) <- cl.le$time
    # drop the timestamp column now that it's the index
    cl.le <- cl.le[-c(1)]
    cl.le
  })
  skills <- reactive({
    sk <- t()[,c(1,8:ncol(t()))]
    # label encode skills
    skill.cols <- colnames(sk[-c(1)])
    for(i in skill.cols){
      sk[,i] <- label_encode(sk[,i])
    }
    rownames(sk) <- sk$time
    sk <- sk[-c(1)]
    sk
  })
  clean <- reactive({
    cl <- t()[,c("time","name")]
    rownames(cl) <- cl$time
    cl <- cl[-c(1)]

    cl2 <- merge(cl,top(),by=0)
    rownames(cl2) <- t()$time
    cl2 <- cl2[-c(1)]

    cl3 <- merge(cl2,le(),by=0)
    rownames(cl3) <- t()$time
    cl3 <- cl3[-c(1)]

    cl4 <- merge(cl3,skills(),by=0)
    rownames(cl4) <- 1:nrow(cl4)
    colnames(cl4)[1] <- "time"
    last <- ncol(cl4)
    cl4 <- cl4[,c(2,4:last,3)]


  })

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
  make_groups <- function(s, max.size, min.size){
    
    max.size <- as.numeric(max.size)
    min.size <- as.numeric(min.size)
    
    sweet.spot <- floor(mean(c(max.size,min.size)))
    group.count <- 0
    
    for(group in seq_along(s)){
      assignments <- vector()
      # define group sizes
      rows <- nrow(s[[group]])
      s[[group]]$assignment <- 0
      # if the number of people in an interest group is equal to the min
      # then they are all in a group together.
      if(rows == min.size){
        group.count <- group.count+1
        assignments <- c(assignments,rep(group.count,rows))
        s[[group]]$assignment <- assignments
        next
      }
      
      rem <- rows %% sweet.spot
      ifelse(rem == 0, stop.at <- 0, stop.at <- rem+sweet.spot)
      while(rows > stop.at){
        group.count <- group.count+1
        assignments <- c(assignments,rep(group.count,sweet.spot))
        rows <- rows-sweet.spot
      }
      if(rows == max.size){
        group.count <- group.count+1
        assignments <- c(assignments,rep(group.count,rows))
        
        s[[group]]$assignment <- assign_groups(s[[group]],assignments)
        next
      }
      else if(sweet.spot-rem == 1){
        group.count <- group.count+1
        assignments <- c(assignments,rep(group.count,sweet.spot))
        
        group.count <- group.count+1
        assignments <- c(assignments,rep(group.count,rem))
        
        s[[group]]$assignment <- assign_groups(s[[group]],assignments)
        next
      }
      else{
        while(rows > 0){
          group.count <- group.count+1
          assignments <- c(assignments,rep(group.count,min.size))
          rows <- rows-min.size
        }
        s[[group]]$assignment <- assign_groups(s[[group]],assignments)
      }
    }
    
    final.df <- s
    for(i in seq_along(final.df)){
      r <- c(final.df[[i]] %>% rownames() %>% as.numeric())
      final.df[[i]]$topics <- t()[r,]$lda
      final.df[[i]]$interest <- t()[r,]$interest
      final.df[[i]]$program <- t()[r,]$program
      final.df[[i]]$graded <- t()[r,]$graded
    }
    final.df <- bind_rows(final.df)
    final.df
  }
  
  # helper function, shuffles groups until there is an 
  # appropriate diversity of skills (group size as limiting factor)
  until_its_good <- function(df,size){
    # important!!!
    last.skill <- ncol(df)-2
    skills.table <- df[,6:last.skill]
    # skills.table[is.na(skills.table)] <- 0
    samp <- skills.table[sample(nrow(df),size),]
    # print(samp)
    sums <- colSums(samp)
    if(sum(samp > size)>0){
      until_its_good(df,size)
    }
    else{
      df[rownames(samp),]
    }
  }
  
  # make sure that none of the interest groups are  
  # smaller than the minimum size
  # (I'm sure there's a cleaner way to do this)
  at_least_min <- function(df){
    drop <- vector()
    for(i in seq_along(df)){
      if(nrow(df[[i]]) < as.numeric(input$min.size)){
        small <- df[[i]]
        drop <- c(drop,i)
        
        for(row in seq(nrow(small))){
          for(j in seq_along(df)){
            if(j == i){
              next()
            }
            if(small[row,]$topics %in% df[[j]]$topics){
              df[[j]] <- rbind(df[[j]],small[row,])
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
    # break up into separate dfs based on interest
    interest.dfs <- split(clean(),as.factor(clean()$interest))
    interest.dfs <- at_least_min(interest.dfs)
    ma <- input$max.size
    mi <- input$min.size
    final.groups <- make_groups(interest.dfs,ma,mi)
    final.groups
    })
  
  output$sheet <- renderDT({
    if(is_null(input$file1)) return(NULL)
    groups()[,c(1,2,ncol(groups()))]
    },
    server= FALSE,
    rownames = FALSE,
    # groups(),
    # x,
    editable = TRUE,
    selection = 'none'
  )
  
  proxy = dataTableProxy('sheet')
  values <- reactiveValues()

  observeEvent(input$sheet_cell_edit,{
                 info = input$sheet_cell_edit
                 str(info)
                 i = info$row
                 j = info$col
                 v = as.numeric(info$value)
                 print(v)
                 print(groups()[i, ncol(groups())])

                 groups()[i, ncol(groups())] <<- DT::coerceValue(v, groups()[i, ncol(groups())])
                 replaceData(proxy, groups(), resetPaging = FALSE)
               }
  )
  
  # renders cluster plot with n_clus = average(max.size,min.size)
  # just because its cool 
  output$clusters <- renderPlotly({
    if(is_null(input$file1)) return(NULL)
    data <- clean()[-c(4,5:(ncol(clean())-1))]
    rownames(data) <- data$name
    data <- data[-c(1)]
    nclus <- floor(nrow(data)/((as.numeric(input$max.size) + as.numeric(input$min.size))/2))
    p <- autoplot(kmeans(data,nclus),data=data,frame=TRUE, label=TRUE,label.size=3)
    ggplotly(p, tooltip = c("cluster"),layerData = 3)
  })
  
  output$skills <- renderPlotly({
    if(is_null(input$file1)) return(NULL)
    input$sheet_cell_edit
    df <- groups()
    df <- df %>% select(assignment,c(5:(ncol(df)-2))) %>% 
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
}