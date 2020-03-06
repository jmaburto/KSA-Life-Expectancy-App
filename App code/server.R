library(ggplot2)
library(data.table)
library(ggthemes)
library(DT)
library(gridExtra)
library(plotly)
library(forcats)
library(reshape2)

load('COD_Decomposition.RData')
load('YLL.RData')
load('UN_Projections.RData')

shinyServer(
  function(input, output) {
    
   
    Periods <- c('2000-2005','2005-2010','2010-2015')
    
    var1 <- reactive(Periods[(which(Periods ==input$period.ind1)+1):3])
    
    output$vx <- renderUI({
      selectInput('final.ind','Ending period',choices = var1())
    })
    
    var2 <- reactive(Periods[(which(Periods ==input$period.ind2)+1):3])
    
    output$vx2 <- renderUI({
      selectInput('final.ind2','Ending period',choices = var2())
    })
    
    base2 <- c("#882E72", "#B178A6", 
               "#1965B0",
               #"#5289C7", "#7BAFDE", "lightblue1", 'blue',
               "seagreen",
               #"#4EB265",
               "seagreen3",
               "#CAE0AB",
               "#E8601C","#F1932D","#F6C141", 
               "lightgrey") 
    
    cause.deleted <- function(Rxi = COD1m, LT = LT1m, Reduction=r,Sex = 'Males', im = r0...){
      
      Rxi[nrow(Rxi)] <- (apply(Rxi,2,cumsum)/cumsum(rowSums(Rxi)))[nrow(Rxi)]
      Nc   <- ncol(Rxi)
      Na   <- nrow(LT)
      p    <- 1-Reduction/100 
      px   <-c(as.numeric(as.character(LT$px[1:(Na-1)])),0)
      pxi  <- px^(t(t(Rxi)*p)) 
      pxi[1,10] <- pxi[1,10]^(1-im/100)
      lxi  <-rep(1,Nc)
      lxi  <-rbind(lxi,lxi*pxi[1,])
      for (x in 2:(Na-1)){
        lxi<-rbind(lxi,lxi[x,]*pxi[x,])
      }
      lx   <-apply(lxi,1,prod)
      dx   <-c(lx[-Na]-lx[-1],lx[Na])
      ax   <-LT$ax
      Lx   <-LT$n[-Na]*lx[-1]+(ax*dx)[-Na]
      Lx   <-c(Lx,(ax*dx)[Na])  
      Tx   <-c(sum(Lx),(rep(sum(Lx),Na)-cumsum(Lx))[-Na])
      ex   <-Tx/lx
      gain <- round(ex-LT$ex,2)
      lxn  <- c(LT$lx/100000,lx)
      
      LTi  <- data.table(cbind(Age = rep(LT$Age,2),lx = lxn,exn = c(LT$ex,ex), e0= round(c(rep(LT$ex[1],Na),rep(ex[1],Na)),2)))
      LTi$Source <- c(rep('Original',Na),rep('New',Na))
      LTi$Sex <- rep(Sex,2*Na)
      return(list(LTi = LTi,e0.original = LT$ex[1], e0.new=ex[1],gain = gain))
    }
    
    LostYears<-function(FLT=LT1,B=COD1){
      lx<-FLT$lx/100000
      dx<-FLT$dx/100000
      Lx<-FLT$Lx/100000
      ax<-FLT$ax
      n <- FLT$n
      
      Rxi <-  B
      G3  <- apply(B,2,cumsum)
      FD  <- cumsum(rowSums(B))
      fxi <- G3/FD
      fxi <- fxi
      
      ## we use the life table functions to separate the person years and 
      ## person lost
      LYLi<-n*(1-lx)*fxi+(n-ax)*dx*Rxi
      return(LYLi)
    }
    
    Dxi<-function(FLT=LT1f,B=COD1f){
      # Now we calculate the total number of lost years up to certain age
      causesN <- c(colnames(B), 'Total')
      
      # for the graph
      lx<-as.numeric(FLT$lx)/100000
      dx<-as.numeric(FLT$dx)/100000
      Fdxi<-B*dx
      dxi <-apply(Fdxi,2,cumsum)
      dxi
    }
    
    output$COD.Distribution <- renderPlotly({
      #y <- '2000-2005'
      y       <- input$year.ind
      
      ifelse(y == '2000-2005', y1 <- 1,ifelse(y == '2005-2010', y1 <- 2 , y1 <- 3))
      
      Data     <- COD.Fig1
      Data     <- Data[Data$Period == y1,]
      Data     <- Data[,c('Period','Sex','Age.group','cause.name','Fraction')]
      
      Data$cause.name <- factor(Data$cause.name, levels = c("Tuberculosis and HIV/AIDS","Lower respiratory and other common infectious diseases",
                                                            "Neoplasms","Cardiovascular diseases","Urogenital, blood, and endocrine diseases",
                                                            "Diabetes","Transport injuries","Unintentional injuries",
                                                            "Self-harm and interpersonal violence","Others"))
      
      
      Data$Fraction <- round(Data$Fraction*100,2)
      names(Data)[5] <- 'Proportion'
      #unique(Data$Age.group)
      
      
      fig <- ggplot(Data,aes(x=Age.group,y = Proportion,fill = cause.name)) +
        geom_bar(aes(group = cause.name), stat = "identity",position = position_stack(reverse = F))+
        theme_light() + 
        facet_wrap(~Sex)+
        labs(x = "Age", y = "Proportion (%)",size=10)+
        scale_fill_manual('Cause of death', values = base2) +
        geom_hline(yintercept = 100)+ 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(text = element_text(size=14))+
        theme(strip.background = element_rect(fill="gray87"))+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black",face="bold"),
              plot.margin = margin(25,10,10,20))
      fig <-ggplotly(fig,width = 1400, height = 600,tooltip = 'Proportion') 
      fig[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -0.07
      fig[['x']][['layout']][['annotations']][[1]][['y']] <- -.13
      # Set legend label position    
      #fig[["x"]][["layout"]][["annotations"]][[3]][["y"]] <- 0.93
      fig <- fig %>% layout(margin = list(l = 120, b=70))
      ggplotly(fig) %>% 
        config(modeBarButtonsToRemove= list('toImage', 
                                            'sendDataToCloud',
                                            'hoverClosestCartesian',
                                            'hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE)
      
      
    })
    
    output$Decompostion.plot <- renderPlotly({
      
      #y1 <- '2000-2005' 
      y1       <- input$period.ind1
      Data     <- COD_Decomposition
      # ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      y2 <- input$final.ind
      Data     <- Data[Data$Period1 == y1 & Data$Period2 == y2,]
      Data$Contribution <- round(Data$Contribution,3)
      Data$Contribution. <- Data$Contribution
      
      p <- ggplot(Data, aes(x = Age.group, y = Contribution, fill =Cause, label= Contribution.)) +
        ggtitle(paste('Age- and cause-of-death contribution to the change in life expectancy between', y1, 'and ', y2))+
        geom_bar(stat = "identity", position = 'stack')+
        facet_wrap(~Sex)+
        scale_fill_manual('Cause of death', values = base2) + 
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = "Age", y = "Contribution (years)",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black", face='bold'))+
        theme(strip.background = element_rect(fill="gray87"))+
              #plot.margin = margin(10,10,10,15))+
        geom_hline(yintercept = 0)
      #  coord_flip()
      rr <- ggplotly(p,width = 1400, height = 450,tooltip = c('Cause','Contribution.'))
      rr[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -0.06
      rr[['x']][['layout']][['annotations']][[1]][['y']] <- -.13
      # Set legend label position    
      #rr[["x"]][["layout"]][["annotations"]][[3]][["y"]] <- 0.93
      rr <- rr %>% layout(margin = list(l = 120, b=70))
      ggplotly(rr) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      #print(rr)
    })
    
    output$DT.summary = renderDataTable({
      
      y1       <- input$period.ind1
      Data     <- UN_LT
      y2 <- input$final.ind
      
      Data <- Data[Data$Age==0 & Data$Period %in% c(y1,y2),] 
      Data <- Data[,c('Period', 'Sex', 'ex')]
      Data$ex <- round(Data$ex,2)
      rownames(Data) <- NULL
      Data$Sex[2] <- ''
      Data$Sex[4] <- ''
      Data$Sex[6] <- ''
      colnames(Data)[3] <- 'Life expectancy at birth'
      Data$Difference <-c(' ',round(diff(Data$'Life expectancy')[1],2), ' ',round(diff(Data$'Life expectancy')[3],2),' ',round(diff(Data$'Life expectancy')[5],2))
      datatable(Data, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F,caption = 'Life expectancy by sex and period')
    })
    
    output$Decompostion.table = renderDataTable({
      #y1 <- '2000-2005' 
      y1       <- input$period.ind1
      Data     <- COD_Decomposition
      y2 <- input$final.ind
      Data     <- Data[Data$Period1 == y1 & Data$Period2 == y2,]
      Data     <- Data[,list(Total = round(sum(Contribution),3)), by = list(Sex,Cause)]
      
      Data <- dcast.data.table(data = Data,formula = Cause~Sex,value.var = 'Total')
      
      # levels(Data$Cause)[2] <- 'Lower respiratory and other common infectious diseases'
      
      datatable(Data, options = list(paging=FALSE,ordering=T,dom = 't'),rownames = F,caption = paste('Life expectancy changes between years ',y1,'to',y2,'.'))
    })
    
    output$YLL.plot <- renderPlotly({
      Infant <- matrix(rep(c(rep(0,9),1),1),nrow = 1,ncol = 10,byrow = T)
      rownames(Infant) <- c('0')
      #y1 <- '2000-2005'
      y1       <- input$LYL.Ind
      # ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      LT1f     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y1,]
      LT1m     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y1,]
      LT1t     <- UN_LT[UN_LT$Sex == 'Total' & UN_LT$Period == y1,]
      
      
      COD1f <- COD_Profiles[[y1]]
      COD1f <- COD1f[COD1f$Sex == 'Female' & COD1f$Age_id <= 19,]
      COD1f <- acast(COD1f, Age.group ~ cause.name , value.var = 'Fraction')
      COD1f <- rbind(Infant,COD1f)
      
      COD1m <- COD_Profiles[[y1]]
      COD1m <- COD1m[COD1m$Sex == 'Male' & COD1m$Age_id <= 19,]
      COD1m <- acast(COD1m, Age.group ~ cause.name , value.var = 'Fraction')
      COD1m <- rbind(Infant,COD1m)
      
      COD1t <- COD_Profiles.total[[y1]]
      COD1t <- COD1t[COD1t$Age_id <= 19,]
      COD1t <- acast(COD1t, Age.group ~ cause.name , value.var = 'Fraction')
      COD1t <- rbind(Infant,COD1t)
      
      dxif <- Dxi(FLT = LT1f[1:19],B = COD1f)[1:18,]
      dxim <- Dxi(FLT = LT1m[1:19],B = COD1m)[1:18,]
      dxit <- Dxi(FLT = LT1t[1:19],B = COD1t)[1:18,]
      
     
      Data.figf <- data.table(melt(dxif))
      Data.figf$Sex <- 'Female'
      Data.figm <- data.table(melt(dxim))
      Data.figm$Sex <- 'Male'
      Data.figt <- data.table(melt(dxit))
      Data.figt$Sex <- 'Total'
      
      
      Data.fig <- rbind(Data.figf,Data.figm,Data.figt)
      
      names(Data.fig) <- c('Age','Cause','LYL', 'Sex')
      
      Data.fig <- Data.fig[,  ymin := 1-cumsum(LYL), by = list(Age, Sex)]
      Data.fig <- Data.fig[,  ymax := c(1,ymin[-length(ymin)]), by = list(Age, Sex)]
      Data.fig$LYL <- round(Data.fig$LYL,2)
    
      
      p <- ggplot(Data.fig, aes(Age, label=LYL))+
        ggtitle(paste('Life years lost for the period', y1))+
        geom_ribbon(aes(ymin = ymin,ymax =ymax, group=fct_rev(Cause), fill=Cause), position = 'identity')+
        facet_wrap(~Sex)+
        scale_fill_manual('Cause of death', values = base2)+
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = "Age", y = "Probability of surviving and life years lost",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black", face='bold'))+
        theme(strip.background = element_rect(fill="gray87"))
      
      rr <- ggplotly(p,width = 1400, height = 450,tooltip = c('Cause','Age','LYL'))
      rr[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -0.06
      rr[['x']][['layout']][['annotations']][[1]][['y']] <- -.13
      rr <- rr %>% layout(margin = list(l = 120, b=70))
      ggplotly(rr) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      })
    
    output$LYL.summary = renderDataTable({
      
      Infant <- matrix(rep(c(rep(0,9),1),1),nrow = 1,ncol = 10,byrow = T)
      rownames(Infant) <- c('0')
      
      y1       <- input$LYL.Ind
      #y1 <- '2000-2005'
      # ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      LT1f     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y1,]
      LT1m     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y1,]
      LT1t     <- UN_LT[UN_LT$Sex == 'Total' & UN_LT$Period == y1,]
      
      
      COD1f <- COD_Profiles[[y1]]
      COD1f <- COD1f[COD1f$Sex == 'Female' & COD1f$Age_id <= 19,]
      COD1f <- acast(COD1f, Age.group ~ cause.name , value.var = 'Fraction')
      COD1f <- rbind(Infant,COD1f)
      
      COD1m <- COD_Profiles[[y1]]
      COD1m <- COD1m[COD1m$Sex == 'Male' & COD1m$Age_id <= 19,]
      COD1m <- acast(COD1m, Age.group ~ cause.name , value.var = 'Fraction')
      COD1m <- rbind(Infant,COD1m)
      
      COD1t <- COD_Profiles.total[[y1]]
      COD1t <- COD1t[COD1t$Age_id <= 19,]
      COD1t <- acast(COD1t, Age.group ~ cause.name , value.var = 'Fraction')
      COD1t <- rbind(Infant,COD1t)
      
      Cause    <- data.table(Cause = colnames(LostYears(LT1m[1:19],COD1m)[1:18,]))
      malesLYL <- data.table( Male = colSums(LostYears(LT1m[1:19],COD1m)[1:18,]))
      femalesLYL <- data.table( Female = colSums(LostYears(LT1f[1:19],COD1f)[1:18,]))
      totalLYL <- data.table( Total = colSums(LostYears(LT1t[1:19],COD1t)[1:18,]))
      
      Cause$Female <- round(femalesLYL$Female,2)
      Cause$Male <- round(malesLYL$Male,2)
      Cause$Total <- round(totalLYL$Total,2)
      
      Total <- cbind(Cause='Total', Male = sum(Cause$Male), 
                     Female = sum(Cause$Female), 
                     Total = sum(Cause$Total))
      Cause <- rbind(Cause,Total)
      
      
      datatable(Cause, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F,caption = paste('Total life years lost by cause and sex for ', y1))
    })
    
    output$CauseDeleted <- renderPlotly({
      y1 <- '2010-2015' 
      Infant <- matrix(rep(c(rep(0,9),1),1),nrow = 1,ncol = 10,byrow = T)
      rownames(Infant) <- c('0')
      
      LT1f     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y1,]
      LT1m     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y1,]
      LT1t     <- UN_LT[UN_LT$Sex == 'Total' & UN_LT$Period == y1,]
      
      COD1f <- COD_Profiles[[y1]]
      COD1f <- COD1f[COD1f$Sex == 'Female']
      COD1f <- acast(COD1f, Age.group ~ cause.name , value.var = 'Fraction')
      COD1f <- rbind(Infant,COD1f,Infant)
      rownames(COD1f)[21:22] <- c('95 - 99','100+')
      
      COD1m <- COD_Profiles[[y1]]
      COD1m <- COD1m[COD1m$Sex == 'Male']
      COD1m <- acast(COD1m, Age.group ~ cause.name , value.var = 'Fraction')
      COD1m <- rbind(Infant,COD1m,Infant)
      rownames(COD1m)[21:22] <- c('95 - 99','100+')
      
      COD1t <- COD_Profiles.total[[y1]]
      COD1t <- acast(COD1t, Age.group ~ cause.name , value.var = 'Fraction')
      COD1t <- rbind(Infant,COD1t,Infant)
      rownames(COD1t)[21:22] <- c('95 - 99','100+')
      
      
      #r9 <- r1 <- r2 <- r3 <- r4<- r5<- r6<- r7<- r8<- r10<- r11<- r12 <- r0 <- 0
      
      r0 <- (input$cause_ind0)
      r1 <- (input$cause_ind1)
      r2 <- (input$cause_ind2)
      r3 <- (input$cause_ind3)
      r4 <- (input$cause_ind4)
      r5 <- (input$cause_ind5)
      r6 <- (input$cause_ind6)
      r7 <- (input$cause_ind7)
      r8 <- (input$cause_ind8)
      r9 <- (input$cause_ind9)
      #r10 <- (input$cause_ind10)
      #r11 <- (input$cause_ind11)
      #r12 <- (input$cause_ind12)
      
      r <- c(r1,r2,r3,r4,r5,r6,r7,r8,r9,0)
      
      males    <- cause.deleted(Rxi = COD1m,LT = LT1m,Reduction = r, Sex = 'Males', im = r0)
      females  <- cause.deleted(Rxi = COD1f,LT = LT1f,Reduction = r , Sex = 'Females', im = r0)
      total    <- cause.deleted(Rxi = COD1t,LT = LT1t,Reduction = r , Sex = 'Total', im = r0)
      
      e0.new   <- data.table(rbind(males$e0.new,females$e0.new,total$e0.new))
      e0.new$Year <- 'New life expectancy'
      e0.new$Sex <- c('Males','Females', 'Total')
      
      e0.original   <- data.table(rbind(males$e0.original,females$e0.original,total$e0.original))
      e0.original$Year <- '2010-2015'
      e0.original$Sex <- c('Males','Females','Total')
      
      e0.projected   <- data.table(UN_Projections[UN_Projections$Time == '2025-2030' & UN_Projections$AgeGrpStart == 0,]$ex)
      e0.projected$Year <- 'Projected 2025-2030'
      e0.projected$Sex <- c('Males','Females','Total')
      
      fig.data <- rbind(e0.new,e0.original,e0.projected)
      setnames(fig.data,'V1','e0')
      fig.data$e0 <- round(fig.data$e0,2)
      
      p <- ggplot(fig.data, aes(x = e0, y = fct_rev(Year),col= Sex,shape = Sex)) +
        geom_point(size=6)+
        scale_colour_manual(name = NULL,values = c('blue','red','black'))+
        theme_light()+
        labs(x = 'Life expectancy', y = NULL,size=12)+
        theme(text = element_text(size=12),
              strip.text.x = element_text(size = 12, colour = "black"),
              plot.margin = margin(25,10,10,20))
      
      #p
      
      ggplotly(p,tooltip = 'e0') %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE)
    
    })
    
    output$GainText <- renderText({
      
      y1 <- '2010-2015' 
      Infant <- matrix(rep(c(rep(0,9),1),1),nrow = 1,ncol = 10,byrow = T)
      rownames(Infant) <- c('0')
      
      LT1f     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y1,]
      LT1m     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y1,]
      
      COD1f <- COD_Profiles[[y1]]
      COD1f <- COD1f[COD1f$Sex == 'Female']
      COD1f <- acast(COD1f, Age.group ~ cause.name , value.var = 'Fraction')
      COD1f <- rbind(Infant,COD1f,Infant)
      rownames(COD1f)[21:22] <- c('95 - 99','100+')
      
      COD1m <- COD_Profiles[[y1]]
      COD1m <- COD1m[COD1m$Sex == 'Male']
      COD1m <- acast(COD1m, Age.group ~ cause.name , value.var = 'Fraction')
      COD1m <- rbind(Infant,COD1m,Infant)
      rownames(COD1m)[21:22] <- c('95 - 99','100+')
      
      #r9 <- r1 <- r2 <- r3 <- r4<- r5<- r6<- r7<- r8<- r10<- r11<- r12 <- r0 <- 0
      
      r0 <- (input$cause_ind0)
      r1 <- (input$cause_ind1)
      r2 <- (input$cause_ind2)
      r3 <- (input$cause_ind3)
      r4 <- (input$cause_ind4)
      r5 <- (input$cause_ind5)
      r6 <- (input$cause_ind6)
      r7 <- (input$cause_ind7)
      r8 <- (input$cause_ind8)
      r9 <- (input$cause_ind9)
      #r10 <- (input$cause_ind10)
      #r11 <- (input$cause_ind11)
      #r12 <- (input$cause_ind12)
      
      r <- c(r1,r2,r3,r4,r5,r6,r7,r8,r9,0)
      
      males    <- cause.deleted(Rxi = COD1m,LT = LT1m,Reduction = r, Sex = 'Males', im = r0)
      females  <- cause.deleted(Rxi = COD1f,LT = LT1f,Reduction = r , Sex = 'Females', im = r0)
      
      
      
      Text <- paste('Potential gains in life expectancy with respect to 2010-2015: ', round(males$gain[1],2) , 'years for males and ', round(females$gain[1],2), ' years for females.')
      
      
    })
    
    output$UserManual <- downloadHandler(
      filename = "UsersManual180530.pdf",
      content = function(file) {
        file.copy("www/UsersManual180530.pdf", file)
      },
      contentType= 'application/pdf'
    )
    
    output$methods <- downloadHandler(
      filename = "SourceMethod180529.pdf",
      content = function(file) {
        file.copy("www/SourceMethod180529.pdf", file)
      },
      contentType= 'application/pdf'
    )
    
  })
