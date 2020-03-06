library(ggplot2)
library(RColorBrewer)
library(data.table)
library(ggthemes)
library(DT)
library(gridExtra)
library(plotly)
library(forcats)
library(reshape2)

#load('Fraction.Rdata') #only if you want to compare WHO with GBD available data
load('COD_Decomposition.RData')
load('COD_Distribution.RData')
load('YLL.RData')
load('UN_Projections.RData')

shinyServer(
  function(input, output) {
    
    base2 <- c("#882E72", "#B178A6", 
               "#1965B0",
               #"#5289C7", "#7BAFDE", "lightblue1", 'blue',
               "seagreen",
               #"#4EB265",
               "seagreen3",
               "#CAE0AB",
               "#E8601C","#F1932D","#F6C141", 
               "lightgrey") 
    
    cause.deleted <- function(Rxi = COD1m, LT = LT1m, Reduction=r,Sex = 'Males',...){
      
      Rxi[nrow(Rxi)] <- (apply(Rxi,2,cumsum)/cumsum(rowSums(Rxi)))[nrow(Rxi)]
      Nc   <- ncol(Rxi)
      Na   <- nrow(LT)
      p    <- 1-Reduction/100 
      px   <-c(as.numeric(as.character(LT$px[1:(Na-1)])),0)
      pxi  <- px^(t(t(Rxi)*p)) 
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
    
    
    Plot_YLL<-function(FLT=LT1,B=COD1, COL = c("blue","lightblue","black","yellow","green","red","Orange","pink", 'grey'), Tit, Total.COD){
      # Now we calculate the total number of lost years up to certain age
      causesN <- c(colnames(B), 'Total')
      Aage<-c(0,1,seq(5,85,by=5))
      # for the graph
      lx<-as.numeric(FLT$lx)/100000
      dx<-as.numeric(FLT$dx)/100000
      Fdxi<-B*dx
      dxi <-apply(Fdxi,2,cumsum)
      ## now we make the plot of the survival function and the causes of death
      ## contributing to the lost years
      plot(c(min(Aage),max(Aage)),xaxt="n",c(0,1),col=0,xlab="Ages",ylab="Probability of surviving and life years lost",las=1,
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      title(Tit,cex.main=1.5)
      axis(1,at=Aage[-2],Aage[-2])
      legend(0,.75,paste0(causesN,' (',c(Total.COD,sum(Total.COD)),')') ,col=c(COL, 'transparent'),pch=19,lty=0,box.lty=0, title= 'Cause of death (years of life lost)', cex=1)
      lines(Aage,lx,col=1,lwd=2)
      xx <- c(Aage, rev(Aage))
      x1 <- lx
      for (i in 1:ncol(B)){
        x2 <- x1 + c(0,dxi[1:18,i])
        yyC <-c(x1,rev(x2))
        polygon(xx, yyC,col=COL[i],border=COL[i])
        x1 <- x1 + c(0,dxi[1:18,i])
      }
    }
    
    output$COD.Distribution <- renderPlotly({
      #base2 <- c("#AA4488","blue", "#CC99BB","#4477AA","#88CCAA","#DDDD77","#771122", "#AA4455", "#DD7788","lightgrey")
      #y1 <- 1999
      y1       <- input$year.ind
      Data     <- GBD_DT
      Data     <- Data[Data$Year == y1,]
      Data     <- Data[,c('Year','Sex','Age.group','cause.name','Fraction')]
      Data     <- Data[Data$Age.group != '90 - 94' & Data$Age.group != '95+']
      
      levels(Data$Age.group)[17] <- '85+'
      
      #unique(Data$Age.group)
      
      fig <- ggplot(Data,aes(x=Age.group,y = Fraction,fill = cause.name)) +
        geom_bar(aes(group = rev(cause.name)), stat = "identity",position = position_fill(reverse = TRUE))+
        theme_light() + 
        facet_wrap(~Sex)+
        labs(x = "Age", y = "Proportion",size=10)+
        scale_fill_manual('Cause of death', values = base2) +
        geom_hline(yintercept = 1)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(text = element_text(size=14))+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 10, colour = "black"),
              plot.margin = margin(25,10,10,20))
      fig <-ggplotly(fig,width = 1400, height = 600,tooltip = 'Fraction') 
      fig[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -0.06
      # Set legend label position    
      #fig[["x"]][["layout"]][["annotations"]][[3]][["y"]] <- 0.93
      fig <- fig %>% layout(margin = list(l = 120, b=70))
      ggplotly(fig) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      
    })
    
    output$Decompostion.plot <- renderPlotly({
      #y1 <- '2000-2005' 
      y1       <- input$period.ind1
      Data     <- COD_Decomposition
      ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      Data     <- Data[Data$Period1 == y1,]
      Data$Contribution <- round(Data$Contribution,3)
      
      p <- ggplot(Data, aes(x = Age.group, y = Contribution, fill =Cause)) +
        ggtitle(paste('Changes between period', y1, 'to period', y2))+
        geom_bar(stat = "identity", position = 'stack')+
        facet_wrap(~Sex)+
        scale_fill_manual('Cause of death', values = base2) + 
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = "Age", y = "Contribution (years)",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
              #plot.margin = margin(10,10,10,15))+
        geom_hline(yintercept = 0)
      #  coord_flip()
      rr <- ggplotly(p,width = 1400, height = 450,tooltip = 'Contribution')
      rr[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -0.06
      # Set legend label position    
      #rr[["x"]][["layout"]][["annotations"]][[3]][["y"]] <- 0.93
      rr <- rr %>% layout(margin = list(l = 120, b=70))
      ggplotly(rr) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
      
      #print(rr)
    })
    
    output$Decompostion.table.males = renderDataTable({
      #y1 <- '2000-2005' 
      y1       <- input$period.ind1
      Data     <- COD_Decomposition
      ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      Data     <- Data[Data$Period1 == y1,]
      Data     <- Data[,list(Total = sum(Contribution)), by = list(Sex,Cause)]
      Data$Total <- round(Data$Total,3)
      levels(Data$Cause)[2] <- 'Diarrhea, and common respiratory and infectious diseases'
      Data <- Data[Data$Sex == 'Male']
      Data <- Data[,2:3]
      
       datatable(Data, options = list(paging=FALSE,ordering=T,dom = 't'),rownames = F,caption = paste('Males. Changes between period ',y1,'to period',y2,'.'))
    })
    
    output$Decompostion.table.females = renderDataTable({
      #y1 <- '2000-2005' 
      y1       <- input$period.ind1
      Data     <- COD_Decomposition
      ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      Data     <- Data[Data$Period1 == y1,]
      Data     <- Data[,list(Total = sum(Contribution)), by = list(Sex,Cause)]
      Data$Total <- round(Data$Total,3)
      levels(Data$Cause)[2] <- 'Diarrhea, and common respiratory and infectious diseases'
      Data <- Data[Data$Sex == 'Female']
      Data <- Data[,2:3]
      
      datatable(Data, options = list(paging=FALSE,ordering=T,dom = 't'),rownames = F,caption = paste('Females. Changes between period ',y1,'to period',y2,'.') )
    })
    
    
    output$Decompostion.plot2 <- renderPlotly({

      #y1 <- '2000-2005'
      y1       <- input$period.ind1
      Data     <- COD_Decomposition
      ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      Data     <- Data[Data$Period1 == y1,]
      Data     <- Data[,list(Total = sum(Contribution)), by = list(Sex,Cause)]
      Data$Total <- round(Data$Total,3)
      levels(Data$Cause)[2] <- 'Diarrhea, and common respiratory and infectious diseases'

      p <- ggplot(Data, aes(x = Total, y = fct_rev(Cause),col= Cause)) +
        ggtitle(paste('Changes between period', y1, 'to period', y2))+
        geom_point(size=3)+
        scale_colour_manual(name = NULL,values = base2)+
        facet_wrap(~Sex)+
        theme_light()+
        labs(x = 'Total', y = " ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 10, colour = "black"),
              plot.margin = margin(25,10,10,20))+
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
      
      #p

     
       ggplotly(p,tooltip = 'Total') %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE)

    
      
    })
    
    output$DT.summary = renderDataTable({
      
      y1       <- input$period.ind1
      Data     <- UN_LT
      ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      
      Data <- Data[Data$Age==0 & Data$Period %in% c(y1,y2),] 
      Data <- Data[,c('Period', 'Sex', 'ex')]
      rownames(Data) <- NULL
      Data$Sex[2] <- ''
      Data$Sex[4] <- ''
      colnames(Data)[3] <- 'Life expectancy'
      Data$Difference <-c(' ',round(diff(Data$'Life expectancy')[1],2), ' ',round(diff(Data$'Life expectancy')[3],2))
      datatable(Data, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F,caption = 'Life expectancy by sex and period')
    })
    

    output$YLL.plot <- renderPlot({
      options(shiny.sanitize.errors = F)
      
      #y1 <- '2000-2005' 
      
      Infant <- matrix(rep(c(rep(0,9),1),2),nrow = 2,ncol = 10,byrow = T)
      rownames(Infant) <- c('0','1 - 4')
      
      
      y1       <- input$period.ind2
      ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      LT1f     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y1,]
      LT1m     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y1,]
      
      COD_Profiles[]
      COD1f <- COD_Profiles[[y1]]
      COD1f <- COD1f[COD1f$Sex == 'Female' & COD1f$Age_id <= 19,]
      COD1f <- acast(COD1f, Age.group ~ cause.name , value.var = 'Fraction')
      COD1f <- rbind(Infant,COD1f)
      
      COD1m <- COD_Profiles[[y1]]
      COD1m <- COD1m[COD1m$Sex == 'Male' & COD1m$Age_id <= 19,]
      COD1m <- acast(COD1m, Age.group ~ cause.name , value.var = 'Fraction')
      COD1m <- rbind(Infant,COD1m)
      
      YLLf <- LostYears(FLT = LT1f,B = COD1f)[1:18,]
      YLLm <- LostYears(FLT = LT1m,B = COD1m)[1:18,]
      
       
       LT1f2     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y2,]
       LT1m2     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y2,]
       
       COD1f2 <- COD_Profiles[[y2]]
       COD1f2 <- COD1f2[COD1f2$Sex == 'Female' & COD1f2$Age_id <= 19,]
       COD1f2 <- acast(COD1f2, Age.group ~ cause.name , value.var = 'Fraction')
       COD1f2 <- rbind(Infant,COD1f2)
       
       COD1m2 <- COD_Profiles[[y2]]
       COD1m2 <- COD1m2[COD1m2$Sex == 'Male' & COD1m2$Age_id <= 19,]
       COD1m2 <- acast(COD1m2, Age.group ~ cause.name , value.var = 'Fraction')
       COD1m2 <- rbind(Infant,COD1m2)
       
       YLLf2 <- LostYears(FLT = LT1f2,B = COD1f2)[1:18,]
       YLLm2 <- LostYears(FLT = LT1m2,B = COD1m2)[1:18,]
       
       Cause.contributionf <- colSums(YLLf2) - colSums(YLLf)
       Cause.contributionf <- data.table(Cause.contributionf)
       Cause.contributionf$Sex <- 'Females'
       Cause.contributionm <- colSums(YLLm2) - colSums(YLLm)
                                                           
      
       par(mfrow = c(1,2))
       Plot_YLL(FLT = LT1f2,B = COD1f2,COL = base2,Tit = paste0('Females, period ',y2),Total.COD = round(colSums(YLLf2),2))
       Plot_YLL(FLT = LT1m2,B = COD1m2,COL = base2,Tit = paste0('Males, period ',y2),Total.COD = round(colSums(YLLm2),2))
      
      
      
    }, height = 500, width = 1400)
    
    
    output$YLL.plot2 <- renderPlotly({
      options(shiny.sanitize.errors = F)

      #y1 <- '2000-2005' 
      Infant <- matrix(rep(c(rep(0,9),1),2),nrow = 2,ncol = 10,byrow = T)
      rownames(Infant) <- c('0','1 - 4')
      
      
      y1       <- input$period.ind2
      ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      LT1m     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y1,]
      
      COD1m <- COD_Profiles[[y1]]
      COD1m <- COD1m[COD1m$Sex == 'Male' & COD1m$Age_id <= 19,]
      COD1m <- acast(COD1m, Age.group ~ cause.name , value.var = 'Fraction')
      COD1m <- rbind(Infant,COD1m)
      
      YLLm <- LostYears(FLT = LT1m,B = COD1m)[1:18,]
      
      LT1m2     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y2,]
      
      COD1m2 <- COD_Profiles[[y2]]
      COD1m2 <- COD1m2[COD1m2$Sex == 'Male' & COD1m2$Age_id <= 19,]
      COD1m2 <- acast(COD1m2, Age.group ~ cause.name , value.var = 'Fraction')
      COD1m2 <- rbind(Infant,COD1m2)
      
      YLLm2 <- LostYears(FLT = LT1m2,B = COD1m2)[1:18,]
      
      Cause.contributionm <- melt(round(colSums(YLLm2) - colSums(YLLm),3),value.name = 'Contribution')
      r1 <- rownames(Cause.contributionm)
      Cause.contributionm$Sex <- 'Male'
      Cause.contributionm <- cbind(Cause = r1,Cause.contributionm)
      Cause.contributionm <- data.table(Cause.contributionm)
      Cause.contributionm[2,]$Cause <- 'Diarrhea, and common respiratory and infectious diseases'
      Cause.contributionm$causes <- 1:13
      Cause.contributionm$causes <- factor(Cause.contributionm$causes,labels = as.character(Cause.contributionm$Cause))
      
      
      LT1f     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y1,]
      
      
      COD1f <- COD_Profiles[[y1]]
      COD1f <- COD1f[COD1f$Sex == 'Female' & COD1f$Age_id <= 19,]
      COD1f <- acast(COD1f, Age.group ~ cause.name , value.var = 'Fraction')
      COD1f <- rbind(Infant,COD1f)
      
      
      YLLf <- LostYears(FLT = LT1f,B = COD1f)[1:18,]
      
      
      LT1f2     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y2,]
      
      COD1f2 <- COD_Profiles[[y2]]
      COD1f2 <- COD1f2[COD1f2$Sex == 'Female' & COD1f2$Age_id <= 19,]
      COD1f2 <- acast(COD1f2, Age.group ~ cause.name , value.var = 'Fraction')
      COD1f2 <- rbind(Infant,COD1f2)
      
      
      YLLf2 <- LostYears(FLT = LT1f2,B = COD1f2)[1:18,]
      
      
      Cause.contributionf <- melt(round(colSums(YLLf2) - colSums(YLLf),3),value.name = 'Contribution')
      r1 <- rownames(Cause.contributionf)
      Cause.contributionf$Sex <- 'Female'
      Cause.contributionf <- cbind(Cause = r1,Cause.contributionf)
      Cause.contributionf <- data.table(Cause.contributionf)
      Cause.contributionf[2,]$Cause <- 'Diarrhea, and common respiratory and infectious diseases'
      Cause.contributionf$causes <- 1:13
      Cause.contributionf$causes <- factor(Cause.contributionf$causes,labels = as.character(Cause.contributionf$Cause))
      
      
      
      Data <- rbind(Cause.contributionm,Cause.contributionf)
      
      
  
      p <- ggplot(Data, aes(x = Contribution, y = fct_rev(causes),col= causes)) +
        ggtitle(paste('Changes between period', y1, 'to period', y2))+
        geom_point(size=3)+
        scale_colour_manual(name = NULL,values = base2)+
        facet_wrap(~Sex)+
        theme_light()+
        labs(x = 'Total', y = " ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 10, colour = "black"),
              plot.margin = margin(25,10,10,20))+
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
      #p
      
      ggplotly(p,tooltip = 'Contribution') %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE)
      
      
      
      
    })
    
    
    output$YLL.table.males <- renderDataTable({
      #y1 <- '2000-2005' 
      Infant <- matrix(rep(c(rep(0,9),1),2),nrow = 2,ncol = 10,byrow = T)
      rownames(Infant) <- c('0','1 - 4')
      
      
      y1       <- input$period.ind2
      ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      LT1m     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y1,]
      
      COD1m <- COD_Profiles[[y1]]
      COD1m <- COD1m[COD1m$Sex == 'Male' & COD1m$Age_id <= 19,]
      COD1m <- acast(COD1m, Age.group ~ cause.name , value.var = 'Fraction')
      COD1m <- rbind(Infant,COD1m)
      
      YLLm <- LostYears(FLT = LT1m,B = COD1m)[1:18,]
      
      LT1m2     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y2,]
      
      COD1m2 <- COD_Profiles[[y2]]
      COD1m2 <- COD1m2[COD1m2$Sex == 'Male' & COD1m2$Age_id <= 19,]
      COD1m2 <- acast(COD1m2, Age.group ~ cause.name , value.var = 'Fraction')
      COD1m2 <- rbind(Infant,COD1m2)
      
      YLLm2 <- LostYears(FLT = LT1m2,B = COD1m2)[1:18,]
      
      Cause.contributionm <- melt(round(colSums(YLLm2) - colSums(YLLm),3),value.name = 'Contribution')
      r1 <- rownames(Cause.contributionm)
      Cause.contributionm$Sex <- 'Male'
      Cause.contributionm <- cbind(Cause = r1,Cause.contributionm)
      Cause.contributionm <- data.table(Cause.contributionm)
      
      datatable(Cause.contributionm[,1:2], options = list(paging=FALSE,ordering=T,dom = 't'),rownames = F,caption = paste('Males. Changes between period ',y1,'to period',y2,'.'))
      
    })
    
    output$YLL.table.females <- renderDataTable({
      #y1 <- '2000-2005' 
      Infant <- matrix(rep(c(rep(0,9),1),2),nrow = 2,ncol = 10,byrow = T)
      rownames(Infant) <- c('0','1 - 4')
      
      
      y1       <- input$period.ind2
      ifelse(y1 == '2000-2005',y2 <- '2005-2010', y2 <- '2010-2015')
      LT1f     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y1,]
      
      
      COD1f <- COD_Profiles[[y1]]
      COD1f <- COD1f[COD1f$Sex == 'Female' & COD1f$Age_id <= 19,]
      COD1f <- acast(COD1f, Age.group ~ cause.name , value.var = 'Fraction')
      COD1f <- rbind(Infant,COD1f)
      
      
      YLLf <- LostYears(FLT = LT1f,B = COD1f)[1:18,]
      
      
      LT1f2     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y2,]
      
      COD1f2 <- COD_Profiles[[y2]]
      COD1f2 <- COD1f2[COD1f2$Sex == 'Female' & COD1f2$Age_id <= 19,]
      COD1f2 <- acast(COD1f2, Age.group ~ cause.name , value.var = 'Fraction')
      COD1f2 <- rbind(Infant,COD1f2)
      
      
      YLLf2 <- LostYears(FLT = LT1f2,B = COD1f2)[1:18,]
      
      
      Cause.contributionf <- melt(round(colSums(YLLf2) - colSums(YLLf),3),value.name = 'Contribution')
      r1 <- rownames(Cause.contributionf)
      Cause.contributionf$Sex <- 'Female'
      Cause.contributionf <- cbind(Cause = r1,Cause.contributionf)
      Cause.contributionf <- data.table(Cause.contributionf)
      
      datatable(Cause.contributionf[,1:2], options = list(paging=FALSE,ordering=T,dom = 't'),rownames = F,caption = paste('Females. Changes between period ',y1,'to period',y2,'.'))
      
    })
    
    
    #output$CauseDeleted <- renderPlotly({
    #   y1 <- '2010-2015' 
    #   Infant <- matrix(rep(c(rep(0,9),1),2),nrow = 2,ncol = 10,byrow = T)
    #   rownames(Infant) <- c('0','1 - 4')
    #   
    #   LT1f     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y1,]
    #   LT1m     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y1,]
    #   
    #   COD1f <- COD_Profiles[[y1]]
    #   COD1f <- COD1f[COD1f$Sex == 'Female' & COD1f$Age_id <= 19,]
    #   COD1f <- acast(COD1f, Age.group ~ cause.name , value.var = 'Fraction')
    #   COD1f <- rbind(Infant,COD1f)
    #   
    #   COD1m <- COD_Profiles[[y1]]
    #   COD1m <- COD1m[COD1m$Sex == 'Male' & COD1m$Age_id <= 19,]
    #   COD1m <- acast(COD1m, Age.group ~ cause.name , value.var = 'Fraction')
    #   COD1m <- rbind(Infant,COD1m)
    #    
    #   #r1 <- r2 <- r3 <- r4<- r5<- r6<- r7<- r8 <- 0
    #    
    #    
    #    r1 <- (input$cause_ind11)
    #    r2 <- (input$cause_ind12)
    #    r3 <- (input$cause_ind2)
    #    r4 <- (input$cause_ind3)
    #    r5 <- (input$cause_ind4)
    #    r6 <- (input$cause_ind5)
    #    r7 <- (input$cause_ind6)
    #    r8 <- (input$cause_ind7)
    #    r9 <- (input$cause_ind8)
    #   
    #    r <- c(r1,r2,r3,r4,r5,r6,r7,r8,r9,0)
    #   
    #   males    <- cause.deleted(Rxi = COD1m,LT = LT1m,Reduction = r, Sex = 'Males')
    #   females  <- cause.deleted(Rxi = COD1f,LT = LT1f,Reduction = r , Sex = 'Females')
    #   fig.data <- rbind(males$LTi,females$LTi)
    #   
    #   Projected <- fig.data[fig.data$Source == 'Original',]
    #   Projected$lx <- UN_Projections[UN_Projections$Time == '2025-2030' & UN_Projections$Sex != 'Total',]$lx
    #   Projected[Projected$Sex=='Males']$e0 <- UN_Projections[UN_Projections$Time == '2025-2030' & UN_Projections$Sex == 'Male',]$ex[1]
    #   Projected[Projected$Sex=='Females']$e0 <- UN_Projections[UN_Projections$Time == '2025-2030' & UN_Projections$Sex == 'Female',]$ex[1]
    #   Projected$Source <- 'Projected 2025-2030 (UN)'
    #   
    #   fig.data <- rbind(fig.data,Projected)
    #   
    #   p <- ggplot(fig.data, aes(x = Age,y = lx,colour=(Source))) +
    #     ggtitle('Probability of surviving by sex and source')+
    #     geom_line(aes(group = e0), size= 1.2) +
    #     facet_wrap(~Sex)+
    #     theme_light()+
    #     labs(y = "Probability of surviving")+
    #     scale_colour_manual('Source', values = c('red','blue','grey')) + 
    #     theme(text = element_text(size=14),strip.text.x = element_text(size = 14, colour = "black"))+
    #     theme(text = element_text(size=10),
    #           strip.text.x = element_text(size = 10, colour = "black"))+
    #           #plot.margin = margin(10, 10, 10,20))+
    #     annotate("text", x = 20, y = .6, label = rev(list(paste0('Life expectancy 2025-2030 = ',Projected[Projected$Sex == 'Males']$e0[1]),paste0('Life expectancy 2025-2030 = ',Projected[Projected$Sex == 'Females']$e0[1]))))+
    #     annotate("text", x = 20, y = .5, label = rev(list(paste0('Life expectancy 2010-2015 = ',males$e0.original),paste0('Life expectancy 2010-2015 = ',females$e0.original))))+
    #     annotate("text", x = 21.5, y = .4, label = rev(list(paste0('Life expectancy with reduction = ',round(males$e0.new,2)),paste0('Life expectancy with reduction = ',round(females$e0.new,2)))))+
    #     annotate("text", x = 21.5, y = .3, label = rev(list(paste0('Gain = ',round(males$gain[1],2)),paste0('Gain = ',round(females$gain[1],2)))))
    #   
    #   
    #   rr <- ggplotly(p,width = 1500, height = 450,tooltip =list('e0'))
    #   rr[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -0.06
    #   # Set legend label position    
    #   #rr[["x"]][["layout"]][["annotations"]][[3]][["y"]] <- 0.93
    #   rr <- rr %>% layout(margin = list(l = 120, b=120))
    #   ggplotly(rr) %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE) 
    #   
    #   
    # })
    
    
output$CauseDeleted <- renderPlotly({
  y1 <- '2010-2015' 
  Infant <- matrix(rep(c(rep(0,9),1),2),nrow = 2,ncol = 10,byrow = T)
  rownames(Infant) <- c('0','1 - 4')
  
  LT1f     <- UN_LT[UN_LT$Sex == 'Female' & UN_LT$Period == y1,]
  LT1m     <- UN_LT[UN_LT$Sex == 'Male' & UN_LT$Period == y1,]
  
  COD1f <- COD_Profiles[[y1]]
  COD1f <- COD1f[COD1f$Sex == 'Female' & COD1f$Age_id <= 19,]
  COD1f <- acast(COD1f, Age.group ~ cause.name , value.var = 'Fraction')
  COD1f <- rbind(Infant,COD1f)
  
  COD1m <- COD_Profiles[[y1]]
  COD1m <- COD1m[COD1m$Sex == 'Male' & COD1m$Age_id <= 19,]
  COD1m <- acast(COD1m, Age.group ~ cause.name , value.var = 'Fraction')
  COD1m <- rbind(Infant,COD1m)
  
  #r9 <- r1 <- r2 <- r3 <- r4<- r5<- r6<- r7<- r8<- r10<- r11<- r12 <- 0
  
  
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
  
  males    <- cause.deleted(Rxi = COD1m,LT = LT1m,Reduction = r, Sex = 'Males')
  females  <- cause.deleted(Rxi = COD1f,LT = LT1f,Reduction = r , Sex = 'Females')
  e0.new   <- data.table(rbind(males$e0.new,females$e0.new))
  e0.new$Year <- 'New life expectancy'
  e0.new$Sex <- c('Males','Females')
  
  e0.original   <- data.table(rbind(males$e0.original,females$e0.original))
  e0.original$Year <- '2010-2015'
  e0.original$Sex <- c('Males','Females')
  
  e0.projected   <- data.table(UN_Projections[UN_Projections$Time == '2025-2030' & UN_Projections$Sex != 'Total' & UN_Projections$AgeGrpStart == 0,]$ex)
  e0.projected$Year <- 'Projected 2025-2030'
  e0.projected$Sex <- c('Males','Females')
  
  Gain.males <- paste('Gain in life expectancy for males:', round(males$gain[1],2))
  Gain.females <- paste('Gain in life expectancy for females:', round(females$gain[1],2))
  
  
  fig.data <- rbind(e0.new,e0.original,e0.projected)
  setnames(fig.data,'V1','e0')
  fig.data$e0 <- round(fig.data$e0,2)
  
  p <- ggplot(fig.data, aes(x = e0, y = fct_rev(Year),col= Sex,shape = Sex)) +
    geom_point(size=6)+
    scale_colour_manual(name = NULL,values = c('blue','red'))+
    theme_light()+
    labs(x = 'Life expectancy', y = NULL,size=12)+
    theme(text = element_text(size=12),
          strip.text.x = element_text(size = 12, colour = "black"),
          plot.margin = margin(25,10,10,20))
  
  #p
  
  
  
  
  ggplotly(p,tooltip = 'e0') %>% config(modeBarButtonsToRemove= list('toImage', 'sendDataToCloud','hoverClosestCartesian','hoverCompareCartesian','autoScale2d'),displaylogo= FALSE,collaborate = FALSE)
  
  
  
})
    
    
    
})
