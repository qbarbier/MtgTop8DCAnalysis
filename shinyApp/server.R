library("shiny")
library("ggrepel")
library("fpc")
library("Rphenograph")

load(file = "www/mtgtop8DC_computeTable.Rdata")
source("www/tierList.R")

server <- function(input, output, session){
  
  values <- reactiveValues(
    tierlist = NULL
  )
  
  updateDateInput(session, "date1", value=max(mat$date)-61)
  updateDateInput(session, "date2", value=max(mat$date))
  
  output$dataBaseSetting <- renderText({
    str <- paste0("Tools version : 0.03. \n",
    "Data downloaded from ",min(mat$date)," to ",max(mat$date),"\n",
    "We have ",dim(mat)[1]," entry and ",length(unique(mat$group))-1," Archetype"
    )
    return(str)
  })

  observe({
    require(input$date1)
    require(input$date2)
    require(input$players)
    #require(input$minPercent)
    
    tierlist <- tier.list.dc.mtgtop8(mat,date.d=input$date1, 
                  date.f = input$date2, limit.players=input$players)
    values$tierlist <- tierlist
    
    th.percent <- mean(tierlist$percent)
    updateSliderInput(session,"minPercent",value=round(th.percent,1))
  })
  
  
  observe({
    if(is.null(values$tierlist)){return(NULL)}
    require(input$minPercent)
    tierlist <- values$tierlist
    
    if(dim(tierlist)[1]==0){
      output$plotTierList <- renderPlot({plot(1, pch=" ");text(1,1,"No data found")})
      output$plotTierList2  <- renderPlot({plot(1, pch=" ");text(1,1,"No data found")})
      output$plotTierList3  <- renderPlot({plot(1, pch=" ");text(1,1,"No data found")})
      return(NULL)
    }
    # browser()
    th.percent <- input$minPercent
    #th.percent <- mean(tierlist$percent)#-(sd(tierlist$percent)/2)
    
    tierlist <- tierlist[which(tierlist$percent>th.percent),]
    tierlist$sd.win.rate <- 100*(1/tierlist$sd.win.rate)
    
    #browser()
    
    x <- scale(tierlist$win.rate, center = T, scale = T)
    y <- scale(tierlist$percent, center=T, scale=T)
    score <- unlist(x+y)[,1]
    tiers <- cbind("decks"=row.names(tierlist),"score"=as.numeric(score))
    #tiers <- tiers[order(as.numeric(tiers[,2])),]
    m <- mean(score)
    sd <- sd(score)
    tier.th <- list(
      "t0"=m+(3*sd),"t0.5"=m+(2*sd),"t1"=m+sd,"t1.5"=m,
      "t2"=m-sd,"t2.5"=m-(2*sd),"t3"=m-(3*sd)
    )
    
    x <- "percent"
    y <- "win.rate"
    tier <- unlist(lapply(c(1:dim(tiers)[1]),function(i){
      return(names(tier.th)[which(tier.th<as.numeric(tiers[i,2]))[1]])
    }))
    
    #browser()
    tierlist <- cbind(tierlist, tier)
    tierlist$tier <- as.factor(tierlist$tier)
    
    output$plotTierList <- renderPlot({
      p <- ggplot(tierlist,aes_string(x=x,y=y,label="group",fill="tier")) +
        geom_point(show.legend = FALSE)+ 
        geom_hline(yintercept = mean(tierlist$win.rate),linetype = "dashed")+
        geom_label_repel()+
        scale_size_continuous(guide = "none")+
        xlab("Percent of presence %")+
        ylab("Estimate Win Rate %")+
        labs(title ="Presence vs Win rate Duel commander",
             subtitle=paste0("From ",input$date1," to ",input$date2,", with ",sum(tierlist$nbr.deck),
                             " decks. Precense thresold ",min(round(tierlist$percent,2))),
             caption=paste0("This archerytpe cover ",round(sum(tierlist$percent),2),"% of all decks"))
      return(p)
    })
    
    #browser()
    
    output$plotTierList2 <- renderPlot({
      order.group <- tierlist$group[order(tierlist$percent,decreasing = F)]
      p <- ggplot(tierlist, aes(x=factor(group,level=order.group),y=nbr.deck,fill=tier))+
                    geom_col() + coord_flip()+
      xlab(NULL)+ylab(NULL)
      
      return(p)
    })
    
    #browser()
    tierlist <- cbind(tierlist, score)
    tierlist <- tierlist[order(as.numeric(tierlist$score)),]
    output$plotTierList3 <- renderPlot({
      p <- ggplot(tierlist,aes(x=factor(group,levels=tierlist$group),y=score,color=tier))+
        geom_point()+
        xlab("Archetype")+
        ylab("Sum of normalize value")+
        geom_hline(yintercept = unlist(tier.th),linetype = "dashed")
      return(p)
    })
    
  })
}
