########## CONVERT MTGTOP8 DATA #######################################
#'@title Convert MTGtop8 Rdata output to Dataframe
#'@description Convert output of scraping function to daframe for computing
#'@author Quentin Barbier
#'@exemple
#'
convert.mtgtop8.to.df <- function(db){
  db.list <- list()
  for(i in db){
    res <- lapply(c(1:length(i$decks)),function(j){
      return(list(
        "decks"=i$decks[[j]],
        "date"=i$date,
        "position"=names(i$decks)[[j]],
        "players"=i$players,
        "top.players"=length(i$decks),
        "events"=i$events
      ))
    })
    db.list[[length(db.list)+1]]<-do.call(rbind, res)
  }
  
  mat <- data.frame(do.call(rbind, db.list))
  return(mat)
}

########## MTGTOP8 ESTIMATE WINRATE #######################################
#'@title Estime win rate of mtgtop8 data
#'@author Quentin Barbier
#'@description  
#'
mtgtop8.estimate.winrate <- function(mat){
  
  ronde.suisse <- list( #Nombre de ronde par rapport au nombre de joueur
    "5-8"=3,
    "9-16"=4,
    "17-32"=5,
    "33-64"=6,
    "65-128"=7,
    "129-226"=8,
    "227-409"=9
  )
  
  pretop8.score <- list( #Nombre de win estimé d'après le nombre de ronde
    "3"=2.5,"4"=3,"5"=3.5,"6"=4,"7"=5,"8"=6,"9"=7
  )
  
  posttop8.score <- data.frame( #Nbr de pt bonus par rapport classement top/top player
    "1"=c(0,1,2,2,2,2,3,3),
    "2"=c(0,0,1,1,2,2,2,2),
    "3-4"=c(0,0,0,1,1,1,1,1),
    "3"=c(0,0,0,1,1,1,1,1),
    "4"=c(0,0,0,0,1,1,1,1),
    "5-8"=c(0,0,0,0,0,1,1,1),
    "5"=c(0,0,0,0,0,1,1,1),
    "6"=c(0,0,0,0,0,0,1,1),
    "7"=c(0,0,0,0,0,0,0,1),
    "8"=c(0,0,0,0,0,0,0,0)
  )
  posttop8.score <- t(posttop8.score)
  row.names(posttop8.score) <- c("1","2","3-4","3","4","5-8","5","6","7","8")
  colnames(posttop8.score) <- c("1","2","3","4","5","6","7","8")
  
  win.rate <- function(players,top.players,ronde.suisse,position){
    res <- lapply(c(1:length(ronde.suisse)),function(i){
      a <- as.numeric(stringr::str_split(names(ronde.suisse)[[i]],"-")[[1]][1])
      b <- as.numeric(stringr::str_split(names(ronde.suisse)[[i]],"-")[[1]][2])
      if(players>=a && players <=b){
        return(ronde.suisse[[i]])
      }
    })
    ronde <- unlist(res)
    score1 <- pretop8.score[[as.character(ronde)]]
    score2 <- posttop8.score[as.character(position),as.character(top.players)]
    return(score1+score2)
  }
  
  match.jouer <- function(players,top.players,ronde.suisse,position){
    res <- lapply(c(1:length(ronde.suisse)),function(i){
      a <- as.numeric(stringr::str_split(names(ronde.suisse)[[i]],"-")[[1]][1])
      b <- as.numeric(stringr::str_split(names(ronde.suisse)[[i]],"-")[[1]][2])
      if(players>=a && players <=b){
        return(ronde.suisse[[i]])
      }
    })
    ronde <- unlist(res)
    return(round(top.players/2)+ronde) #a ameliorer
  }
  
  score <- unlist(lapply(c(1:dim(mat)[1]),function(i){
    players <- as.numeric(mat[i,"players"])
    position <- mat[i,"position"]
    top.players <- as.numeric(mat[i,"top.players"])
    sc <- win.rate(players,top.players,ronde.suisse,position)
    return(sc)
  }))
  
  match.jouer <- unlist(lapply(c(1:dim(mat)[1]),function(i){
    players <- as.numeric(mat[i,"players"])
    position <- mat[i,"position"]
    top.players <- as.numeric(mat[i,"top.players"])
    sc <- match.jouer(players,top.players,ronde.suisse,position)
    return(sc)
  }))
  
  mat <- cbind(mat, score, match.jouer, win.rate=(score/match.jouer)*100)
  
  return(mat)
  
}


########## MTGTOP8 ARCHEYTPE #############################
#'@title
#'
#'
#'
mtgtop8.archetype <- function(mat, archetype, type=NULL){
  group <- list()
  for(i in c(1:dim(mat)[1])){
    d.name <- mat[i,'decks']
    id <- which(lapply(archetype, function(k){grep(k, d.name)})==1)
    if(length(id)!=1){
      g <- NA
    } else {
      g <- names(archetype)[id]
    }
    group[[length(group)+1]] <- g
  }
  group <- unlist(group)
  mat <- cbind(mat, group)
  
  # win.rate <- apply(mat[,c(6,7)],1, function(x){
  #   return((x[[1]]/x[[2]])*100)
  # })
  # mat <- cbind(mat, win.rate)
  
  mat$date <- as.Date(unlist(mat$date),"%d/%m/%y")
  mat$players <- unlist(mat$players)
  return(mat)
}



########## MTGTOP8 SINGLECARD ANALYSIS ####################
#'@title
#'
#'
#'
#'
mtgtop8.sc <- function(mat, card){
  
  archetype <- lapply(c(1:length(card)),function(i){
    return(unique(mat[which(mat$decks==card[[i]]$decks.name),"group"]))
  })
  
  card.archetype <- card[which(!is.na(unlist(archetype)))]
  card.archetype <- lapply(card.archetype, function(i){
    return(i$cardlist)
  })
  
  names.card.archetype <- as.vector(unlist(archetype[which(!is.na(unlist(archetype)))]))
  
  unique.card <- unique(unlist(card.archetype))
  unique.archetype <- unique(names.card.archetype)
  
  df <- data.frame(matrix(0, nrow=length(unique.card), ncol=length(unique.archetype)))
  colnames(df) <- unique.archetype
  row.names(df) <- unique.card
  
  for(x in c(1:dim(df)[1])){
    print(x)
    for(y in c(1:dim(df)[2])){
      #print(y)
      a <- colnames(df)[y]
      c <- row.names(df)[x]
      value <- 0
      for(l in which(names.card.archetype==a)){
        for(d in card.archetype[[l]]){
          if(length(grep(c,d))>0){
            value <- value+1
          }
        }
      }
      #print(value)
      df[x,y] <- value
    }
  }
  return(df)
}
