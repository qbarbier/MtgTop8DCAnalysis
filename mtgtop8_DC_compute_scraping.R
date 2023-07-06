load("mtgtop8DC.Rdata")

db.list <- list()
for(i in db){
  res <- lapply(c(1:length(i$decks)),function(j){
    return(list(
      "decks"=i$decks[[j]],
      "date"=i$date,
      "position"=names(i$decks)[[j]],
      "players"=i$players,
      "top.players"=length(i$decks)
    ))
  })
  db.list[[length(db.list)+1]]<-do.call(rbind, res)
}

mat <- data.frame(do.call(rbind, db.list))

ronde.suisse <- list(
  "5-8"=3,"9-16"=4,"17-32"=5,"33-64"=6,
  "65-128"=7,"129-226"=8,"227-409"=9
)
pretop8.score <- list("3"=3,
                      "4"=3,"5"=3,"6"=4,"7"=5,"8"=6,"9"=7
)
posttop8.score <- data.frame(
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

mtgtop8.compute.score <- function(mat,ronde.suisse,pretop8.score,posttop8.score){
  
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
    return(round(top.players/2)+ronde)
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
  
  mat <- cbind(mat, score, match.jouer)
  
  return(mat)
  
}

mat <- mtgtop8.compute.score(mat,ronde.suisse,pretop8.score,posttop8.score)

archetype  <- list(
  "Grist"=".*Grist.*",
  "Weenie White"="(.*Isamaru.*|.*Skrelv*.|.*Weenie.*)",
  "Yoshimaru"=".*Yoshi.*",
  "Red Deck Win"="(^Kari{1}.Zev.*)|(^Feldon.*)|(^Ragavan.*)",
  "Raffine"=".*Raffine.*",
  "Ghyrson"=".*Ghyrson.*",
  "Gut"=".*Gut.*",
  "Dennick"=".*Dennick.*",
  "Hogaak"="(.*Hogaak.*|.*Hoogak.*)",
  "Leovold"=".*Leovold.*",
  "Aminatou"=".*(a|A)minatou.*",
  "Golos"="^Golow.*",
  "Wilson"=".*Wilson.*",
  "Saskia"=".*Saskia.*",
  "Klothys"=".*Klothys.*",
  "Kinnan"=".*Kinnan.*",
  "Niv-mizzet"="(.*Niv-mizzet.*|.*Niv.*)",
  "Tevesh"=".*Tevesh.*",
  "Juri"=".*Juri.*",
  "Kroxa"=".*Kroxa.*",
  "Sythis"=".*Sythis.*",
  "Balmor"=".*Balmor.*",
  "Dihada"="(.*Dihada,.*|.*Dihada .*)",
  "Tivit"=".*Tivit.*",
  "Light Paw"=".*Light-(p|P)aws.*",
  "Prossh"=".*Prossh.*",
  "WILL"="(.*Will .*|.*Cecily.*)",
  "Ertai"=".*Ertai.*",
  "Atraxa"=".*Atraxa.*",
  "Bar a Ka"=".*Baral .*",
  "Elminster"=".*Elminster.*",
  "Octavia"=".*Octavia.*"
)

group <- list()
for(i in c(1:dim(mat)[1])){
  d.name <- mat[i,'decks']
  id <- which(lapply(archetype, function(k){grep(k, d.name)})==1)
  if(length(id)!=1){
    g <- "Other"
  } else {
    g <- names(archetype)[id]
  }
  group[[length(group)+1]] <- g
}
group <- unlist(group)
mat <- cbind(mat, group)

win.rate <- apply(mat[,c(6,7)],1, function(x){
  return((x[[1]]/x[[2]])*100)
})
mat <- cbind(mat, win.rate)

mat$date <- as.Date(unlist(mat$date),"%d/%m/%y")
mat$players <- unlist(mat$players)

save(mat, file="mtgtop8DC_computeTable.Rdata")
