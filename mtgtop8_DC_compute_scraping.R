########## CONVERT MTGTOP8 DATA #######################################
#'@title Convert MTGtop8 Rdata output to Dataframe
#'@description Convert output of scraping function to daframe for computing
#'@author Quentin Barbier
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
        "top.players"=length(i$decks)
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
  
  win.rate <- apply(mat[,c(6,7)],1, function(x){
    return((x[[1]]/x[[2]])*100)
  })
  mat <- cbind(mat, win.rate)
  
  mat$date <- as.Date(unlist(mat$date),"%d/%m/%y")
  mat$players <- unlist(mat$players)
  return(mat)
}


########## RUN SCRIPT #####

load("mtgtop8DC.Rdata")
mat <- convert.mtgtop8.to.df(db)
mat <- mtgtop8.estimate.winrate(mat)

archetype.dc  <- list(
  "Aminatou"="(.*(a|A)minatou.*|.*(R|r)eaminatou)",
  "Aragorn Control"=".*Aragorn, King.*",
  "Aragorn Aggro"=".*Aragorn (4c.*|, The Unit.*)",
  "Atraxa"=".*Atraxa.*",
  "Azusa"=".*Azusa.*",
  
  "Balmor"=".*Balmor.*",
  "Bar a Ka"=".*Baral .*",
  "Beamtown"="(.*Beamtown.*|.*Brute.*|.*Poutre.*)",
  
  "Dennick"=".*Dennick.*",
  "Dihada"="(.*Dihada,.*|.*Dihada .*|Dihada)",
  
  "Ertai"=".*Ertai.*",
  "Elminster"=".*Elminster.*",
  
  "Grist"=".*Grist.*",
  "Ghyrson"=".*Ghyrson.*",
  "Gut"=".*Gut.*",
  "Golos"=".*Golos.*",
  "Gitrog"=".*Gitrog.*",
  "Grenzo"=".*Grenzo.*",
  
  "Hogaak"="(.*Hogaak.*|.*Hoogak.*)",
  
  "Juri"=".*Juri.*",
  
  "Kess"=".*Kess.*",
  "Kroxa"=".*Kroxa.*",
  "Klothys"=".*Klothys.*",
  "Kinnan"=".*Kinnan.*",
  
  "Light Paw"=".*Light-(p|P)aws.*",
  "Ludevic"=".*Ludevic.*",
  "Leovold"=".*Leovold.*",
  "Livio"="Livio.*",
  
  "Magda"=".*Magda.*",
  "Marchesa"=".*Marchesa.*",
  "Marath"=".*Marath.*",
  
  "Niv-mizzet"="(.*Niv-mizzet.*|.*Niv.*)",
  
  "Octavia"=".*Octavia.*",
  
  "Prossh"=".*Prossh.*",
  
  "Red Deck Win"="(^Kari{1}.Zev.*|^Feldon.*|*^Ragavan.*|Rdw)",
  "Raffine"=".*Raffine.*",
  "Rokiric"=".*Rokiric.*",
  "Rustein"=".*Rutstein.*",
  
  "Saskia"=".*Saskia.*",
  "Sythis"=".*Sythis.*",
  "Sai"=".*Sai.*",
  "Shorikai"=".*Shorikai.*",
  "Stickfingers"="Stickfingers",
  
  "Tevesh"=".*Tevesh.*",
  "Tivit"=".*Tivit.*",
  "Titania"=".*Titania.*",
  
  "UR-Dragon"=".*U-dragon.*",
  "Weenie White"="(.*Isamaru.*|.*Skrelv*.|.*Weenie.*|.*Thalia.*)",
  "WILL"="(.*Will .*|.*Cecily.*|.*WLL.*|.*Wll.*)",
  "Wilson"=".*Wilson.*",
  "Yoshimaru"=".*Yoshi.*"
)

type.list = list(
  "Combo"=c("Prossh","Sythis","Kinnan","WILL","Beamtown"),
  "Control"=c("Octavia","Elminster","Atraxa","Ertai","Ludevic"),
  "Aggro"=c("Weenie White","Red Deck Win","Yoshimaru","Sai","Balmor","Magda",
            "Dihada","Aminatou","Sai","Rokiric","Ligth Paw"),
  "MidRange"=c("Grist","Raffine","Golos","Kroxa","Tivit")
)

mat <- mtgtop8.archetype(mat, archetype, type=NULL)
save(mat, file="mtgtop8DC_computeTable.Rdata")
