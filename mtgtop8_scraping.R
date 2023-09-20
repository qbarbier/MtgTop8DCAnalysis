############### MTGTOP8 R scarping functions ################
#'@title MTGTOP8 Scaping
#'@description 
#'
#'@import rvest
#'
#'@param db.deck Un objet R contenant la sortie de la fonction de scraping. Si cet argument est pas NULL alors imin est pas pris en compte
#'@param db.card
#'@param imax 
#'@param form Chaine de caractere qui va indiquer quelle format seras enregistrer lors du parsing du site mtgtop8 parmis 'Duel Commander','Modern','Pauper','Leagcy' ou  'Standard
#'@param imin Un chiffre indiquand le numero de event maximum
#'
#'@examples
#'
#'@author Quentin Barbier
#'
mtgtop8.scraping <- function(db.deck=NULL,db.card=NULL,imax=41200,form="Duel Commander",imin=NULL){
  if(!form%in%c("Duel Commander","Modern","Pauper","Legacy","Standard","Pioneer")){
    return("Error format please select in 'Duel Commander', 
           'Modern', 'Pauper', 'Legacy','Pioneer' or 'Standard'")
  }
  if(is.null(db.deck)){
    db.deck <- list()
    if(is.null(imin)){
      imin <- 1 
    }
  } else {
    if(is.null(imin)){
      imin <- db.deck[[length(db.deck)]]$events+1
    }
  }
  
  if(is.null(db.card)){
    db.card <- list()
  }
  
  for(i in c(imin:imax)){
    print(i)
    
    tryCatch({
      # page <- read_html(i)
      
      html<-rvest::read_html(paste0("https://www.mtgtop8.com/event?e=",i))
      format<-rvest::html_text(rvest::html_nodes(html,".meta_arch"))
      format <- gsub(" *$","",format)
      if(!rlang::is_empty(format) && format == form){
        html.decks <- rvest::html_text(rvest::html_nodes(html, ".S14"))
        decks.id <- grep("(^{1}[0-8]$)|(^{1}[0-8]-{1}[0-8]$)",html.decks)
        decks <- html.decks[decks.id+1]
        decks <- gsub("\r\n","",decks)
        decks <- gsub(" *$","",decks)
        names(decks) <- html.decks[decks.id]
        print(decks)
        
        html_deck_link <- rvest::html_attr(rvest::html_nodes(html, "a")[grep("d=",rvest::html_nodes(html,"a"))],"href")  
        html_deck_link <- unique(html_deck_link)
        html_deck_link <- html_deck_link[-grep("(switch|mtgo|dec)",html_deck_link)]
        decks.list.id <- list()
        for(l in c(1:length(html_deck_link))){
          dl <- html_deck_link[[l]]
          print(decks[[l]])
          card_html <- rvest::read_html(paste0("https://www.mtgtop8.com/event",dl))
          decks.id <- gsub("&f","",strsplit(dl,"=")[[1]][3])
          cardlist <- rvest::html_text(rvest::html_nodes(card_html,".L14"))
          db.card[[length(db.card)+1]] <- list("cardlist"=cardlist,"decks.name"=decks[[l]],
                                               "events"=i,"decks.id"=decks.id)
          decks.list.id[[length(decks.list.id)+1]] <- decks.id
        }
        
        if(!rlang::is_empty(decks)){
          html.date <- rvest::html_text(rvest::html_nodes(html,".S14"))
          date <- html.date[grep("players - ",html.date)[length(grep("players - ",html.date))]]
          players <- stringr::str_extract(date,"\n\t.* players -")
          players <- gsub("\n\t","",players)
          players <- gsub(" players -","",players)
          date <- gsub(".* - ","",date)
          date <- gsub("\n.*","",date)
          print(date)
          print(players)
          if(!rlang::is_empty(date)){
            db.deck[[length(db.deck)+1]] <- list("date"=date,"format"=gsub(" *$","",format),
                                                 "decks"=decks,"players"=as.numeric(players),
                                                 "events"=i,"decks.id"=decks.list.id)
          }
        }
      }
      
    }, error = function(e) {
      cat("Erreur : La page", i, "n'existe pas ou n'est pas accessible.\n")
    })
    
  }
  return(list(db.deck,db.card))
}
