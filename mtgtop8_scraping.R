mtgtop8.scraping <- function(db=NULL,imax=41200,form="Duel Commander",imin=NULL){
  if(!form%in%c("Duel Commander","Modern","Pauper","Legacy","Standard")){
    return("Error format please select in 'Duel Commander', 
           'Modern', 'Pauper', 'Legacy' or 'Standard'")
  }
  if(is.null(db)){
    db <- list()
    if(is.null(imin)){
      imin <- 1 
    }
  } else {
    if(is.null(imin)){
      imin <- db[[length(db)]]$events+1
    }
  }
  for(i in c(imin:imax)){
    print(i)
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
            db[[length(db)+1]] <- list("date"=date,"format"=gsub(" *$","",format),
                                       "decks"=decks,"players"=as.numeric(players),
                                       "events"=i)
        }
      }
    }
  }
  return(db)
}

load("mtgtop8DC.Rdata")
db <- mtgtop8.scraping(db=db,imax=45586,"Duel Commander",imin=NULL)
save(db, file = "mtgtop8DC.Rdata")
