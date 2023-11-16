source("scraping.R")
source("analyse_decklist.R")

############### DUEL COMMANDER UPDATE #######
load("mtgtop8DC.Rdata")
load("mtgtop8DCard.Rdata")
res <- mtgtop8.scraping(db.deck=db,db.card = card,imax=49380,
                        form="Duel Commander",imin=NULL)
db <- res[[1]]
save(db, file = "mtgtop8DC.Rdata")
card <- res[[2]]
save(card, file="mtgtop8DCard.Rdata")

############## COMPUTE DECK ################
load("mtgtop8DC.Rdata")
mat <- convert.mtgtop8.to.df(db)
mat <- mtgtop8.estimate.winrate(mat)

mat <- mtgtop8.archetype(mat, type=NULL)
save(mat, file="../mtg_top8_dc_ui/www/mtgtop8DC_computeTable.Rdata")

load("mtgtop8DCard.Rdata")
df <- mtgtop8.sc(card)
save(df,file="../mtg_top8_dc_ui/www/mtgtop8DC_scdata.Rdata")
