source("analyse_decklist.R")

load("mtgtop8DC.Rdata")
mat <- convert.mtgtop8.to.df(db)
mat <- mtgtop8.estimate.winrate(mat)

archetype.dc  <- list(
  "Acererak"=".*Acererak.*",
  "Aminatou"="(.*(a|A)minatou.*|.*(R|r)eaminatou)",
  "Aragorn Control"=".*Aragorn, King.*",
  "Aragorn Aggro"=".*Aragorn( 4c.*|, The Unit.*)",
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
  
  "Hogaak"="(.*Hogaak.*|.*Hoogak.*|.*Hogakk.*)",
  
  "Juri"=".*Juri.*",
  "Judith"=".*Judtith.*",
  
  "Kess"=".*Kess.*",
  "Kroxa"=".*Kroxa.*",
  "Klothys"=".*Klothys.*",
  "Kinnan"=".*Kinnan.*",
  
  "Light Paw"=".*Light-(p|P)aws.*",
  "Ludevic"="^Ludevic.*",
  "Leovold"=".*Leovold.*",
  "Livio"="Livio.*",
  
  "Magda"=".*Magda.*",
  "Marchesa"=".*Marchesa.*",
  "Marath"=".*Marath.*",
  
  "Niv-mizzet"="(.*Niv-mizzet.*|.*Niv.*)",
  
  "Octavia"=".*Octavia.*",
  
  "Prossh"=".*Prossh.*",
  
  "Red Deck Win"="(^Kari{1}.Zev.*|^Feldon.*|*^Ragavan.*|Rdw|^Red (D|d)eck.*)",
  "Raffine"=".*Raffine.*",
  "Rokiric"=".*Rokiric.*",
  "Rustein"=".*Rutstein.*",
  
  "Saskia"=".*Saskia.*",
  "Sythis"=".*Sythis.*",
  "Sai"=".*Sai.*",
  "Shorikai"=".*Shorikai.*",
  "Stickfingers"="Stickfingers",
  "Slimefoot and skwee"="Slimefoot.*",
  
  "Tevesh"=".*Tevesh.*",
  "Tivit"=".*Tivit.*",
  "Titania"=".*Titania.*",
  "Thrasta"=".*Thrasta.*",
  "Trelasarra"="Trelasarra.*",
  
  "UR-Dragon"=".*U-dragon.*",
  "Weenie White"="(.*Isamaru.*|.*Skrelv*.|.*Weenie.*|.*Thalia.*)",
  "WILL"="(.*Will .*|.*Cecily.*|.*WLL.*|.*Wll.*)",
  "Wilson"=".*Wilson.*",
  "Yoshimaru"="(.*Yoshi.*|^Yoshimaru,.*)"
)

type.list.dc = list(
  "Combo"=c("Prossh","Sythis","Kinnan","WILL","Beamtown"),
  "Control"=c("Octavia","Elminster","Atraxa","Ertai","Ludevic"),
  "Aggro"=c("Weenie White","Red Deck Win","Yoshimaru","Sai","Balmor","Magda",
            "Dihada","Aminatou","Sai","Rokiric","Ligth Paw"),
  "MidRange"=c("Grist","Raffine","Golos","Kroxa","Tivit")
)

mat <- mtgtop8.archetype(mat, archetype.dc, type=NULL)
save(mat, file="mtgtop8DC_computeTable.Rdata")

load("mtgtop8DCard.Rdata")
df <- mtgtop8.sc(mat, card)
save(df,file="mtgtop8DC_scdata.Rdata")
