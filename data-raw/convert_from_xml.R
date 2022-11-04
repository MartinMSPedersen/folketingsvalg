library(xml2)
library(lubridate)

# globale envs. til at holde strukturen af valgkredse
landsdel_navne <- new.env()
storkreds_navne <- new.env()
opstillingskreds_navne <- new.env()
afstemningsomraade_navne <- new.env()

storkreds_til_landsdel <- new.env()
opstillingskreds_til_storkreds <- new.env()
afstemningsomraade_til_opstillingskreds <- new.env()

# finde strukturen af kredse baseret på indholdet af fintal_0.xml filen
valg_struktur <- function(uri) {
  
  # globale envs. til at holde strukturen af valgkredse
  landsdel_navne <<- new.env()
  storkreds_navne <<- new.env()
  opstillingskreds_navne <<- new.env()
  afstemningsomraade_navne <<- new.env()
  storkreds_til_landsdel <<- new.env()
  opstillingskreds_til_storkreds <<- new.env()
  afstemningsomraade_til_opstillingskreds <<- new.env()
  
  # og så igang
  x <- read_xml(uri)

  landsdel_head <- xml_find_first(x, ".//Landsdele")
  landsdel <- xml_find_all(landsdel_head, ".//Landsdel")
  landsdel_id <- xml_attr(landsdel, "landsdel_id")
  landsdel_navn <- xml_text(landsdel)
  for (idx in seq_along(landsdel_id)) {
    assign(landsdel_id[[idx]],landsdel_navn[[idx]], envir = landsdel_navne)
  }
  
  storkreds_head <- xml_find_first(x, ".//Storkredse")
  storkreds <- xml_find_all(storkreds_head, ".//Storkreds")
  storkreds_id <- xml_attr(storkreds, "storkreds_id")
  storkreds_navn <- xml_text(storkreds)
  landsdel_id <- xml_attr(storkreds, "landsdel_id") 
  for (idx in seq_along(storkreds_id)) {
    assign(storkreds_id[[idx]],storkreds_navn[[idx]], envir = storkreds_navne)
    assign(storkreds_id[[idx]],landsdel_id[[idx]], envir = storkreds_til_landsdel)
  }
  
  opstillingskreds_head <- xml_find_first(x, ".//Opstillingskredse")
  opstillingskreds <- xml_find_all(opstillingskreds_head, ".//Opstillingskreds")
  opstillingskreds_id <- xml_attr(opstillingskreds, "opstillingskreds_id")
  opstillingskreds_navn <- xml_text(opstillingskreds)
  storkreds_id <- xml_attr(opstillingskreds, "storkreds_id") 
  for (idx in seq_along(opstillingskreds_id)) {
    assign(opstillingskreds_id[[idx]],opstillingskreds_navn[[idx]], envir = opstillingskreds_navne)
    assign(opstillingskreds_id[[idx]],storkreds_id[[idx]], envir = opstillingskreds_til_storkreds)
  }
  
  afstemningsomraade_head <- xml_find_first(x, ".//Afstemningsomraader")
  afstemningsomraade <- xml_find_all(afstemningsomraade_head, ".//Afstemningsomraade")
  afstemningsomraade_id <- xml_attr(afstemningsomraade, "afstemningsomraade_id")
  opstillingskreds_id <- xml_attr(afstemningsomraade, "opstillingskreds_id")
  for (idx in seq_along(afstemningsomraade_id)) {
    assign(afstemningsomraade_id[[idx]],opstillingskreds_id[[idx]], envir = afstemningsomraade_til_opstillingskreds)
  }
  
}

convert_one_file <- function(uri) {
    x <- read_xml(uri)

    Sted <- xml_find_first(x, ".//Sted")
    StedType <- xml_attr(Sted, "Type")
    StedID <- xml_attr(Sted, "Id")
    StedTekst <- xml_text(Sted)

    Landsdel <- NA
    Storkreds <- NA
    Opstillingskreds <- NA
    Afstemningsomraade <- NA
    if (StedType == "Landsdel") {
      Landsdel <- StedTekst
    }
    if (StedType == "StorKreds") {
      Storkreds <- StedTekst
      landsdel_id <- get(StedID, envir = storkreds_til_landsdel)
      Landsdel <- get(landsdel_id, envir = landsdel_navne)
    }
    if (StedType == "OpstKreds") {
      Opstillingskreds <- StedTekst
      storkreds_id <- get(StedID, envir = opstillingskreds_til_storkreds)
      Storkreds <- get(storkreds_id, envir = storkreds_navne)
      landsdel_id <- get(storkreds_id, envir = storkreds_til_landsdel)
      Landsdel <- get(landsdel_id, envir = landsdel_navne)
    }
    if (StedType == "Afstemningsomraade") {
      Afstemningsomraade <- StedTekst
      opstillingskreds_id <- get(StedID, envir = afstemningsomraade_til_opstillingskreds)
      Opstillingskreds <- get(opstillingskreds_id, envir = opstillingskreds_navne)
      storkreds_id <- get(opstillingskreds_id, envir = opstillingskreds_til_storkreds)
      Storkreds <- get(storkreds_id, envir = storkreds_navne)
      landsdel_id <- get(storkreds_id, envir = storkreds_til_landsdel)
      Landsdel <- get(landsdel_id, envir = landsdel_navne)
    }
    
    
    ValgDato <- xml_find_first(x, ".//ValgDato")
    datoTekst <- dmy(xml_text(ValgDato))

    # først parti-stemmerne
    Stemmer <- xml_find_first(x, ".//Stemmer")
    Parti <- xml_find_all(Stemmer, ".//Parti")
    PartiBogstav <- xml_attr(Parti, "Bogstav")
    PartiBogstav[is.na(PartiBogstav)] <- ""
    PartiNavn <- xml_attr(Parti, "Navn")

    StemmeAntal <- xml_attr(Parti, "StemmerAntal")
    Stemmeberettigede <- xml_integer(xml_find_first(x, ".//Stemmeberettigede"))
    DeltagelsePct <- xml_double(xml_find_first(x, ".//DeltagelsePct"))
    BlankeStemmer <- xml_integer(xml_find_first(x, ".//BlankeStemmer"))
    AndreUgyldigeStemmer <- xml_integer(xml_find_first(x, ".//AndreUgyldigeStemmer"))

    nyeNavne <- c("Dato", "Landsdel", "Storkreds", "Opstillingskreds", "Afstemningsomraade", "Parti", "Bogstav", "Antal", "Stemmeberettigede", "DeltagelsePct", "Person")
    df1 <- data.frame(datoTekst, Landsdel, Storkreds, Opstillingskreds, Afstemningsomraade, PartiNavn, PartiBogstav, 
    	                StemmeAntal, Stemmeberettigede, DeltagelsePct, NA,
 		                  stringsAsFactors = FALSE)
    df1_blanke <- data.frame(datoTekst, Landsdel, Storkreds, Opstillingskreds, Afstemningsomraade, "Blank", "", 
                             BlankeStemmer, Stemmeberettigede, DeltagelsePct, NA,
                             stringsAsFactors = FALSE)
    df1_ugyldige <- data.frame(datoTekst, Landsdel, Storkreds, Opstillingskreds, Afstemningsomraade, "Andre ugyldige", "", 
                               AndreUgyldigeStemmer, Stemmeberettigede, DeltagelsePct, NA,
                               stringsAsFactors = FALSE)
    colnames(df1) <- nyeNavne
    colnames(df1_blanke) <- nyeNavne
    colnames(df1_ugyldige) <- nyeNavne
    df <- rbind(df1, df1_blanke, df1_ugyldige)
    
    # person-stemmerne
    Personer <- xml_find_first(x, ".//Personer")
    Partier <- xml_find_all(Personer, ".//Parti")
    for (aparti in Partier) {
      PartiBogstav <- xml_attr(aparti, "Bogstav")
      if (is.na(PartiBogstav)) {
        PartiNavn <- "Uden for partier"
        personnavne <- xml_attr(aparti, "navn")
        personstemmetal <- as.integer(xml_attr(aparti, "PersonligeStemmer"))
      } else {
        PartiNavn <- xml_attr(aparti, "navn")
        Person <- xml_find_all(aparti, ".//Person")
        personnavne <- xml_attr(Person, "Navn")
        personstemmetal <- as.integer(xml_attr(Person, "PersonligeStemmer"))
      }
      df2 <- data.frame(datoTekst, Landsdel, Storkreds, Opstillingskreds, Afstemningsomraade, PartiNavn, PartiBogstav, 
                        personstemmetal, Stemmeberettigede, DeltagelsePct, personnavne,
                        stringsAsFactors = FALSE)
      colnames(df2) <- nyeNavne
      df <- rbind(df, df2)  
    }
    df
}

# main
folketingsvalg <- data.frame()

for (aar in c(11,15,19,22)) {
  valg_struktur(paste0("ft",aar,"/fintal.xml"))
  raw_path <- paste0("ft",aar)
  filenames <- dir(path = raw_path, pattern = "fintal_.*", full.names = T)
    
  for (afile in filenames) {
    message(afile)
  	returnCode <-  tryCatch({
  	    folketingsvalg <- rbind(folketingsvalg, convert_one_file(afile))
  	}, error = function(e) {
  	    cat(paste0(afile," failed:\n",e,"\n"))
  	})
  }
}
folketingsvalg$Dato <- factor(folketingsvalg$Dato)
folketingsvalg$Landsdel <- factor(folketingsvalg$Landsdel)
folketingsvalg$Storkreds <- factor(folketingsvalg$Storkreds)
folketingsvalg$Opstillingskreds <- factor(folketingsvalg$Opstillingskreds)
folketingsvalg$Afstemningsomraade <- factor(folketingsvalg$Afstemningsomraade)
folketingsvalg$Parti <- factor(folketingsvalg$Parti)
folketingsvalg$Bogstav <- factor(folketingsvalg$Bogstav)
folketingsvalg$Person <- factor(folketingsvalg$Person)
folketingsvalg$Antal <- as.integer(folketingsvalg$Antal)


save(folketingsvalg, file = "folketingsvalg.RData")

