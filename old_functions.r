#######################
#### MySQL_connect ####
#######################
# handling opening and closing MySQL conenctions
# args  configFile    chr path to mysql_user.cnf with suer credentials
#       dbName        chr of data base name
#       discon        bool (TRUE) if false, all connections are closed
# value MySQLConnection obj if discon == TRUE, else chr with message
# side effects establishes or closes MySQL conenctions
MySQL_connect <- function( configFile = "~/.mysql_user.cnf", dbName = "Finanzen", discon = FALSE ){

  all_cons <- RMySQL::dbListConnections(RMySQL::MySQL())
  
  if( !discon ){
    source(configFile, local = TRUE)

    # establish new connection if needed
    if( length(all_cons) == 0 ){
      con <- RMySQL::dbConnect(RMySQL::MySQL(), user = MySQLuser, password = MySQLpassword, dbname = dbName)  
    } else {
      con <- all_cons[[1]]  
    }
    return(con)
  
  } else {
    for(con in all_cons) RMySQL::dbDisconnect(con)
    return("All MySQLConnections closed.")
  }
}




#################
#### ReadCSV ####
#################
# read, clean and check CSV file
# args  filepath      chr for file path
#       bHeader       bool (TRUE) whether 1st row = column names
#       cComments     chr how comments are marked in file ("" = no comments)
#       cFieldsep     chr field seperator (";")
#       cDec          chr decimal seperator (",")
#       bFactors      bool (FALSE) strings.as.factors
#       necessary     chr arr column names of necessary columns
#       dateFormat    chr with date formatting ("%d.%m.%Y")
#       nDatum        int which column must be a date
#       nNum          int which column must be a number (balance)
# value list(2) $error  list(2)  $status bool whether sth went wrong
#                                $msg chr of according error message
#               $data   data.frame of uploaded data; in some cases displayed even though error (for checking)
ReadCSV <- function( filepath, 
    bHeader = TRUE, cComments = "", cFieldsep = ";", cDec = ",", bFactors = FALSE,
    necessary = c("Auftragskonto", "Valutadatum", "Buchungstext", "Verwendungszweck", "Beguenstigter.Zahlungspflichtiger", "Kontonummer", "BLZ", "Betrag", "Waehrung", "Info"),
    dateFormat = "%d.%m.%Y", nDatum = 2, nNum = 8){
  
  # initial values
  error <- list(status = FALSE, msg = character(0))
  out <- NULL
  
  # try to read table
  res <- try(read.table( file = filepath, header = bHeader, sep = cFieldsep, dec = cDec, 
                  comment.char = cComments, stringsAsFactors = bFactors ))
  if( class(res) == "try-error" ){
    error$status <- TRUE
    error$msg <- c(error$msg, paste("read.table Error:", paste(res, collapse = "")))
    return(list(error = error, data = out))
  }
  df <- res

  # identify and purge unnecessary columns
  unnecessary <- names(df)[!(names(df) %in% necessary)]
  for( i in unnecessary ) df[[i]] <- NULL

  # check if all necessary columns were identified
  if( any(!(necessary %in% names(df))) ){
    error$status <- TRUE
    error$msg <- c(error$msg, paste("Notwendige Spalten wurden nicht erkannt:",
        paste(necessary[!(necessary %in% names(df))], collapse = ", ")))
    out <- df
    return(list(error = error, data = out))
  }

  # try date conversion
  res <- try(as.Date(df[[nDatum]], format = dateFormat))
  class(res)
  if( class(res) == "try-error" ){
    error$status <- TRUE
    error$msg <- c(error$msg, paste("Error bei Datum in Spalte", nDatum, "mit", dateFormat, ":",
                                    paste(res, collapse = "")))
  } else if( any(is.na(res)) || any(is.null(res)) ){
    error$status <- TRUE
    error$msg <- c(error$msg, paste("Datum in Spalte", nDatum, "nicht erkannt mit", dateFormat))
  } else {
    df[[nDatum]] <- res
  }
  
  # try numeric conversion
  res <- as.numeric(df[[nNum]])
  if( any(is.na(res)) || any(is.null(res)) || any(res == "") ){
    error$status <- TRUE
    error$msg <- c(error$msg, paste("Numerisch in Spalte", nNum, "nicht erkannt"))
  } else {
    df[[nNum]] <- res 
  }
  
  # convert rest to character
  nChar <- which(!(1:ncol(df) %in% c(nDatum, nNum)))
  for( i in nChar ){
    df[[i]] <- as.character(df[[i]])
  }
  
  # return output
  out <- df
  return(list(error = error, data = out))
}




############################
#### Predict_neueKonten ####
############################
# durchsucht Tabelle mit Transaktionen nach Konten, die noch keinen Datenbankeintrag haben 
# wird an IBAN zusammen mit BIC erkannt. versucht diese Konten zuzuordnen über aufgerufene Funktion
# args    data              data.frame der tabelle
#         con               MySQLConnection object of data base
#         colNames          list(5) $name   chr colname von Kontoinhaber in Tabelle
#                                   $zweck  chr colname von Verwendungszweck in Tabelle
#                                   $betrag chr colname von Betrag in Tabelle
#                                   ... iban und bic genauso
# value data.frame mit Konten, die neu gefunden wurden und einen Vorschlag wie man sie anlegen könnte. 
#       Zusätzliche Spalten werden angezeigt um die entscheidung zu erleichtern
Predict_neueKonten <- function( data, con,
    colNames = list(name = "Beguenstigter.Zahlungspflichtiger", zweck = "Verwendungszweck",
    buchung = "Buchungstext", betrag = "Betrag", iban = "Kontonummer", bic = "BLZ") ){
  
  # fetch konten
  konten <- fetch(dbSendQuery(con, "SELECT * FROM konten;"))
  
  # set empty ibans
  data[[colNames$iban]][ is.na(data[[colNames$iban]]) ] <- ""
  data[[colNames$iban]][ data[[colNames$iban]] == "" ] <- "unknown_iban"

  # identify new accounts by comparing accounts in csv with konten from database
  total <- apply(konten[, c("iban", "bic")], 1, function(x) paste(x, collapse = ";"))
  uni <- apply(unique(data[, c(colNames$iban, colNames$bic)]), 1, function(x) paste(x, collapse = ";"))
  rows <- as.numeric(names(uni)[!(uni %in% total)])

  # organize new accounts
  neuKonten <- data.frame(inhaber = data[[colNames$name]][rows], zweck = data[[colNames$zweck]][rows], 
      buchung = data[[colNames$buchung]][rows], betrag = data[[colNames$betrag]][rows], 
      prop.inhaber = character(length(rows)), prop.iban = data[[colNames$iban]][rows],  
      prop.bic = data[[colNames$bic]][rows], prop.art = rep("Konto", length(rows)), 
      stringsAsFactors = FALSE)

  # make predictions about their names
  neuKonten <- Classify_neueKonten(neuKonten)

  # return output
  return(neuKonten)
}




##############################
#### Predict_neueTransakt ####
##############################
# Ließt Transaktionen aus data.frame einer SParkassen Abrechnungs CSV file,
# mapped teilnehmende Kontos mit denen der Datenbank, versucht den zweck der Transaktionen vorher
# zu sagen und gibt die Vorhersagen als data.frame aus
# args  data          data.frame von Tabelle mit Transaktionen
#       con           MySQLConnection zur Datenbank
#       bezugsKonto   chr arr (2) mit 1. iban und 2. bic des Bezugskontos
#       colNames      list(6) chr iban, bic, zweck, datum, betrag, waehrung welche angibt wie in der Tabelle data
#                     die Spalten heißen in der die entsprechenden Angaben zu finden sind
# vlaue list(2) $error      list(2)  $status bool ob error gab
#                                    $msg    chr arr für entsprechende error message
#               $vorschalg  data.frame mit Vorschlag wie Transaktionen in DB eingetragen würden
#                           Zusätzliche Spalten werden angezeigt um Entscheidung zu erleichtern
Predict_neueTransakt <- function( data, con, 
    bezugsKonto = c("DE02673525650002103216", "SOLADES1TBB"), colNames = list(iban = "Kontonummer", bic = "BLZ", 
      zweck = "Verwendungszweck", buchung = "Buchungstext", datum = "Valutadatum", betrag = "Betrag", waehrung = "Waehrung") ){
  
  # initial objects
  error <- list(status = FALSE, msg = character(0))
  nrows <- nrow(data)
  out <- data.frame(von_id = numeric(nrows), nach_id = numeric(nrows), von = character(nrows), nach = character(nrows),
      datum = data[[colNames$datum]], prop.zweck = character(nrows), zweck = data[[colNames$zweck]], buchung = data[[colNames$buchung]],
      betrag = numeric(nrows), waehrung = data[[colNames$waehrung]], stringsAsFactors = FALSE)

  # get updated list of accounts
  konten <- fetch(dbSendQuery(con, "SELECT * FROM konten;"))

  # set empty iban
  data[[colNames$iban]][ is.na(data[[colNames$iban]]) ] <- ""
  data[[colNames$iban]][data[[colNames$iban]] == ""] <- "unknown_iban"

  # find IDs of accounts occuring in data
  d <- apply(cbind(konten$iban, konten$bic), 1, function(x) paste(x, collapse = ";"))
  e <- apply(cbind(data[[colNames$iban]], data[[colNames$bic]]), 1, function(x) paste(x, collapse = ";"))
  ids <- konten$id[as.numeric(sapply(e, function(x) which(d == x)))]

  # find owner account ID
  o <- paste(bezugsKonto, collapse = ";")
  oid <- konten$id[which(d == o)]

  # check whether accounts can all be mapped
  if( length(ids) != nrow(data) || any(is.null(ids)) || any(is.na(ids)) ){
    error$status <- TRUE
    error$msg <- c(error$msg, "Für manche Transaktionen wurde kein Konto gefunden.")
    return(list(error = error, vorschlag = NULL))
  }

  # map accounts ids and names and write balances
  # then make predictions about purpose
  for( i in 1:nrows ){
    if( data[[colNames$betrag]][i] > 0 ){
      out$von_id[i] <- ids[i]
      out$von[i] <- konten$inhaber[konten$id == out$von_id[i]]
      out$nach_id[i] <- oid
      out$nach[i] <- konten$inhaber[konten$id == out$nach_id[i]]
    } else {
      out$von_id[i] <- oid
      out$von[i] <- konten$inhaber[konten$id == out$von_id[i]]
      out$nach_id[i] <- ids[i]
      out$nach[i] <- konten$inhaber[konten$id == out$nach_id[i]]
    }
    out$betrag[i] <- abs(data[[colNames$betrag]][i])
  
    out$prop.zweck[i] <- Classify_neueTransakt(list(von = out$von[i], nach = out$nach[i], 
                              betrag = out$betrag[i], zweck = out$zweck[i], buchung = out$buchung[i]))
  }
  
  # values
  rownames(out) <- NULL
  return(list(error = error, vorschlag = out))
}




####################
#### WriteMySQL ####
####################
# write table into database, MySQLConnection must already be established
# if error occurs nothing is written
# args  data      data.frame with data as it should appear in table, cols must be in correct order
#       tableName chr name of target table in database
#       cNames    chr arr of column names as in database table, must be in correct order
#       con       MySQLConnection obj connection to database
# value list(2) $status   bool whether error happened
#               $msg      chr arr of according error messages
# side effect writes into database
WriteMySQL <- function( data, tableName, cNames, con ){
  
  # initial objects
  error <- list(status = FALSE, msg = character(0))
  
  # check columns
  if( ncol(data) != length(cNames) ){
    error$status <- TRUE
    error$msg <- c(error$msg, "Number of columns in data and number of column names does not match.")
  }
  if( any(grepl("\\.", cNames)) ){
    error$status <- TRUE
    error$msg <- c(error$msg, "Column names contain full stops ('.').")
  }
  
  # check for empty fields as NOT NULL in DB
  if( any(apply(data, 2, function(x) any(is.null(x)) || any(is.na(x)) || any(x == ""))) ){
    error$status <- TRUE
    error$msg <- c(error$msg, "There are empty fields in the table.")
  }
  
  # write into table
  string <- paste("INSERT INTO", tableName, "(", paste(cNames, collapse = ","), ") VALUES", 
    paste(apply(data, 1, function(x) paste0("('", paste(x, collapse = "','"), "')")), collapse = ","),
    ";")
  res <- dbSendQuery(con, string)
  
  # return status
  return(error)
}




##############################
#### MySQLquery_innerJoin ####
##############################
# handling MySQL query to fetch tables in a database, INNER JOIN can be used
# purpose is to fetch joined table for cashflow
# args  con         MySQLConnection to database
#       tables      list chrs of table names, list names are used as table aliases
#       showCols    list chrs of columns to show, already need alias ('tableAlias.colName')
#                   list names are col aliases
#       joins       chr arr or NULL: how to join if there are more than 1 table
#       conditions  chr or NULL: write out condition to append
# value data.frame of fetched table
MySQLquery_innerJoin <- function( con, 
    tables = list(t = "transaktionen", von = "konten", nach = "konten"),
    showCols = list(von_id = "von.id", von_inhaber = "von.inhaber", von_art = "von.art", nach_id = "nach.id", 
        nach_inhaber = "nach.inhaber", nach_art = "nach.art", datum = "t.datum", zweck = "t.zweck", 
        betrag = "t.betrag", waehrung = "t.waehrung"),
    joins = c("t.von_id = von.id", "t.nach_id = nach.id"), 
    conditions = NULL
  ){

  if( !(length(tables) == length(joins) + 1) ) stop(paste("for", length(tables), "tables there need to be", length(tables) - 1, "joins"))  

  # show columns
  string <- paste( "SELECT", 
    paste(apply(cbind(unlist(showCols), names(showCols)), 1, function(x) paste(x, collapse = " AS ")), collapse = ","),
    "FROM")

  # 1st table
  string <- paste(string, tables[[1]], "AS", names(tables)[1])
  
  # inner joins
  if( !is.null(joins) ){
    for( i in 2:length(tables) ) string <- paste(string, "INNER JOIN", tables[[i]], "AS", names(tables)[i], "ON", joins[i - 1])
  }
    
  # conditions
  if( !is.null(conditions) ) string <- paste(string, "WHERE", conditions)

  # send query and return result
  string <- paste0(string, ";")
  return( fetch(dbSendQuery(con, string)) )
}




###########################
#### Predict_geldfluss ####
###########################
# Kategorie für Geldfluss vorhersagen und als Tabelle präsentieren
# greift auf Klassifikator Modell Classify_geldfluss() zurück
# args  con         MySQLConnection to database
#       startday    date of starting day (this day is included in cashflow)
#       endday      date of ending day (this day is included in cashflow)
#       bezugsKonto chr arr of iban and bic of reference account
# value data.frame of information about transactions and proposed 'kategorie'
# side effects  send SELECT query to database
Predict_geldfluss <- function( con, startday, endday, 
    bezugsKonto = c("DE02673525650002103216", "SOLADES1TBB") ){
  
  df <- MySQLquery_innerJoin( con,
      conditions = paste0("t.datum >= '", as.character(startday),"' AND t.datum <= '", as.character(endday), "'"))
  df[["prop.kategorie"]] <- ""
  
  # retrieve id of user account
  string <- paste0("SELECT id FROM konten WHERE iban = '", bezugsKonto[1], "' AND bic = '", bezugsKonto[2], "';")
  oid <- fetch(dbSendQuery(con, string))
  
  # devide in expenses and income
  zudf <- df[df$nach_id == 1, ]
  abdf <- df[df$von_id == 1, ]
  
  # retrieve classification model
  res <- Classify_geldfluss()
  keyIN <- res$income
  keyOUT <- res$expenses
  
  # turn into vectors
  keyIn <- sapply(names(keyIN), function(x) keyIN[[x]]$t)
  keyOut <- sapply(names(keyOUT), function(x) keyOUT[[x]]$t)
  wIn <- sapply(names(keyIN), function(x) keyIN[[x]]$w)
  wOut <- sapply(names(keyOUT), function(x) keyOUT[[x]]$w)
  
  # create metric by summing up matching key words and applying weights
  mIn <- matrix(NA, ncol = length(keyIn), nrow = nrow(zudf))
  if( nrow(zudf) > 0 ){
    for( i in 1:nrow(zudf) ){
      mIn[i, ] <- sapply(keyIn, function(x) sum(sapply(x, function(x) grepl(x, tolower(zudf$zweck[i])))))
    }
    mIn <- sweep(mIn, MARGIN = 2, wIn, '*')
  }
  mOut <- matrix(NA, ncol = length(keyOut), nrow = nrow(abdf))
  if( nrow(abdf) > 0 ){
    for( i in 1:nrow(abdf) ){
      mOut[i, ] <- sapply(keyOut, function(x) sum(sapply(x, function(x) grepl(x, tolower(abdf$zweck[i])))))
    }
    mOut <- sweep(mOut, MARGIN = 2, wOut, '*')
  }
    
  # make similarity based prediction
  zudf$prop.kategorie <- apply(mIn, 1, function(x){ 
    if( max(x) > 0 ) return(names(keyIn)[which(x == max(x))[1]]) else return("")
  })
  abdf$prop.kategorie <- apply(mOut, 1, function(x){ 
    if( max(x) > 0 ) return(names(keyOut)[which(x == max(x))[1]]) else return("")
  })
  
  # sign amount
  abdf$betrag <- abdf$betrag * (-1)
  
  # sort by zweck and return
  out <- rbind(zudf[order(zudf$prop.kategorie), ], abdf[order(abdf$prop.kategorie), ])
  rownames(out) <- NULL
  return(out)
}




#############################
#### Aggregate_geldfluss ####
#############################
# aggregiert Beträge der Transaktionen nach kategorien präsentiert dieses data.frame als
# vorläufigen Geldfluss mit zusätzlichen Gerüsten  für die Zuweisung von Bar- und Kreditflüssen
# args  data        data.frame der transaktionen mit zugeordneten kategorien
#       startday    date des starttages (wird mit einbezogen)
#       endday      date des endtages (wird mit einbezogen)
#       barName     chr des Kategorienamens für Barabhebungen
#       kreditName  chr des Kategorienamens für Kreditkartenabrechnungen
# value list(3) $geldfluss  data.frame des vorläufigen geldflusses
#               $bar        data.frame als Gerüst für das EIntragen von Barabbuchungen
#               $kredit     data.frame als Gerüst für das Eintragen von Kreditkarten Belastungen
Aggregate_geldfluss <- function( data, startday, endday,
      barName = "Barabhebungen", kreditName = "Kreditkartenabrechnung" ){
  
  df <- data
  if( any(df$prop.kategorie == "") ) stop("Manche Transaktionen sind nicht zugeordnet.")
  
  # aggregate cashflows
  uni <- unique(df$prop.kategorie)
  cf <- data.frame(kategorie = uni, starttag = startday, endtag = endday, 
    betrag = sapply(uni, function(x) sum(df$betrag[df$prop.kategorie == x])), 
    waehrung = "EUR")
  rownames(cf) <- NULL
  
  bar <- data.frame(kategorie = c("Freizeit", "Lebensmittel", "Anschaffungen",
      "Mobilitaet", "Dienstleistungen", "Geschenkausgaben", "Urlaub"),
      starttag = startday, endtag = endday, betrag = 0, waehrung = "EUR")
  kredit <- data.frame(kategorie = c("Anschaffungen", "Arbeitsmittel",
      "Mobilitaet", "Dienstleistungen", "Geschenkausgaben", "Urlaub"),
      starttag = startday, endtag = endday, betrag = 0, waehrung = "EUR")
  
  # total bar u kredit fluss
  tk <- cf$betrag[cf$kategorie == kreditName]
  ##### !!!!!!!!! für bar muss eigentlich abgleich mit vermögen noch passieren
  tb <- cf$betrag[cf$kategorie == barName]

  return(list(geldfluss = cf, bar = bar, kredit = kredit, totalBar = tb, totalKredit = tk))
}












string <- "SELECT * FROM x.xy"

condition <- character()

if( is.null(input$kdname) || input$kdname == "" ){
  condition <- append(condition, paste0("kdname IN '", paste(input$kdname, collapse = "', '"), "'"))
}

if( is.null(input$abm) || input$abm == "" ){
  condition <- append(condition, paste0("abmessung IN '", paste(input$abm, collapse = "', '"), "'"))
}

condition <- paste(condition, collapse = " AND")



# 1st table
string <- paste(string, tables[[1]], "AS", names(tables)[1])

# inner joins
if( !is.null(joins) ){
  for( i in 2:length(tables) ) string <- paste(string, "INNER JOIN", tables[[i]], "AS", names(tables)[i], "ON", joins[i - 1])
}

# conditions
if( !is.null(conditions) ) string <- paste(string, "WHERE", conditions)







