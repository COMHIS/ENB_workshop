
#' @title Polish Publisher Generic
#' @description Generic cleanup of the publisher field.
#' @param x Character vector of publisher names
#' @return Data frame with orig, mod
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("fennica")
#' @keywords utilities
polish_publisher <- function (x) {
  
  # Lowercase		 
  # x <- tolower(x)
  
  # .test -> test
  x <- gsub("^\\.*", "", x)
  
  # test. -> test
  x <- gsub("\\.*$", "", x)
  
  # s.n -> ""
  #x <- gsub("^\\[*s\\.*n\\.*\\]*$", " ", x)
  f <- read.csv("ENB_workshop/data/sl.csv") 
  terms <- as.character(f[,1])
  x <- remove_sl(x, terms)
  
  x <- condense_spaces(x)
  
  x
}

#' @title Remove s.l etc. clauses
#' @description Remove s.l etc. clauses 
#' @param x A character vector
#' @param terms Terms to be removed (a character vector). Optional
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("fennica")
#' @examples # x2 <- remove_sl(c("s.l.", "London"))
#' @keywords utilities
remove_sl <- function (x, terms = NULL) {
  
  # Will requires serious rethinking with these stupid hacks.
  
  # Get printing terms from a table
  if (is.null(terms)) {
    f <- read.csv("ENB_workshop/data/sl.csv") 
    terms <- as.character(f[,1])
  }
  
  x <- remove_terms(x, terms, include.lowercase = FALSE)
  
  x <- gsub("\\[s.n.\\?\\]", NA, x)
  x <- gsub("\\[s.n.\\]", NA, x)
  x <- gsub("\\[s. n.\\]", NA, x)    
  x <- gsub("s. a", NA, x)
  x <- gsub("s i", NA, x)    
  
  x
  
}

#' @title Remove Terms
#' @description Remove the given terms from the strings.
#' @param x A vector
#' @param terms Terms to be removed
#' @param where Locations to be removed ("all" / "begin" / "middle" / "end")
#' @param include.lowercase Include also lowercase versions of the terms
#' @param polish polish the entries after removing the terms (remove trailing spaces and periods)
#' @param recursive Apply the changes recursively along the list ?
#' @return Vector with terms removed
#' @details After removing the numerics, beginning, double and ending 
#'          spaces are also removed from the strings.
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("fennica")
#' @examples x2 <- remove_terms("test this", c("this", "works"), where = "all")
#' @keywords utilities
remove_terms <- function (x, terms, where = "all", include.lowercase = FALSE, polish = TRUE, recursive = FALSE) {
  
  # If removal is recursive, then go through the list as is in the given order
  # otherwise, optionally include lowercase, remove unique terms and sort by length 
  if (!recursive) {
    
    # Add lowercase versions
    if (include.lowercase) {
      terms <- c(terms, tolower(terms))
    }
    
    # List all unique terms
    terms <- sort(unique(terms))
    
    # Go from longest to shortest term to avoid nested effects
    terms <- terms[rev(order(sapply(terms, nchar, USE.NAMES = FALSE)))]
    
  }
  
  tmp <- matrix(sapply(terms, function (term) grepl(term, x), USE.NAMES = FALSE),
                ncol = length(terms))
  
  for (i in 1:length(terms)) {
    if (any(tmp[, i])) {
      x[tmp[, i]] <- remove_terms_help(x[tmp[, i]], terms[[i]], where)
    }
  }
  
  if (polish) {
    x <- condense_spaces(x)
    x <- remove_trailing_periods(x)
  }
  
  x 
  
}



remove_terms_help <- function (x, term, where) {
  
  # remove elements that are identical with the term		   
  x[x == term] = ""
  
  # Speedup: return if all handled already
  if (all(x == "")) {return(x)}
  
  # Here no spaces around the term needed, elsewhere yes
  if ("all" %in% where) {
    
    # begin
    rms <- paste("^", term, "[ |\\.|\\,]", sep = "")
    x <- gsub(rms, " ", x)
    
    # middle
    x <- gsub(paste(" ", term, "[ |\\.|\\,]", sep = ""), " ", x)
    
    # end
    rms <- paste(" ", term, "$", sep = "")
    x <- gsub(rms, " ", x)
  }
  
  if ("full" %in% where) {
    
    x <- gsub(term, " ", x)
    
  }
  
  if ("begin" %in% where) {
    
    rms <- paste("^", term, "[ |\\.|\\,]", sep = "")
    x <- gsub(rms, " ", x)
    
  }
  
  if ("middle" %in% where) {
    
    x <- gsub(paste(" ", term, "[ |\\.|\\,]", sep = ""), " ", x)
    
  }
  
  if ("end" %in% where) {
    
    rms <- paste(" ", term, "$", sep = "")
    x <- gsub(rms, " ", x)
    
  }
  
  x
  
}


#' @title Remove Trailing Periods
#' @description Remove trailing periods.
#' @param x A vector
#' @return A polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("fennica")
#' @examples \dontrun{x2 <- remove_trailing_periods(x)}
#' @keywords utilities
remove_trailing_periods <- function (x){ 
  
  if (all(is.na(x))) {return(x)}
  x <- gsub("\\.+$", "", x)
  x <- gsub("^\\.+", "", x)
  
  x
}


#' @title Polish author
#' @description Polish author.
#' @param s Vector of author names
#' @param stopwords Stopwords
#' @param verbose verbose 
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("fennica")
#' @examples # s2 <- polish_author("Smith, William")
#' @keywords utilities
polish_author <- function (s, stopwords = NULL, verbose = FALSE) {
  
  if (is.null(stopwords)) {
    message("No stopwords provided for authors. Using ready-made stopword lists")
    
    # TODO Use instead the notnames function here ?
    
    f <- read.csv("ENB_workshop/data/stopwords.csv")
    stopwords.general <- as.character(f[,1])
    stopwords.general <- c(stopwords.general, stopwords(kind = "en"))
    
    f <- read.csv("ENB_workshop/data/stopwords_for_names.csv")
    stopwords.names <- as.character(f[,1])
    
    f <- read.csv("ENB_workshop/data/organizations.csv")
    stopwords.organizations <- as.character(f[,1])
    
    f <- read.csv("ENB_workshop/data/stopwords_titles.csv")
    stopwords.titles <- as.character(f[,1])
    stopwords <- unique(c(stopwords.general, stopwords.organizations, stopwords.names, stopwords.titles))
  }
  
  # Accept some names that may be on the stopword lists
  # TODO add here all known names
  f <- read.csv("ENB_workshop/data/author_accepted.csv")
  author.accepted <- as.character((f[,1]))
  
  pseudo <- get_pseudonymes()
  
  accept.names <- unique(c(pseudo, author.accepted))
  
  # Also add individual terms in these names on the list
  accept.names <- c(accept.names, unique(unlist(strsplit(accept.names, " "))))
  # Remove special chars and make lowercase to harmonize
  accept.names <- unique(condense_spaces(gsub("\\,", " ", gsub("\\.", "", tolower(accept.names)))))
  
  
  # Then remove those in stopwords (ie accept these in names)
  # Exclude some names and pseudonyms from assumed stopwords
  stopwords <- setdiff(stopwords, accept.names)
  
  # -------------------------------------------------------------
  
  s <- tolower(as.character(s))
  
  # Only handle unique entries, in the end map back to original indices
  sorig <- s
  suniq <- unique(s)
  s <- suniq
  
  if (verbose) {
    message(paste("Polishing author field: ", length(suniq), "unique entries"))
  }
  
  # Remove numerics
  s <- gsub("[0-9]", " ", s) 
  
  # Remove brackets and ending commas / periods
  # Cannot be merged into one regexp ?
  s <- gsub("\\[", " ", s)
  s <- gsub("\\]", " ", s)
  s <- gsub("\\(", " ", s)
  s <- gsub("\\)", " ", s)
  s <- gsub("\\?", " ", s)
  s <- gsub("-+", "-", s)      
  s <- str_trim(s)
  s <- gsub("[\\.|\\,]+$", "", s)
  
  # Map back to original indices, then make unique again. Helps to further reduce cases.
  sorig <- s[match(sorig, suniq)]
  s <- suniq <- unique(sorig)
  
  # ----------------------------------------------------------------
  
  if (verbose) {
    message("Separating names")
  }
  # Assume names are of format Last, First
  # TODO O. K. Humble, Verner -> First: Verner O K Last: Humble
  # pseudonymes are taken as such
  # convert to character type
  first <- last <- as.character(rep(NA, length(s)))
  
  pseudo.inds <- which(s %in% pseudo)
  
  inds <- inds1 <- setdiff(grep(",", s), pseudo.inds)
  if (length(inds) > 0) {
    first[inds] <- pick_firstname(s[inds], format = "last, first")
    last[inds]  <-  pick_lastname(s[inds], format = "last, first")
  }
  
  
  
  inds <- inds2 <- setdiff(setdiff(grep(" ", s), inds1), pseudo.inds)
  if (length(inds) > 0) {
    first[inds] <- pick_firstname(s[inds], format = "first last")
    last[inds]  <-  pick_lastname(s[inds], format = "first last")
  }
  # Where the name did not match the assumed formats, use the complete form as
  # the last name
  inds <- inds3 <- setdiff(which(is.na(first) & is.na(last)), pseudo.inds)
  if (length(inds) > 0) {
    last[inds] <- as.character(s[inds])
  }
  # Mark pseudonymes as first names
  inds <- inds4 <- pseudo.inds
  if (length(pseudo.inds) > 0) {
    first[inds] <- as.character(s[inds])
  }  
  
  # ------------------------------------------------------------
  
  if (verbose) { message("Formatting names") }
  # Some additional formatting
  # eg. "Wellesley, Richard Wellesley" -> "Wellesley, Richard"
  inds <- which(!is.na(first) | !is.na(last))
  for (i in inds) {
    
    fi <- first[[i]]
    if (!is.na(fi)) {
      fi <- unlist(strsplit(fi, " "), use.names = FALSE)
    }
    
    la <- last[[i]]    
    if (!is.na(la)) {    
      la <- unlist(strsplit(la, " "), use.names = FALSE)
    }
    
    if (length(fi) == 0) {fi <- NA}
    if (length(la) == 0) {la <- NA}    
    if (!is.na(fi) && !is.na(la)) {
      if (la == fi[[length(fi)]]) {
        fi <- fi[-length(fi)]
      }
    }
    first[[i]] <- paste(fi, collapse = " ")
    last[[i]] <- paste(la, collapse = " ")    
  }
  
  message("Name table")
  nametab <- data.frame(last = unname(last),
                        first = unname(first),
                        stringsAsFactors = FALSE
  )
  rownames(nametab) <- NULL
  
  message("Remove single letter last names")
  nametab$last[nchar(as.character(nametab$last)) == 1] <- NA   
  
  if (verbose) { message("Capitalize names")}
  nametab$last  <- capitalize(nametab$last, "all.words")
  nametab$first <- capitalize(nametab$first, "all.words")
  
  message("Remove periods")
  nametab$first <- condense_spaces(gsub("\\.", " ", nametab$first))
  nametab$last  <- condense_spaces(gsub("\\.", " ", nametab$last))  
  
  if (verbose) { message("Collapse accepted names to the form: Last, First") }
  full.name <- apply(nametab, 1, function (x) { paste(x, collapse = ", ") })
  full.name <- unname(full.name)
  full.name[full.name == "NA, NA"] <- NA
  full.name <- gsub("\\, NA$", "", full.name) # "Tolonen, NA" -> "Tolonen"
  full.name <- gsub("^NA, ", "", full.name) # "NA, Mikael" -> "Mikael"
  
  if (verbose) { message("Map to the original indices") }
  full.name[match(sorig, suniq)]   
  
}

get_pseudonymes <- function (...) {
  pseudo <- as.character(read.csv("ENB_workshop/data/custom_pseudonymes.csv")[,1])
  
  # Remove extra spaces
  pseudo <- condense_spaces(pseudo)
  pseudo <- tolower(pseudo)  
  
  # Also consider removing periods, commas, dashes etc ?
  
  # Organize
  pseudo <- sort(unique(pseudo))
  
  pseudo
  
}


#' @title Pick First Name
#' @description Pick first name from full name, assuming the format is known 
#' @param x a vector of full names
#' @param format name format
#' @param keep.single If the name is without comma ('Shakespeare,
#'  William' versus 'William'), interpret the name as first name.
#'  Note that in this case also 'Shakespeare' will be interpreted as first name.
#' @return a vector of first names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("fennica")
#' @examples pick_firstname("Hobbes, Thomas")
#' @keywords utilities
pick_firstname <- function (x, format = "last, first", keep.single = FALSE) {
  
  xorig <- as.character(x)
  xuniq <- unique(xorig)
  x <- xuniq
  
  if (format == "last, first") {
    first <- sapply(x, function (x) {y <- unlist(strsplit(x, ", "), use.names = FALSE); if (length(y)>1) y[[2]] else if (keep.single) {y[[1]]} else {NA} }, USE.NAMES = FALSE)
  } else if (format == "first last") {
    first <- sapply(x, function (x) {unlist(strsplit(x, " "), use.names = FALSE)[[1]]}, USE.NAMES = FALSE)
  } else {
    stop("Correct the unknown format in pick_firstname function.")
  }
  
  # Remove possible life year info
  first <- gsub(" \\([0-9|N|A]+-[0-9|N|A]+\\)", "", first)
  first <- condense_spaces(first)
  
  first[match(xorig, xuniq)]
  
}

#' @title Pick Last Name
#' @description Pick last name from full name, assuming the format is known 
#' @param x a vector of full names
#' @param format name format
#' @param keep.single If the name is without comma ('Shakespeare,
#'  William' versus 'William'), interpret the name as first name.
#'  Note that in this case also 'Shakespeare' will be interpreted as first name.
#'  In the current implementation keep.single is always TRUE.
#' @return a vector of last names
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("fennica")
#' @examples pick_lastname("Hobbes, Thomas")
#' @keywords utilities
pick_lastname <- function (x, format = "last, first", keep.single = TRUE) {
  
  x <- as.character(x)
  
  if (format == "last, first") {
    last <- sapply(x, function (x) {y <- unlist(strsplit(x, ", "), use.names = FALSE); if (length(y)>=1) y[[1]] else NA}, USE.NAMES = FALSE) 
  } else if (format == "first last") {
    last <- sapply(x, function (x) {y <- unlist(strsplit(x, " "), use.names = FALSE); y[[length(y)]]}, USE.NAMES = FALSE)
  } else {
    stop("Correct the unknown format in pick_lastname function.")
  }
  
  # Remove possible life year info
  last <- gsub(" \\([0-9|N|A]+-[0-9|N|A]+\\)", "", last)
  
  last
  
}

#' Harmonize places (ENB)
#'
#' This function harmonizes places in ENB
#' @param genres the data.table format work_by_row table
#' @keywords places
#' @export
#' @examples
#' @return a table with the original columns and a new column for the harmonised place names
#' harmonize_places()
#' see https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/ on installation
#' 
#' 

harmonize_places <- function(works,fuzzy=T){
  
  
  
  works <- data.table(works)
  
  
  
  works[,koht_orig:=koht]
  works[,koht:=str_replace(koht,";","")]
  works[,koht:=str_replace(koht,",","")]
  works[,koht:=str_replace(koht,"s$","")]
  works[,koht:=str_replace(koht,"l$","")]
  
  
  #works <- works#[!is.na(koht),.N,by=koht]
  works <- works[,koht:=str_replace_all(koht,"s$","")]
  works <- works[,koht:=str_replace_all(koht,"l$","")]
  #works <- works[!duplicated(works)]
  #places[places!=""&places!="S.l."&places!="S.l.,"&places!="S. l."&places!="S. l.,"]
  works <- works[koht!=""&koht!="S.l."&koht!="S.l.,"&koht!="S. l."&koht!="S. l.,"]
  works[str_detect(koht,"Paide"),koht:="Paide"]
  works[str_detect(koht,"Weissenstein"),koht:="Paide"]
  works[str_detect(koht,"Вейсенштейн"),koht:="Paide"]
  works[str_detect(koht,"Вейссенштейн"),koht:="Paide"]
  works[str_detect(koht,"Haapsalu"),koht:="Haapsalu"]
  works[str_detect(koht,"Hapsa"),koht:="Haapsalu"]
  works[str_detect(koht,"Гапсаль"),koht:="Haapsalu"]
  
  works[str_detect(koht,"Keila"),koht:="Keila"]
  works[str_detect(koht,"Rakvere"),koht:="Rakvere"]
  works[str_detect(koht,"Wesenberg"),koht:="Rakvere"]
  works[str_detect(koht,"Везенберг"),koht:="Rakvere"]
  
  works[,koht:=str_replace_all(koht,"w","v")]
  works[,koht:=str_replace_all(koht,"W","V")]
  works[,koht:=str_replace_all(koht,"\\?","")]
  works[str_detect(koht,"Tall"),koht:="hotell Tallinn"]
  works[str_detect(koht,"Talinn"),koht:="hotell Tallinn"]
  works[str_detect(koht,"Reva"),koht:="hotell Tallinn"]
  works[str_detect(koht,"Revel"),koht:="hotell Tallinn"]
  works[str_detect(koht,"Ревель"),koht:="hotell Tallinn"]
  works[str_detect(koht,"Reve"),koht:="hotell Tallinn"]
  works[str_detect(koht,"Таллинн"),koht:="hotell Tallinn"]
  works[str_detect(koht,"Таллин"),koht:="hotell Tallinn"]
  works[str_detect(koht,"hotell Tallinn"),koht:="Tallinn"]
  works[str_detect(koht,"Kures"),koht:="Kuressaare"]
  works[str_detect(koht,"Kurres"),koht:="Kuressaare"]
  works[str_detect(koht,"Arensburg"),koht:="Kuressaare"]
  works[str_detect(koht,"Аренсбург"),koht:="Kuressaare"]
  
  
  works[str_detect(koht,"Rakwere"),koht:="Rakvere"]
  works[str_detect(koht,"Rakvere"),koht:="Rakvere"]
  works[str_detect(koht,"Tartu"),koht:="Tartu"]
  works[str_detect(koht,"Taaralinn"),koht:="Tartu"]
  works[str_detect(koht,"Jurjev"),koht:="Tartu"]
  works[str_detect(koht,"Jürjev"),koht:="Tartu"]
  works[str_detect(koht,"Юрьев"),koht:="Tartu"]
  #works[str_detect(koht,"Jurjew"),koht:="Tartu"]
  works[str_detect(koht,"Dorpat"),koht:="Tartu"]
  works[str_detect(koht,"Derpt"),koht:="Tartu"]
  works[str_detect(koht,"Дерпт"),koht:="Tartu"]
  works[str_detect(koht,"Тарту"),koht:="Tartu"]
  
  works[str_detect(koht,"Tarto"),koht:="Tartu"]
  works[str_detect(koht,"Leningrad"),koht:="Peterburi"]
  works[str_detect(koht,"Leningraad"),koht:="Peterburi"]
  works[str_detect(koht,"Petersburg"),koht:="Peterburi"]
  works[str_detect(koht,"Peterburg"),koht:="Peterburi"]
  works[str_detect(koht,"Peterburi"),koht:="Peterburi"]
  works[str_detect(koht,"Petrograd"),koht:="Peterburi"]
  works[str_detect(koht,"Петроград"),koht:="Peterburi"]
  works[str_detect(koht,"Петрогад"),koht:="Peterburi"]
  works[str_detect(koht,"Петербург"),koht:="Peterburi"]
  works[str_detect(koht,"Leeningrad"),koht:="Peterburi"]
  works[str_detect(koht,"Leeningraad"),koht:="Peterburi"]
  works[str_detect(koht,"Peeterburg"),koht:="Peterburi"]
  works[str_detect(koht,"Peeterburi"),koht:="Peterburi"]
  works[str_detect(koht,"СПБ"),koht:="Peterburi"]
  works[str_detect(koht,"Ленинград"),koht:="Peterburi"]
  works[str_detect(koht,"Кохтла-Ярве"),koht:="Kohtla-Järve"]
  works[str_detect(koht,"Wittenberg"),koht:="Vittenberg"]
  works[str_detect(koht,"Vittenberg"),koht:="Vittenberg"]
  works[str_detect(koht,"Киев"),koht:="Kiev"]
  works[str_detect(koht,"Немме"),koht:="Nõmme"] #can be from other Tallinn's too
  works[str_detect(koht,"Gutmannsbach"),koht:="Häädemeeste"]
  
  works[str_detect(koht,"Viljandi"),koht:="Viljandi"]
  works[str_detect(koht,"Fellin"),koht:="Viljandi"]
  works[str_detect(koht,"Феллин"),koht:="Viljandi"]
  works[str_detect(koht,"Pärnu"),koht:="Pärnu"]
  works[str_detect(koht,"Perno"),koht:="Pärnu"]
  works[str_detect(koht,"Pernu"),koht:="Pärnu"]
  works[str_detect(koht,"Pernov"),koht:="Pärnu"]
  works[str_detect(koht,"Pernau"),koht:="Pärnu"]
  works[str_detect(koht,"Пернов"),koht:="Pärnu"]
  works[str_detect(koht,"Põltsamaa"),koht:="Põltsamaa"]
  works[str_detect(koht,"Oberpahlen"),koht:="Põltsamaa"]
  
  works[str_detect(koht,"Narva"),koht:="Narva"]
  works[str_detect(koht,"Нарва"),koht:="Narva"]
  works[str_detect(koht,"Riga"),koht:="Riia"]
  works[str_detect(koht,"Riia"),koht:="Riia"]
  works[str_detect(koht,"Рига"),koht:="Riia"]
  works[str_detect(koht,"Rija"),koht:="Riia"]
  works[str_detect(koht,"Ria"),koht:="Riia"]
  works[str_detect(koht,"Valga"),koht:="Valga"]
  works[str_detect(koht,"Valk"),koht:="Valga"]
  works[str_detect(koht,"Валк"),koht:="Valga"]
  works[str_detect(koht,"Vilniu"),koht:="Vilnius"]
  
  works[str_detect(koht,"Vändra"),koht:="Vändra"]
  works[str_detect(koht,"Keila"),koht:="Keila"]
  works[str_detect(koht,"Võru"),koht:="Võru"]
  works[str_detect(koht,"Verro"),koht:="Võru"]
  works[str_detect(koht,"Верро"),koht:="Võru"]
  works[str_detect(koht,"Helsingi"),koht:="Helsinki"]
  works[str_detect(koht,"Helsinki"),koht:="Helsinki"]
  works[str_detect(koht,"Nev-York"),koht:="New-York"]
  works[str_detect(koht,"Nev York"),koht:="New York"]
  works[str_detect(koht,"Москва"),koht:="Moskva"]
  works[str_detect(koht,"Vaivara"),koht:="Vaivara"]
  works[str_detect(koht,"Vaivara"),koht:="Vaivara"]
  works[str_detect(koht,"Eesti"),koht:="Eesti"]
  works[,koht:=trimws(koht)]
  
  works[str_detect(koht,"Aarhu"),koht:="Aarhus"]
  
  if (fuzzy==T){
    #make this into a function
    # standardize places that have only one character or less difference in their first 8 characters
    plotdata_kohad <- works[!is.na(koht)&koht!=""][,.N,by=koht][order(koht)]#[N>100]
    
    #[order(N)][,koht:=factor(koht,levels=unique(koht))]
    
    
    plotdata_kohad[,prevkoht:=lag(koht)]
    plotdata_kohad[,subkoht:=str_extract(koht,"[^ ]+")]
    plotdata_kohad[,subprev:=str_extract(prevkoht,"[^ ]+")]
    #plotdata_kohad[,subkoht:=substr(koht,1,8)]
    #plotdata_kohad[,subnext:=substr(nextkoht,1,8)]
    plotdata_kohad[,distthem:=stringdist(subkoht,subprev)]
    #plotdata_kohad[distthem<=1]
    #plotdata_kohad[subkoht==subnext]
    to <- plotdata_kohad[distthem<=1,prevkoht]
    from <- plotdata_kohad[distthem<=1,koht]
    map = setNames(to, from)
    plotdata_kohad[,koht2:=map[koht]]
    plotdata_kohad[is.na(koht2),koht2:=koht]
    
    plotdata_kohad[,prevkoht:=lag(koht2)]
    plotdata_kohad[,subkoht:=str_extract(koht2,"[^ ]+")]
    plotdata_kohad[,subprev:=str_extract(prevkoht,"[^ ]+")]
    plotdata_kohad[,distthem:=stringdist(subkoht,subprev)]
    to <- plotdata_kohad[distthem<=1,prevkoht]
    from <- plotdata_kohad[distthem<=1,koht2]
    map = setNames(to, from)
    plotdata_kohad[,koht2:=map[koht2]]
    plotdata_kohad[is.na(koht2),koht2:=koht]
    
    
    plotdata_kohad[,prevkoht:=lag(koht2)]
    plotdata_kohad[,subkoht:=str_extract(koht2,"[^ ]+")]
    plotdata_kohad[,subprev:=str_extract(prevkoht,"[^ ]+")]
    plotdata_kohad[,distthem:=stringdist(subkoht,subprev)]
    to <- plotdata_kohad[distthem<=1,prevkoht]
    from <- plotdata_kohad[distthem<=1,koht2]
    map = setNames(to, from)
    plotdata_kohad[,koht2:=map[koht2]]
    plotdata_kohad[is.na(koht2),koht2:=koht]
    
    
    
    plotdata_kohad[,prevkoht:=lag(koht2)]
    plotdata_kohad[,subkoht:=str_extract(koht2,"[^ ]+")]
    plotdata_kohad[,subprev:=str_extract(prevkoht,"[^ ]+")]
    plotdata_kohad[,distthem:=stringdist(subkoht,subprev)]
    to <- plotdata_kohad[distthem<=1,prevkoht]
    from <- plotdata_kohad[distthem<=1,koht2]
    map = setNames(to, from)
    plotdata_kohad[,koht2:=map[koht2]]
    plotdata_kohad[is.na(koht2),koht2:=koht]
    
    
    to <- plotdata_kohad[,koht2]
    from <- plotdata_kohad[,koht]
    map = setNames(to, from)
    
    #overview
    plotdata_kohad2 <- plotdata_kohad[,.(N=sum(N)),by=koht2]
    
    works[,koht:=map[koht]]
    
    #check
    #works[,.N,by=koht]
  }
}


#' Harmonize publishers (ENB)
#'
#' This function harmonizes publishers in ENB
#' @param genres the data.table format work_by_row table
#' @keywords publishers
#' @export
#' @examples
#' @return a similar table as the input but with the publisher column replaced with harmonised publishers
#' harmonize_publishers()
#' see https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/ on installation
#' 
#' 

harmonize_publishers <- function(works){
  
  works <- data.table(works)
  works[,kirjastus:=trimws(kirjastus)]
  works[kirjastus=="[s.n.],",kirjastus:=""]
  works[kirjastus=="[s.n.],",kirjastus:=""]
  works[kirjastus=="[s.n.,",kirjastus:=""]
  works[kirjastus=="s.n.,",kirjastus:=""]
  works[kirjastus=="s.n.],",kirjastus:=""]
  works[kirjastus=="s. n.,",kirjastus:=""]
  works[kirjastus=="s.n,",kirjastus:=""]
  works[kirjastus=="s.n.",kirjastus:=""]
  
  works[,kirjastus:=str_replace_all(kirjastus,",","")]
  works[,kirjastus:=str_replace_all(kirjastus,"-"," ")]
  works[,kirjastus:=trimws(kirjastus)]
  works[,kirjastus:=tolower(kirjastus)]
  works[,kirjastus:=str_replace_all(kirjastus,"\\. ",".")]
  works[,kirjastus:=str_replace_all(kirjastus,"'i rmtkpl. ","")]
  works[kirjastus=="3.diviisi staap",kirjastus:="3.diviis"]
  works[kirjastus=="a.busch'i rmtkpl.",kirjastus:="a.busch"]
  works[kirjastus=="a.ingelman",kirjastus:="a.ingelmann"]
  works[kirjastus=="b.pärli",kirjastus:="br.pärli"]
  works[kirjastus=="c.mathiesen",kirjastus:="k.mattiesen"]
  works[kirjastus=="draamastuudio teater",kirjastus:="draamateater"]
  works[kirjastus=="eesti draama studio",kirjastus:="draamateater"]
  works[kirjastus=="eesti draamastuudio",kirjastus:="draamateater"]
  works[kirjastus=="eesti draamateater",kirjastus:="draamateater"]
  works[kirjastus=="draamateaater",kirjastus:="draamateater"]
  works[kirjastus=="eesti karskuseseltside kestoimekond",kirjastus:="eesti karskusliit"]
  works[kirjastus=="eesti karskusseltside kesktoimekond",kirjastus:="eesti karskusliit"]
  works[kirjastus=="eesti kirjanduse seltsi kodu uurimise toimkond",kirjastus:="eesti kirjanduse selts"]
  works[kirjastus=="eesti kirjanduse seltsi koolikirjanduse toimkond",kirjastus:="eesti kirjanduse selts"]
  works[kirjastus=="eesti kirjanduse kirjastus",kirjastus:="eesti kirjandus"]
  works[kirjastus=="eesti kubermangu kooliõp.vastastikkuse abiandm.selts",kirjastus:="eesti kubermangu kooliõpetajate vastastikku abiandmise selts"]
  works[kirjastus=="eestimaa konsistooriumi kirj kassa",kirjastus:="eestimaa konsistooriumi kirjastuskassa"]
  works[kirjastus=="eestimaa kooliõpetajate vastast.abiandm.selts",kirjastus:="eestimaa kooliõpetajate selts"]
  works[kirjastus=="eestimaa kooliõpetajate vastastiku abiandmise seltsi raamatukauplus",kirjastus:="eestimaa kooliõpetajate selts"]
  works[kirjastus=="eestimaa kooliõpetajate selts",kirjastus:="eesti õpetajate liit"]
  #works[kirjastus=="eestimaa kooliõpetajate vastastiku abiandmise selts (k./ü.\\"\\"\\"\\"kool\\"\\"\\"\\")",kirjastus:="eestimaa kooliõpetajate selts"]
  works[kirjastus=="eesti nmkü liit",kirjastus:="eesti noorte meeste kristlike ühingute liit"]
  works[kirjastus=="eesti põllutöö kirjanduse ühendus",kirjastus:="eesti põllutöö ühendus"]
  works[kirjastus=="eesti töörahva kommuuna kultuura ja ariduse valitsus",kirjastus:="eesti töörahwa kommuuna kultuura ja ariduse walitsus"]
  works[kirjastus=="estoonia muusika osakond",kirjastus:="estonia muusika osakond"]
  works[kirjastus=="f.brandt",kirjastus:=""]
  works[kirjastus=="fr.kreutzwald",kirjastus:="f.r.kreutzwald"]
  works[kirjastus=="füüsika õpetamise komisjon",kirjastus:="füüsika õpetamiskomisjon"]
  works[kirjastus=="g.pihlaka raamatukauplus",kirjastus:="g.pihlakas"]
  works[kirjastus=="g.pihlaka ramatukauplus",kirjastus:="g.pihlakas"]
  works[kirjastus=="g.pihlaka rmtkpl.",kirjastus:="g.pihlakas"]
  works[kirjastus=="pihlakas'e kirjastus",kirjastus:="g.pihlakas"]
  works[kirjastus=="g.vänja",kirjastus:="g.wänja kuluga rosenberg"]
  works[kirjastus=="g.v.rosenberg",kirjastus:="g.wänja kuluga rosenberg"]
  works[kirjastus=="g.v.rosenberg?",kirjastus:="g.wänja kuluga rosenberg"]
  works[kirjastus=="g.vänjag.rosenberg",kirjastus:="g.wänja kuluga rosenberg"]
  works[kirjastus=="hariduse ja sotsiaalministeerium",kirjastus:="hariduse ja sotsiaalministeerium"]
  works[kirjastus=="hariduse ja sotsiaalministeeriumi koolivalitsus",kirjastus:="hariduse ja sotsiaalministeerium"]
  works[kirjastus=="haridus ja sotsiaalministeerium",kirjastus:="hariduse ja sotsiaalministeerium"]
  works[kirjastus=="haridus ja sotsiaalministeeriumi koolivalitsus",kirjastus:="hariduse ja sotsiaalministeerium"]
  works[kirjastus=="haridusministeeriumi kirjastus",kirjastus:="haridusministeerium"]
  works[kirjastus=="haridusministeeriumi koolivalitsus",kirjastus:="haridusministeerium"]
  works[kirjastus=="haridusministeeriumi kutseoskuse osakond",kirjastus:="haridusministeerium"]
  works[kirjastus=="h.laasi raamatukauplus",kirjastus:="h.laasi rmtkpl."]
  works[kirjastus=="h.leoke'se rmtkpl.",kirjastus:="h.leoke"]
  works[kirjastus=="h.treffneri gümnaasium",kirjastus:="hugo treffneri gümnaasium"]
  works[kirjastus=="internationali traktaati selts osakond tartus",kirjastus:="internatsionaali traktaadiselts"]
  works[kirjastus=="internatsionali traktadiselts",kirjastus:="internatsionaali traktaadiselts"]
  works[kirjastus=="dreimanni rmtkpl.",kirjastus:="j.dreimann"]
  works[kirjastus=="j.dreimanni rmtkpl",kirjastus:="j.dreimann"]
  works[kirjastus=="j.treimann",kirjastus:="j.dreimann"]
  works[kirjastus=="j.e.meigas ja j.kehlmann",kirjastus:="j.e.meigase rmtkpl"]
  works[kirjastus=="j.g.marquardt)",kirjastus:="j.g.marquardt"]
  works[kirjastus=="j.h.gressel)",kirjastus:="j.h.gressel"]
  works[kirjastus=="j.kalkun'i raamatukauplus",kirjastus:="j.kalkuni raamatukauplus"]
  works[kirjastus=="j.keerdo",kirjastus:="j.kerdo"]
  works[kirjastus=="j.kerdo :",kirjastus:="j.kerdo"]
  works[kirjastus=="j.lass",kirjastus:="joh.lass"]
  works[kirjastus=="joh.semmiste",kirjastus:="joh.semm"]
  works[kirjastus=="j.palmgren'i raamatukauplus",kirjastus:="j.palmgren"]
  works[kirjastus=="j.palmgreni rmtkpl",kirjastus:="j.palmgren"]
  works[kirjastus=="j.palmgreni rmtkpl.",kirjastus:="j.palmgren"]
  works[kirjastus=="j.ploompuu rmtkpl.",kirjastus:="j.ploompuu"]
  works[kirjastus=="j.ploompuu raamatukauplus",kirjastus:="j.ploompuu"]
  works[kirjastus=="ploompuu raamatukauplus",kirjastus:="j.ploompuu"]
  works[kirjastus=="ploompuu",kirjastus:="j.ploompuu"]
  works[kirjastus=="j.reimanni pealadu",kirjastus:="j.reimanni rmtkpl"]
  works[kirjastus=="reimanni raamatukauplus",kirjastus:="j.reimanni rmtkpl"]
  works[kirjastus=="j.r.rerzold",kirjastus:="j.r.rezold"]
  works[kirjastus=="j.tamman",kirjastus:="j.tammann"]
  works[kirjastus=="jummala sanna väljalautamisse sõbbrad",kirjastus:="jummala sanna wäljalautamisse söbbrad"]
  works[kirjastus=="jutuleht :",kirjastus:="jutuleht"]
  works[kirjastus=="kaitseliidu peastaap",kirjastus:="kaitseliit"]
  works[kirjastus=="kaitseväe ühendatud õppeasutiste staap",kirjastus:="kaitseväe ühendatud õppeasutised"]
  works[kirjastus=="kaitseväe ühendatud õppeasutused",kirjastus:="kaitseväe ühendatud õppeasutised"]
  works[kirjastus=="kaitseväe ühendatud õppeasutuste ohvitseride kogu",kirjastus:="kaitseväe ühendatud õppeasutised"]
  works[kirjastus=="kaitsevägede ühendatud õppeasutiste staap",kirjastus:="kaitseväe ühendatud õppeasutised"]
  works[kirjastus=="kaitsevägede staabi ii osakond",kirjastus:="kaitsevägede staap"]
  works[kirjastus=="kaitsevägede staabi vi osakond",kirjastus:="kaitsevägede staap"]
  works[kirjastus=="kaitsevägede staabi v osakond",kirjastus:="kaitsevägede staap"]
  works[kirjastus=="kaitsevägede ülemjuhataja",kirjastus:="kaitsevägede staap"]
  works[kirjastus=="k.busch'i raamatukauplus",kirjastus:="k.busch"]
  works[kirjastus=="k.busch'i rmtkpl.",kirjastus:="k.busch"]
  works[kirjastus=="k.buschi rmtkpl.",kirjastus:="k.busch"]
  works[kirjastus=="kili & co.",kirjastus:="kili & co"]
  works[kirjastus=="kili & ko",kirjastus:="kili & co"]
  works[kirjastus=="kindralstaabi 6.osakond",kirjastus:="kindralstaap"]
  works[kirjastus=="kindralstaabi ii osakond",kirjastus:="kindralstaap"]
  works[kirjastus=="kindralstaabi iv osakond",kirjastus:="kindralstaap"]
  works[kirjastus=="kindralstaabi vi osakond",kirjastus:="kindralstaap"]
  works[kirjastus=="k.laurmanni rmtkpl.",kirjastus:="k.laurmann"]
  works[kirjastus=="kluge ja ströhmi raamatukauplus",kirjastus:="kluge ja ströhm"]
  works[kirjastus=="c.mattiesen",kirjastus:="k.mattiesen"]
  works[kirjastus=="kristliku rahwakirjanduse agentur",kirjastus:="kristliku rahvakirjanduse agentur"]
  works[kirjastus=="kristlik 7 da päeva adventistide eesti ühisus",kirjastus:="kristlik seitsmenda päewa adwentistide eesti ühisus"]
  works[kirjastus=="k.sööt ja g.roht",kirjastus:="k.sööt"]
  works[kirjastus=="küõ õppejaoskond",kirjastus:="kaitseväe ühendatud õppeasutised"]
  works[kirjastus=="küõ sõjakool",kirjastus:="kaitseväe ühendatud õppeasutised"]
  works[kirjastus=="küõ sõjakooli patarei",kirjastus:="kaitseväe ühendatud õppeasutised"]
  works[kirjastus=="mattiesen",kirjastus:="k.mattiesen"]
  works[kirjastus=="merekindluste staap",kirjastus:="merejõudude staap"]
  works[kirjastus=="merelaevastiku divisjoni staap",kirjastus:="merejõudude staap"]
  works[kirjastus=="merijõudude staap",kirjastus:="merejõudude staap"]
  works[kirjastus=="naiskodukaitse keskjuhatus",kirjastus:="naiskodukaitse"]
  works[kirjastus=="n.erna ;",kirjastus:="n.erna"]
  works[kirjastus=="noorte kotkaste päästaap",kirjastus:="noorte kotkaste peastaap"]
  works[kirjastus=="odamees carl sarap",kirjastus:="odamees"]
  works[kirjastus=="paltimaa õigeusus vennaste selts ja riia peetri pauluse vennasteselts",kirjastus:="paltimaa õigeusu vennaste selts"]
  works[kirjastus=="paltimaa õigeusu vennaste selts ja riia peetri pauluse vennaste selts",kirjastus:="paltimaa õigeusu vennaste selts"]
  works[kirjastus=="paltimaa õigeusu vennasteselts ja riia peetri pauluse vennaste selts",kirjastus:="paltimaa õigeusu vennaste selts"]
  works[kirjastus=="pärnu maakonnavalitsuse haridusosakond",kirjastus:="pärnu maakonna koolivalitsus"]
  works[kirjastus=="p.erna raamatukauplus",kirjastus:="p.erna"]
  works[kirjastus=="p.erna rmtkpl",kirjastus:="p.erna"]
  works[kirjastus=="p.erna rmtkpl.",kirjastus:="p.erna"]
  works[kirjastus=="postimehe rmtkpl",kirjastus:="postimees"]
  works[kirjastus=="postitalitus",kirjastus:="postivalitsus"]
  works[kirjastus=="posti telegraafi telefoni valitsus",kirjastus:="postivalitsus"]
  works[kirjastus=="r.krupp.",kirjastus:="r.krupp"]
  works[kirjastus=="r.põder'i rmtkpl",kirjastus:="r.põder"]
  works[kirjastus=="r.põderi rmtkpl.",kirjastus:="r.põder"]
  works[kirjastus=="schnackenburg",kirjastus:="schnakenburg"]
  works[kirjastus=="schnakenburg :",kirjastus:="schnakenburg"]
  works[kirjastus=="seiler",kirjastus:="seileri mootoritehas"]
  works[kirjastus=="s n.",kirjastus:=""]
  works[kirjastus=="s.n",kirjastus:=""]
  works[trimws(kirjastus)=="s.n.",kirjastus:=""]
  works[str_detect(kirjastus,"^s.n."),kirjastus:=""]
  works[str_detect(kirjastus,"s.n"),kirjastus:=""]
  works[str_detect(kirjastus,"^sõjav"),kirjastus:="sõjavägi"]
  works[str_detect(kirjastus,"^sõjamini"),kirjastus:="sõjaministeerium"]
  works[str_detect(kirjastus,"siuru"),kirjastus:="siuru"]
  works[kirjastus=="spadv eesti liit",kirjastus:="spadv kog.eesti liit"]
  works[kirjastus=="spadvk eesti liit",kirjastus:="spadv kog.eesti liit"]
  works[kirjastus=="spadwk eesti liit",kirjastus:="spadv kog.eesti liit"]
  works[kirjastus=="tallinna eesti kirjastuse ühisus ;",kirjastus:="tallinna eesti kirjastuse ühisus"]
  works[kirjastus=="tallinna eesti kirjastus ühisus",kirjastus:="tallinna eesti kirjastuse ühisus"]
  works[kirjastus=="tallinna kubermangu karskuse kuratoorium",kirjastus:="tallinna kubermangu karskuse kuratorium"]
  works[kirjastus=="tallinna kubermangu karskuse kuratoriumi väljaanne",kirjastus:="tallinna kubermangu karskuse kuratorium"]
  works[kirjastus=="tallinna kub.karskuse kuratorium",kirjastus:="tallinna kubermangu karskuse kuratorium"]
  works[kirjastus=="t.m.franzdorfi raamatukauplus",kirjastus:="t.m.franzdorf"]
  works[kirjastus=="t.mutsu teatrikirjastus",kirjastus:="t.mutsu"]
  works[kirjastus=="vanemuine",kirjastus:="vanemuise näitelava"]
  works[kirjastus=="vanemuise näielava",kirjastus:="vanemuise näitelava"]
  works[kirjastus=="vanemuise näitelava.",kirjastus:="vanemuise näitelava"]
  works[kirjastus=="vanemuise teater",kirjastus:="vanemuise näitelava"]
  works[kirjastus=="wanemuise näitelava",kirjastus:="vanemuise näitelava"]
  works[kirjastus=="wanemuise näitelawa",kirjastus:="vanemuise näitelava"]
  works[kirjastus=="tarto wannemuine selts",kirjastus:="vanemuise näitelava"]
  works[kirjastus=="võru õpetajateseminar",kirjastus:="võru õpetajate seminar"]
  works[kirjastus=="võru seminar",kirjastus:="võru õpetajate seminar"]
  works[kirjastus=="w.bormi pär.",kirjastus:="w.borm"]
  works[kirjastus=="f.w.borm",kirjastus:="w.borm"]
  works[str_detect(kirjastus,"noor eesti"),kirjastus:="noor eesti"]
  works[str_detect(kirjastus,"mutsu"),kirjastus:="t.mutsu"]
  works[str_detect(kirjastus,"ploompuu"),kirjastus:="j.ploompuu"]
  works[str_detect(kirjastus,"postimehe"),kirjastus:="postimees"]
  works[str_detect(kirjastus,"postimees"),kirjastus:="postimees"]
  works[str_detect(kirjastus,"h\\.laakmann"),kirjastus:="h.laakmann"]
  works[str_detect(kirjastus,"eesti kirjastus"),kirjastus:="eesti kirjastuse ühisus"]
  works[kirjastus=="eesti kirjanduseselts",kirjastus:="eesti kirjanduse selts"]
  works[kirjastus=="hermann",kirjastus:="k.a.hermann"]
  works[kirjastus=="hermann'i raamatukauplus",kirjastus:="k.a.hermann"]
  works[kirjastus=="pealadu hermanni rmtkpl.",kirjastus:="k.a.hermann"]
  works[kirjastus=="hermanni raamatukauplus",kirjastus:="k.a.hermann"]
  works[kirjastus=="hermann'i rmtkpl.",kirjastus:="k.a.hermann"]
  works[kirjastus=="pealadu hermanni kaupluses",kirjastus:="k.a.hermann"]
  works[kirjastus=="leoke'se kirjastus",kirjastus:="h.leoke"]
  works[kirjastus=="leoke",kirjastus:="h.leoke"]
  works[kirjastus=="leoke'se raamatuäri antikvariaat",kirjastus:="h.leoke"]
  works[str_detect(kirjastus,"h\\.leoke"),kirjastus:="h.leoke"]
  works[str_detect(kirjastus,"a\\.busch"),kirjastus:="a.busch"]
  works[str_detect(kirjastus,"jummala"),kirjastus:="jummala sanna wäljalautamisse söbbrad"]
  works[kirjastus=="eesti karskuseseltside kesktoimekond",kirjastus:="eesti karskusliit"]
  works[kirjastus=="eesti karskusseltside kesktoimkond",kirjastus:="eesti karskusliit"]
  works[kirjastus=="briti ja väljamaa piibliselts",kirjastus:="briti ja välismaa piibliselts"]
  works[kirjastus=="briti ja väljamaa piibliselts",kirjastus:="briti ja välismaa piibliselts"]
  works[kirjastus=="briti ja väljamaa piibliselts",kirjastus:="briti ja välismaa piibliselts"]
  works[kirjastus=="briti ja väljamaa piibliselts",kirjastus:="briti ja välismaa piibliselts"]
  works[kirjastus=="briti ja väljamaa piibliselts",kirjastus:="briti ja välismaa piibliselts"]
  works[str_detect(kirjastus,"Lindfors"),kirjastus:="Lindfors,"]
  works[str_detect(kirjastus,"\\["),kirjastus:=str_replace_all(kirjastus,"\\[","")]
  works[str_detect(kirjastus,"\\]"),kirjastus:=str_replace_all(kirjastus,"\\]","")]
  works[kirjastus=="",kirjastus:=NA]
  
  works[,kirjastus_first:=min(aeg,na.rm = T),by=kirjastus]
  works[is.na(kirjastus),kirjastus_first:=NA]
  
  # the standardization was done with a focus on 1850-1930 and top 50 publishers
  # many remain unstandardized
  
}


#' @title extract stard and end year
#' @description Extract start and end year in simple cases
#' @param data: a data frame, field: character string specifying the name of the field with start and end information for authors.
#' @return the input data frame with the new columns start_year and end_year that include the parsed start and end years of the author.

extract_start_and_end <- function(data,field="autor_dates"){
  
  start_end <-  str_extract(string=as.character(data[,field]),pattern="([0-9]{4}-[0-9]{4})|([0-9]{4}-)|(-[0-9]{4})") 
  start_year <- str_extract(start_end,"[0-9]{4}-") %>% gsub(.,pattern = "-",replacement = "") %>% as.integer(.)
  end_year <- str_extract(start_end,"-[0-9]{4}") %>% gsub(.,pattern = "-",replacement = "") %>% as.integer(.)
  data <- cbind.data.frame(data,start_year,end_year)
  return(data)  
}


#' @title Remove Persons
#' @description Remove persons.
#' @param x A vector
#' @param who names to be removed
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- remove_persons(x)}
#' @keywords utilities
remove_persons <- function (x, who = NULL) {
  
  # Get printing terms from a table
  # TODO later add names from the complete name list as well ?
  if (is.null(who)) {
    f <- "ENB_workshop/data/persons.csv"
    terms <- as.character(read.csv(f)[,1])
  }
  
  x <- remove_terms(x, terms, include.lowercase = TRUE)
  
  x
  
}



#' @title Condense Spaces
#' @description Trim and remove double spaces from the input strings.
#' @param x A vector
#' @importFrom stringr str_trim
#' @return A vector with extra spaces removed
#' @details Beginning, double and ending spaces are also removed from the strings.
#' @export 
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica") 
#' @examples x2 <- condense_spaces(" a  b cd ") # "a b cd"
#' @keywords utilities
condense_spaces <- function (x) {
  
  x <- str_trim(x, "both")
  x <- gsub(" +", " ", x)
  x[x == ""] <- NA
  
  x
  
}




#' @title Remove Special Chars
#' @description Remove special characters.
#' @param x Character vector
#' @param chars Characters to be removed
#' @param niter Number of iterations 
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- remove_special_chars("test;")
#' @keywords utilities
remove_special_chars <- function (x, chars = c(",", ";", ":", "\\(", "\\)", "\\?", "--", "\\&"), niter = 5) {
  
  for (n in 1:niter) {
    
    x <- str_trim(x)
    
    for (char in chars) {
      x <- gsub(paste(char, "$", sep = ""), " ", x)
      x <- gsub(paste("^", char, sep = ""), " ", x)
      x <- gsub(char, " ", x)
    }
    
    for (char in c("\\[", "]")) {
      x <- gsub(paste(char, "$", sep = ""), "", x)
      x <- gsub(paste("^", char, sep = ""), "", x)
      x <- gsub(char, "", x)
    }
  }
  
  x <- condense_spaces(x)
  
  x[x == ""] <- NA
  
  x
  
}



#' @title Remove Brackets from Letters
#' @description Remove brackets surrounding letters.
#' @param x A vector
#' @param myletters Letters to remove
#' @return A polished vector 
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- remove_brackets_from_letters("[p]")
#' @keywords utilities
remove_brackets_from_letters <- function (x, myletters = NULL) {
  
  if (is.null(myletters)) {myletters <- c(letters, LETTERS)}
  
  # [P] -> P
  for (l in myletters) {
    x <- gsub(paste("\\[", l, "\\]", sep = ""), l, x)
    x <- gsub(paste("\\(", l, "\\)", sep = ""), l, x)
  }   
  x
  
}


#' @title Map Terms
#' @description Map between two sets of terms; used to harmonize data.
#' @details Map the input vector to harmonized versions based on the synonyme
#'          table.
#' @param x A character vector 
#' @param synonymes synonyme table with the fields 'synonyme' and 'name'
#' @param remove.unknown Logical. Remove terms that do not have synonymes.
#' @param mode 'exact.match' replaces the terms based on the synonyme list
#'        if an exact match is found; 'match' replaces the parts that match
#'        synonymes; 'recursive' replaces all (sub)strings recursively in
#'        the same order as in the synonyme table
#' @param verbose verbose
#' @param from field that will be replaced
#' @param to field that contains the final names
#' @param keep.names Keep the original names in the returned vector
#'        (may slow down analysis in large data sets)
#' @return Vector of harmonized terms
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- map(x, synonymes)}
#' @keywords utilities
map <- function (x, synonymes, remove.unknown = FALSE, mode = "exact.match", verbose = FALSE, from = "synonyme", to = "name", keep.names = FALSE) {
  
  if (verbose) {message("Map synonymes to selected names: NA if mapping not available")}
  xorig <- as.character(x)
  xx <- xuniq <- unique(xorig)
  synonymes <- synonymes[, c(from, to)]
  colnames(synonymes) <- c("synonyme", "name")
  for (i in 1:ncol(synonymes)) {
    synonymes[,i] <- as.character(synonymes[,i])
  }
  
  if (verbose) {message(mode)}
  
  if (mode == "exact.match") {
    
    # By default each term maps to itself
    # TODO to speed up remove first those that match directly
    # Only check those cases that overlap
    #inds <- which(xuniq %in% synonymes$synonyme)
    # Hack due some odd R bug that renders the previous line unfunctional
    inds <- which(xuniq %in% intersect(synonymes$synonyme, xuniq))
    
    if (remove.unknown && length(inds) == 0) {
      xx <- rep(NA, length(xx))
    } else if (remove.unknown && length(inds) > 0 && length(inds) < length(xx)) {
      xx[-inds] <- NA
    }
    
    for (i in inds) {
      
      inds2 <- which(synonymes$synonyme == xuniq[[i]])
      xh <- unique(synonymes$name[inds2])
      
      if (length(xh) == 1) {
        xx[[i]] <- xh
      } else if (length(xh) > 1)  {
        warning(paste("No unique mapping for", xuniq[[i]]))
        xx[[i]] <- NA
      } else if (length(xh) == 0 && remove.unknown)  {
        xx[[i]] <- NA
      }
    }
    
  } else {
    
    if (mode == "match") {
      # Go through synonymes from longest to shortest
      synonymes <- synonymes[rev(order(nchar(synonymes[, "synonyme"]))),]
    }
    
    # mode: "match" and "recursive"
    for (i in 1:nrow(synonymes)) {
      
      from <- synonymes[i, "synonyme"]
      
      if (!is.null(from) && (length(from) > 0 ) && (!is.na(from))) {
        
        #if (verbose) {message("-------------")}      
        #if (verbose) {message(xx)}
        #if (verbose) {message(from)}
        
        inds <- grep(from, xx)
        if (length(inds) > 0) {
          
          to <- synonymes[i, "name"]
          xx[inds] <- gsub(from, to, xx[inds])
          
          #print(synonymes[i,])
          #print(unique(xx[inds]))
          #print("---")
          
        }
        
      }
    }
  }
  
  
  # Map back to original inds 
  xx <- xx[match(xorig, xuniq)]
  
  if (keep.names) {
    names(xx) <- xorig
  }
  
  xx
  
}





#' @title Read Mapping
#' @description Read mapping table.
#' @param file Input file
#' @param mode The input file type: "list", "vector" or "table"; see details.
#' @param sep Separator mark.
#' @param self.match Include self matches
#' @param include.lowercase Include lowercase
#' @param ignore.empty Ignore the empty cases
#' @param sort Sort synonymes
#' @param verbose verbose
#' @param remove.ambiguous Remove ambiguous terms.
#' @param lowercase.only All synonymes considered in lowercase only
#' @param from field that will be replaced
#' @param to field that contains the final names
#' @param fast Use the fast method fread (sensitive to problems in table format)
#' @param encoding Character encoding (needed in Windows environment)
#' @param trim trim empty spaces from the beginning and end of the strings
#' @param empty.is.na Interpret empty conversions as NA. By default this is FALSE.
#' @return Synonyme data frame with the fields 'name' (the selected term) and 'synonyme' (the alternative terms).
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @details If mode = "list", each row of the input file corresponds to a unique entry with potentially multiple name variants, separated by semicolon. The first element gives the selected version of the name, the subsequent elements list synonymes that will be mapped to the selected version. If mode = "table", the file has two columns where each row corresponds to a unique entry and has the selected name and a single alternative name.
#' @examples \dontrun{syn <- read_mapping(file)}
#' @keywords utilities
read_mapping <- function (file, mode = "table", sep = ";", self.match = FALSE, include.lowercase = FALSE, ignore.empty = FALSE, sort = FALSE, verbose = FALSE, remove.ambiguous = TRUE, lowercase.only = FALSE, from = "synonyme", to = "name", fast = FALSE, encoding="UTF-8", trim = FALSE, empty.is.na = FALSE) {
  
  # TODO sort by desired field
  
  if (mode == "list") {
    
    rf <- suppressWarnings(readLines(file, encoding=encoding))
    
    aa <- lapply(rf, function (x) {unique(unlist(strsplit(x, sep), use.names = FALSE))})
    names(aa) <- sapply(rf, function (x) {unlist(strsplit(x, sep))[[1]]})
    
    map <- NULL
    for (nam in names(aa)) {
      map <- rbind(map, cbind(rep(nam, length(aa[[nam]])), aa[[nam]]))
    }
    aa <- as.data.frame(map, stringAsFactors = FALSE)
    names(aa) <- c("name", "synonyme")
    
    aa$name <- as.character(aa$name)
    aa$synonyme <- as.character(aa$synonyme)
    aa <- aa[!duplicated(aa),]
    
  } else if (mode %in% c("table", "vector")) {
    
    # aa <- read.csv(file, sep = sep, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    
    if (fast) {
      aa <- suppressWarnings(fread(file, sep = sep, header = TRUE, encoding=encoding))
      aa <- as.data.frame(aa)
      
    } else {
      # hegroiva 20191108: added the quote="" in order to read values containing quotes
      aa <- read.csv(file, sep = sep, header = TRUE, encoding = encoding, quote="")
    }
    
    # Temporarily name columns as name and synonyme
    # (needed in check_synonymes)
    aa <- aa[, c(from, to)]
    colnames(aa) <- c("synonyme", "name")
    
  }
  
  if (lowercase.only) {
    aa[[from]] <- tolower(aa[[from]])
  }
  
  # Polish the synonyme table
  aa <- suppressWarnings(check_synonymes(aa, include.lowercase = include.lowercase, verbose = verbose, sort = sort, self = self.match, ignore.empty = ignore.empty, remove.ambiguous = remove.ambiguous))
  
  # Return original field names
  colnames(aa) <- gsub("name", to, colnames(aa))
  colnames(aa) <- gsub("synonyme", from, colnames(aa))    
  
  # Trim empty spaces from the end
  if (trim) {
    aa$synonyme = str_trim(aa$synonyme)
    aa$name = str_trim(aa$name)    
  }
  
  # Trim empty spaces from the end
  if (!empty.is.na) {
    for (field in names(aa)) {
      aa[[field]][is.na(aa[[field]])] <- ""
    }
  }    
  if (trim) {
    aa$synonyme = str_trim(aa$synonyme)
    aa$name = str_trim(aa$name)    
  }
  
  if (mode == "vector") {
    vec <- as.vector(aa[, to])
    names(vec) <- as.character(aa[, from])
    aa <- vec    
  }
  
  aa 
  
}



#' @title Check Synonymes
#' @description Check synonyme table. Remove ambiguous mappings.
#' @param synonymes synonymes data.frame with the self-explanatory fields 'name' and 'synonyme'.
#' @param include.lowercase Include lowercase versions of the synonymes
#' @param verbose verbose
#' @param sort Sort synonymes
#' @param self Ensure that each name is synonyme for itself (this may
#'             cause ambiguous mappings, use with care !)
#' @param ignore.empty If name field is empty, accept the given synonyme as the final name.
#' @param remove.ambiguous Remove ambiguous terms.
#' @return Polished synonyme table
#' @export
#' @details Remove duplicated information. Ensure identical matches
#' are included in synonyme list.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{s <- check_synonymes(synonymes)}
#' @keywords utilities
check_synonymes <- function (synonymes, include.lowercase = TRUE, verbose = FALSE, sort = FALSE, self = FALSE, ignore.empty = FALSE, remove.ambiguous = TRUE) {
  
  synonyme <- NULL
  synonymes <- as.data.frame(synonymes, stringsAsFactors = FALSE)
  
  # Remove comment columns
  synonymes <- synonymes[, c("synonyme", "name")]
  synonymes$name <- as.character(synonymes$name)
  synonymes$synonyme <- as.character(synonymes$synonyme)    
  
  # If name field is not given, accept the synonyme as the final name
  if (ignore.empty) {
    inds <- which(synonymes$name == "")
    if (length(inds) > 0) {
      synonymes$name[inds] <- synonymes$synonyme[inds]
    }
  }
  
  # Ensure each proper name is synonyme also for itself
  if (self) {
    tmp1 <- cbind(name = as.character(synonymes$name),
                  synonyme = as.character(synonymes$synonyme))
    tmp2 <- cbind(name = as.character(synonymes$name),
                  synonyme = as.character(synonymes$name))
    synonymes <- as.data.frame(rbind(tmp1, tmp2), stringsAsFactors = FALSE)
  }
  
  # Include lowercase versions of the synonymes
  if (include.lowercase) {
    if (verbose) { message("Including lowercase versions of the synonymes") }
    synonymes <- rbind(synonymes, cbind(name = as.character(synonymes$name), synonyme = tolower(as.character(synonymes$synonyme))))
  }
  
  # Remove duplicated info
  synonymes <- unique(synonymes)
  
  # May cause a bug - to be checked
  # synonymes <- synonymes[which(!synonymes$synonyme == synonymes$name),]
  
  # Remove ambiguous names that map to many higher-level names
  spl <- split(as.character(synonymes$name), as.character(synonymes$synonyme))  
  ambiguous <- names(which(sapply(spl, length) > 1))
  # synonymes.ambiguous <- subset(synonymes, synonyme %in% ambiguous)
  if (remove.ambiguous) {
    if (length(ambiguous) > 0) {
      #warning(paste("Removing ambiguous terms from synonyme list (no unique mapping): ", paste(ambiguous, collapse = ",")))
      synonymes <- subset(synonymes, !synonyme %in% ambiguous)
    }
  }
  
  # Order alphabetically
  if (sort) {
    synonymes <- synonymes[order(as.character(synonymes$name)),]
  }
  
  # Unique entries
  synonymes <- unique(synonymes)  
  
  synonymes
  
}






#The following function is from the Bibliographica package.
#' @title Polish Place
#' @description Polish place names.
#' @param x A vector of place names
#' @param synonymes Synonyme table for place names
#' @param remove.unknown Logical. Remove places that are not validated (ie. listed in the synonyme table)?
#' @param verbose verbose
#' @param harmonize Harmonize the polished names. Logical.
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples # x2 <- polish_place(c("London", "Paris"))
#' @keywords utilities
polish_place <- function (x, synonymes = NULL, remove.unknown = FALSE, verbose = FALSE, harmonize = TRUE) {
  
  if (all(is.na(x))) {return(x)}
  
  if (is.null(synonymes)) {
    # Harmonize places with synonyme table
    f <-  "ENB_workshop/data/PublicationPlaceSynonymes.csv"
    synonymes <- suppressWarnings(read_mapping(f, include.lowercase = T, self.match = T, ignore.empty = FALSE, mode = "table", trim = TRUE))
    
    if (verbose) { message(paste("Reading special char table", f)) }
    # Harmonize places with synonyme table
    f <-  "ENB_workshop/data/replace_special_chars.csv"
    spechars <- suppressWarnings(read_mapping(f, sep = ";", mode = "table", include.lowercase = TRUE))
    
    if (verbose) { message(paste("Reading publication place synonyme table", f)) }
    f <-  "ENB_workshop/data/harmonize_place.csv"
    synonymes.spec <- suppressWarnings(read_mapping(f, sep = ";", mode = "table", include.lowercase = TRUE))
    if (verbose) { message(paste("Reading publication place synonyme table", f)) }
    
  }
  
  # Prepare
  if (verbose) { message("Convert to lowercase character and make unique list") }
  x0 <- x
  xorig <- tolower(as.character(x))
  x <- xuniq <- unique(xorig)
  
  if (verbose) { message("Replace special characters") }
  x <- as.character(map(x, spechars, mode = "recursive", verbose = verbose))
  
  if (verbose) { message("Remove brackets") }
  # Lo[n]don -> London  
  x <- remove_brackets_from_letters(x)
  
  # Some trivial trimming to speed up
  # TODO should go to synonyme list?
  # Remove numerics
  x <- gsub("[0-9]", " ", x) 
  x <- remove_special_chars(x, chars = c("\\(", "\\)", "\\[", "\\]", "\\{", "\\}", "\\="), niter = 1)
  x <- gsub("[_|:|;|,|\\?|\\&|\\.|/|>|<|\"| ]+", " ", x)
  x <- gsub("-+", " ", x)
  x <- gsub("'", " ", x)  
  x <- condense_spaces(x)
  x <- gsub(" sic ", " ", x)
  x <- gsub("^and ", "", x)
  x <- gsub("^from ", "", x)            
  x <- gsub("^re ", "", x)
  x <- gsub("^[a-z] +[a-z]$", " ", x)
  x <- gsub("^[a-z]\\. [a-z]$", " ", x)
  x[nchar(gsub(" ", "", x)) <= 2] = NA  
  x <- condense_spaces(x)
  
  # Back to original indices, then unique again;
  # reduces number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  if (verbose) {message(paste("Polishing ", length(xuniq), " unique place names", sep = ""))}
  x <- remove_persons(x)
  
  if (verbose) {message("Remove print statements")}
  x <- remove_print_statements(x)
  x <- condense_spaces(x)
  
  # Back to original indices, then unique again;
  # reduces number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  if (verbose) {message("Remove stopwords")}
  f <- "ENB_workshop/data/stopwords_for_place.csv"
  message(paste("Reading stopwords from file ", f))
  stopwords <- unique(tolower(str_trim(as.character(read.csv(f)[,1]))))
  x <- suppressWarnings(remove_terms(x, stopwords, c("begin", "middle", "end"), recursive = TRUE))
  
  if (verbose) {message("Harmonize ie")}
  x <- harmonize_ie(x)
  if (length(x) == 0) {return(rep(NA, length(xorig)))}
  
  # Back to original indices, then unique again;
  # reduces number of unique cases further
  if (verbose) {message("Match to original")}
  # Back to original indices, then unique again;
  # reduces number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  if (verbose) {message("Detailed polishing")}
  #s <- synonymes$synonyme
  
  x <- suppressWarnings(unname(sapply(x, function (x) {polish_place_help(unlist(x, use.names = FALSE), synonymes$synonyme, verbose = verbose)}, USE.NAMES = FALSE)))
  if (length(x) == 0) { return(rep(NA, length(xorig))) }
  
  # Back to original indices, then unique again; reduces
  # number of unique cases further
  if (verbose) {message("Match to original")}
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  if (length(x) == 0) {return(rep(NA, length(xorig)))}
  
  if (verbose) { message("Harmonize the synonymous names") }
  x <- suppressWarnings(as.character(map(x, synonymes.spec, mode = "recursive")))
  
  # Once more remove stopwords
  # Warning: the names discarded here wont be visible in
  # summary lists of discarded names !
  # For validation purposes might be good to comment this out
  # for initial runs.
  x <- suppressWarnings(remove_terms(x, stopwords, c("begin", "middle", "end"), recursive = TRUE))
  
  # Remove roman numerals
  x <- sapply(strsplit(x, " "), function (xi) {paste(xi[!is.roman(suppressWarnings(as.roman(xi)))], collapse = " ")}, USE.NAMES = FALSE)
  
  if (harmonize) {
    
    # If the term is not on synonyme or name list but
    # all subterms are, select the first subterm
    ss <- unique(tolower(c(synonymes$synonyme, synonymes$name)))
    inds <- which(!x %in% ss)
    if (length(inds)>0) {
      spl <- strsplit(x[inds], " ")
      inds2 <- which(sapply(spl, function (xi) {all(xi %in% ss) && length(xi) > 0}, USE.NAMES = FALSE))
      if (length(inds2)>0) {
        x[inds[inds2]] = sapply(spl[inds2], function (xi) {xi[[1]]}, USE.NAMES = FALSE)
      }
    }
    
    # abo abo abo = abo
    x <- sapply(strsplit(x, " "), function (xi) {paste(unique(xi), collapse = " ")})
    
    # Then match place names to synonymes		
    x <- as.character(map(x, synonymes,
                          remove.unknown = remove.unknown,
                          mode = "exact.match"))
    
  }
  
  # Remove too short names 
  x[nchar(gsub(" ", "", x)) <= 2] = NA
  x <- gsub("^[a-z] [a-z]$", " ", x)
  x <- gsub("^[a-z]\\.[a-z]$", " ", x)  
  
  if (length(x) == 0) {return(rep(NA, length(xorig)))}
  
  if (verbose) {message("Replace special cases")}
  x[x %in% c("", " ", "na")] <- NA
  
  if (verbose) {message("Capitalize all names")}    
  x <- capitalize(x)
  
  if (verbose) {message("Convert back to original indices and return")}
  x[match(xorig, xuniq)]
  
}


#' @title Remove Trailing Periods
#' @description Remove trailing periods.
#' @param x A vector
#' @return A polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- remove_trailing_periods(x)}
#' @keywords utilities
remove_trailing_periods <- function (x){ 
  
  if (all(is.na(x))) {return(x)}
  x <- gsub("\\.+$", "", x)
  x <- gsub("^\\.+", "", x)
  
  x
}

#' @title Remove Print Statements
#' @description Remove print statements.
#' @param x a vector
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- remove_print_statements("Printed in London")
#' @keywords utilities
remove_print_statements <- function (x) {
  
  x0 = xorig <- tolower(as.character(x))
  xuniq <- unique(xorig)
  x <- xuniq
  
  terms.single = c()
  terms.multi = c()  
  
  ### Get printing terms from tables in various languages
  for (lang in c("finnish", "french", "german", "swedish", "english")) {
    
    f <- paste0("ENB_workshop/data/printstop_", lang, ".csv")
    terms <- unique(str_trim(tolower(read.csv(f, stringsAsFactors = FALSE)[,1])))
    
    # Harmonize the terms 
    terms.multi <- c(terms.multi, terms[nchar(terms) > 1])
    terms.single <- c(terms.single, terms[nchar(terms) == 1])
    
  }
  
  terms.multi = unique(terms.multi)
  terms.single = unique(terms.single)  
  
  x <- remove_terms(x, terms.multi, where = "all", polish = FALSE, include.lowercase = FALSE)
  x <- condense_spaces(x)
  
  # Back to original indices, then unique again; reduces
  # number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  # Individual characters not removed from the end
  x <- remove_terms(x, terms.single, where = "begin", polish = FALSE, include.lowercase = FALSE)
  
  # Back to original indices, then unique again; reduces
  # number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  x <- remove_terms(x, terms.single, where = "middle", polish = FALSE, include.lowercase = FALSE)
  x <- condense_spaces(x)
  x <- remove_trailing_periods(x)
  
  # Back to original indices, then unique again; reduces
  # number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  # remove sine loco
  f <- "ENB_workshop/data/sl.csv" 
  sineloco <- as.character(read.csv(f)[,1])
  x <- remove_terms(x, sineloco, include.lowercase = TRUE)
  
  # Back to original indices, then unique again; reduces
  # number of unique cases further
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  # handle some odd cases manually
  # FIXME: estc-specific, move there
  # "122 s"; "2 p"
  x[grep("^[0-9]* [s|p]$", x)] <- NA
  x <- gsub("[0-9]\\. p\\.;","",x)
  x <- gsub("^(.*?);.*$","\\1",x) # nb. non-greedy match
  x[x==""] <- NA
  
  x = x[match(xorig, xuniq)]
  
  x
}


#' @title Harmonize ie
#' @description Harmonize ie statements.
#' @param x A vector
#' @param separator The separator string (i.e by default)
#' @return A vector polished
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples \dontrun{x2 <- harmonize_ie("i.e.")}
#' @keywords utilities
harmonize_ie <- function (x, separator = "i.e") {
  
  # Harmonized form
  h <- " i.e "
  
  x <- tolower(as.character(x))
  x <- condense_spaces(x)
  x <- gsub("-+", "-", x)
  
  x <- gsub("\\[oik[\\.]*", "[i.e ", x)
  x <- gsub("p\\.o\\.", " i.e ", x) # Finnish: pitaisi olla
  x <- gsub("p\\.o ", " i.e ", x) # Finnish: pitaisi olla  
  x <- gsub("\\[po[\\.]*", " i.e ", x) # Finnish: pitaisi olla  
  
  x <- gsub(" ie ", " i.e ", x)  
  x <- gsub("\\[ie ", "[i.e ", x)
  x <- gsub("\\[i\\. *e\\.* ", "[i.e ", x)
  x <- gsub("\\[i *e\\.* ", "[i.e ", x)
  x <- gsub("\\[i\\.e\\.* ", "[i.e ", x)      
  x <- gsub("^ie ", "i.e ", x)
  x <- gsub("i\\.e\\.*, ", "i.e ", x)      
  
  x <- gsub("\\,* +i\\.* *e+ *[\\.|\\,]*", h, x)
  
  x <- gsub("\\[* +i\\.* *e+ *[\\.|\\,]*", h, x)
  
  x <- gsub("^\\,* *i\\.* *e+ *[\\.|\\,]*", h, x)
  
  x <- gsub(" +i\\.* *e+ *[\\.|\\,]*", h, x)
  
  x <- gsub("p\\. i\\.* *e+ *[\\.|\\,]*", h, x) 
  x <- gsub("^p\\. i\\.* *e+ *[\\.|\\,]*", h, x)
  x <- gsub("\\[ *", "\\[", x)
  x <- gsub("^\\. *", "", x)
  
  x <- condense_spaces(x)
  
  x
  
}



polish_place_help <- function (x, s, verbose = FALSE) {
  
  # London i.e. The Hague ->  The Hague
  # In the Yorke at London -> London
  # TODO use handle_ie function here or make an improved version
  x <- splitpick(x, " i.e ", 2)
  x <- splitpick(x, " at ", 2)  
  
  # NOTE: this step may loose info on original country
  # london re/now edinburgh -> london
  x <- splitpick(x, " re ", 1)
  
  # This may loose country info so skip for now
  #x <- splitpick(x, " now ", 1) # Should we pick latter here instead ?  
  
  # New York N Y -> New York NY
  if (length(grep(" [a-z] [a-z]$", x))>0) {
    n <- nchar(x)
    x <- paste(substr(x, 1, n-2), substr(x, n, n), sep = "")
  }
  
  # Remove - may loose considerable information ?
  # However looks very useful in practice
  # If the term is not on synonyme list but all its subterms are,
  # then select the first term
  # ie "zurich cologne" becomes "zurich"  
  if (!is.na(x) && any(!is.na(s)) && !(x %in% na.omit(s))) {
    spl <- unlist(strsplit(x, " "), use.names = FALSE)
    inds <- which(!is.na(match(spl, c(s, "new"))))
    
    if (length(inds) > 0) {
      # Keep only those terms that are on synonyme list
      # and exception terms "new"
      x <- paste(unique(spl[inds]), collapse = " ")
    }
  }
  
  
  
  return(x)
  
}

#' @title Pick Substring Indicated by Separator
#' @description Split the string by separator and pick the former or latter part.
#' @param x Input string
#' @param sep Separator string
#' @param which Indicate which part to pick
#' @return Polished string
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- splitpick("London re Edinburgh", " re ", 1)
#' @keywords utilities
splitpick <- function (x, sep, which) {
  # london re edinburgh -> london
  inds <- grep(sep, x)
  if (length(inds) > 0) {
    spl <- unlist(strsplit(x[inds], sep), use.names = FALSE)
    x[inds] <- spl[[which]]
  }
  
  x
  
}




is.roman <- function (x) {
  
  x <- gsub("\\.", NA, x)
  
  check.roman <- function (x) {
    
    if (x == "" || is.na(x)) {return(FALSE)}
    
    xs <- unlist(strsplit(x, "-"), use.names = FALSE)
    isr <- c()
    
    for (i in 1:length(xs)) {  
      x <- xs[[i]]
      tmp <- suppressWarnings(as.numeric(x))
      tmp2 <- suppressWarnings(as.numeric(as.roman(x)))
      not.numeric <- length(na.omit(tmp)) > 0
      roman.numeric <- is.numeric(tmp2)
      
      isr[[i]] <- !(not.numeric && roman.numeric) && !is.na(tmp2) 
    }
    # iii-7 TRUE; iii-iv TRUE; 4-7 FALSE
    any(isr)
  }
  
  sapply(x, check.roman, USE.NAMES = FALSE)
  
}




roman2arabic <- function (x) {
  
  if (length(grep("vj", x)) == 1) {
    x <- gsub("j", "i", x)
  }
  
  helpf <- function(xi) {
    
    # Return numeric hits immediately (such as "4387")
    if (grepl("^[0-9]+$", xi)) {
      xr <- xi
    } else if (length(grep("-", xi)) > 0) {
      x2 <- str_trim(unlist(strsplit(xi, "-"), use.names = FALSE))
      n <- suppressWarnings(as.numeric(as.roman(x2)))
      n[is.na(n)] <- x2[is.na(n)] # vii-160
      xr <- paste(n, collapse = "-")
    } else {
      xr <- suppressWarnings(as.numeric(as.roman(xi)))
    }
    xr
  }
  
  sapply(x, function (xi) {helpf(xi)}, USE.NAMES = FALSE)
  
}









#' @title Capitalize strings
#' @description Capitalize the first letter.
#' @param x a character vector to capitalize
#' @param format Capitalization format: "first.letter" (first letter
#'   capitalized) or "all.words" (all words capitalized)
#' @return Capitalized character vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples capitalize("print")
#' @keywords utilities
capitalize <- function (x, format = "first.letter") {
  
  # Speed up by considering unique instances only
  xorig <- x
  x <- xuniq <- unique(xorig)
  
  if (format == "first.letter") {
    for (a in letters) {
      x <- gsub(paste0("^", a), toupper(a), x)
    }
  } else if (format == "all.words") {
    x <- sapply(strsplit(x, " "), function (x) {paste(capitalize(x, format = "first.letter"), collapse = " ")}, USE.NAMES = FALSE)
  }
  
  x[match(xorig, xuniq)]
  
}

#' @title Get Country
#' @description Map geographic places to country names.
#' @param x A vector of region names (cities or municipalities etc.)
#' @param map data.frame with region to country mappings (fields 'region' and 'country')
#' @return Country vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("bibliographica")
#' @examples x2 <- get_country("Berlin")
#' @keywords utilities
get_country <- function (x, map = NULL) {
  
  # TODO could add here some more countries from geonames
  # We could standardize country names but problem is that e.g.
  # England, Scotland
  # etc are not mapped (as UK). But is potentially useful later.
  # devtools::install_github("dsself/standardizecountries")
  # library(standard)
  # df.preprocessed$publication_country2 <- country_name(df.preprocessed$publication_country)
  # df.preprocessed$publication_country.code <- country_code(df.preprocessed$publication_country, "country", "iso3c")
  
  # Speed up by handling unique cases only
  xorig <- as.character(x)
  xorig.unique <- unique(xorig)  
  x <- xorig.unique
  
  if (is.null(map)) {
    f <- "ENB_workshop/data/reg2country.csv"
    message(paste("Reading region-country mappings from file ", f))
    map <- read_mapping(f, mode = "table", sep = ";", sort = TRUE, self.match = FALSE, include.lowercase = FALSE, ignore.empty = FALSE, remove.ambiguous = TRUE, lowercase.only = FALSE, from = "region", to = "country") 
  }
  
  message("Map each region in x to a country")
  # use lowercase
  # country <- map$country[match(tolower(x), tolower(map$region))]
  spl <- split(tolower(map$country), tolower(map$region)) 
  spl <- spl[tolower(x)]
  
  # If mapping is ambiguous, then name the country as ambiguous
  spl <- lapply(spl, unique)
  spl[which(sapply(spl, function (x) {length(unique(x)) > 1}, USE.NAMES = FALSE))] <- "ambiguous"
  spl[which(sapply(spl, function (x) {length(x) == 0}, USE.NAMES = FALSE))] <- NA  
  spl <- unlist(as.vector(spl))
  country <- spl
  
  # If multiple possible countries listed and separated by |;
  # use the first one (most likely)
  country <- str_trim(sapply(strsplit(as.character(country), "\\|"), function (x) {ifelse(length(x) > 0, x[[1]], NA)}, USE.NAMES = FALSE))
  
  # Use the final country names
  country <- map$country[match(tolower(country), tolower(map$country))]
  
  # The function was sped up by operating with unique terms
  country[match(xorig, xorig.unique)]
  
}


#' @title Polish Years
#' FIXME: UNIT TESTS MISSING NOW!
#' @description Pick and polish the year interval (start and end years) from a time field which is of the form 1800 or 1823-1845 etc.
#' @param x year field (a vector) 
#' @param start_synonyms Synonyme table for start year
#' @param end_synonyms Synonyme table for end year
#' @param verbose verbose
#' @param min.year Minimum year accepted
#' @param max.year Maximum year accepted
#' @param check If true, remove entries (replace by NA) where start > end
#' @return data.frame with the fields 'start' and 'end'
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @examples \dontrun{df <- polish_years(c("1746", "1745-1750"))}
#' @keywords utilities
polish_years <- function(x, start_synonyms=NULL, end_synonyms=NULL, verbose = TRUE, check = FALSE, min.year = -3000, max.year = as.numeric(format(Sys.time(), "%Y")) + 50) {
  
  # Delete suspiciously long strings
  inds <- which(nchar(x) > 200 & grepl("\t", x))
  if (length(inds) > 0) {
    x[inds] <- NA
    warning(paste("Removed ", length(inds), "publication year entries that are suspiciously long (over 200 characters) and include tabs"))
  }
  
  x <- gsub("\\.$", "", x)
  x <- gsub("\\[blank\\]", "?", x)
  x <- gsub("\\[sic\\.\\]", "", x)    
  x <- gsub("^&lt;", "", x)
  x <- gsub("&gt;$", "", x)
  x <- gsub("&gt;-$", "", x)
  x <- gsub(", *\\[*[0-9]\\]*$", "", x)
  x <- gsub("\\[̂", "[", x)
  #x <- gsub("\\?\\]", "]", x)
  #x <- gsub("\\[[0-9]{3}-\\]", "", x)
  x <- gsub("(^|[-[])[0-9]{3}[?]", "\\1", x)
  x <- gsub("(^|[-[])[0-9]{2}[?][?]", "\\1", x)
  x <- gsub("I([0-9]{3})([^0-9]|$)", "1\\1", x)
  # 1974 [p.o. 1976] -> 1976
  x <- gsub("[0-9]{4} ?[[]p[.]?o[.]? ?([0-9]{4})[]]", "\\1", x)
  x <- gsub("[\\^]", "", x)
  x <- gsub("[\\|]", "", x)
  x <- gsub("[u]", "", x)
  
  inds <- grep("\\[*[M,C,D,X,L,\\.]*\\]*", x)
  if (length(inds) > 0) {
    x[inds] <- sapply(x[inds], function (x) {gsub("\\.", "", x)})
  }
  
  inds <- intersect(grep("^--", x), grep("--$", x))
  x[inds] <- gsub("--$", "", gsub("^--", "", x[inds]))
  
  inds <- grep("^\\[*l[0-9]{3}\\.*\\]*$", x)
  if (length(inds) > 0) {
    x[inds] <- sapply(x[inds], function (x) {gsub("l", "1", x)})
  }
  
  f <- "ENB_workshop/data/months.csv"
  months <- as.character(read.csv(f, header = TRUE)[,1])
  months <- unique(c(months, tolower(months)))
  # Handle from longest to shortest to avoid problems
  months <- months[rev(order(nchar(months)))]
  
  # These seem unnecessary assignments.
  xorig <- tolower(as.character(x))
  xuniq <- unique(xorig)
  x <- xuniq
  
  if (verbose) {
    message(paste("Polishing years:", length(xuniq), "unique cases"))
  }
  
  # "[1.12.1584 jalkeen]" -> 1584 jalkeen
  inds <- grep("[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}", x)
  x[inds] <- gsub("[0-9]{1,2}\\.[0-9]{1,2}\\.", "", x[inds])
  x <- gsub(",", ".", x)
  
  # 23.1967 -> 1967 excluding 1.5.6.7
  if (length(grep("[0-9]\\.[0-9]\\.[0-9]\\.[0-9]", x)) == 0) {
    x <- gsub(" [0-9]{1,2}\\.", "", x)
    x <- gsub("\\.[0-9]$", "", x)
  } else {
    x <- gsub("\\.", "", x)
  }
  
  # "Printed in the Yeare,;1648."
  inds <- grep(";", x)
  x[inds] <- unlist(sapply(x[inds], function (x) {x <- unlist(strsplit(x, ";")); paste(x[grep("[0-9]", x)], collapse = ", ")}), use.names = FALSE)
  x <- gsub("\\.", " ", x)
  x <- gsub(" or later", " ", x)  
  x <- gsub("0[0-9]*;m[a-z]*terio [a-z]*\\.* pauli", " ", x)
  
  # 18th century (remove separately before removing other letters)
  x <- gsub("[0-9]{1,4}th", "", x)  
  
  # Map back to original indices and make unique again. To speedup further.
  xorig <- x[match(xorig, xuniq)]
  
  x <- xuniq <- unique(xorig)
  x <- harmonize_ie(x)
  
  x <- gsub("-a", "- a", x) # -approximately
  x <- remove_print_statements(x)
  
  # Map back to original indices and make unique again. To speedup further.
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  x <- sapply(x, function (xi) {handle_ie(xi, harmonize = FALSE)}, USE.NAMES = FALSE)
  x <- condense_spaces(gsub("\\.", " ", x))
  
  x <- remove_time_info(x, verbose = F, months)
  
  # Map back to original indices and make unique again. To speedup further.
  xorig <- x[match(xorig, xuniq)]
  x <- xuniq <- unique(xorig)
  
  # 1642 [1643] -> 1643
  if (length(grep("^[0-9]* \\[[0-9]*\\]$", x)) > 0) {
    inds <- grep("^[0-9]* \\[[0-9]*\\]$", x)
    for (i in inds) {
      spl <- unlist(strsplit(x[[i]], " "))      
      if (length(spl) > 1) {
        x[[i]] <- spl[[2]]
      } else {
        x[[i]] <- spl[[1]]
      }
    }
  }
  
  
  
  # 1642[1643] -> 1643
  if (length(grep("^[0-9]{4}\\[[0-9]{4}\\]$", x)) > 0) {
    inds <- grep("^[0-9]{4}\\[[0-9]{4}\\]$", x)
    for (i in inds) {
      x[[i]] <- substr(x[[i]], 6, 9)
    }
  }
  
  # 1642[1643]-45 -> 1643-1645
  if (length(grep("^[0-9]{4}\\[[0-9]{4}\\]-[0-9]{2}$", x)) > 0) {
    inds <- grep("^[0-9]{4}\\[[0-9]{4}\\]-[0-9]{2}$", x)
    for (i in inds) {
      spl <- strsplit(x[[i]], "-")[[1]]
      start <- pick_year(spl[[1]])
      end <- paste0(substr(spl[[1]], 1, 2), spl[[2]])
      x[[i]] <- paste(start, end, sep = "-")
    }
  }
  
  # 1642[3] -> 1643
  inds <- grep("^[0-9]{4}\\[[0-9]{1}\\]$", x)
  if (length(inds) > 0) {
    for (i in inds) {
      x[[i]] <- paste0(substr(x[[i]], 1, 3), substr(x[[i]], 6, 6))
    }  
  }
  
  clean2 <- function (x) {
    
    x <- strsplit(x, "\\]-\\[")[[1]]
    
    start <- polish_year(x[[1]])[[1]]
    
    end <- NA
    if (length(x) > 1) {
      end <- polish_year(x[[2]])
      if (length(end) > 0) {end <- end[[1]]}
    }
    
    if (!is.na(start) & !is.na(end) & end < start) {
      end <- ""
    }
    
    x <- paste(start, end, "-")          
    
  } 
  
  
  # "[1900?]-[190-?]"
  inds <- grep("\\]-\\[", x)
  if (length(inds) > 0) {
    x[inds] <- sapply(x[inds], function (xx) {clean2(xx)})
  }
  
  # "[1900?]-190-?"
  clean1 <- function (x) {
    x <- strsplit(x, "\\]-")[[1]]
    
    start <- polish_year(x[[1]])
    
    end <- ""
    if (length(x) > 1) {
      end <- polish_year(x[[2]])
    }
    
    x <- paste(start, end, "-")    
  }
  
  inds <- grep("\\]-[?]*", x)
  if (length(inds) > 0) {
    x[inds] <- sapply(x[inds], function (xx) {clean1(xx)})
  }
  
  # 1966 [=1971]  -> 1971
  x <- gsub("[0-9]{4}  ?[[][=]([0-9]{4})[]]", "\\1", x)
  
  # Remove some other info
  x <- gsub("price [0-9] d", "", x)
  x <- gsub("-[0-9]{2,3}\\?{1,2}$", "", x)
  x <- gsub("\\?", "", x)
  x <- gsub("\\!", " ", x)  
  x <- gsub("^& ", "", x)
  
  x <- condense_spaces(gsub("\\[\\]", " ", x))
  x <- gsub(" -", "-", gsub("- ", "-", x))
  x <- gsub("-+", "-", x)
  x <- gsub("1̂", "1", x)
  
  x <- gsub("\\[[a-z| ]*\\]", "", x)
  
  x <- gsub("[[]", "", x)
  x <- gsub("[]]", "", x)
  x <- harmonize_christian(x)
  
  inds <- grep(" or ", x)
  if (length(inds)>0) {
    x[inds] <- sapply(x[inds], function (x) unlist(strsplit(x, " or "), use.names = FALSE)[[2]])
  }
  x[inds] <- str_trim(x[inds])
  
  # Convert romans
  x <- gsub("m\\.d", "md", x)
  x <- gsub("m\\.d\\.", "md", x)
  x <- gsub("m d", "md", x)
  x <- gsub("m d ", "md", x)  
  x <- gsub("m\\,d", "md", x)
  x <- gsub("m\\,d\\,", "md", x)
  x <- gsub("md x", "mdx", x)
  x <- gsub("md l", "mdl", x)  
  x <- gsub("ij$", "ii", x)
  
  num <- suppressWarnings(as.numeric(as.roman(gsub(" ", "", x))))
  inds <- which(!is.na(num))
  if (length(inds) > 0) {
    x[inds] <- num[inds]
  }
  
  # Map back to original indices and make unique again. To speedup further.
  xorig <- x[match(xorig, xuniq)]
  xuniq <- unique(xorig)
  x <- xuniq
  
  if (length(grep("[0-9]{4}\\[[0-9]", x))>1) {
    x <- substr(x, 1, 4)
  }
  
  res <- suppressWarnings(
    lapply(x,
           function (xi) {
             a <- try(polish_year(xi,
                                  start_synonyms = start_synonyms,
                                  end_synonyms = end_synonyms,
                                  months,
                                  verbose)
             )
             
             if (class(a) == "try-error") {
               return(c(NA, NA))
             } else {
               return(a)
             }
           }
    )
  )
  
  res <- do.call("rbind", res)
  start_year <- res[,1]
  end_year   <- res[,2]
  
  if (check) {
    inds <- which(start_year > end_year)
    if (length(inds) > 0) {
      start_year[inds] <- NA
      end_year[inds] <- NA      
    }
  }
  
  start_year[is.infinite(start_year)] <- NA
  end_year[is.infinite(end_year)] <- NA  
  
  # Do not accept years below and above these values	    
  start_year[start_year < min.year | start_year > max.year] <- NA
  end_year[end_year < min.year | end_year > max.year] <- NA  
  
  df <- data.frame(from = as.numeric(as.character(start_year)),
                   till = as.numeric(as.character(end_year)))
  
  message(paste("Checking that the years are within the accepted range:", min.year, "-", max.year))
  # Manually checked for Fennica - 3 publications before 1400;
  # FIXME make more explicit in the final reports; maybe with a separate enrich function as earlier
  # in all cases it seems that this is misspelling and the original year cant be inferred from the entry
  df$from[which(df$from > max.year)] <- NA
  df$from[which(df$from < min.year)] <- NA
  df$till[which(df$till > max.year)] <- NA
  df$till[which(df$year < min.year)] <- NA
  
  # Match the unique cases to the original indices
  # before returning the df
  return(df[match(xorig, xuniq), c("from", "till")])
  
}

#' @title Handle ie
#' @description Handle ie statements.
#' @param x Character vector
#' @param harmonize Logical. Harmonize ie statements efore interpretation?
#' @param separator The separator string (i.e by default)
#' @return A vector polished
#' @importFrom stringr str_sub
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("fennica")
#' @examples \dontrun{x2 <- handle_ie("i.e.")}
#' @export
#' @keywords utilities
handle_ie <- function (x, harmonize = TRUE, separator = "i.e") {
  
  # 183 i.e 297 -> 297	  
  # 183-285 i.e 297 -> 183-297	  
  # 183-285 [i.e 297] -> 183-297
  # 183-285 i.e 297-299 -> 297-299
  
  y <- x
  
  if (harmonize) {
    y <- x <- harmonize_ie(y, separator = separator)
  }
  x <- condense_spaces(x)
  
  if (length(x) == 1 && (is.na(x) || x == separator)) {return(x)}
  
  # z [i.e y] -> y
  if (length(grep("[0-9|a-z]*\\.* \\[i\\.e [0-9|a-z]*\\]", x))>0) {
    
    # This is for "1905 [ie. 15]" -> 1915
    spl <- strsplit(x, " \\[i\\.e ")
    x1 <- sapply(spl, function (x) {x[[1]]})
    x2 <- gsub("\\]", "", sapply(spl, function (x) {x[[2]]}))
    inds <- which(nchar(x1) == 4 & nchar(x2) == 2)
    x[inds] <- sapply(inds, function (ind) {paste(substr(x1[[ind]], 1, 2), x2[[ind]], sep = "")})
    
    x <- gsub("^[0-9|a-z]*\\.* \\[i\\.e", "", x)
    
    x <- gsub("\\]$", "", x)
    
  }
  
  
  # "[1-3] 4-43 [44-45] 45-51 [i.e 46-52]"
  # keep the first part and just remove "45-51 ie"
  if (length(grep("[0-9]+-[0-9]+ i\\.e [0-9]+-[0-9]+", x)) == 1) {
    spl <- unlist(strsplit(x, " "), use.names = FALSE)    
    rmind <- which(spl == "i.e")
    rmind <- (rmind-1):rmind
    x <- paste(spl[-rmind], collapse = " ")
  }
  
  # " p 113-111 i.e 128] " -> 113-128
  if (length(grep("-[0-9]+ i\\.e [0-9]+", x)) == 1) {
    spl <- unlist(strsplit(x, "-"), use.names = FALSE)
    spl <- sapply(spl, function (spli) {handle_ie(spli)})
    x <- paste(spl, collapse = "-")
  }
  
  if (length(grep("-", x)) > 0 && length(grep("i\\.e", x)) > 0) {
    
    spl <- unlist(strsplit(x, "i\\.e"), use.names = FALSE)
    
    if (length(grep("-", spl)) == 2) {
      # 1-3 ie 2-4 -> 2-4
      x <- spl[[2]]
    } else {
      
      # [1658]-1659 [i.e. 1660] -> 1658-1660
      spl <- unlist(strsplit(x, "-"), use.names = FALSE)
      u <- sapply(spl, function (s) {handle_ie(s)}, USE.NAMES = FALSE)
      x <- paste(u, collapse = "-")
    }
    
  } else if (length(grep("\\[[0-9|a-z]* *i\\.e [0-9|a-z]*\\]", x))>0) {
    
    # z [x i.e y] -> z [y]  
    x <- unlist(strsplit(x, "\\["), use.names = FALSE)
    inds <- grep("i\\.e", x)
    u <- unlist(strsplit(x[inds], "i\\.e"), use.names = FALSE)
    x[inds] <- u[[min(2, length(u))]]
    x <- paste(x, collapse = "[")
    
  } else if (length(grep(" i\\.e ", x))>0) {
    
    # x i.e y -> y
    x <- unlist(strsplit(x, "i\\.e"), use.names = FALSE)
    x <- x[[min(2, length(x))]]
    
  } else if (length(grep("\\[i\\.e", x))>0) {
    
    # x [i.e y] -> y
    x <- unlist(strsplit(x, "\\[i\\.e"), use.names = FALSE)
    x <- x[[min(2, length(x))]]
    x <- gsub("\\]*$", "", x)
    
  } else if (length(grep("\\[[0-9|a-z]* i\\.e [0-9|a-z]*\\]", x))>0) {
    # "mdcxli [1641 i.e 1642]" -> mdcxli [1642]
    x <- unlist(strsplit(x, "\\["), use.names = FALSE)
    inds <- grep("i\\.e", x)    
    x[inds] <- handle_ie(x[inds])
    x <- paste(x, collapse = "[")
    
  }
  
  x <- gsub("\\[ ", "[", x)
  x <- gsub("^\\.*", "", x)  
  x <- str_trim(x)
  
  x
}

#' @title Remove Time Info
#' @description Remove time information.
#' @param x Vector (time field)
#' @param verbose verbose
#' @param months months to remove
#' @return Polished vector
#' @export
#' @details Remove months, year terms and numerics
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("fennica")
#' @examples \dontrun{x2 <- remove_time_info(x)}
#' @keywords utilities
remove_time_info <- function (x, verbose = FALSE, months = NULL) {
  
  if (is.null(months)) {
    f <- system.file("extdata/months.csv", package = "fennica")
    months <- as.character(read.csv(f, header = TRUE)[,1])
    months <- unique(c(months, tolower(months)))
    
    # Handle from longest to shortest to avoid problems
    months <- months[rev(order(nchar(months)))]
  }
  
  # 17th century 
  x <- condense_spaces(gsub("[0-9]*th century", " ", x))
  
  # July 8
  for (month in months) {
    
    # "march-1777"
    s <- paste0(month, "-")
    if (verbose) { message(paste("Removing", s)) }
    x <- gsub(s, " ", x)    
    
    # "17 or 18 February"
    s <- paste("[0-9]{1,2} or [0-9]{1,2} ", month, sep = "")
    if (verbose) {message(paste("Removing", s))}    
    x <- gsub(s, " ", x)    
    
    # " 17 February"
    s <- paste(" [0-9]{1,2} ", month, sep = "")
    if (verbose) {message(paste("Removing", s))}    
    x <- gsub(s, " ", x)
    
    # "[17 February"
    s <- paste("\\[[0-9]{1,2} ", month, sep = "")
    if (verbose) {message(paste("Removing", s))}    
    x <- gsub(s, " ", x)
    
    # "^17 February"
    s <- paste("^[0-9]{1,2} ", month, sep = "")
    if (verbose) {message(paste("Removing", s))}    
    x <- gsub(s, " ", x)
    
    s <- paste(month, " [0-9]{1,2} ", sep = "")
    s2 <- paste(month, " [0-9]{1,2}\\]", sep = "")    
    s3 <- paste(month, " [0-9]{1,2}$", sep = "")
    s4 <- paste(month, " [0-9]{1,2}\\,", sep = "")
    s5 <- paste(month, "\\, [0-9]{1,2}", sep = "")            
    if (verbose) {message(paste("Removing", s))}
    x <- gsub(s, " ", x)
    if (verbose) {message(paste("Removing", s2))}    
    x <- gsub(s2, " ", x)
    if (verbose) {message(paste("Removing", s3))}    
    x <- gsub(s3, " ", x)
    if (verbose) {message(paste("Removing", s4))}    
    x <- gsub(s4, " ", x)
    if (verbose) {message(paste("Removing", s5))}    
    x <- gsub(s5, " ", x)                
    
  }
  
  # other time information
  terms <- c("Anno\\.", "An\\. Do\\.", "year", "anno dom", "anno")
  toremove <- c(months, terms)
  
  x <- remove_terms(x, toremove)
  
  x
  
}

christian2numeric <- function (x) {
  
  inds <- grep("a.d", x)
  if (length(inds) > 0) {
    x[inds] <- as.numeric(str_trim(gsub("a.d", "", x[inds])))
  }
  
  inds <- grep("b.c", x)
  if (length(inds) > 0) {
    x[inds] <- -as.numeric(str_trim(gsub("b.c", "", x[inds])))
  }
  
  return(x)
}

harmonize_christian <- function (x) {
  
  x <- str_trim(as.character(x))
  
  #x <- gsub("anno dom.", "A.D", x)
  #x <- gsub("an. dom.", "A.D", x)  
  #x <- gsub("anno domini", "A.D", x)    
  #x <- gsub("a.d.", "A.D", x)
  #x <- gsub("A\\.D", "", x) # redundant
  
  x <- gsub("anno dom\\.", " ", x)
  x <- gsub("an\\. dom\\.", " ", x)  
  x <- gsub("anno domini", " ", x)    
  x <- gsub("a\\.d\\.", " ", x)
  x <- gsub("ad", " ", x)  
  x <- gsub("A\\.D", " ", x) # redundant
  x <- gsub("anno d⁻ni", " ", x)
  x <- gsub("anno dñi", " ", x)
  x <- gsub("domini", " ", x)   
  
  x <- gsub("bc", "B.C", x)
  x <- gsub("b\\.c\\.", "B.C", x)  
  x <- gsub("b\\.c\\.", "before christian era", x)  
  x <- gsub("before christian era", "B.C", x)
  
  # Remove space
  x <- gsub(" B\\.C", "B.C", x)
  x <- condense_spaces(x)
  
  x
}

polish_year <- function(x, start_synonyms = NULL, end_synonyms = NULL, months, verbose = FALSE, min.year = -3000, max.year = 2100) {
  
  x <- gsub("^\\[", "", x)
  x <- gsub("\\]$", "", x)
  x <- gsub("\\?$", "", x)
  
  # x <- gsub("[0-9]{1,2} [a-z+]", "", x)
  if (length(grep("^[a-z|=]*-*[a-z|=]+[0-9]*", x)) > 0) {
    x <- gsub("^[a-z|-|=]*-*[a-z|-|=]+", "", x)
    x <- gsub("[a-z|=]+", "", x)
    x <- gsub("- +", "", x)     
  }
  
  # Some quick returns for simple cases to speed up
  if (length(grep("^[0-9]{4}$", x)) > 0) {
    
    # 1900
    start <- x # gsub("^([0-9]+)$", "\\1", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))
  } else if (length(grep("^[0-9]{3}-$", x)) > 0) {
    
    # 190-
    start <- gsub("-", "0", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))    
    
  } else if (length(grep("^[0-9]{4}\\?$", x)) > 0) {
    
    # 1900?
    start <- gsub("\\?", "", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))
    
  } else if (length(grep("^[0-9]{3}-\\?$", x)) > 0) {
    
    # 190-?
    start <- gsub("-\\?", "0", x)
    
    end <- NA
    return (c(from=as.numeric(start), till=end))
    
  } else if (length(grep("^[0-9]{2}-$", x)) > 0) {
    
    # 19-
    start <- paste0(substr(x, 1, 2), "00")
    end <- NA
    return (c(from=as.numeric(start), till=end))
    
  } else if (is.na(x)) {
    return(c(NA, NA))
  } else if (length(grep("^[0-9]{1,3}[.]?$", x)) >  0) {
    return(c(NA, NA))
  } else if (length(grep("^[0-9]{5}[.]?$", x)) >  0) {
    return(c(NA, NA))
  } else if (length(grep("^[0-9]{4}[-]$", x)) > 0) {
    # 1994-
    start <- gsub("^([0-9]{4})-", "\\1", x)
    end <- NA
    return(c(start, end))
  } else if ((x == "-") | (x == "[-]")) {
    # - ; [-]
    return(c(NA, NA))
  } else if (length(grep("^[0-9]+[-|,]+[0-9]+[-|,]+[0-9]+[-|,][0-9]+", gsub(" ", "", x))) > 0) {
    # "1921-1922;1921-1922;1922"
    x <- condense_spaces(gsub(",", " ", gsub("-", " ", x)))
    x <- unlist(strsplit(x, " "), use.names = FALSE)
    x <- na.omit(as.numeric(x))
    start <- min(x)
    end <- max(x)
  } else if (x=="fin") {
    return(c(NA, NA))
  } else if (length(grep("^[0-9]+[-|,]+[0-9]+[-|,]+[0-9]", gsub(" ", "", x))) > 0) {
    # "1921-1922;1921
    x <- condense_spaces(gsub(",", " ", gsub("-", " ", x)))
    x <- unlist(strsplit(x, " "), use.names = FALSE)
    x <- na.omit(as.numeric(x))
    start <- min(x)
    end <- max(x)
  } else if (x=="NA-NA" | x=="NA-" | x=="-NA" | x=="-") {
    return(c(NA, NA))
  } else if (length(grep("^[0-9]{4}$", gsub("\\]", "", gsub("\\[", "", x)))) > 0) {
    # 190[1]] / 19[83]
    x <- gsub("\\[", "", x)
    x <- gsub("\\]", "", x)    
    start <- x
    end <- NA
    return (c(from=as.numeric(start), till=end))    
  } else if (length(grep("^\\[*[0-9]{4}\\]*[-]\\[*[0-9]{4}\\]*$", x)) > 0) {
    # 1900-1910 / [1929]-[1930]
    start <- gsub("^\\[*([0-9]{4}).*\\]*", "\\1", x)
    end <- gsub("\\[*.*([0-9]{4})\\]*$", "\\1", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))
  } else if (length(grep("^\\[*[0-9]{4}\\]*[-]\\]* *\\[*[0-9]{4}\\]*$", x)) > 0) {
    # "[1904-] 1905"
    start <- gsub("^\\[*([0-9]{4}).*", "\\1", x)
    end <- gsub(".*([0-9]{4})$", "\\1", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))
    
  } else if (length(grep("^[0-9]{4}-[0-9]{4}", gsub("\\]", "", gsub("\\[", "", x)))) > 0) {
    # "18[35-18]42"
    x <- gsub("\\]", "", gsub("\\[", "", x))
    spl <- unlist(strsplit(x, "-"), use.names = FALSE)
    start <- spl[[1]]
    end <- spl[[2]]
    
    return (c(from=as.numeric(start), till=as.numeric(end)))
  } else if (length(grep("^[0-9]{1,4}B\\.C-[0-9]{1,4}B\\.C$", x)) > 0) {
    # 30bc-26bc
    tmp <- as.numeric(gsub("B\\.C", "", unlist(strsplit(x, "-"))))
    start <- -tmp[[1]]
    end <- -tmp[[2]]
    return (c(from=as.numeric(start), till=as.numeric(end)))
  } else if (length(grep("^[0-9]{1,4}B\\.C-[0-9]{1,4}$", x)) > 0) {
    # 30bc-26
    tmp <- as.numeric(gsub("B\\.C", "", unlist(strsplit(x, "-"))))
    start <- -tmp[[1]]
    end <- tmp[[2]]
    return (c(from=as.numeric(start), till=as.numeric(end)))        
  } else if (length(grep("^[0-9]{1,4}B\\.C$", x)) > 0) {
    start <- -as.numeric(gsub("B\\.C", "", x))
    end <- NA
    return (c(from=as.numeric(start), till=as.numeric(end)))    
  } else if (length(grep("^[0-9| ]+$", x))>0) {
    n <- as.numeric(unlist(strsplit(condense_spaces(x), " ")))
    start <- min(n)
    end <- max(n)
    return (c(from=as.numeric(start), till=end))    
  } else if (length(grep("^(NA)?[-][0-9]{4}$", x)) > 0) {
    # NA-1910; -1910
    start <- NA
    end <- gsub("(NA)?[-](.*)", "\\2", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))    
  }
  
  # More complex cases..
  # "mdccx-mdccxi [1710-1711]" -> [1710-1711]
  if (length(grep("[[:lower:]]*-[[:lower:]]* \\[[0-9]*-[0-9]*\\]", x))>0) {
    x <- remove_letters(x)
  } else if (length(grep("^[0-9]{4} *\\[[0-9]\\]$", x))>0) {
    # "1646[7]" -> 1647
    x <- gsub(" \\[", "\\[", x)
    x <- paste(substr(x, 1, 3), substr(x, 6, 6), sep = "")
  } else if (length(grep("^[0-9]{4}-\\[[0-9]*\\]", x))>0) {
    # "1646-[47]" -> 1646-47
    x <- gsub("\\[", "", gsub("\\]", "", x))
  } else if (length(grep("^[0-9]{4}\\[[0-9]{4}\\]-[0-9]{2}", x))>0) {
    # "1646[1647]-50" -> 1647-50
    x <- gsub("\\]", "", substr(x, 6, nchar(x)))
  } else if (length(grep("^[0-9]{4}, [0-9]\\:o$", x))>0) {
    # "1731, 4:o" -> 1731
    x <- substr(x, 1, 4)
  }  
  
  # Now Remove some special chars
  x <- gsub("\\(\\)", "", x)  
  x <- gsub("-+", "-", x)
  x <- gsub("^[<|>]*", "", x)
  x <- gsub("[<*|>]", "", x)  
  x <- condense_spaces(x)
  x <- gsub("[\\{|\\}]", " ", x)
  x <- gsub("\\\\>", " ", x)
  x <- gsub("\\[=", "[", x)
  x <- gsub("\\[\\]", "-", x)    
  x <- gsub("[[:lower:]]", "", x)
  x <- condense_spaces(x)
  
  if (length(grep("^[0-9]{2}\\[[0-9]{2}\\]$", x))>0) {
    # 19[99]
    x <- gsub("\\[", "", x)
    x <- gsub("\\]", "", x)    
  } else if (length(grep("^[0-9]{3}\\[[0-9]{1}\\]$", x))>0) {
    # 199[9]
    x <- gsub("\\[", "", x)
    x <- gsub("\\]", "", x)    
  } else if (length(grep("^[0-9]{4}/[0-9] \\[[0-9]{4}\\]$", x))>0) {
    # 1695/6 [1696]
    x <- gsub("\\]", "", unlist(strsplit(x, "\\["), use.names = FALSE)[[2]])
  } else if (length(grep("^[-][0-9]{4}$", x)) > 0) {
    x <- gsub("^-", "NA-", x)
  } else if (length(grep("^[0-9]{4} {0,1}\\[[0-9]*\\]$", x))>0) {
    # 1726[1727] -> 1727
    x <- gsub(" \\[", "[", x)
    x <- substr(x, 6, 9)
  } else if (all(!unlist(strsplit(x, "")) %in% letters) && length(grep("\\[[0-9]\\]", x))>0) {
    x <- gsub("\\[", " ", x)
    x <- gsub("\\]", " ", x)
    x <- gsub("\\(", " ", x)
    x <- gsub("\\)", " ", x)    
    x <- gsub("-", " ", x)
    x <- gsub(";", " ", x)
    x <- gsub(",", " ", x)
    x <- gsub("/", " ", x)
    x <- unlist(strsplit(x, " "), use.names = FALSE)
    x <- na.omit(as.numeric(x))
    x <- x[x >= 100] # ignore years below 100
    x <- paste(min(x), max(x), sep = "-")
  }
  
  x <- gsub("\\[[0-9]{2,3}-*\\]", "", x)  
  x <- gsub("-\\]-", "-", x)
  x <- gsub("\\[[0-9]{2}-\\]", "NA", x)
  x <- gsub("\\[[0-9]{2,3}-\\?", "NA", x)
  x <- gsub("\\[[0-9]{2}\\?+", "NA", x)
  x <- gsub("-[0-9]{2}-\\]", "-NA", x)
  x <- gsub("-[0-9]{2}-\\?", "-NA", x)
  x <- gsub("-[0-9]{2}\\?+", "-NA", x)
  x <- gsub("\\[", " ", x)
  x <- gsub("\\]", " ", x)      
  x <- gsub("^\\(", "", gsub("\\)$", "", x))
  x <- gsub(" -", "-", gsub("- ", "-", x))  
  x <- gsub("^-", "NA-", gsub("-$", "-NA", x))
  x <- gsub("[[:lower:]]", "", x)
  x <- condense_spaces(x)
  x <- gsub("^[\\:|\\=]", "", x)
  
  if (length(x) == 1 && (x == "" || is.na(x))) {
    x <- "NA"
  }
  if (length(x) > 1) {
    x <- na.omit(x)
  }
  
  x <- gsub(" *-+ *","-",x)
  
  if (length(grep("\\([0-9]+ */*[0-9]+\\)-[0-9]+ *\\([0-9]{4}/*[0-9]+", x))>0) {
    # "1(1861/1865)-8(1896/1900"
    x <- gsub("\\(", " ", x)
    x <- gsub("\\)", " ", x)
    x <- gsub("/", " ", x)
    x <- gsub("-", " ", x)            
    x <- unlist(strsplit(x, " "), use.names = FALSE)
    x <- na.omit(as.numeric(x))
    x <- x[x >= 100] # ignore years below 100
    x <- paste(min(x), max(x), sep = "-")  
  } else if (length(grep("^[0-9]{4}/[0-9]{4}-[0-9]{4}/[0-9]{4}$", x))>0) {
    # "1885/1886- 1894/1895"
    x <- na.omit(as.numeric(unlist(strsplit(unlist(strsplit(x, "-"), use.names = FALSE), "/"), use.names = FALSE)))
    x <- paste(min(x), max(x), sep = "-")
  } else if (length(grep("NA-[0-9]{4}-[0-9]{4}$", x))>0) {
    # NA-1524-1700
    x <- substr(x, 4, 13)
  } else if (length(grep("NA-[0-9]{4}-NA$", x))>0) {
    # 1736-49
    x <- substr(x, 4, 8)
  } else if (length(grep("[0-9]{4}-[0-9]{1,2}$", x))>0) {
    spl <- unlist(strsplit(x, "-"), use.names = FALSE)
    x <- paste(spl[[1]], paste(substr(spl[[1]], 1, 4-nchar(spl[[2]])), spl[[2]], sep = ""), sep = "-")
  } else if (length(grep("^[0-9]{4}/[0-9]*$", x))>0) {  
    x <- substr(x, 1, 4)
  } else if (length(grep("[0-9]{4} [0-9]*$", x))>0) {
    # 1780 1800 > 1780-1800
    x <- gsub(" ", "-", x)
  }
  
  #x <- unlist(strsplit("3: 1-26 ( 1904/1905 ) , 1: : 1-54 (1905/1906", ""))
  #x <- unlist(strsplit(x, ""))  
  #x <- as.numeric(x)
  #x[is.na(x)] <- "-"
  #x <- condense_spaces(paste(x, collapse = ""))
  #x <- unlist(strsplit(x, "-"), use.names = FALSE)
  #x <- na.omit(as.numeric(x))
  #x <- x[x >= 100] # ignore less than 100
  #x <- paste(min(x), max(x), sep = "-")
  
  # "( ) 25 1643"
  spl <- unlist(strsplit(x, " "), use.names = FALSE)
  spl <- unique(spl[grep("[0-9]{4}", spl)])
  
  if (length(spl) == 1) {
    x <- spl
  }
  
  # 1690, 1690 -> 1690
  if (length(grep("[0-9]{4}, [0-9]{4}$", x))>0) {  
    x <- as.character(min(as.numeric(unique(unlist(strsplit(x, ","), use.names = FALSE)))))
  }
  
  # "1885/1886--1889/1890-1885-1885-2"
  x <- gsub("'", " ", x)
  x <- gsub(",", " ", x)    
  x <- gsub("\\(", " ", x)
  x <- gsub("\\)", " ", x)
  x <- gsub(" -+", "-", x)
  x <- gsub("-+ ", "-", x)
  x <- gsub(":", " ", x)
  x <- gsub("\\.", " ", x)
  x <- condense_spaces(x)
  x <- gsub(" ", "-", x)
  x <- gsub("-+", "-", x)
  
  if (length(grep("^[0-9|/|-]+$", x))>0) {
    n <- unlist(strsplit(unlist(strsplit(x, "-"), use.names = F), "/"), use.names = F)
    n <- na.omit(as.numeric(n))
    n <- n[n>=500] # do not accept years below this one in this special case
    
    # there shouldn't be any of these as they've already been gsubbed to " " above
    # n <- unlist(strsplit(unlist(strsplit(as.character(n), "\\("), use.names = F), "\\)"), use.names = F)
    # n <- na.omit(as.numeric(n))
    
    start <- NA
    if (length(n) > 0) {
      start <- min(n)
    }
    end <- NA
    if (length(n) > 1) {
      end <- max(n)
    }
    return (c(from=as.numeric(start), till=as.numeric(end)))  
  }
  
  if (length(grep("^[0-9]{4}$", x)) > 0) {
    # 1900
    start <- gsub("^([0-9]+)$", "\\1", x)
    end <- NA
    return (c(from=as.numeric(start), till=end))
  } else if (length(grep("^[0-9]{4}[-][0-9]{4}$", x)) > 0) {
    # 1900-1910
    start <- gsub("^([0-9]{4}).*", "\\1", x)
    end <- gsub(".*([0-9]{4})$", "\\1", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))
  }
  
  
  # Proceed to more complex cases
  if (!is.null(start_synonyms)) {
    start <- map(x, start_synonyms)
    start <- as.character(start)
  } else {
    start <- x
  }
  
  if (length(grep("-", x))>0) {
    spl <- unlist(strsplit(as.character(start), "-+"), use.names = FALSE)
    spl <- as.numeric(spl)
    
    # HR 20190225: Do not make this assumption. It's harmful.
    #if (sum(is.na(spl))>1) {
    #  # NA, 3, NA -> 3, NA (assume the year is start year if it is not clear)
    #  spl <- spl[min(which(!is.na(c(NA, 3, NA)))):length(spl)]
    #}
    #x <- spl
    start <- spl[[1]]
    #print(spl)
    if (length(spl) > 1) {
      end <- spl[[2]]
    } else {
      end <- NA
    }
    #x <- paste(x, collapse = "-")
  }
  
  if (length(grep("^(NA)?[-][0-9]{4}$", x)) > 0) {
    # NA-1910; -1910
    start <- NA
    end <- gsub("NA-", "", x)
    return (c(from=as.numeric(start), till=as.numeric(end)))    
    
  } else if (length(grep("^[0-9]{4}[-]NA$", x)) > 0) {
    # 1900-NA
    start <- gsub("-NA", "", x)
    end <- NA
    return (c(from=as.numeric(start), till=as.numeric(end)))        
  } else if (length(grep("\\[[0-9]*\\]", x)) > 0) {
    # MDCCLXVIII. [1768]  
    spl <- unlist(strsplit(x, " "), use.names = FALSE)
    if (length(spl) > 1) {
      spl <- spl[[2]]} else {spl <- spl[[1]]
      }
    start <- gsub("\\[", "", gsub("\\]", "", spl))
    end <- NA    
  } else if (length(grep("^[0-9]*\\.", x)) > 0) {
    # "1798. (price one dollar)"  
    start <- unlist(strsplit(x, "\\."), use.names = FALSE)[[1]]
    end <- NA
  } else if (length(grep("[0-9]+", x)) > 0) {
    # MDCCLXVIII. 1768
    spl <- unlist(strsplit(unlist(strsplit(x, " "), use.names = FALSE), "-"), use.names = FALSE)
    spl <- na.omit(as.numeric(spl))
    spl <- spl[spl > 100] # Do not accept years earlier than this
    #if (length(spl) > 1) {spl <- spl[[2]]} else {spl <- spl[[1]]}
    #start <- spl
    #end <- NA
    start <- min(spl)
    end <- NA
    if (length(spl) > 1) {
      end <- max(spl)
    }
  } else if (is.na(x)) {
    # somewhere along the way some NAs get in, possibly.
    # That produced a stream of error messages. -vv
    end <- NA
  } else if (x == "NA") {
    end <- NA
  }
  
  start[start == ""] <- NA
  start[start == " "] <- NA  
  
  start <- christian2numeric(start) 
  start_year <- as.numeric(start)
  
  # FIXME "cannot coerce type 'closure' to vector of type 'character'" error here.
  # There are cases where end has not yet been assigned at this point. Therefore
  # function below catches to a function base R called end() and hilarity ensues.
  # I'll implement a quick fix for now. -vv
  if (!(exists("end", mode = "character") || exists("end", mode = "numeric"))) {
    end <- NA
  }
  
  if (!is.null(end_synonyms)) {
    end <- map(end, end_synonyms)
    end <- as.character(end)
  }
  end <- christian2numeric(end)   
  end_year <- as.numeric(end)
  
  if (length(start_year) == 0) {start_year <- NA}
  if (length(end_year) == 0) {end_year <- NA}  
  if (length(start_year) > 1) {start_year <- NA}
  if (length(end_year) > 1) {end_year <- NA}
  
  return(c(start_year, end_year))
}






