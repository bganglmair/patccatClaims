## ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
## PATCCAT
## Authors: Bernhard Ganglmair, W. Keith Robinson

## Version: Spring 2026

## R versions
# Mac OSX: 4.2.3
# Windows 11: 4.x

## FILE: generating functions for patccat
## VERSION: Feb 11, 2026 [new setup]

## THIS script version: June 19, 2022 - [edited Beauregard claims]
## OLD script version: June 4, 2022 - [added Beauregard claims; preambleTerm output; better blablabla processes; and a few other changes - see changes for version 3.4.0]
## OLD script version: January 25, 2022 - [added preambleTerms extracted, line types/numbers exported, preamble text stubs extracted]
## OLD script version: August 21, 2021 - changed as.integer to as.numeric in fn.singlesplitter
## OLD script version: May 1, 2021 - R version 3.5.0
## Old script version: December 8, 2020

## ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

## time stamp
# [pkg build] begin.time <- Sys.time()
# [pkg build] print(paste0("Begin: ", print(as.character(begin.time))))

## SHORT DESCRIPTION
## (1) Functions for patent claim classifier (patccat) in R

## FUNCTIONS
## fn.claimtype,
## fn.claimtype.single,
## fn.independent,
## fn.jepson,
## fn.jepsonreformat,
## fn.means,
## fn.combination,
## fn.POStagger,
## fn.preambletype,
## fn.process.simple,
## fn.singleline,
## fn.singlesplitter,
## fn.beginWithIn,
## fn.reformatdata,
## fn.textlength,
## fn.wordcount,
## fn.dependent.claims,
## fn.patccat,
## fn.benchmarking,
## fn.diagnostics,
## fn.rule.testing,

 
## ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  
## WRITE CATEGORIZER FUNCTIONS
## ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.independent
## FUNCTION to identify dependent and independent claims
## data -- dataframe with claims data

## Output: dataframe with PatentClaim and independent
##
fn.independent <- function(data) {
  
  ## Is 'dependency' information in the data?
  if(!"dependency" %in% colnames(data)) {data["dependency"] <- 0}
  
  # EMPTY DATAFRAME
  tmp.fn <- as.data.frame(unique(data["PatentClaim"]),stringsAsFactor=F)
  
  # ONLY LEVEL 1 and DEPENDENCY = 0
  tmp.ind <- unique(filter(data, level==1, dependency==0)$PatentClaim) 
  # we should add a sequence check - to avoid level=1 inside the patent claim
  
  tmp.fn["independent"] <- 0
  tmp.fn[which(tmp.fn$PatentClaim %in% tmp.ind),"independent"] <- 1

  return(tmp.fn)
}
##


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.singleline
## FUNCTION to identify singleline format claims
## data -- dataframe with claims data

## Output: dataframe with columns PatentClaim and singleLine
##
fn.singleline <- function(data) {
  
  tmp.fn <- group_by(data, PatentClaim)
  tmp.fn <- summarise(tmp.fn, lines = n())
  tmp.fn["singleLine"] <- 0; tmp.fn[which(tmp.fn$lines==1),"singleLine"] <- 1
  tmp.fn <- select(tmp.fn, PatentClaim, singleLine)
  
  return(tmp.fn)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.jepson
## FUNCTION to identify a Jepson claim
## data -- dataframe with claims data
## sources -- if TRUE, the sources for Jepson claim specification are added (needed for Jepson claim reformating)

## Output: dataframe with columns patentClaimID and Jepson
##
fn.jepson <- function(data,
                      sources) {
  
  # regex for Jepson claim- only in preamble or level=2 (sequence = 2) (or full single-line claim)
  
  tmp.by <- group_by(data, PatentClaim)
  tmp.by <- summarise(tmp.by, min.sequence = min(sequence, na.rm=T))
  data <- merge(data, tmp.by, all.x=T)
  data["min.sequence1"] <- data$min.sequence + 1
  data0 <- filter(data, level %in% c(1:2)) # widened 2026-08: all level 1-2 lines (Jepson pivot can sit past the first two lines when the prior-art head is long)
  
  ## Delete claim number
  data0["text"] <- gsub("^[(]?[0-9a-z]{1,3}[)] ","",data0$text)
  data0["text"] <- gsub("^[0-9a-z]{1,3}[.] ","",data0$text)
  data0["text"] <- gsub("^[0-9]{1,3} ","",data0$text)
  
  ## CHECK FOR "improvement", beginning with "in", string "improv"
  data0["improvement"] <- grepl("(?i)\\bimprovements?\\b",data0$text, perl = TRUE)
  data0["beginIn"] <- grepl("(?i)^in\\b", data0$text, perl = TRUE)
  data0["improv"] <- grepl("(?i)\\bimprov", data0$text, perl = TRUE)
  
  ## A claim is a Jepson claim if
  # the preamble (or entire claim) uses the word improvement
  # the claim begin with the word "in" and includes the string "improv" 
  
  # processing: aux per-claim helper columns (used by fn.jepsonreformat to locate the split)
  tmp.fn <- group_by(data0, PatentClaim)
  tmp.fn <- summarise(tmp.fn,
                      improvement = sum(improvement, na.rm=T),
                      beginIn = sum(beginIn, na.rm=T),
                      improv = sum(improv, na.rm=T))

  ## JEPSON FLAG (2026-08): detect the Jepson transitional "pivot" phrase over the full
  ## preamble / prior-art recitation (all level 1-2 lines), replacing the old trigger (bare
  ## word "improvement", or begins-with-"in" + "improv", within the first two lines only).
  ## (i) article (+ up to 2 non-preposition modifiers) + "improvement(s)" NOT followed by a
  ##     compound noun / stative verb (excludes "improvement device/function", "improvement is
  ##     achieved", "a method for improvement of", "hearing improvement device"); or
  ## (ii) "wherein/whereby the improvement". Recovers genuine Jepson with a long prior-art head
  ##     (pivot past the first two lines); drops incidental "improvement". Validated on 153,071
  ##     archive claims (+308 recovered, -30 incidental/ambiguous). See _TODO-remaining.md.
  jep.win <- summarise(group_by(data0, PatentClaim), wt = tolower(paste(text, collapse = " ")))
  jepson.incidental <- paste0("(function|technique|techniques|device|devices|factor|factors|",
    "ratio|rate|program|module|value|values|score|amount|technology|is\\b|are\\b|was\\b|were\\b|",
    "can\\b|will\\b|would\\b|has\\b|have\\b|achieved|occurs|means\\b)")
  jepson.regex.pivot <- paste0("(?i)\\b(the|said|that|this|an?|another)\\s+((?!for\\b|of\\b|in\\b|to\\b|on\\b|by\\b|with\\b|from\\b)\\w+[- ]){0,2}",
    "improvements?\\b(?!\\s+", jepson.incidental, ")")
  jepson.regex.wherein <- "(?i)\\b(wherein|whereby)[, ]+(the|rhe|teh|an?)\\s+((?!for\\b|of\\b|in\\b|to\\b|on\\b|by\\b|with\\b|from\\b)\\w+[- ]){0,2}improvements?\\b"
  jep.win["Jepson"] <- as.integer(grepl(jepson.regex.pivot, jep.win$wt, perl = TRUE) |
                                   grepl(jepson.regex.wherein, jep.win$wt, perl = TRUE))
  tmp.fn <- merge(tmp.fn, jep.win[, c("PatentClaim", "Jepson")], all.x = TRUE)
  tmp.fn[which(is.na(tmp.fn$Jepson)), "Jepson"] <- 0
  
  if (sources) {
    tmp.fn <- select(tmp.fn, PatentClaim, Jepson, improvement, beginIn, improv)
  } else {
    tmp.fn <- select(tmp.fn, PatentClaim, Jepson)
  }

  return(tmp.fn)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.jepsonreformat
## FUNCTION to reformat/convert a Jepson claim
## data -- dataframe with claims data
## 

## Output: dataframe with the converted data
## 
fn.jepsonreformat <- function(data) {
  
  ## Identify 
  jepson.detect <- fn.jepson(data, sources = TRUE)
  jepson.df <- filter(jepson.detect, Jepson == 1)
  
  # Jepson claims
  data1 <- filter(data, PatentClaim %in% jepson.df$PatentClaim)
  data1 <- merge(data1, jepson.detect, all.x=T)

  ## Delete claim number
  data1["text"] <- gsub("^[(]?[0-9a-z]{1,3}[)] ","",data1$text)
  data1["text"] <- gsub("^[0-9a-z]{1,3}[.] ","",data1$text)
  data1["text"] <- gsub("^[0-9]{1,3} ","",data1$text)

  # Jepson claims level=1 and level=2
  data.lev1 <- filter(data1, level==1)
  data.lev2 <- filter(data1, level==2)
  
  ## APPROACHES / STEPS
  ## (1) SPLIT AT improvement (when data2$improvement is =1) in level=1
  ## (2) SPLIT AT improvement (when data2$improvement is =1) in level=2
  ## (3) SPLIT AT improv[a-z]? (when data2$beginIn AND data2improv are =1, but not improvement ==1)
  ## (4): SPLIT AT improv[a-z]? (when data2$beginIn AND data2improv are = 1, but not improvement); in level=2
  
  tmp.collect <- data.frame()
  
  ## STEP (1): SPLIT AT improvement (when data2$improvement is = 1; when improvement in level=1)
  # tmp.data2 <- filter(data2, improvement >= 1)
  tmp.data2 <- filter(data.lev1, grepl("(?i)\\bimprovements?\\b",text, perl = TRUE))
  impr.claims.lev1 <- tmp.data2$PatentClaim
  if (dim(tmp.data2)[1]>0) {
    for (i in 1:nrow(tmp.data2)) {                                   # plural-safe split at first whole-word improvement(s) (#11)
      orig <- tmp.data2$text[i]
      mm <- regexpr("(?i)\\bimprovements?\\b", orig, perl = TRUE)
      if (mm[1] > 0) {
        ep <- mm[1] + attr(mm, "match.length")[1] - 1L
        tmp.data2[i,"statusquoText"] <- trimws(substr(orig, 1L, ep))
        tmp.data2[i,"text"]          <- trimws(substring(orig, ep + 1L))
      } else { tmp.data2[i,"statusquoText"] <- NA; tmp.data2[i,"text"] <- trimws(orig) }
    }
    tmp.data2["JepsonReformat"] <- 1
    tmp.collect <- rbind(tmp.collect, tmp.data2)
  }
  
  # ADD LEVEL=2 and higher back to tmp.collect
  data2.add <- filter(data1, level>1, PatentClaim %in% impr.claims.lev1)
  
  if (dim(data2.add)[1]>0) {
    data2.add["statusquoText"] <- NA
    data2.add["JepsonReformat"] <- NA
    tmp.collect <- rbind(tmp.collect, data2.add)
    tmp.collect <- arrange(tmp.collect, id, sequence)
  }
  
  ## STEP (2): SPLIT AT improvement (when data2$improvement is >= 1; when improvement in level=2)
  # For these, level=1 text is empty [EMPTY AFTER JEPSON REFORMAT]; and level=2 text is split
  tmp.data2 <- filter(data.lev2, improvement>=1, !PatentClaim %in% tmp.collect$PatentClaim)
  impr.claims.lev2 <- tmp.data2$PatentClaim
  tmp.data3 <- filter(tmp.data2, grepl("(?i)\\bimprovements?\\b",text, perl = TRUE)) 
  tmp.data4 <- filter(tmp.data2, !grepl("(?i)\\bimprovements?\\b",text, perl = TRUE)) 
  
  if (dim(tmp.data3)[1] > 0) {
    for (i in 1:nrow(tmp.data3)) {                                   # plural-safe split (#11)
      orig <- tmp.data3$text[i]
      mm <- regexpr("(?i)\\bimprovements?\\b", orig, perl = TRUE)
      if (mm[1] > 0) {
        ep <- mm[1] + attr(mm, "match.length")[1] - 1L
        tmp.data3[i,"statusquoText"] <- trimws(substr(orig, 1L, ep))
        tmp.data3[i,"text"]          <- trimws(substring(orig, ep + 1L))
      } else { tmp.data3[i,"statusquoText"] <- NA; tmp.data3[i,"text"] <- trimws(orig) }
    }
    tmp.data3["JepsonReformat"] <- 1
    tmp.collect <- rbind(tmp.collect, tmp.data3)
  }
  
  # ADD the LEVEL=1 lines of these claims ([EMPTY AFTER JEPSON REFORMAT])
  data3.add <- filter(data.lev1, PatentClaim %in% tmp.data3$PatentClaim)
  if (dim(data3.add)[1]>0) {
    data3.add["statusquoText"] <- data3.add$text
    data3.add["text"] <- "[EMPTY AFTER JEPSON REFORMAT]"
    data3.add["JepsonReformat"] <- 1
    tmp.collect <- rbind(tmp.collect, data3.add)
    tmp.collect <- arrange(tmp.collect, id, sequence)
  }
  
  # ADD the remaining LEVEL=2 and back to tmp.collect
  if (dim(tmp.data4)[1]>0) {
    tmp.data4["statusquoText"] <- NA
    tmp.data4["JepsonReformat"] <- NA
    tmp.collect <- rbind(tmp.collect, tmp.data4)
    tmp.collect <- arrange(tmp.collect, id, sequence)
  }
  
  # ADD LEVEL=3 and higher back to tmp.collect
  data2.add <- filter(data1, level>2, PatentClaim %in% impr.claims.lev2)
  
  if (dim(data2.add)[1]>0) {
    data2.add["statusquoText"] <- NA
    data2.add["JepsonReformat"] <- NA
    tmp.collect <- rbind(tmp.collect, data2.add)
    tmp.collect <- arrange(tmp.collect, id, sequence)
  }
  
  
  ## STEP (3): SPLIT AT improv[a-z]? (when data2$beginIn AND data2improv are =1, but not improvement)
  # tmp.data2 <- filter(data2, beginIn >= 1, improv>=1, improvement==0)
  tmp.data2 <- filter(data.lev1, 
                      !PatentClaim %in% tmp.collect$PatentClaim, 
                      grepl("(?i)\\bimprov", text, perl = TRUE), 
                      grepl("(?i)^in\\b", text, perl = TRUE), 
                      !grepl("(?i)\\bimprovements?\\b",text, perl = TRUE))
  impr.claims.lev1 <- tmp.data2$PatentClaim
  if (dim(tmp.data2)[1]>0) {
    tmp.split <- strsplit(tmp.data2$text,"(?i)improv[a-z]?",perl=TRUE)
    for (i in 1:length(tmp.split)) {
      tmp.data2[i,"statusquoText"] <- if (length(tmp.split[[i]]) >= 2) trimws(paste0(tmp.split[[i]][1],"improvXYZ")) else NA
      tmp.data2[i,"text"] <- if (length(tmp.split[[i]]) >= 2) trimws(paste0(tmp.split[[i]][-1], collapse = "improvXYZ")) else trimws(tmp.data2$text[i])
    }
    tmp.data2["JepsonReformat"] <- 1
    tmp.collect <- rbind(tmp.collect, tmp.data2)
  }
  
  # ADD LEVEL=2 and higher back to tmp.collect
  data2.add <- filter(data1, level>1, PatentClaim %in% impr.claims.lev1)
  
  if (dim(data2.add)[1]>0) {
    data2.add["statusquoText"] <- NA
    data2.add["JepsonReformat"] <- NA
    tmp.collect <- rbind(tmp.collect, data2.add)
    tmp.collect <- arrange(tmp.collect, id, sequence)
  }
  

  ## STEP (4): SPLIT AT improv[a-z]? (when data2$beginIn AND data2improv are = 1, but not improvement); in level=2
  # For these, level=1 text is empty [EMPTY AFTER JEPSON REFORMAT]; and level=2 text is split
  tmp.data2 <- filter(data.lev2, beginIn>=1, improv>=1, improvement==0, !PatentClaim %in% tmp.collect$PatentClaim)
  impr.claims.lev2 <- tmp.data2$PatentClaim
  tmp.data3 <- filter(tmp.data2, grepl("(?i)\\bimprov", text, perl = TRUE)) 
  tmp.data4 <- filter(tmp.data2, !grepl("(?i)\\bimprov", text, perl = TRUE)) 
  
  if (dim(tmp.data3)[1] > 0) {
    tmp.split <- strsplit(tmp.data3$text,"(?i)improv[a-z]?",perl=TRUE)
    for (i in 1:length(tmp.split)) {
      tmp.data3[i,"statusquoText"] <- if (length(tmp.split[[i]]) >= 2) trimws(paste0(tmp.split[[i]][1],"improvXYZ")) else NA
      tmp.data3[i,"text"] <- if (length(tmp.split[[i]]) >= 2) trimws(paste0(tmp.split[[i]][-1], collapse = "improvXYZ")) else trimws(tmp.data3$text[i])
    }
    tmp.data3["JepsonReformat"] <- 1
    tmp.collect <- rbind(tmp.collect, tmp.data3)
  }
  
  # ADD the LEVEL=1 lines of these claims ([EMPTY AFTER JEPSON REFORMAT])
  data3.add <- filter(data.lev1, PatentClaim %in% tmp.data3$PatentClaim)
  if (dim(data3.add)[1]>0) {
    data3.add["statusquoText"] <- data3.add$text
    data3.add["text"] <- "[EMPTY AFTER JEPSON REFORMAT]"
    data3.add["JepsonReformat"] <- 1
    tmp.collect <- rbind(tmp.collect, data3.add)
    tmp.collect <- arrange(tmp.collect, id, sequence)
  }
  
  # ADD the remaining LEVEL=2 and back to tmp.collect
  if (dim(tmp.data4)[1]>0) {
    tmp.data4["statusquoText"] <- NA
    tmp.data4["JepsonReformat"] <- NA
    tmp.collect <- rbind(tmp.collect, tmp.data4)
    tmp.collect <- arrange(tmp.collect, id, sequence)
  }
  
  # ADD LEVEL=3 and higher back to tmp.collect
  data2.add <- filter(data1, level>2, PatentClaim %in% impr.claims.lev2)
  
  if (dim(data2.add)[1]>0) {
    data2.add["statusquoText"] <- NA
    data2.add["JepsonReformat"] <- NA
    tmp.collect <- rbind(tmp.collect, data2.add)
    tmp.collect <- arrange(tmp.collect, id, sequence)
  }
  
  ## DROP AUX COLUMNS
  tmp.collect["Jepson"] <- NULL
  tmp.collect["improvement"] <- NULL
  tmp.collect["beginIn"] <- NULL
  tmp.collect["improv"] <- NULL

  ## FALLBACK (2026-08): pass through any Jepson-flagged claim not captured by STEP (1)-(4)
  ## instead of dropping it (a flagged claim absent from tmp.collect would otherwise vanish,
  ## as the branch below only re-adds Jepson==0 claims). Normally empty with the pivot detector
  ## (every flagged claim has "improvement" at level 1-2, caught by STEP 1/2); a safety net that
  ## keeps the detection<->reformat coupling robust.
  jep.missing <- setdiff(jepson.df$PatentClaim, unique(tmp.collect$PatentClaim))
  if (length(jep.missing) > 0) {
    data.miss <- filter(data1, PatentClaim %in% jep.missing)
    data.miss[["Jepson"]] <- NULL; data.miss[["improvement"]] <- NULL
    data.miss[["beginIn"]] <- NULL; data.miss[["improv"]] <- NULL
    data.miss[["statusquoText"]] <- NA
    data.miss[["JepsonReformat"]] <- NA
    data.miss[which(data.miss$level == 1), "JepsonReformat"] <- 0
    if (nrow(tmp.collect) > 0) {
      data.miss <- data.miss[, names(tmp.collect), drop = FALSE]
      tmp.collect <- rbind(tmp.collect, data.miss)
    } else {
      tmp.collect <- data.miss
    }
    tmp.collect <- arrange(tmp.collect, PatentClaim, sequence)
  }
  
  ## COMBINE WITH NON-JEPSON CLAIMS
  regular.df <- filter(jepson.detect, Jepson == 0)
  
  if (dim(regular.df)[1] > 0) {
    data3 <- filter(data, PatentClaim %in% regular.df$PatentClaim)
    data3["statusquoText"] <- NA
    data3["JepsonReformat"] <- NA; 
    data3[which(data3$level==1),"JepsonReformat"] <- 0
    data.final <- rbind(tmp.collect,data3)
  } else {
    data.final <- tmp.collect
  }
  
  data.final <- arrange(data.final, PatentClaim, sequence)
    
  return(data.final)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.singlesplitter
## FUNCTION to split a single-line claim into preamble and body-levels 
## Used after fn.jepsonreformat to split single-line jepson claims
## data -- dataframe with claims data
## 

## Output: dataframe with the converted data
## 
fn.singlesplitter <- function(data) {
  
  my.names <- names(data)
  
  ## Identify 
  single.detect <- fn.singleline(data)
  single.df <- filter(single.detect, singleLine == 1)
  data1 <- filter(data, PatentClaim %in% single.df$PatentClaim)
  
  ## Delete claim number
  data1["text"] <- gsub("^[(]?[0-9a-z]{1,3}[)] ","",data1$text)
  data1["text"] <- gsub("^[0-9a-z]{1,3}[.] ","",data1$text)
  data1["text"] <- gsub("^[0-9]{1,3} ","",data1$text)
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## SPLIT PREAMBLE FROM BODY
  
  ## split at the highest-priority transitional phrase present (parameterized cascade)
  transitions <- c("comprising","comprises","comprised of",
                   "consisting essentially of","consisting of","consisting","consists","composed of",
                   "steps of","steps"," acts of"," acts","containing","contains","including",
                   ":","having","wherein",
                   "characterized by","characterised by","characterized in that","characterised in that")
  trans.pattern <- vapply(transitions, function(m) if (grepl("^[a-z]+$", m)) paste0("\\b", m, "\\b") else m, character(1))
  tmp.collect <- data.frame()
  assigned <- rep(FALSE, nrow(data1))
  for (k in seq_along(transitions)) {
    tr <- transitions[k]; pat <- trans.pattern[k]
    sel <- which(!assigned & grepl(pat, data1$text))
    if (length(sel) > 0) {
      tmp.split <- data1[sel, , drop = FALSE]
      parts <- strsplit(tmp.split$text, pat)
      if (tr == "wherein") {
        tmp.split[["part1"]] <- vapply(parts, function(p) trimws(p[1]), character(1))
        tmp.split[["part2"]] <- vapply(parts, function(p) trimws(paste0("wherein", p[-1], collapse = "wherein")), character(1))
      } else {
        tmp.split[["part1"]] <- vapply(parts, function(p) trimws(paste0(p[1], tr)), character(1))
        tmp.split[["part2"]] <- vapply(parts, function(p) trimws(paste0(p[-1], collapse = tr)), character(1))
      }
      tmp.collect <- rbind(tmp.collect, tmp.split)
      assigned[sel] <- TRUE
    }
  }
  
  # clean part2 a bit
  tmp.collect["part2"] <- gsub("^[;:,]","",tmp.collect$part2)
  tmp.collect["part2"] <- trimws(tmp.collect$part2)
  
  tmp.collect["text"] <- NULL
  
  ## BUILD: move column part2 into text column with level=2 and higher sequence
  if (dim(tmp.collect)[1] > 0) {
    tmp.built <- data.frame()
    for (i in 1:dim(tmp.collect)[1]) {
      tmp.built.i <- tmp.collect[i,]
      tmp.built.i[2,] <- tmp.built.i[1,]
      
      tmp.built.i[2,"part1"] <- tmp.built.i[1,"part2"]
      tmp.built.i["part2"] <- NULL
      tmp.built.i[2,"level"] <- 2
      colnames(tmp.built.i)[colnames(tmp.built.i) == "part1"] <- "text"
      tmp.built.i["sequence"] <- c(tmp.built.i[1,"sequence"],tmp.built.i[1,"sequence"]+1)
      
      tmp.built.i <- tmp.built.i[,my.names]
      tmp.built.i["singleReformat"] <- 2
      
      tmp.built <- rbind(tmp.built,tmp.built.i)
    }
    tmp.built <- tmp.built[,c(my.names,"singleReformat")]
  } else {
    tmp.built <- setNames(data.frame(matrix(ncol = length(my.names)+1, nrow = 0)), c(my.names, "singleReformat"))
  }

  
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## SPLIT BODY FOR singleReformat == 2
  
  tmp.level1 <- filter(tmp.built, singleReformat == 2, level==1)# %>% select(PatentClaim, text)
  tmp <- filter(tmp.built, singleReformat == 2, level==2)# %>% select(PatentClaim, text)
  
  ## inline-guard: a counter right after "formula"/"formulae" is a formula LABEL, not a list
  ## counter (e.g. "compounds of formulae (i) ... (ii)"); strip its parens so it is not split on
  tmp["text"] <- gsub("(?i)(formulae?[ :]*)[(]([ivxlcdma-z0-9]+)[)]", "\\1\\2", tmp$text, perl=TRUE)
  
  regex.doublebrackets <- "[[]{2}[0-9]+[]]{2}"
  regex.brackets <- "([[]{1})[0-9]+([]]{1})"
  regex.numbers <- "[(][0-9]+[)]"
  regex.letters <- "[(][a-z]+[)]"
  regex.LETTERS <- "[(][A-Z]+[)]"
  regex.roman.lower <- "[(][ivxlcdm]+[)]"
  regex.roman.LETTERS <- "[(][IVXLCDM]+[)]"
  regex.numbersRight <- "( [0-9]+[)])|(^[0-9]+[)])"
  regex.lettersRight <- "( [a-z]+[)])|(^[a-z]+[)])"
  regex.LETTERSRight <- "( [A-Z]+[)])|(^[A-Z]+[)])"
  regex.semicolon <- ";"

  fn.roman2int <- function(s) {
    if (is.na(s) || s == "") return(NA_real_)
    rv <- c(i=1, v=5, x=10, l=50, c=100, d=500, m=1000)
    ch <- strsplit(s, "")[[1]]
    if (!all(ch %in% names(rv))) return(NA_real_)
    x <- rv[ch]
    tot <- 0
    for (t in seq_along(x)) {
      if (t < length(x) && x[t] < x[t+1]) tot <- tot - x[t] else tot <- tot + x[t]
    }
    as.numeric(tot)
  }

  fn.has.roman.seq <- function(txt, pat = "[(][ivxlcdm]+[)]") {
    vapply(txt, function(s) {
      toks <- regmatches(s, gregexpr(pat, s))[[1]]
      if (length(toks) < 2) return(FALSE)
      val <- vapply(gsub("[^ivxlcdm]","",tolower(toks)), fn.roman2int, numeric(1))
      val <- val[!is.na(val)]
      if (length(val) < 2) return(FALSE)
      d <- val[-1] - head(val,-1); n <- 1
      for (k in seq_along(d)) { if (!is.na(d[k]) && d[k] == 1) n <- n + 1 else break }
      n >= 2
    }, logical(1), USE.NAMES = FALSE)
  }
  
  tmp["bulletpoints.doublebrackets"] <- grepl(regex.doublebrackets,tmp$text); # table(tmp$points.doublebrackets); tmp2 <- filter(tmp, points.doublebrackets)
  tmp["bulletpoints.brackets"] <- grepl(regex.brackets,tmp$text); #table(tmp$bulletpoints.brackets); tmp2 <- filter(tmp, bulletpoints.brackets)
  tmp["bulletpoints.numbers"] <- grepl(regex.numbers,tmp$text); #table(tmp$points.numbers); tmp2 <- filter(tmp, points.numbers)
  tmp["bulletpoints.letters"] <- grepl(regex.letters,tmp$text); #table(tmp$points.letters); tmp2 <- filter(tmp, points.letters)
  tmp["bulletpoints.LETTERS"] <- grepl(regex.LETTERS,tmp$text); #table(tmp$points.LETTERS); tmp2 <- filter(tmp, points.LETTERS)
  tmp["bulletpoints.roman.lower"] <- fn.has.roman.seq(tmp$text)
  tmp["bulletpoints.roman.LETTERS"] <- fn.has.roman.seq(tmp$text, "[(][IVXLCDM]+[)]")
  tmp["bulletpoints.numbersRight"] <- grepl(regex.numbersRight,tmp$text); #table(tmp$bulletpoints.numberRight); tmp2 <- filter(tmp, bulletpoints.numberRight)
  tmp["bulletpoints.lettersRight"] <- grepl(regex.lettersRight,tmp$text); #table(tmp$bulletpoints.letterRight); tmp2 <- filter(tmp, bulletpoints.letterRight)
  tmp["bulletpoints.LETTERSRight"] <- grepl(regex.LETTERSRight,tmp$text); #table(tmp$bulletpoints.LETTERRight); tmp2 <- filter(tmp, bulletpoints.LETTERRight)
  tmp["bulletpoints.semicolon"] <- grepl(regex.semicolon,tmp$text); #table(tmp$points.semicolon); tmp2 <- filter(tmp, points.semicolon)

  ## BUILD: split body lines
  tmp.level2 <- data.frame()
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## split: by double-bracketed numbers
  tmp2 <- filter(tmp, bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.doublebrackets,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      tmp.splitters.i <- tmp3[[i]]
      # tmp.i <- as.integer(gsub("[^[:alnum:]]","",tmp.splitters.i))
      tmp.i <- as.numeric(gsub("[^[:alnum:]]","",tmp.splitters.i))
      
      # Must have at least two captures
      if (length(tmp.i)<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # Is it an increasing sequence?
      tmp.diff <- tmp.i[-1] - head(tmp.i,-1)
      # use the first n entries with 1 (the first sequence without gaps)
      my.diff <- 0
      for (k in 1:length(tmp.diff)) {
        if (tmp.diff[k] == 1) {my.diff <- my.diff + 1} else {break}
      }
      sequence.length <- my.diff + 1
      tmp.splitters.i <- tmp.splitters.i[1:sequence.length]
      
      # if sequence length is only 1, fill dataframe with statusLevel=2 raw data (statusLevel=2 does not change)
      if (sequence.length<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1

      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## split: by bracketed numbers
  tmp2 <- filter(tmp, bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.brackets,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      tmp.splitters.i <- tmp3[[i]]
      # tmp.i <- as.integer(gsub("[^[:alnum:]]","",tmp.splitters.i))
      tmp.i <- as.numeric(gsub("[^[:alnum:]]","",tmp.splitters.i))
      
      # Must have at least two captures
      if (length(tmp.i)<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # Is it an increasing sequence?
      tmp.diff <- tmp.i[-1] - head(tmp.i,-1)
      # use the first n entries with 1 (the first sequence without gaps)
      my.diff <- 0
      for (k in 1:length(tmp.diff)) {
        if (tmp.diff[k] == 1) {my.diff <- my.diff + 1} else {break}
      }
      sequence.length <- my.diff + 1
      tmp.splitters.i <- tmp.splitters.i[1:sequence.length]
      
      # if sequence length is only 1, fill dataframe with statusLevel=2 raw data (statusLevel=2 does not change)
      if (sequence.length<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1
      
      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## split: by parenthesed numbers
  tmp2 <- filter(tmp, bulletpoints.numbers,
                 !bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.numbers,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      tmp.splitters.i <- tmp3[[i]]
      # tmp.i <- as.integer(gsub("[^[:alnum:]]","",tmp.splitters.i))
      tmp.i <- as.numeric(gsub("[^[:alnum:]]","",tmp.splitters.i))
      
      # Must have at least two captures
      if (length(tmp.i)<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # Is it an increasing sequence?
      tmp.diff <- tmp.i[-1] - head(tmp.i,-1)
      # use the first n entries with 1 (the first sequence without gaps)
      my.diff <- 0
      for (k in 1:length(tmp.diff)) {
        if (tmp.diff[k] == 1) {my.diff <- my.diff + 1} else {break}
      }
      sequence.length <- my.diff + 1
      tmp.splitters.i <- tmp.splitters.i[1:sequence.length]
      
      # if sequence length is only 1, fill dataframe with statusLevel=2 raw data (statusLevel=2 does not change)
      if (sequence.length<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1
      
      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }

  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## split: by parenthesed roman numeral (lower case)
  tmp2 <- filter(tmp, bulletpoints.roman.lower,
                 !bulletpoints.numbers,
                 !bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.roman.lower,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      tmp.splitters.i <- tmp3[[i]]
      tmp.raw <- gsub("[^ivxlcdm]","",tolower(tmp.splitters.i))
      tmp.val <- vapply(tmp.raw, fn.roman2int, numeric(1))
      keep <- !is.na(tmp.val)
      tmp.splitters.i <- tmp.splitters.i[keep]
      tmp.i <- unname(tmp.val[keep])
      
      # Must have at least two captures
      if (length(tmp.i)<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # Is it an increasing sequence?
      tmp.diff <- tmp.i[-1] - head(tmp.i,-1)
      # use the first n entries with 1 (the first sequence without gaps)
      my.diff <- 0
      for (k in 1:length(tmp.diff)) {
        if (tmp.diff[k] == 1) {my.diff <- my.diff + 1} else {break}
      }
      sequence.length <- my.diff + 1
      tmp.splitters.i <- tmp.splitters.i[1:sequence.length]
      
      # if sequence length is only 1, fill dataframe with statusLevel=2 raw data (statusLevel=2 does not change)
      if (sequence.length<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1
      
      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }
  
  
  ## split: by parenthesed letter
  tmp2 <- filter(tmp, bulletpoints.letters,
                 !bulletpoints.roman.lower,
                 !bulletpoints.numbers,
                 !bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.letters,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      tmp.splitters.i <- tmp3[[i]]
      tmp.i <- gsub("[^[:alnum:]]","",tmp.splitters.i)
      
      tmp.k <- c()
      for (k in 1:length(tmp.i)) {
        tmp.k <- c(tmp.k, which(letters == tmp.i[k]))
      }
      tmp.i <- tmp.k
      
      # Must have at least two captures
      if (length(tmp.i)<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # Is it an increasing sequence?
      tmp.diff <- tmp.i[-1] - head(tmp.i,-1)
      # use the first n entries with 1 (the first sequence without gaps)
      my.diff <- 0
      for (k in 1:length(tmp.diff)) {
        if (tmp.diff[k] == 1) {my.diff <- my.diff + 1} else {break}
      }
      sequence.length <- my.diff + 1
      tmp.splitters.i <- tmp.splitters.i[1:sequence.length]
      
      # if sequence length is only 1, fill dataframe with statusLevel=2 raw data (statusLevel=2 does not change)
      if (sequence.length<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1
      
      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## split: by parenthesed roman numeral (UPPER CASE)
  tmp2 <- filter(tmp, bulletpoints.roman.LETTERS,
                 !bulletpoints.letters,
                 !bulletpoints.roman.lower,
                 !bulletpoints.numbers,
                 !bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.roman.LETTERS,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      tmp.splitters.i <- tmp3[[i]]
      tmp.raw <- gsub("[^ivxlcdm]","",tolower(tmp.splitters.i))
      tmp.val <- vapply(tmp.raw, fn.roman2int, numeric(1))
      keep <- !is.na(tmp.val)
      tmp.splitters.i <- tmp.splitters.i[keep]
      tmp.i <- unname(tmp.val[keep])
      
      # Must have at least two captures
      if (length(tmp.i)<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # Is it an increasing sequence?
      tmp.diff <- tmp.i[-1] - head(tmp.i,-1)
      # use the first n entries with 1 (the first sequence without gaps)
      my.diff <- 0
      for (k in 1:length(tmp.diff)) {
        if (tmp.diff[k] == 1) {my.diff <- my.diff + 1} else {break}
      }
      sequence.length <- my.diff + 1
      tmp.splitters.i <- tmp.splitters.i[1:sequence.length]
      
      # if sequence length is only 1, fill dataframe with statusLevel=2 raw data (statusLevel=2 does not change)
      if (sequence.length<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1
      
      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## split: by parenthesed LETTER (UPPER CASE)
  tmp2 <- filter(tmp, bulletpoints.LETTERS, 
                 !bulletpoints.roman.LETTERS,
                 !bulletpoints.roman.lower,
                 !bulletpoints.letters,
                 !bulletpoints.numbers,
                 !bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.LETTERS,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      tmp.splitters.i <- tmp3[[i]]
      tmp.i <- gsub("[^[:alnum:]]","",tmp.splitters.i)
      
      tmp.k <- c()
      for (k in 1:length(tmp.i)) {
        tmp.k <- c(tmp.k, which(toupper(letters) == tmp.i[k]))
      }
      tmp.i <- tmp.k
      
      # Must have at least two captures
      if (length(tmp.i)<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # Is it an increasing sequence?
      tmp.diff <- tmp.i[-1] - head(tmp.i,-1)
      # use the first n entries with 1 (the first sequence without gaps)
      my.diff <- 0
      for (k in 1:length(tmp.diff)) {
        if (tmp.diff[k] == 1) {my.diff <- my.diff + 1} else {break}
      }
      sequence.length <- my.diff + 1
      tmp.splitters.i <- tmp.splitters.i[1:sequence.length]
      
      # if sequence length is only 1, fill dataframe with statusLevel=2 raw data (statusLevel=2 does not change)
      if (sequence.length<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1
      
      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## split: by parenthesed numbers (only right)
  tmp2 <- filter(tmp, bulletpoints.numbersRight,
                 !bulletpoints.roman.LETTERS,
                 !bulletpoints.roman.lower,
                 !bulletpoints.LETTERS, 
                 !bulletpoints.letters,
                 !bulletpoints.numbers,
                 !bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.numbersRight,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      tmp.splitters.i <- tmp3[[i]]
      # tmp.i <- as.integer(gsub("[^[:alnum:]]","",tmp.splitters.i))
      tmp.i <- as.numeric(gsub("[^[:alnum:]]","",tmp.splitters.i))
      
      # Must have at least two captures
      if (length(tmp.i)<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # Is it an increasing sequence?
      tmp.diff <- tmp.i[-1] - head(tmp.i,-1)
      # use the first n entries with 1 (the first sequence without gaps)
      my.diff <- 0
      for (k in 1:length(tmp.diff)) {
        if (tmp.diff[k] == 1) {my.diff <- my.diff + 1} else {break}
      }
      sequence.length <- my.diff + 1
      tmp.splitters.i <- tmp.splitters.i[1:sequence.length]
      
      # if sequence length is only 1, fill dataframe with statusLevel=2 raw data (statusLevel=2 does not change)
      if (sequence.length<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1
      
      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## split: by parenthesed letter (only right)
  tmp2 <- filter(tmp, bulletpoints.lettersRight,
                 !bulletpoints.roman.LETTERS,
                 !bulletpoints.roman.lower,
                 !bulletpoints.numbersRight,
                 !bulletpoints.LETTERS, 
                 !bulletpoints.letters,
                 !bulletpoints.numbers,
                 !bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.lettersRight,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      tmp.splitters.i <- tmp3[[i]]
      tmp.i <- gsub("[^[:alnum:]]","",tmp.splitters.i)
      
      tmp.k <- c()
      for (k in 1:length(tmp.i)) {
        tmp.k <- c(tmp.k, which(letters == tmp.i[k]))
      }
      tmp.i <- tmp.k
      
      # Must have at least two captures
      if (length(tmp.i)<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # Is it an increasing sequence?
      tmp.diff <- tmp.i[-1] - head(tmp.i,-1)
      # use the first n entries with 1 (the first sequence without gaps)
      my.diff <- 0
      for (k in 1:length(tmp.diff)) {
        if (tmp.diff[k] == 1) {my.diff <- my.diff + 1} else {break}
      }
      sequence.length <- my.diff + 1
      tmp.splitters.i <- tmp.splitters.i[1:sequence.length]
      
      # if sequence length is only 1, fill dataframe with statusLevel=2 raw data (statusLevel=2 does not change)
      if (sequence.length<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1
      
      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## split: by parenthesed LETTER (UPPER CASE - only right)
  tmp2 <- filter(tmp, bulletpoints.LETTERSRight, 
                 !bulletpoints.roman.LETTERS,
                 !bulletpoints.roman.lower,
                 !bulletpoints.lettersRight,
                 !bulletpoints.numbersRight,
                 !bulletpoints.LETTERS, 
                 !bulletpoints.letters,
                 !bulletpoints.numbers,
                 !bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.LETTERSRight,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      tmp.splitters.i <- tmp3[[i]]
      tmp.i <- gsub("[^[:alnum:]]","",tmp.splitters.i)
      
      tmp.k <- c()
      for (k in 1:length(tmp.i)) {
        tmp.k <- c(tmp.k, which(toupper(letters) == tmp.i[k]))
      }
      tmp.i <- tmp.k
      
      # Must have at least two captures
      if (length(tmp.i)<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # Is it an increasing sequence?
      tmp.diff <- tmp.i[-1] - head(tmp.i,-1)
      # use the first n entries with 1 (the first sequence without gaps)
      my.diff <- 0
      for (k in 1:length(tmp.diff)) {
        if (tmp.diff[k] == 1) {my.diff <- my.diff + 1} else {break}
      }
      sequence.length <- my.diff + 1
      tmp.splitters.i <- tmp.splitters.i[1:sequence.length]
      
      # if sequence length is only 1, fill dataframe with statusLevel=2 raw data (statusLevel=2 does not change)
      if (sequence.length<2) {
        tmp.level2.i <- tmp2[i,c(my.names,"singleReformat")]
        tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
        next
      }
      
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1
      
      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## split: by semicolon
  tmp2 <- filter(tmp, bulletpoints.semicolon,
                 !bulletpoints.roman.LETTERS,
                 !bulletpoints.roman.lower,
                 !bulletpoints.LETTERSRight, 
                 !bulletpoints.lettersRight,
                 !bulletpoints.numbersRight,
                 !bulletpoints.LETTERS, 
                 !bulletpoints.letters,
                 !bulletpoints.numbers,
                 !bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  
  if (dim(tmp2)[1] > 0) {
    tmp3 <- regmatches(tmp2$text,gregexpr(regex.semicolon,tmp2$text))
    
    #BEGIN FOR
    for (i in 1:length(tmp3)) {
      tmp.level2.i <- data.frame()
      
      # a single semicolon splitter is OK; makes two lines
      tmp.splitters.i <- tmp3[[i]]
    
      # if valid.i, split along tmp.splitters.i and fill dataframe with split body (and statusLevel=1)
      my.text.part2 <- tmp2[i,"text"]
      for (j in 1:length(tmp.splitters.i)) {
        split.j <- unlist(strsplit(my.text.part2, split=tmp.splitters.i[j],fixed=T) )
        my.text.part1 <- trimws(split.j[1])
        my.text.part2 <- trimws(paste0(split.j[-1], collapse = tmp.splitters.i[j]))
        
        tmp.level2.i[j,"PatentClaim"] <- tmp2[i,"PatentClaim"]
        tmp.level2.i[j,"id"] <- tmp2[i,"id"]
        tmp.level2.i[j,"text"] <- my.text.part1 
        tmp.level2.i[j,"sequence"] <- tmp2[i,"sequence"]
        tmp.level2.i[j,"level"] <- 2
        tmp.level2.i[j,"statusquoText"] <- tmp2[i,"statusquoText"]
        tmp.level2.i[j,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
        tmp.level2.i[j,"singleReformat"] <- 1
      }
      
      tmp.level2.i[length(tmp.splitters.i)+1,"PatentClaim"] <- tmp2[i,"PatentClaim"]
      tmp.level2.i[length(tmp.splitters.i)+1,"id"] <- tmp2[i,"id"]
      tmp.level2.i[length(tmp.splitters.i)+1,"text"] <- my.text.part2
      tmp.level2.i[length(tmp.splitters.i)+1,"sequence"] <- tmp2[i,"sequence"]
      tmp.level2.i[length(tmp.splitters.i)+1,"level"] <- 2
      tmp.level2.i[length(tmp.splitters.i)+1,"statusquoText"] <- tmp2[i,"statusquoText"]
      tmp.level2.i[length(tmp.splitters.i)+1,"JepsonReformat"] <- tmp2[i,"JepsonReformat"]
      tmp.level2.i[length(tmp.splitters.i)+1,"singleReformat"] <- 1
      
      tmp.level2.i <- filter(tmp.level2.i, !text=="")
      tmp.level2.i["sequence"] <- seq(min(tmp.level2.i$sequence),min(tmp.level2.i$sequence)+length(tmp.level2.i$sequence)-1,1)
      
      # fix singleReformat in tmp.level1
      tmp.level1[which(tmp.level1$PatentClaim == tmp2[i,"PatentClaim"]),"singleReformat"] <- 1
      
      # add on
      tmp.level2 <- rbind(tmp.level2, tmp.level2.i)
    }
    #END FOR
  }
  
  
  ## ADD ALL THAT DO NOT HAVE ANY ENUMERATORS
  tmp2 <- filter(tmp, 
                 !bulletpoints.semicolon,
                 !bulletpoints.LETTERSRight, 
                 !bulletpoints.lettersRight,
                 !bulletpoints.numbersRight,
                 !bulletpoints.LETTERS, 
                 !bulletpoints.letters,
                 !bulletpoints.numbers,
                 !bulletpoints.brackets,
                 !bulletpoints.doublebrackets)
  tmp2 <- tmp2[,c(my.names,"singleReformat")]
  tmp.level2 <- rbind(tmp.level2,tmp2)
  
  ## ADD LEVEL=1 BACK IN
  tmp.built <- rbind(tmp.level1, tmp.level2)
  tmp.built <- arrange(tmp.built, PatentClaim, sequence)
  length(unique(tmp.built$PatentClaim))
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## add those that couldn't be split (no preamble-body split)
  tmp.split <- filter(data1, !PatentClaim %in% tmp.collect$PatentClaim)
  
  if (dim(tmp.split)[1] > 0) {
    tmp.split <- tmp.split[,my.names]
    tmp.split["singleReformat"] <- 3
    tmp.built <- rbind(tmp.built,tmp.split)
  }

  tmp.collect <- NULL
  
  
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
  ## add regular claims:
  regular.df <- filter(single.detect, singleLine == 0)
  
  if (dim(regular.df)[1] > 0) {
    data2 <- filter(data, PatentClaim %in% regular.df$PatentClaim)
    data2 <- data2[,my.names]
    data2["singleReformat"] <- 0
    data.final <- rbind(tmp.built,data2)
  } else {
    data.final <- tmp.built
  }
  
  ## TABULATE THE TYPES!
  table(filter(data.final, level == 1)$singleReformat)
  
  return(data.final)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## fn.beginWithIn
## FUNCTION to delete the in-phrase of a preamble
## data -- dataframe with claims data

## Output: List with -df- (identifying beginWithIn claims) and -data- (data.frame with the converted data)
## 
fn.beginWithIn <- function(data) {
  
  my.names <- names(data)
  
  ## CLEAN EACH LINE
  data1 <- filter(data, level==1) # we should add a sequence check - to avoid level=1 inside the patent claim
  data1["text"] <- gsub("^[(]?[0-9a-z]{1,3}[)] ","",data1$text)
  data1["text"] <- gsub("^[0-9a-z]{1,3}[.] ","",data1$text)
  data1["text"] <- gsub("^[0-9]{1,3} ","",data1$text)
  
  # DETECT in-begin
  data1["inBegin"] <- grepl("(?i)^(in|for)\\b",data1$text, perl = TRUE); table(data1$inBegin)
  tmp.begin <- select(data1, PatentClaim, inBegin)
  tmp.begin[which(tmp.begin$inBegin),"inBegin"] <- 1
  tmp.begin[which(!tmp.begin$inBegin),"inBegin"] <- 0
  tmp.begin <- tmp.begin[!duplicated(tmp.begin$PatentClaim),]   # B3: one inBegin flag per claim (guard malformed multi-level-1)
  
  # Add Jepson
  tmp.jepson <- fn.jepson(data = data1, sources = FALSE)
  data1 <- merge(data1, tmp.jepson, all.x=T)
  
  # Split at the first comma
  tmp.split <- filter(data1, inBegin, Jepson==0)
  ## CONSERVATIVE ENVIRONMENT-HEAD GUARD (2026-08):
  ## 'In [environment], ...' claims: the paper trims the environment (text up to the first comma), which
  ## discards the category-determining head noun. When the environment is LED by a process word
  ## ('In a method/process/treatment of ...'), keep the full preamble so the process signal survives
  ## (parallel to the Jepson prior-art-head fix). Only when a process word is the LEFTMOST statutory
  ## keyword in the first 8 words; product-led environments are left trimmed (their apparatus+process
  ## conflicts belong to the deferred product-by-process decision).
    proc.words.env <- .patccat$process.words
    prod.words.env <- .patccat$product.words
  prod.stems.env <- gsub("[s]?", "", prod.words.env, fixed = TRUE)
  stemmatch.env <- function(w, dict) w %in% dict | sub("s$","",w) %in% dict | sub("es$","",w) %in% dict
  ## in-environment head window = param1.nPreamble (was hardcoded 8); read the global .patccat$params[1] with an 8-word fallback (#4b)
  nkeep.env <- tryCatch(as.integer(.patccat$params[1]), error = function(e) 8L)
  if (length(nkeep.env) != 1L || is.na(nkeep.env) || nkeep.env < 1L) nkeep.env <- 8L
  keep.env <- rep(FALSE, nrow(tmp.split))
  if (nrow(tmp.split) > 0) {
    for (i in 1:nrow(tmp.split)) {
      ws <- head(strsplit(tolower(tmp.split$text[i]), "[^a-z]+")[[1]], nkeep.env)
      pp <- which(vapply(ws, stemmatch.env, logical(1), dict = proc.words.env))
      qq <- which(vapply(ws, stemmatch.env, logical(1), dict = prod.stems.env))
      keep.env[i] <- length(pp) > 0 && (length(qq) == 0 || min(pp) < min(qq))
    }
  }
  tmp.split2 <- strsplit(tmp.split$text,",")
  if (length(tmp.split2) > 0) {
    for (i in 1:length(tmp.split2)) {
      if (isTRUE(keep.env[i])) {
        tmp.split[i,"part2"] <- tmp.split$text[i]
      } else {
        tmp.split[i,"part2"] <- trimws(paste0(tmp.split2[[i]][-1], collapse = ","))
      }
    }
  }
  tmp.split["text"] <- tmp.split$part2
  tmp.split["part2"] <- NULL 
  data.final <- tmp.split
  
  ## ADD LEVEL 1 not corrected
  tmp.built <- filter(data1, !PatentClaim %in% tmp.split$PatentClaim)
  data.final <- rbind(data.final, tmp.built)
  data.final <- data.final[,c(my.names)]
  
  ## ADD ALL OTHER LEVELS
  tmp.built <- filter(data, level!=1)
  data.final <- rbind(data.final, tmp.built)
  data.final <- arrange(data.final, id, sequence)
  
  output.list <- list()
  output.list[["df"]] <- tmp.begin
  output.list[["data"]] <- data.final
  return(output.list)  
  
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## fn.reformatData
## FUNCTION to that reformats the data: fn.jepsonreformat, fn.singlesplitter, fn.beginWithIn
## data -- dataframe with claims data

## Output: A list of two: -df- with per-claim information on the status, and -data- with the converted data
## 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
## fn.integrity -- first-step structural cleaning / integrity pass (run before the reformat/splitters)
## Repairs upstream-splitter defects: R1 demote continuation level-1 lines; R2 promote a preamble when
## none; R3 drop empty-text lines. Adds per-claim integ_* flags. Conservative: no-op on well-formed claims.
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
fn.integrity <- function(data, varPatentClaim = "PatentClaim", varLevel = "level",
                         varSequence = "sequence", varText = "text") {
  d <- data
  d$.pc <- d[[varPatentClaim]]; d$.tx <- d[[varText]]
  d$.origord <- seq_len(nrow(d))

  ## robust coercions (defensive: only bite on malformed third-party input)
  lv.num <- suppressWarnings(as.integer(d[[varLevel]]))
  d$.badlv <- is.na(lv.num)                                  # NA or non-integer level
  d$.lv <- lv.num
  sq.num <- suppressWarnings(as.numeric(d[[varSequence]]))
  d$.badsq <- is.na(sq.num)                                  # NA or non-numeric sequence
  d$.sqn <- sq.num
  d$.sqeff <- ifelse(is.na(sq.num), d$.origord, sq.num)      # order falls back to input order

  ## Unicode-aware emptiness: ASCII ws + \p{Z} separators + \p{C} control/format; NA text -> empty
  d$.empty <- !nzchar(gsub("[\\s\\p{Z}\\p{C}]+", "", ifelse(is.na(d$.tx), "", d$.tx), perl = TRUE))
  d$.isNum <- grepl("^\\s*[0-9]{1,3}\\s*[.]", ifelse(is.na(d$.tx), "", d$.tx), perl = TRUE)

  ## stable order: claim, effective sequence, then input order (deterministic tie-break)
  d <- d[order(d$.pc, d$.sqeff, d$.origord), ]

  ## per-claim integrity flags on the RAW claim (before any repair); NA-safe
  fl <- d %>% group_by(.pc) %>% summarise(
    integ_seqGap     = as.integer({ s <- .sqn[!is.na(.sqn)]; if (length(s) >= 1) (max(s) - min(s) + 1) != length(s) else 0L }),
    integ_multiL1    = as.integer(sum(.lv == 1, na.rm = TRUE) >= 2),
    integ_noL1       = as.integer(!any(.lv == 1, na.rm = TRUE)),
    integ_emptyLine  = as.integer(any(.empty)),
    integ_emptyClaim = as.integer(all(.empty)),
    integ_badLevel   = as.integer(any(.badlv)),
    integ_badSeq     = as.integer(any(.badsq)),
    integ_levelGap   = as.integer({ u <- sort(unique(.lv)); if (length(u) >= 1) (!identical(as.integer(u), min(u):max(u))) | (!any(.lv == 1, na.rm = TRUE)) else 1L }),
    .groups = "drop")

  ## R0 -- coerce NA/non-integer level to body (level 2)
  d$.lv[is.na(d$.lv)] <- 2L

  ## R3 -- drop empty-text lines, but keep the first line of an all-empty claim (never lose a claim)
  d <- d %>% group_by(.pc) %>% mutate(.allE = all(.empty), .rk0 = row_number()) %>% ungroup()
  d <- d[!d$.empty | (d$.allE & d$.rk0 == 1), ]

  ## recompute positions after the drop
  d <- d %>% group_by(.pc) %>% mutate(.rk = row_number(),
        .firstL1 = if (any(.lv == 1)) which(.lv == 1)[1] else NA_integer_,
        .nL1 = sum(.lv == 1)) %>% ungroup()
  ## R1 -- demote continuation level-1 lines to level 2
  demote <- !is.na(d$.lv) & d$.lv == 1 & !is.na(d$.firstL1) & d$.rk != d$.firstL1 & !d$.isNum
  d$.lv[demote] <- 2L
  ## R2 -- promote first line to level 1 for claims with no level-1
  promote <- d$.nL1 == 0 & d$.rk == 1
  d$.lv[promote] <- 1L

  d[[varLevel]] <- d$.lv
  d <- merge(d, fl, by = ".pc", all.x = TRUE)
  d <- d[order(d$.pc, d$.sqeff, d$.origord), ]
  tmp <- c(".pc", ".tx", ".origord", ".badlv", ".lv", ".sqn", ".badsq", ".sqeff",
           ".empty", ".isNum", ".allE", ".rk0", ".rk", ".firstL1", ".nL1")
  d[, setdiff(names(d), tmp)]
}


fn.reformatdata <- function(data, integrityCheck = TRUE) {
  
  # STRUCTURAL INTEGRITY PASS -- first-step cleaning of upstream-splitter defects (multiple level-1,
  # no-level-1 / missing preamble, empty lines). Repairs the level structure so the classifier receives
  # well-formed input; a no-op on well-formed claims. Set integrityCheck = FALSE to disable. See fn.integrity.
  if (isTRUE(integrityCheck)) {
    .keepcols <- names(data)
    data <- fn.integrity(data, varPatentClaim = "PatentClaim", varLevel = "level",
                         varSequence = "sequence", varText = "text")
    data <- data[, .keepcols, drop = FALSE]   # keep repaired levels; drop integ_* flags before the splitters
  }
  
  # if data is empty before reformat
  data1 <- data
  data1[which(data1$text == "" | is.na(data1$text)),"text"] <- "[EMPTY]"

  ## AS-FILED LENGTH (#11): measure wordsPreamble/wordsBody on the claim AS FILED (raw post-integrity
  ## text), NOT on the reformatted classifier-facing text. The Jepson reformat below moves a claim's
  ## prior-art head out of the preamble into statusquoText (collapsing the measured preamble); begin-with-in
  ## redistributes text too. So length is taken HERE, before those steps. Multi-line claims use their raw
  ## preamble (level 1) / body (level >1) directly; single-line claims are split by fn.singlesplitter only
  ## (verified to leave multi-line claims untouched) so they still get a body.
  .lenmax    <- tapply(data1$level, data1$PatentClaim, function(z) max(z, na.rm = TRUE))
  .single.pc <- names(.lenmax)[is.finite(.lenmax) & .lenmax == 1]
  .lkeep     <- c("PatentClaim", "text", "level")
  if (length(.single.pc) > 0) {
    .s <- data1[data1$PatentClaim %in% .single.pc, , drop = FALSE]
    .s[["statusquoText"]] <- NA; .s[["JepsonReformat"]] <- NA
    .s <- fn.singlesplitter(data = .s)
    .ld <- rbind(data1[!(data1$PatentClaim %in% .single.pc), .lkeep, drop = FALSE], .s[, .lkeep, drop = FALSE])
  } else {
    .ld <- data1[, .lkeep, drop = FALSE]
  }
  .l1 <- which(.ld$level == 1)                     # strip the leading claim number (not claim content)
  .ld$text[.l1] <- gsub("^[(]?[0-9a-z]{1,3}[)][ .]+", "", .ld$text[.l1], perl = TRUE)
  .ld$text[.l1] <- gsub("^[0-9]{1,3}[. ]+", "", .ld$text[.l1], perl = TRUE)
  data.lengths <- fn.wordcount(data = .ld)
  
  # Jepson Reformat
  data1 <- fn.jepsonreformat(data = data1)
  
  # Single-Line Splitter
  data1 <- fn.singlesplitter(data = data1)
  
  # Begin-with-in splitter
  data.begin <- fn.beginWithIn(data = data1)
  data1 <- data.begin[["data"]]
  
  # if text field is empty: "empty after REFORMAT
  data1[which(data1$text == "" | is.na(data1$text)),"text"] <- "[EMPTY AFTER REFORMAT]"
  
  output.list <- list()
  output.list[["df"]] <- data.begin[["df"]]
  output.list[["data"]] <- data1
  output.list[["lengths"]] <- data.lengths
  return(output.list)  
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.preambletype
## FUNCTION to identify the type of a preamble
## data -- dataframe with claims data
## processwords -- vector of keywords for process preamble
## productwords -- vector of keywords for product preamble
## param1.nPreamble -- number of words to use to search for process and product words
## param2.nPreambleByProcess -- number of words to use to search for by-process structure
## param10.nbyGap
## singleLine -- is the the claim a single-line claim?

## Output: dataframe with PatentClaim and preambleType
##
fn.preambletype <- function(data, 
                            processwords,
                            productwords,
                            param1.nPreamble,
                            param2.nPreambleByProcess,
                            param10.nbyGap,
                            singleLine,
                            extractPreambleTerms=TRUE,
                            extractPreambleText=FALSE) {
  
  ## if it is a regular formatted claim, then search for by-process phrase in entire preamble
  ## if it is a single-line formatted claim, then search for by-process phrase within number param2.
  # ORIGINAL
  # if (singleLine == F | param2.nPreambleByProcess == Inf) {param2.nPreambleByProcess <- ""}
  # ALTERNATIVE WORD LENGTH DETECTION
  if (singleLine == F) {param2.nPreambleByProcess <- Inf}
  
  ### ##### ##### ##### ##### ##### #####
  ### DEFINE VARIOUS REGULAR EXPRESSIONS
  
  # regex1 - for the short text for capture
  regex1 <- paste0("(?i)^(?:\\S+ ){0,",param1.nPreamble,"}")

  # regex1a - for the short text for capture; short + 1 word
  param1.nPreamblea <- param1.nPreamble + 1L
  regex1a <- paste0("(?i)^(?:\\S+ ){0,",param1.nPreamblea,"}")
  
    
  # regex2 - for the long text for capture
  # for single-line claims, the long text is limited by param2.nPreambleByProcess
  # for multi-line claims, the long text is the entire preamble
  regex2 <- paste0("(?i)^(?:\\S+ ){0,",param2.nPreambleByProcess,"}")
  
  # regex3 - for a text capture longer than the short, but for output purposes (15 words)
  # the length of the preambleTextStub is hard-coded at 15
  regex3 <- paste0("(?i)^(?:\\S+ ){0,15}")
  
  # reformat processwords vector and turn into Regex
  regex.proc1 <- paste0("(?i)^.+?(?=(",paste(processwords,collapse="|"),")(?:es|s)?\\b)")
  regex.proc2 <- paste0("(?i)\\b(",paste(processwords,collapse="|"),")(?:es|s)?\\b")
  regex.proc3 <- paste0("(?i)(.*?)\\b(",paste(processwords,collapse="|"),")(?:es|s)?\\b")
  
  # reformat productwords vector and turn into Regex
  regex.prod1 <- paste0("(?i)^.+?(?=(",paste(productwords,collapse="|"),")\\b)")
  regex.prod2 <- paste0("(?i)\\b(",paste(productwords,collapse="|"),")\\b")
  
  ### REGEX FOR computer-implemented and others!
  
  #machine
  #computer
  #processor
  #device
  #microprocessor ?
  
  #readable
  #implemented, implementable
  #contro[l]+ed (a lot of others!) ???
  #accessible
  #executable
  #based (a lot of others!) ???
  #usable
  #recordable
  #assisted
  #aided
  #effected
  #directed
  #stored
  #recognizable
  #accessible
  #readably
  #readeable
  #stored
  #automated
  #enabled
  #searchable
  #executed
  #facilitated
  #useable
  #generated
  #centered
  #operated
  
  # regex for "computer-implemented process.words"
  regex.compimpl.process <- paste0("(?i)((machine|computer|processor|device|microprocessor|system)[- ](readable|implemented|implementable|contro[l]+ed|accessible|executable|based|usable|recorded|assisted|aided|effected|directed|stored|recognizable|accessible|readably|readeable|stored|automated|enabled|searchable|executed|facilitated|useable|generated|centered|operated)[ ]+(storage )?)(",paste(processwords,collapse="|"),")(?:es|s)?\\b")

  # regex for "computer-implemented product.words"
  regex.compimpl.product <- paste0("(?i)((machine|computer|processor|device|microprocessor|system)[- ](readable|implemented|implementable|contro[l]+ed|accessible|executable|based|usable|recorded|assisted|aided|effected|directed|stored|recognizable|accessible|readably|readeable|stored|automated|enabled|searchable|executed|facilitated|useable|generated|centered|operated)[ ]+(storage )?)(",paste(productwords,collapse="|"),")\\b")
  
  # byprocess regex
  regex.byprocess <- paste0("(?i)\\b(by (?:\\S+ ){0,",param10.nbyGap,"}(process|method)(?:es|s)?)\\b")
  
  # for regex / by regex
  regex.byNeg <- "(?i)\\bby\\b"
  regex.forNeg <- "(?i)\\bfor\\b"
  
  ### ##### ##### ##### ##### ##### #####
  ### CLEAN EACH LINE
  
  # we take only level=1 lines
  # check if there are higher-sequence level=1 lines (i.e., if there are multiple level=1 in a claim)
  # if another line is a level=1 line, drop it
  data1 <- filter(data, level==1) %>% arrange(PatentClaim, sequence)
  dupl <- duplicated(data1[,c("PatentClaim","level")]); table(dupl)
  data1 <- data1[!dupl,]
  
  # drop enumerators at the beginning of the line
  data1["text"] <- gsub("^[(]?[0-9a-z]{1,3}[)] ","",data1$text)
  data1["text"] <- gsub("^[0-9a-z]{1,3}[.] ","",data1$text)
  data1["text"] <- gsub("^[0-9]{1,3} ","",data1$text)
  
  ### ##### ##### ##### ##### ##### #####
  ### REGEX CAPTURES
  
  # CAPTURE THE FIRST N WORDS (by param1) AND WRITE INTO "short"
  # CAPTURE THE FIRST N WORDS (by param2) AND WRITE INTO "long"
  # CAPTURE THE FIRST 15 WORDS AND WRITE INTO "preambleTextStub"
  
  # EXTRACTING WORDS USING stringr::word
  # data1["nPreambleWords"] <- lengths(gregexpr("[^a-zA-Z0-9_-]+", data1$text))
  # first determine the max number of words in the preamble; used to avoid extracting beyond the max number of words
  data1["nPreambleWords"] <- lengths(gregexpr("\\S+", data1$text))
  fn.extract.text <- function(text, paramwords, maxwords) {
    end.word <- min(paramwords, maxwords)
    stringr::word(text,start=1,end=end.word)
  }
  
  data1["short"] <- mapply(fn.extract.text, data1$text, param1.nPreamble, data1$nPreambleWords)
  data1["shorta"] <- mapply(fn.extract.text, data1$text, param1.nPreamblea, data1$nPreambleWords)
  data1["long"] <- mapply(fn.extract.text, data1$text, param2.nPreambleByProcess, data1$nPreambleWords)
  if (extractPreambleText == T) {
    data1["preambleTextStub"] <- mapply(fn.extract.text, data1$text, 15, data1$nPreambleWords)
  }
  
  ### ##### ##### ##### ##### ##### #####
  ### PROCESS KEY WORDS AND CHECKS
  
  ###
  # in data1$short, find process words, then write text up to that word (not including) into pre.process.text
  # the pre.process.text is used in the next step
  
  # extract the words leading up to the process word
  data1["pre.process.text"] <- stringr::str_extract(data1$short,regex.proc1)
  
  # extract the process word (with regex.proc2)  
  data1["processTerm"] <- stringr::str_extract(data1$short,regex.proc2)

  ###
  # is a product word in "pre.process.text" -- is there a product word BEFORE the process word?
  data1["procNeg"] <- grepl(regex.prod2, data1$pre.process.text, perl=T)

  ###
  # do we have a 'computer-implemented process/method structure' in the line with the process word
  # these may use a product word, but they are processes.
  data1["process.text"] <- apply(data1[,c("pre.process.text","processTerm")], 1,paste, collapse= "")
  data1["compImplProcess"] <- grepl(regex.compimpl.process, data1$process.text, perl=T)
  
  ###
  # what the next two steps do: are the terms for or by used before the process word?
  # is word 'by' in "pre.process.text" -- is this process word in a by-process structure?
  data1["byNeg"] <- grepl(regex.byNeg, data1$pre.process.text, perl=T)
  # is the word for at the end of "pre.process.text" -- an indicator that the process word is more like "something for a process word"
  data1["forNeg"] <- grepl(regex.forNeg, data1$pre.process.text, perl=T)
  
  ### ##### ##### ##### ##### ##### #####
  ### PRODUCT KEY WORDS AND CHECKS
  
  ###
  # in data1$short, find product words, then write text up to that word (not including) into pre.product.text
  # the pre.product.text is used in the next step
  
  # extract the words leading up to the product word
  data1["pre.product.text"] <- stringr::str_extract(data1$short,regex.prod1)
  
  # extract the product word (with regex.prod2)  
  data1["productTerm"] <- stringr::str_extract(data1$short,regex.prod2)
  
  ###
  # is a process word in "pre.product.text" -- is there a process word BEFORE the product word?
  data1["prodNeg"] <- grepl(regex.proc2,data1$pre.product.text,perl=T)
  
  ###
  # do we have a 'computer-implemented product' structure in data1$short
  # these are product words; unlike in the process case above, we now look in the entire data1$short
  # the reason: we may have computer-implemented medium; so far, we'd capture computer when in fact we want to capture medium
  data1["compImplProduct"] <- grepl(regex.compimpl.product, data1$short, perl=T)
  
  ### ##### ##### ##### ##### ##### #####
  ### BY-PROCESS STUCTURES
  
  ###
  # Capture a "by-process" structure
  data1["byprocess.short"] <- grepl(regex.byprocess,data1$short,perl=T)
  data1["byprocess.long"] <- grepl(regex.byprocess,data1$long,perl=T)
  
  ### ##### ##### ##### ##### ##### #####
  ### IS A PROCESS WORD IMMEDIATELY FOLLOWED BY A NOUN?
  # if it's followed by a noun, then the process word is part of a compound and likely not a process
  # we check for nouns after the process word; and check if those nouns are in the process word list; or if they are step[s]
  
  # do we have a process word in our short text section?
  data1["split.processNoun"] <- grepl(regex.proc2, data1$short, perl = TRUE);
  tmp.split <- filter(data1, split.processNoun)
  
  # do we have a split.processNoun?
  if (dim(tmp.split)[1] > 0) {
    # if we do have a process word, then delete the text up to first occurrence of that process word (regex.proc3)
    # but now with data1$shorta (so we have at least one more word)
    tmp.split["part2"] <- trimws(sub(regex.proc3,"",tmp.split$shorta, perl = TRUE))
    
    # Drop all non-alphanumerical characters
    tmp.split["part2"] <- gsub("[^[:alnum:]]"," ",tmp.split$part2)
    tmp.split["part2"] <- trimws(tmp.split$part2)
    
    # the first word immediately following the process word
    tmp.split[which(grepl("(?i)^\\S+ ", tmp.split$part2, perl = TRUE)),"part2word"] <- 
      unlist(regmatches(tmp.split$part2, gregexpr("(?i)^\\S+ ", tmp.split$part2, perl = TRUE)))
    tmp.split["part2word"] <- tolower(tmp.split$part2word)
    tmp.split["part2word"] <- trimws(tmp.split$part2word)
    
    # POS-tag the following word IN CONTEXT (isolated tagging mislabels ambiguous nouns, e.g. 'green', 'assembly').
    # Use the regex-derived part2word (robust to hyphenated compounds like 'post-process') and read ITS in-context tag.
    tmp.split["POStag"] <- NA_character_
    ann.fn <- as.data.frame(udpipe::udpipe_annotate(.patccat$udmodel, x = tmp.split$shorta, doc_id = as.character(seq_len(nrow(tmp.split))), tokenizer = "tokenizer", tagger = "default", parser = "none"))
    for (i in 1:dim(tmp.split)[1]) {
      p2 <- tmp.split$part2word[i]
      if (is.na(p2) || !nzchar(p2)) next
      aa <- ann.fn[ann.fn$doc_id == as.character(i), ]
      hh <- which(tolower(aa$token) == p2)
      if (length(hh)) tmp.split[i,"POStag"] <- aa$xpos[hh[1]]
    }
    
    # is that first word following the process word a noun?
    tmp.split["followedNoun"] <- NA
    tmp.split[which(grepl("NN",tmp.split$POStag)),"followedNoun"] <- 1
    
    # if the part2word is in the processwords list or is step[s], then followedNoun == NA
    followedNoun.process.words <- c(processwords,"step","steps")
    tmp.split[which(tmp.split$part2word %in% followedNoun.process.words),"followedNoun"] <- NA
    
    # merge with original data 
    data1 <- merge(data1, tmp.split[,c("PatentClaim","followedNoun","part2word")],all.x = T);
  } else {
    data1["followedNoun"] <- NA
    data1["part2word"] <- NA
  }
  

  
  # tmp.split2 <- strsplit(tmp.split$shorta, regex.proc2)
  # 
  # # if statement: only if there are such splits
  # if (length(tmp.split2) > 0) {
  #   
  #   # extract the text following the process word
  #   for (i in 1:length(tmp.split2)) {
  #     tmp.split[i,"part2"] <- trimws(paste0(tmp.split2[[i]][c(2:length(tmp.split2[[i]]))], collapse = "PROCESS"))
  #   }
  #   
  #   # Drop all non-alphanumerical characters
  #   tmp.split["part2"] <- gsub("[^[:alnum:]]"," ",tmp.split$part2)
  #   tmp.split["part2"] <- trimws(tmp.split$part2)
  #   
  #   # the first word immediately following the process word
  #   tmp.split[which(grepl("(?i)^\\S+ ", tmp.split$part2)),"part2word"] <- 
  #     unlist(regmatches(tmp.split$part2, gregexpr("(?i)^\\S+ ", tmp.split$part2)))
  #   tmp.split["part2word"] <- tolower(tmp.split$part2word)
  #   tmp.split["part2word"] <- trimws(tmp.split$part2word)
  #   
  #   # POS-tagging of that first word
  #   for (i in 1:dim(tmp.split)[1]) {
  #     if (!is.na(tmp.split[i,"part2word"])) {
  #       tmp.split[i,"POStag"] <- fn.POStagger(tmp.split[i,"part2word"])
  #     }
  #   }
  #   
  #   # is that first word following the process word a noun?
  #   tmp.split["followedNoun"] <- NA
  #   tmp.split[which(grepl("NN",tmp.split$POStag)),"followedNoun"] <- 1
  #   
  #   # if the part2word is in the processwords list or is step[s], then followedNoun == NA
  #   followedNoun.process.words <- c(processwords,"step","steps")
  #   tmp.split[which(tmp.split$part2word %in% followedNoun.process.words),"followedNoun"] <- NA
  #   
  #   # merge with original data 
  #   data1 <- merge(data1, tmp.split[,c("PatentClaim","followedNoun","part2word")],all.x = T);
  # }
  # # end if statement

  
  ### ##### ##### ##### ##### ##### #####
  ### PREAMBLE CATEGORIZER
  
  ### PROCESS CLAIM
  # a preamble is a process preamble if
  # - has a process term (from processwords), but
  ## # - not preceded by a product word [data1$procNeg] !!!! the for and by and later the compImp should take care of this
  # - not preceded by 'for' (it's something for a process/method) [data1$forNeg]
  # - not preceded by 'by' (as in a by-process structure) [data1$byNeg]
  # - not followed by a noun (that would indicate the process word is first part in a compound)
  # moreover, a preamble is a preamble if it's a computer-implemented method structure
  data1["processVerb"] <- FALSE
  va.terms <- c("process","processes","approach","approaches","practice","practices","scheme","schemes")
  pv.rows <- which(tolower(data1$processTerm) %in% va.terms)
  if (length(pv.rows) > 0) {
    ann.pv <- as.data.frame(udpipe::udpipe_annotate(.patccat$udmodel, x = data1$short[pv.rows], doc_id = as.character(pv.rows), tokenizer = "tokenizer", tagger = "default", parser = "none"))
    for (k in seq_along(pv.rows)) {
      r <- pv.rows[k]
      aa <- ann.pv[ann.pv$doc_id == as.character(r), ]
      h <- which(tolower(aa$token) == tolower(data1$processTerm[r]))
      if (length(h)) {
        hi <- h[1]
        prevTo <- hi > 1 && tolower(aa$token[hi - 1]) == "to"
        if (grepl("^VB", aa$xpos[hi]) || prevTo) data1[r, "processVerb"] <- TRUE
      }
    }
  }

  data1["processPreamble"] <- 0;
  data1[which(!is.na(data1$processTerm) 
              #& !data1$procNeg 
              & !data1$forNeg 
              & !data1$byNeg 
              & !data1$processVerb 
              & is.na(data1$followedNoun)),"processPreamble"] <- 1;
  data1[which(data1$compImplProcess),"processPreamble"] <- 1
  
  ### PRODUCT-BY-PROCESS CLAIM
  # a by-process structure anywhere in the claim is an indicator for a by-process preamble.
  # but: if the preamble is a process preamble, then it is not a by-process preamble (then it describes: process ... by the following process)
  data1["byprocessPreamble"] <- 0; #table(data1$byprocessPreamble)
  data1[which(data1$byprocess.long),"byprocessPreamble"] <- 2; #table(data1$byprocessPreamble)
  data1[which(data1$processPreamble == 1),"byprocessPreamble"] <- 0; #table(data1$byprocessPreamble)
  
  ### PRODUCT CLAIM: same rules for single-line claims and multi-line claims
  # WE ALLOW FOR EMPTY PREAMBLE
  # a preamble is a product preamble if it uses a product word, but
  # - does not use a process word before the product word
  # - is not a compImplProcess structure
  # - is not a processPreamble
  data1["productPreamble"] <- 0;
  data1[which(!is.na(data1$productTerm) 
              & !data1$prodNeg
              & !data1$compImplProcess
              & !data1$byprocess.long
              & data1$processPreamble==0),"productPreamble"] <- 4; 
  # if it uses a by-process structure (by-process or process preamble, then it is not a product preamble)
  # data1[which(data1$byprocess.long),"productPreamble"] <- 0; #table(data1$productPreamble)
  

  ### FOR VERIFICATION
  # we can have only values 1, 2, 4
  # if there are other values, then we have double
  data1["combined"] <- data1["processPreamble"] + data1["productPreamble"] + data1["byprocessPreamble"]
  check.preamble <- names(table(data1$combined))
  if (length(setdiff(check.preamble,c("0","1","2","4")))>0) {stop("fn.preambletype: preamble flags appear double-counted (unexpected combined values)")}
  
  ### DEFINE PREAMBLE TYP
  data1["preambleType"] <- 0 # no type
  data1[which(data1$processPreamble == 1),"preambleType"] <- 1 # process preamble
  data1[which(data1$productPreamble == 4),"preambleType"] <- 2 # product preamble
  data1[which(data1$byprocessPreamble == 2),"preambleType"] <- 3 # by-process preamble
  table(data1$preambleType)
  

  ### EXTRACT THE RELEVANT TERM(s)
  if (extractPreambleTerms == T) {
    data1["preambleTerm"] <- NA
    # data1["preambleTermAlt"] <- NA
    
    ## preamble term is the captured process or product term
    data1["processTerm2"] <- stringr::str_extract(data1$short,regex.compimpl.process)
    data1[which(!is.na(data1$processTerm2)),"processTerm"] <- data1[which(!is.na(data1$processTerm2)),"processTerm2"]
    data1[which(data1$preambleType == 1),"preambleTerm"] <- 
      data1[which(data1$preambleType == 1),"processTerm"]
    
    data1["productTerm2"] <- stringr::str_extract(data1$short,regex.compimpl.product)
    data1[which(!is.na(data1$productTerm2)),"productTerm"] <- data1[which(!is.na(data1$productTerm2)),"productTerm2"]
    data1[which(data1$preambleType == 2),"preambleTerm"] <- 
      data1[which(data1$preambleType == 2),"productTerm"]
    
    data1["preambleTerm"] <- tolower(data1$preambleTerm)
    sort(table(data1$preambleTerm), decreasing=T)
    
    # ## the alternative term is the respective other term
    # data1[which(data1$preambleType == 1&!is.na(data1$pre.product.text)),"preambleTermAlt"] <- 
    #   data1[which(data1$preambleType == 1&!is.na(data1$pre.product.text)),"productTerm"]
    # data1[which(data1$preambleType == 2&!is.na(data1$pre.process.text)),"preambleTermAlt"] <- 
    #   data1[which(data1$preambleType == 2&!is.na(data1$pre.process.text)),"processTerm"]
    # data1["preambleTermAlt"] <- tolower(data1$preambleTermAlt)
  }
  
  
  ### ##### ##### ##### ##### ##### #####
  ### JEPSON PRIOR-ART HEAD OVERRIDE (J1)
  ### For Jepson (improvement) claims, the reformatted preamble is the improvement part,
  ### which usually carries no statutory keyword. Per 37 CFR 1.75(e) / MPEP 2129, the
  ### improvement is a portion of the SAME claimed combination named in the prior-art part;
  ### its statutory category is fixed by that prior-art subject (a mixed apparatus+method
  ### claim is indefinite: IPXL Holdings, MPEP 2173.05(p)). We therefore classify the preamble
  ### of a Jepson claim by the leftmost statutory keyword in the prior-art head (statusquoText),
  ### searched within the first param1.nPreamble words.
  if (all(c("statusquoText","JepsonReformat") %in% names(data1))) {
    jep.rows <- which(data1$JepsonReformat == 1 & !is.na(data1$statusquoText))
    if (length(jep.rows) > 0) {
      jep.nw <- lengths(strsplit(data1$statusquoText[jep.rows], "[ ]+"))
      jep.head <- mapply(fn.extract.text, data1$statusquoText[jep.rows], param1.nPreamble, jep.nw, USE.NAMES = FALSE)
      jep.mproc <- regexpr(regex.proc2, jep.head, perl = TRUE)
      jep.mprod <- regexpr(regex.prod2, jep.head, perl = TRUE)
      jep.pos.proc <- ifelse(jep.mproc > 0, jep.mproc, Inf)
      jep.pos.prod <- ifelse(jep.mprod > 0, jep.mprod, Inf)
      jep.headtype <- ifelse(is.finite(jep.pos.proc) & jep.pos.proc <= jep.pos.prod, 1L,
                       ifelse(is.finite(jep.pos.prod) & jep.pos.prod <  jep.pos.proc, 2L, 0L))
      rows.proc <- jep.rows[jep.headtype == 1]
      rows.prod <- jep.rows[jep.headtype == 2]
      if (length(rows.proc) > 0) data1[rows.proc, "preambleType"] <- 1
      if (length(rows.prod) > 0) data1[rows.prod, "preambleType"] <- 2
      if (extractPreambleTerms == T) {
        jep.proc.term <- tolower(stringr::str_extract(jep.head, regex.proc2))
        jep.prod.term <- tolower(stringr::str_extract(jep.head, regex.prod2))
        if (length(rows.proc) > 0) data1[rows.proc, "preambleTerm"] <- jep.proc.term[jep.headtype == 1]
        if (length(rows.prod) > 0) data1[rows.prod, "preambleTerm"] <- jep.prod.term[jep.headtype == 2]
      }
    }
  }

  # OUTPUT
  if (extractPreambleText == T) {
    tmp.fn <- select(data1,
                     PatentClaim,
                     preambleType,
                     preambleTerm,
                     # preambleTermAlt,
                     preambleTextStub)
  } else {
    tmp.fn <- select(data1,
                     PatentClaim,
                     preambleType,
                     preambleTerm#,
                     # preambleTermAlt
                     )
  }
  
  
  # table(tmp.fn$preambleType, useNA="always")
  # table(tmp.fn$preambleTerm, useNA="always")
  # table(tmp.fn$preambleTermAlt, useNA="always")
  return(tmp.fn)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.POStagger
## FUNCTION to identify the type of an individual body line

## Output: column for dataframe with POS tags in one string, sep = "|"
## 
## +++ Model: fn.POStagger() tags via udpipe, reading the model from ++++++++++++
## `.patccat$udmodel`. Call patccat_load_model() once per session to populate it
## (it downloads + caches english-ewt-ud-2.5-191206 on first use); fn.patccat
## also lazy-loads it on first classification if unset. No global `udmodel`.
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
fn.POStagger <- function(token) {
  if (length(token) != 1 || is.na(token) || !nzchar(trimws(token))) return("")
  ann <- as.data.frame(udpipe::udpipe_annotate(.patccat$udmodel, x = token, tokenizer = "tokenizer", tagger = "default", parser = "none"))
  paste(ann$xpos[!is.na(ann$xpos)], collapse = "|")
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.bodytype
## FUNCTION to identify the type the body
## data -- dataframe with claims data
## param5.nBodyline -- number of words to capture in each bodyline
## param6.nBodySteps -- number of tokens to use for determination of steps
## param7.nBodyItems -- number of tokens to use for determination of items
## param8.stepRatio -- minimum fraction of step lines for bodyType = process body = 1
## param9.itemRatio -- minimum fraction of item lines for bodyType = product body = 2

## Output: data.frame with PatentClaim and bodyType
##
fn.bodytype <- function(data,
                        param5.nBodyline,
                        param6.nBodySteps,
                        param7.nBodyItems,
                        param8.stepRatio,
                        param9.itemRatio) {

  ## WRITE REGEX expressions   
  
  ##
  # regex1
  # for the short text for capture
  regex1 <- paste0("(?i)^((?:\\S+ ){0,",param5.nBodyline,"})")
  # # regex2
  # # for the short text for capture with Said
  # regex2 <- paste0("(?i)^((?:\\S+ ){0,",param10.nBodylineSaid,"})")
  
  ##
  # regex.gerund.drop -- to drop gerund from gerund.expections
  regex.gerund.drop1 <- paste0("(?i)\\b(",paste0(gerund.exceptions,collapse="|"),")\\b")
  regex.gerund.drop2 <- paste0("(?i)^(",paste0(gerund.exceptions,collapse="|"),")\\b")
  regex.gerund.drop3 <- paste0("(?i)\\b(",paste0(gerund.exceptions,collapse="|"),")[:;,]")
  regex.gerund.drop4 <- paste0("(?i)^(",paste0(gerund.exceptions,collapse="|"),")[:;,]")
  
  ##
  # regex for -ing form
  regex.ing <- "(?i)(ing\\b|ing$|ing[:;,])"
  regex.ing.first <- "(?i)(^[a-z]+ing\\b)"
  
  ##
  # regex to find VBG POS TAG in the first param6.nBodySteps tokens
  # trim POStag down to param6.nBodySteps number of tags
  param.gerund <- param6.nBodySteps - 1
  regex.gerund <- paste0("(?i)^(([^|]{1,5}[|]){0,",param.gerund,"})(VBG)")
  regex.gerund2 <- paste0("(?i)^(([^|]{1,5}[|]){0,",param6.nBodySteps,"})")
  
  ##
  # regex to find count or determinant (CD or DT)
  regex.count <- "^(DT|CD)"
  
  ##
  # regex to find noun(s) before VBG
  regex.noun.before.VBG <- "(?i)((NN|NNS|NNP|NNPS|NP)[|])((([^|]{1,})[|]){0,3})(VBG)"

  ##
  # regex for means (anywhere in the line)
  regex.means <- "(?i)(\\bmeans\\b)"
  
  ##
  # regex for said, whereas, ... (must be at the beginning of a line)
  regex.said <- "(?i)(^(said|where[a-z]?|when|and where[a-z]?))"
  
  ##
  # regex for any noun
  param.noun <- param7.nBodyItems - 1
  regex.noun1 <- paste0("(?i)^(([^|]{1,5}[|]){0,",param.noun,"})([^|]{1,5})?")
  regex.noun2 <- "(?i)(NN|NNS|NNP|NNPS|NP)"
  # regex.noun1 <- paste0("(?i)^(([^|]{1,5}[|]){0,",param7.nBodyItems,"})")
  # regex.noun2 <- "(?i)(NN|NNS|NNP|NNPS|NP)[|]"
  
  ##
  # regex for exception rules: if VBG, then these are elements!
  # (1) VBG-NN-VBP/VBZ
  # (2) CD/DT-VBG-NN # needs nBodySteps >1
  # (3) CD/DT-JJ/JJR/JJS-NN
  # (4) CD/DT-JJ/JJR/JJS-JJ/JJR/JJS/VBG-NN # needs nBodySteps >2
  param.exception1 <- max(0, param6.nBodySteps - 1)
  param.exception2 <- max(0, param6.nBodySteps - 2)
  param.exception4 <- max(0, param6.nBodySteps - 3)
  regex.exception1 <- paste0("(?i)^(([^|]{1,5}[|]){0,",param.exception1,"})(VBG[|](NN|NNS|NNP|NNPS|NP)[|](VB[A-Z]?))")
  regex.exception2 <- paste0("(?i)^(([^|]{1,5}[|]){0,",param.exception2,"})((CD|DT)[|]VBG[|](NN|NNS|NNP|NNPS|NP))")
  #regex.exception3 <- paste0()
  regex.exception4 <- paste0("(?i)^(([^|]{1,5}[|]){0,",param.exception4,"})((CD|DT)[|](JJ[A-Z]?[|])(JJ[A-Z]?|VBG)[|](NN|NNS|NNP|NNPS|NP))")
  
  ## SELECT LINE LEVEL: level=2 if there are more level 2 than 3, otherwise level 2+3
  tmp.level2 <- filter(data, level==2)
  tmp.level3 <- filter(data, level==3)
  tmp.by <- group_by(tmp.level2, PatentClaim)
  tmp.bylevel2 <- summarise(tmp.by, count2 = n())
  tmp.by <- group_by(tmp.level3, PatentClaim)
  tmp.bylevel3 <- summarise(tmp.by, count3 = n())

  data <- merge(data, tmp.bylevel2, all.x=T)
  data <- merge(data, tmp.bylevel3, all.x=T)
  data[which(is.na(data$count2)),"count2"] <- 0
  data[which(is.na(data$count3)),"count3"] <- 0

  data["levelselect"] <- 0
  data[which(data$level == 2),"levelselect"] <- 1
  data[which(data$count3>=data$count2 & data$level == 3),"levelselect"] <- 1
  data1 <- filter(data, levelselect==1) # we should add a sequence check - to avoid level=1 inside the patent claim

  # ## SELECT LINE LEVEL: level=2
  # data1 <- filter(data, level==2) # we should add a sequence check - to avoid level=1 inside the patent claim  
  
  # ## SELECT LINE LEVEL: level=2 and 3
  # data1 <- filter(data, level %in% c(2:4)) # we should add a sequence check - to avoid level=1 inside the patent claim
  
  ## CLEAN EACH LINE
  data1["text"] <- gsub("^[(]?[0-9a-z]{1,3}[)] ","",data1$text)
  data1["text"] <- gsub("^[0-9a-z]{1,3}[.] ","",data1$text)
  data1["text"] <- gsub("^[0-9]{1,3} ","",data1$text)
  data1["text"] <- gsub("(?i)^(the |a |an |said )?(step|steps|act|acts) of ","",data1$text, perl = TRUE)
  #my.tmp <- filter(data1, level==2) %>% select(text)
  
  ## CAPTURES
  # CAPTURE THE FIRST N WORDS (by param5) AND WRITE INTO "short"
  data1["short"] <- unlist(regmatches(data1$text, gregexpr(regex1, data1$text, perl = TRUE)))
  #data1["shortSaid"] <- unlist(regmatches(data1$text, gregexpr(regex2, data1$text, perl = TRUE)))
  
  
  ## delete my modalverbs in gerund
  data1["short"] <- gsub(regex.gerund.drop1,"",data1$short, perl = TRUE)
  data1["short"] <- gsub(regex.gerund.drop2,"",data1$short, perl = TRUE)
  data1["short"] <- gsub(regex.gerund.drop3,"",data1$short, perl = TRUE)
  data1["short"] <- gsub(regex.gerund.drop4,"",data1$short, perl = TRUE)
  
  ## IS IT USING ING FORM? 
  # first select all those with ing\\b in the text, then run POStagger (in case the tagger finds other things)
  data1["useIng"] <- grepl(regex.ing, data1$short, perl = TRUE)
  data1["useIngFirst"] <- grepl(regex.ing.first, data1$short, perl = TRUE)
  #table(data1$useIng)

  ## IS IT GERUND (POS TAG VBG)?
  
  # USE the udpipe POS tagger (via fn.POStagger)
  # THIS ONE IS TIME CONSUMING
  for (i in seq_along(data1$short)) {
    #print(i)
    if (data1[i,"short"] == "" | data1[i,"short"] == " ") {
      data1[i,"POStags"] <- ""
    } else {
      data1[i,"POStags"] <- fn.POStagger(data1[i,"short"])
    }
  }
  
  # use the gerund in the first param6.nBodySteps tokens 
  data1["POStags2"] <- gsub("-LRB-(.*)+-RRB-[|]","",data1$POStags) # DROP parantheses (still necessary after gsub earlier?)
  data1["POStags2"] <- gsub("[A-Z0-9]{1,3}[|][.][|]","",data1$POStags2) # DROP enumeration with dot (still necessary after gsub earlier?)
  data1["useGerund"] <- grepl(regex.gerund,data1$POStags2, perl = TRUE)
  
  ## DOES IT USE A COUNTER OR DETERMINANT IN THE BEGINNING?
  data1["useCount"] <- grepl(regex.count,data1$POStags2)
    
  ## DOES IT USE A NOUN BEFORE A VBG?
  data1[which(!is.na(data1$POStags2)),"POStags3"] <- unlist(regmatches(data1$POStags2, gregexpr(regex.gerund2, data1$POStags2, perl = TRUE)))
  data1["useNounVBG"] <- grepl(regex.noun.before.VBG,data1$POStags3, perl=TRUE)
  
  ## DOES IT USE MEANS?
  data1["useMeans"] <- grepl(regex.means,data1$short, perl = TRUE)
  
  ## DOES IT USE SAID, WHEREx?
  data1["useSaid"] <- grepl(regex.said,data1$short, perl = TRUE)
  #my.tmp <- select(data1, short, useSaid); my.tmp[which(!my.tmp$useSaid),"useSaid"] <- NA
  
  ## DOES IT USE A NOUN?
  data1[which(!is.na(data1$POStags2)),"POStags4"] <- unlist(regmatches(data1$POStags2, gregexpr(regex.noun1, data1$POStags2, perl = TRUE)))
  data1["useNoun"] <- grepl(regex.noun2,data1$POStags4, perl = TRUE)
  
  ## EXCEPTIONS TO GERUND RULES:
  data1["useException1"] <- grepl(regex.exception1,data1$POStags2, perl = TRUE); table(data1$useException1)
  data1["useException2"] <- grepl(regex.exception2,data1$POStags2, perl = TRUE); table(data1$useException2)
  data1["useException4"] <- grepl(regex.exception4,data1$POStags2, perl = TRUE); table(data1$useException4)
    
  ## DEFINING LINE TYPES
  
  # any line that uses the term "means" is a means-plus-function line - these lines cannot be steps!
  data1["isMeans"] <- 0; data1[which(data1$useMeans),"isMeans"] <- 1; 
  
  # any line that uses said, whereas and other things  is isSaid; neither Step nor Item
  data1["isSaid"] <- 0; data1[which(data1$useSaid),"isSaid"] <- 2;   
  
  # What is a step?
  # use an -ing word ending (just to address false positive VBG tags)
  # use a gerund (by VBG POS tag) - to address false positives from -ing ending
  # BUT: not means
  # BUT: not Said
  # BUT: does not begin without count or determinant (useCount)
  # BUT: does not have noun before the VBG
  # BUT: not one of the exceptions
  # OR: the first word ends in -ing
  data1["isStep"] <- 0; data1[which((data1$useIng
                                    & data1$useGerund 
                                    & !data1$useMeans 
                                    & !data1$useSaid 
                                    & !data1$useCount 
                                    & !data1$useNounVBG
                                    & !data1$useException1
                                    & !data1$useException2
                                    & !data1$useException4
                                    )|(data1$useIngFirst 
                                      & !data1$useMeans)
                                    ),"isStep"] <- 4

  # What is an item?
  # use a noun
  # BUT: not Said
  # BUT: not Step
  # BUT: not using -ing first
  # or MEANS
  data1["isItem"] <- 0; data1[which((!data1$isStep
                                    & !data1$useSaid
                                    & (data1$useNoun|data1$useNounVBG)
                                    & !data1$useIngFirst
                                    )|data1$useMeans
                                    |data1$useException1
                                    |data1$useException2
                                    |data1$useException4
                                    ),"isItem"] <- 8
  
  # do check the output; do we have multiples?
  # we should not have =5, =6, =12
  data1["combined"] <- data1$isMeans + data1$isSaid + data1$isStep + data1$isItem 
  table(data1$combined)
  
  ## SOME MANUAL ADDITIONS
  # a housing
  data1[which(data1$combined == 0),"useNoun"] <- grepl("(?i)a (\\S+ ){0,5}housing",data1[which(data1$combined == 0),"text"], perl=TRUE)
  data1["isItem"] <- 0; data1[which((!data1$isStep
                                     & !data1$useSaid
                                     & (data1$useNoun|data1$useNounVBG)
                                     & !data1$useIngFirst) | data1$useMeans | data1$useException1 | data1$useException2 | data1$useException4),"isItem"] <- 8
  data1["combined"] <- data1$isMeans + data1$isSaid + data1$isStep + data1$isItem 
  table(data1$combined)
  
  tmp <- filter(data1, useIng, !useMeans) %>% select(short,
                                                     isMeans,
                                                     isSaid,
                                                     isStep,
                                                     isItem,
                                                     useGerund,
                                                     useCount,
                                                     useNounVBG,
                                                     useSaid,
                                                     POStags,
                                                     POStags2,
                                                     POStags3,
                                                     POStags4)
  
  ## DEFINE THE LINE TYPE
  # =1 is step; =2 is item; =3 is said
  data1["lineType"] <- 0
  data1[which(data1$isStep == 4),"lineType"] <- 1
  data1[which(data1$isItem == 8),"lineType"] <- 2
  data1[which(data1$isSaid == 2),"lineType"] <- 3
  table(data1$lineType)
  my.tmp <- select(data1, text, POStags, short, lineType)#; my.tmp[which(!my.tmp$useSaid),"useSaid"] <- NA
  my.tmp <- filter(data1, lineType == 0)
  
  ## MAKE bodyType
  data1[which(data1$isStep == 4),"isStep"] <- 1
  data1[which(data1$isItem == 8),"isItem"] <- 1
  
  data2 <- group_by(data1, PatentClaim)
  data2 <- summarise(data2,
                     nStep = sum(isStep),
                     nElement = sum(isItem),
                     nTotal = n())
  
  data2["fractionStep"] <- data2$nStep/(data2$nStep + data2$nElement)
  data2["fractionElement"] <- data2$nElement/(data2$nStep + data2$nElement)
  table(data2$fractionStep, useNA = "always")
  
  if (param8.stepRatio + param9.itemRatio > 1) {
    data2["bodyType"] <- 0 # mixed body
    data2[which(data2$fractionStep >= param8.stepRatio),"bodyType"] <- 1 # process body
    data2[which(data2$fractionElement >= param9.itemRatio),"bodyType"] <- 2 # product body
    data2[which(data2$nStep + data2$nElement == 0),"bodyType"] <- 3 # empty body
  } else {
    data2["bodyType"] <- NA
    print("Revise the step and item ratios. Have to be larger than unity!")
  }
  
  
  tmp.df <- select(data2,
                   PatentClaim,
                   nStep,
                   nElement,
                   nTotal,
                   bodyType)
  colnames(tmp.df)[colnames(tmp.df) == "nStep"] <- "bodyLinesStep"
  colnames(tmp.df)[colnames(tmp.df) == "nElement"] <- "bodyLinesElement"
  colnames(tmp.df)[colnames(tmp.df) == "nTotal"] <- "bodyLinesTotal"
  table(tmp.df$bodyType)
  
  return(tmp.df)

}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.beauregard
## FUNCTION to identify Beauregard claims

## Output: 
## 
fn.beauregard <- function(data) {
  
  # Data is the output from fn.preamble and fn.bodytype
  # Contains: PatentClaim, preambleType, bodyType, preambleTerm, preambleTextStub
  # We take two steps/versions:
  # (1) if extractPreambleText
  # (2) if extractPreambleText is on, then a longer version
  tmp <- data
  
  #### RULES
  # a claim is Beauregard claim if:
  # - it is a Pm claim or Em label, or Px label (with stepRatio >= 0.5) or Ex label (with stepRatio >= 0.5)
  # - and uses 'machine-readable (and similar) + bla' structures (where bla are specific terms in beauregard words)
  # - or uses bla + machine-readable (and similar)
  # - or uses bla + "readable by"
  
  
  ### (1) claimlabel
  tmp["claimLabel"] <- 0; table(tmp$claimLabel, useNA="always")
  # Pm label
  tmp[which(tmp$preambleType == 2  & tmp$bodyType == 1),"claimLabel"] <- 1; table(tmp$claimLabel, useNA="always")
  # Em label
  tmp[which(tmp$preambleType == 0  & tmp$bodyType == 1),"claimLabel"] <- 1; table(tmp$claimLabel, useNA="always")
  # Px label with stepRatio >= 0.5
  tmp[which(tmp$preambleType == 2  & tmp$bodyType == 0 & tmp$bodyLinesStep/(tmp$bodyLinesStep+tmp$bodyLinesElement) >= 0.5 ),"claimLabel"] <- 1; table(tmp$claimLabel, useNA="always")
  # Ex label with stepRatio >= 0.5
  tmp[which(tmp$preambleType == 0  & tmp$bodyType == 0 & tmp$bodyLinesStep/(tmp$bodyLinesStep+tmp$bodyLinesElement) >= 0.5 ),"claimLabel"] <- 1; table(tmp$claimLabel, useNA="always")
  
  ### (2) the machine-readable part:
  tmp["machineReadable"] <- 0
  
  ### (2.a1) machine-readable + bla [in the preambleTerm -- we have done good preliminary work on these preambleTerms]
  # regex for "computer readable bla" (it is taken directly from fn.preambletype)
  # maybe: system, machien, assembly, article, product, manufacture, sensor, mechanism, module
  beauregards.words <- c("medium","media","device[s]?","apparatus","apparatuses","component[s]?","system[s]?")
  beauregard.regex.a <- paste0("(?i)((machine|computer|processor|device|microprocessor|system)[- ](readable|implemented|implementable|contro[l]+ed|accessible|executable|based|usable|recorded|assisted|aided|effected|directed|stored|recognizable|accessible|readably|readeable|stored|automated|enabled|searchable|executed|facilitated|useable|generated|centered|operated)[ ]+)(storage )?(",paste(beauregards.words,collapse="|"),")\\b")
  tmp[which(grepl(beauregard.regex.a,tmp$preambleTerm, perl = TRUE)),"machineReadable"] <- 1; table(tmp$machineReadable)
  
  ### (2.a2) machine-readable + bla [in the preambleTextStub]
  tmp[which(grepl(beauregard.regex.a,tmp$preambleTextStub, perl = TRUE)),"machineReadable"] <- 1; table(tmp$machineReadable)
  
  ### (2.b) bla + machine-readable[in the preambleTextStub]
  beauregard.regex.b <- paste0("(?i)(storage )?(",paste(beauregards.words,collapse="|"),")(.*)?((machine|computer|processor|device|microprocessor|system)[- ](readable|implemented|implementable|contro[l]+ed|accessible|executable|based|usable|recorded|assisted|aided|effected|directed|stored|recognizable|accessible|readably|readeable|stored|automated|enabled|searchable|executed|facilitated|useable|generated|centered|operated))\\b")
  tmp[which(grepl(beauregard.regex.b,tmp$preambleTextStub, perl = TRUE)),"machineReadable"] <- 1; table(tmp$machineReadable)
  
  ### (2.c) bla + 'readable by'[in the preambleTextStub]
  beauregard.regex.c <- paste0("(?i)(storage )?(",paste(beauregards.words,collapse="|"),")(.*)?(readable by)\\b")
  tmp[which(grepl(beauregard.regex.c,tmp$preambleTextStub, perl = TRUE)),"machineReadable"] <- 1; table(tmp$machineReadable)

  ### (2.d) computer-program-product / program-storage-device forms (canonical Beauregard, no '-readable' modifier)
  beauregard.regex.d <- "(?i)(computer[- ]?program product|program storage device|program product|article of manufacture (comprising|having) (a )?(computer|machine))"
  tmp[which(grepl(beauregard.regex.d, tmp$preambleTextStub, perl = TRUE)),"machineReadable"] <- 1
  tmp[which(grepl(beauregard.regex.d, tmp$preambleTerm,      perl = TRUE)),"machineReadable"] <- 1

  # (e) storage/recording MEDIUM as the claimed article (HEAD of the preamble), even without an explicit "computer-readable" modifier
  #     e.g. "a storage medium storing a program", "a computer storage medium having instructions" (validated on the 115k archive sample: +88, all genuine Beauregards).
  #     Anchored at the head so "an apparatus comprising a storage medium ..." is NOT caught.
  beauregard.regex.e <- "(?i)^\\s*([0-9]+\\s*\\.?\\s*)?(a |an |the |said )?(non[- ]?transitory )?(computer[- ]?|machine[- ]?|data |information )?(storage|recording) (medium|media)\\b"
  tmp[which(grepl(beauregard.regex.e, tmp$preambleTextStub, perl = TRUE)),"machineReadable"] <- 1

  ### (2.e) code/instructions cue -- the software-vs-physical-media discriminator (replaces the body gate)
  beauregard.codecue <- "(?i)(instructions|program|code|software|logic|routine|executable|when executed|encoded|stored thereon|data structure|for causing|for controlling|for performing|for directing)"
  tmp["codeCue"] <- grepl(beauregard.codecue, tmp$preambleTextStub, perl = TRUE) | grepl(beauregard.codecue, tmp$preambleTerm, perl = TRUE)
  
  ### (3) a Beauregard claim:
  tmp["beauregard"] <- 0
  tmp[which(tmp$machineReadable == 1 & tmp$codeCue & tmp$preambleType %in% c(0,2)),"beauregard"] <- 1   # body-agnostic; exclude method/by-process preambles
  table(tmp$beauregard, useNA="always")
  
  # select for output
  tmp.df <- select(tmp,
                PatentClaim,
                beauregard)
  
  return(tmp.df)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.claimtype.single
## FUNCTION to identify the type the claim of single-line claims that cannot be reformated
## data -- dataframe with claims data
## processwords -- vector of keywords for product preamble (here: for entire claim)
## productwords -- vector of keywords for product preamble (here: for entire claim)
## param3.nPreambleSingle,
## param4.nPreambleSingleByProcess
## param10.nbyGap

## Output: data.frame with columns PatentClaim and claimType
## 
fn.claimtype.single <- function(data, 
                                processwords,
                                productwords,
                                param3.nPreambleSingle,
                                param4.nPreambleSingleByProcess,
                                param10.nbyGap,
                                extractPreambleTerms=TRUE,
                                extractPreambleText=FALSE) {
  
  tmp.fn <- fn.preambletype(data=data,
                            processwords = processwords,
                            productwords = productwords,
                            param1.nPreamble = param3.nPreambleSingle,
                            param2.nPreambleByProcess = param4.nPreambleSingleByProcess,
                            param10.nbyGap = param10.nbyGap,
                            singleLine = T,
                            extractPreambleTerms = extractPreambleTerms,
                            extractPreambleText = extractPreambleText)
  
  # NOTE: single-line claims (no Pm labels!) cannot be classified as Beauregards following our definition
  if (extractPreambleTerms==T & extractPreambleText==T) {
    colnames(tmp.fn) <- c("PatentClaim",
                          "preambleType",
                          "preambleTerm",
                          # "preambleTermAlt",
                          "preambleTextStub")
    tmp.fn["claimType"] <- tmp.fn$preambleType
    tmp.fn["beauregard"] <- NA
    tmp.fn <- select(tmp.fn,
                     PatentClaim,
                     claimType,
                     beauregard,
                     preambleType,
                     preambleTerm,
                     preambleTextStub)
  } else if (extractPreambleTerms==T & extractPreambleText==F) {
    colnames(tmp.fn) <- c("PatentClaim",
                          "preambleType",
                          "preambleTerm"
                          # "preambleTermAlt"
                          )
    tmp.fn["claimType"] <- tmp.fn$preambleType
    tmp.fn["beauregard"] <- NA
    tmp.fn <- select(tmp.fn,
                     PatentClaim,
                     claimType,
                     beauregard,
                     preambleType,
                     preambleTerm)
    
  } else if (extractPreambleTerms==F & extractPreambleText==T) {
    colnames(tmp.fn) <- c("PatentClaim",
                          "preambleType",
                          "preambleTextStub")
    tmp.fn["claimType"] <- tmp.fn$preambleType
    tmp.fn["beauregard"] <- NA
    tmp.fn <- select(tmp.fn,
                     PatentClaim,
                     claimType,
                     beauregard,
                     preambleType,
                     preambleTextStub)
    
  } else if (extractPreambleTerms==F & extractPreambleText==F) {
    colnames(tmp.fn) <- c("PatentClaim",
                          "preambleType")
    tmp.fn["claimType"] <- tmp.fn$preambleType
    tmp.fn["beauregard"] <- NA
    tmp.fn <- select(tmp.fn,
                     PatentClaim,
                     claimType,
                     beauregard,
                     preambleType)
  }
  
  
  ## Product-by-process (single-line, by-process preamble): base type PRODUCT + flag (parallel to Beauregard)
  tmp.fn["prodByProcess"] <- 0
  tmp.fn[which(tmp.fn$claimType == 3),"prodByProcess"] <- 1
  tmp.fn[which(tmp.fn$claimType == 3),"claimType"]     <- 2

  return(tmp.fn)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.claimtype
## FUNCTION to identify the type of the claim: combining preambleType and bodyType
## also identifies Beauregard claims: they need preambleType and and bodyType, and are needed for claim classification
## data -- dataframe with claims data
## param1.nPreamble -- number of words to use to search for process and product words
## param2.nPreambleByProcess -- number of words to use to search for by-process structure
## param5.nBodyline -- number of words to capture in each bodyline
## param6.nBodySteps -- number of tokens to use for determination of steps
## param7.nBodyItems -- number of tokens to use for determination of items
## param8.stepRatio -- minimum fraction of step lines for bodyType = process body = 1
## param9.itemRatio -- minimum fraction of item lines for bodyType = product body = 2
## param10.nbyGap
## claim.rules -- a matrix with the claim rules

## Output: data.frame with variables PatentClaim, claimtype, preambleType, and bodyType
## 
fn.claimtype <- function(data,
                         processwords,
                         productwords,
                         param1.nPreamble,
                         param2.nPreambleByProcess,
                         param3.nPreambleSingle,
                         param4.nPreambleSingleByProcess,
                         param5.nBodyline,
                         param6.nBodySteps,
                         param7.nBodyItems,
                         param8.stepRatio,
                         param9.itemRatio,
                         param10.nbyGap,
                         claim.rules,
                         extractPreambleTerms=TRUE,
                         extractPreambleText=TRUE) {
  
  
  ## REGULAR CLAIMS
  # data.regular <- filter(data, singleLine==0)
  data.regular <- data
  
  # BODY TYPE
  tmp.fn <- fn.bodytype(data = data.regular,
                        param5.nBodyline = param5.nBodyline,
                        param6.nBodySteps = param6.nBodySteps,
                        param7.nBodyItems = param7.nBodyItems,
                        param8.stepRatio = param8.stepRatio,
                        param9.itemRatio = param9.itemRatio)
  bodytype.regular <- tmp.fn; table(bodytype.regular$bodyType, useNA = "always")
  
  # PREAMBLE TYPE
  tmp.fn <- fn.preambletype(data = data.regular,
                            processwords = processwords,
                            productwords = productwords,
                            param1.nPreamble = param1.nPreamble,
                            param2.nPreambleByProcess = param2.nPreambleByProcess,
                            param10.nbyGap = param10.nbyGap,
                            singleLine = F,
                            extractPreambleTerms = extractPreambleTerms,
                            extractPreambleText = extractPreambleText)
  preambletype.regular <- tmp.fn; table(preambletype.regular$preambleType)
  
  ## B2: keep body-less regular claims (no level-2/3 line) instead of dropping them via an
  ## inner join; classify them from the preamble alone as empty body (bodyType = 3), which
  ## the rule matrix already handles. Only genuinely body-less claims are set to 3.
  bodyless.claims <- setdiff(preambletype.regular$PatentClaim, bodytype.regular$PatentClaim)
  preamble.body.combinations <- merge(bodytype.regular, preambletype.regular, all.y = TRUE)
  preamble.body.combinations[preamble.body.combinations$PatentClaim %in% bodyless.claims, "bodyType"] <- 3
  
  ### BEAUREGARD CLAIMS
  # Must come before the final claim categorizer; Beauregard claims (article of manufacture) are set to product;
  # a Beauregard claim overrides other categorizations (here, in this case, a product-by-process claim)
  beauregard.df <- fn.beauregard(data = preamble.body.combinations)
  
  # merge with the preamble-body combinations df
  preamble.body.combinations <- merge(preamble.body.combinations, beauregard.df, all.x=T)
  
  # MAKE CLAIM TYPE
  # 0 not known
  # 1 is process
  # 2 is product
  # 3 is product-by-process
  # use .patccat$claim.rules; but: Beauregard claims are process claims.
  claimtype.regular <- preamble.body.combinations
  table(claimtype.regular$bodyType, useNA = "always")
  table(claimtype.regular$preambleType, useNA = "always")
  
  claimtype.regular["claimType"] <- NA
  
  ## CLAIM RULES!
  
  # BEGIN LOOP
  for (i in 1:dim(claimtype.regular)[1]) {
    claimtype.regular[i,"claimType"] <- claim.rules[as.character(claimtype.regular[i,"bodyType"]),as.character(claimtype.regular[i,"preambleType"])]
  }
  # END LOOP
  table(claimtype.regular$claimType,useNA="always")
  
  ## Beauregard claims are process claims
  claimtype.regular[which(claimtype.regular$beauregard == 1),"claimType"] <- 2   # Beauregard = article of manufacture = PRODUCT (MPEP 2113); flag kept separately

  ## Product-by-process: statutory base type is PRODUCT (MPEP 2113); tracked via a separate flag (parallel to Beauregard)
  claimtype.regular["prodByProcess"] <- 0
  claimtype.regular[which(claimtype.regular$claimType == 3),"prodByProcess"] <- 1
  claimtype.regular[which(claimtype.regular$claimType == 3),"claimType"]     <- 2
  
  claimtype.regular <- select(claimtype.regular, 
                              PatentClaim, 
                              claimType,
                              beauregard,
                              prodByProcess,
                              preambleType,
                              preambleTerm,
                              # preambleTermAlt,
                              preambleTextStub,
                              bodyType,
                              bodyLinesStep,
                              bodyLinesElement,
                              bodyLinesTotal)
  
  # DROP extra columns
  if (extractPreambleText==F) {
    claimtype.regular["preambleTextStub"] <- NULL
  }
  if (extractPreambleTerms==F) {
    claimtype.regular["preambleTerm"] <- NULL
    # claimtype.regular["preambleTermAlt"] <- NULL
  }
  
  claimtype <- arrange(claimtype.regular, PatentClaim)
  
  # OUTPUT
  return(claimtype)
  
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 
## fn.process.simple
## FUNCTION to identify a simple process claim (from preamble or full text)
## data -- dataframe with claims data
## processwords -- vector of keywords for process preamble - for the simple version
## singleLine -- is the the claim a single-line claim?

## Output: dataframe with PatentClaim, processPreamble, processBody, processSimple
##
fn.process.simple <- function(data,
                              processwords,
                              singleLine) {
  
  # reformat processwords vector and turn into Regex
  regex <- paste0("(?i)\\b(",paste(processwords,collapse="|"),")\\b")
  
  if (singleLine == F) {
    ## PREAMBLE
    # text of preamble only
    data1 <- filter(data, level==1) # we should add a sequence check - to avoid level=1 inside the patent claim
    data1["proc"] <- grepl(regex,data1$text,perl=T)
    
    tmp1 <- group_by(data1, PatentClaim)
    tmp1 <- summarise(tmp1, procs = sum(proc, na.rm=T))
    tmp1["processPreamble"] <- 0; tmp1[which(tmp1$procs>=1),"processPreamble"] <- 1
    tmp1["procs"] <- NULL
    
    ## BODY
    # text of body only
    data2 <- filter(data, level>1) # we should add a sequence check - to avoid level=1 inside the patent claim
    data2["proc"] <- grepl(regex,data2$text,perl=T)
    
    tmp2 <- group_by(data2, PatentClaim)
    tmp2 <- summarise(tmp2, procs = sum(proc, na.rm=T))
    tmp2["processBody"] <- 0; tmp2[which(tmp2$procs>=1),"processBody"] <- 1
    tmp2["procs"] <- NULL
    
    ## FULL CLAIM
    tmp.fn <- merge(tmp1,tmp2, all=T)
    tmp.fn$processPreamble[is.na(tmp.fn$processPreamble)] <- 0   # B8: a missing process flag is 0, not NA (a body-less claim keeps its preamble signal)
    tmp.fn$processBody[is.na(tmp.fn$processBody)] <- 0
    tmp.fn["sum"] <- tmp.fn$processPreamble + tmp.fn$processBody
    tmp.fn["processSimple"] <- 0; tmp.fn[which(tmp.fn$sum >= 1),"processSimple"] <- 1
    tmp.fn <- select(tmp.fn, PatentClaim, processPreamble, processBody, processSimple)
    
  } else {
    ## PREAMBLE
    # text of preamble only
    data1 <- filter(data, level==1) # we should add a sequence check - to avoid level=1 inside the patent claim
    data1["proc"] <- grepl(regex,data1$text,perl=T)
    
    tmp1 <- group_by(data1, PatentClaim)
    tmp1 <- summarise(tmp1, procs = sum(proc, na.rm=T))
    tmp1["processPreamble"] <- 0; tmp1[which(tmp1$procs>=1),"processPreamble"] <- 1
    tmp1["procs"] <- NULL
    
    ## FULL CLAIM
    tmp.fn <- tmp1
    tmp.fn["sum"] <- tmp.fn$processPreamble# + tmp.fn$processBody
    tmp.fn["processSimple"] <- 0; tmp.fn[which(tmp.fn$sum >= 1),"processSimple"] <- 1
    
    tmp.fn["processBody"] <- NA
    tmp.fn <- select(tmp.fn, PatentClaim, processPreamble, processBody, processSimple)
    
  }
                  
  return(tmp.fn)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## fn.means
## FUNCTION to identify means-plus-function claims
## data -- dataframe with claims data
## linesOnly -- flag if only the line-wise output is needed; if FALSE, output is a list

## Output: a list
##
fn.means <- function(data,
                     linesOnly) {
  
  # regex for means
  # regex for means-plus-function (35 USC 112(f)): count "means" ELEMENTS, excluding the prepositional "by means of"
  regex.means  <- "(?i)\\bmeans\\b"
  regex.bymean <- "(?i)by means(?! (for|to)\\b)"   # prepositional "by means [of/whereof/comprising/...]"; keeps "by means for/to [function]"
  
  # line-by-line: number of 112(f) means limitations = (# "means") - (# "by means of")
  data.lines <- data
  n.means  <- lengths(regmatches(data.lines$text, gregexpr(regex.means,  data.lines$text, perl = TRUE)))
  n.bymean <- lengths(regmatches(data.lines$text, gregexpr(regex.bymean, data.lines$text, perl = TRUE)))
  data.lines["meansCountLine"] <- pmax(0, n.means - n.bymean)
  data.lines["isMeansLine"]    <- as.integer(data.lines$meansCountLine >= 1)
  data.lines <- select(data.lines,
                       PatentClaim,
                       sequence,
                       level,
                       isMeansLine,
                       meansCountLine)
  
  if (linesOnly) {
    return(data.lines)
  } else {
    # count by preamble and by body (robust: 0 not NA when a part is absent; isMeans stays consistent with meansCount)
    df1 <- summarise(group_by(filter(data.lines, level==1), PatentClaim), isMeansPreamble = as.integer(sum(isMeansLine, na.rm=T) >= 1), .groups="drop")
    df2 <- summarise(group_by(filter(data.lines, level>1),  PatentClaim), isMeansBody     = as.integer(sum(isMeansLine, na.rm=T) >= 1), .groups="drop")
    # count of 112(f) means limitations per claim (all lines)
    df3 <- summarise(group_by(data.lines, PatentClaim), meansCount = sum(meansCountLine, na.rm=T), .groups="drop")
    
    # assemble on the full claim list (df3 has every claim); absent preamble/body -> 0
    data.parts <- merge(merge(df3, df1, all.x=T), df2, all.x=T)
    data.parts[which(is.na(data.parts$isMeansPreamble)),"isMeansPreamble"] <- 0
    data.parts[which(is.na(data.parts$isMeansBody)),"isMeansBody"] <- 0
    data.parts["isMeans"] <- as.integer(data.parts$meansCount >= 1)
    
    # output
    tmp.list <- list("byline" = data.lines,
                     "bypart" = data.parts)
    return(tmp.list)
  }
  
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   



# ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
# ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
# ## fn.combination
# ## FUNCTION to identify in-combination phrases in preamble and body
# ## data -- dataframe with claims data
# ## linesOnly -- flag if only the line-wise output is needed; if FALSE, output is a list
# 
# ## Output: 
# ##
# fn.combination <- function(data,
#                            linesOnly) {
#   
#   # regex for incombination
#   regex.combination <- "(?i)(\\bin combination\\b)"
#   
#   # line-by-line regex looking for term 'in combination'
#   # this function captures 'means' in the entire text of the respective line!!!
#   data.lines <- data
#   data.lines["isCombination"] <- grepl(regex.combination,data.lines$text)
#   data.lines["isCombinationLine"] <- 0; data.lines[which(data.lines$isCombination),"isCombinationLine"] <- 1
#   data.lines <- select(data.lines,
#                        PatentClaim,
#                        sequence,
#                        level,
#                        isCombinationLine)
#   
#   if (linesOnly) {
#     return(data.lines)
#   } else {
#     # count by preamble and by body
#     
#     # means in the preamble
#     data0 <- filter(data.lines, level==1) # we should add a sequence check - to avoid level=1 inside the patent claim
#     df1 <- group_by(data0, PatentClaim)
#     df1 <- summarise(df1, isCombinationPreamble = sum(isCombinationLine,na.rm=T))
#     df1[which(df1$isCombinationPreamble >=1),"isCombinationPreamble"] <- 1
#     # table(df1$isCombinationPreamble)
#     
#     # means in the body
#     data0 <- filter(data.lines, level>1) # we should add a sequence check - to avoid level=1 inside the patent claim
#     df2 <- group_by(data0, PatentClaim)
#     df2 <- summarise(df2, isCombinationBody = sum(isCombinationLine,na.rm=T))
#     df2[which(df2$isCombinationBody >=1),"isCombinationBody"] <- 1
#     # table(df2$isCombinationBody)
#     
#     # data-frame
#     data.parts <- merge(df1,df2,all=T)
#     data.parts["isCombination"] <- data.parts$isCombinationPreamble + data.parts$isCombinationBody
#     data.parts[which(data.parts$isCombination>=1),"isCombination"] <- 1
#     data.parts[which(is.na(data.parts$isCombinationBody)),"isCombination"] <- data.parts[which(is.na(data.parts$isCombinationBody)),"isCombinationPreamble"]
#     
#     # output
#     tmp.list <- list("byline" = data.lines,
#                      "bypart" = data.parts)
#     return(tmp.list)
#   }
#   
# }
# ## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## fn.textlength
## FUNCTION to calculate text length of preamble and body 
## data -- dataframe with claims data
## patentClaimID -- column name for the patent-claim identifier (patent number-claim number)
## levelID -- column name for the level identifier

## Output: dataframe with columns PatentClaim, lengthPreamble, lengthBody
##
fn.textlength <- function(data) {
  
  # Is 'level' in data?
  if(!"level" %in% colnames(data)) {data["level"] <- 1}
  
  # count characters of all lines
  data["nchar"] <- nchar(data$text)
  
  # length preamble
  data0 <- filter(data, level==1) # we should add a sequence check - to avoid level=1 inside the patent claim
  df1 <- group_by(data0, PatentClaim)
  df1 <- summarise(df1, lengthPreamble = sum(nchar,na.rm=T))
  
  # length body
  data0 <- filter(data, level>1) # we should add a sequence check - to avoid level=1 inside the patent claim
  df2 <- group_by(data0, PatentClaim)
  df2 <- summarise(df2, lengthBody = sum(nchar,na.rm=T))
  
  # data-frame
  df <- merge(df1,df2,all=T)
  
  # output data
  return(df)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## fn.wordcount
## FUNCTION to calculate word count of preamble and body 
## data -- dataframe with claims data
## patentClaimID -- column name for the patent-claim identifier (patent number-claim number)
## levelID -- column name for the level identifier

## Output: dataframe with columns PatentClaim, lengthPreamble, lengthBody
##
fn.wordcount <- function(data) {
  
  # WORD COUNT: separate for preamble and body
  
  # https://stackoverflow.com/questions/8920145/count-the-number-of-all-words-in-a-string
  # require(stringr)
  # nwords <- function(string, pseudo=F){
  #   ifelse( pseudo, 
  #           pattern <- "\\S+", 
  #           pattern <- "[[:alpha:]]+" 
  #   )
  #   str_count(string, pattern)
  # }
  # 
  # nwords("one,   two three 4,,,, 5 6")
  # # 3
  # 
  # nwords("one,   two three 4,,,, 5 6", pseudo=T)
  # # 6
  
  # str_count("How many words are in this sentence", '\\w+')
  
  # Is 'level' in data?
  if(!"level" %in% colnames(data)) {data["level"] <- 1}
  
  # count words in all lines
  data["wordcount"] <- str_count(data$text, '\\w+')
  
  # length preamble
  data0 <- filter(data, level==1) # we should add a sequence check - to avoid level=1 inside the patent claim
  df1 <- group_by(data0, PatentClaim)
  df1 <- summarise(df1, wordsPreamble = sum(wordcount,na.rm=T))
  
  # length body
  data0 <- filter(data, level>1) # we should add a sequence check - to avoid level=1 inside the patent claim
  df2 <- group_by(data0, PatentClaim)
  df2 <- summarise(df2, wordsBody = sum(wordcount,na.rm=T))
  
  # data-frame
  df <- merge(df1,df2,all=T)
  
  # output data
  return(df)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## fn.dependent.claims
## FUNCTION to count the number of dependent claims of a given independent claim
## data -- dataframe with claims data

## Output: dataframe with columns PatentClaim, dependentClaims
##
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%
## fn.fixdependency
## RAW-STAGE repair of two upstream dependency-parse errors, run in script 07 right after
## ingest -- BEFORE the independent selection and fn.dependent.claims ("before we count and
## later drop dependent claims"). See _UPSTREAM-data-integrity-issues.md (A1, A4).
##
##  (1) BODY line (level > 1) with dependency == 0. Invalid: a body line depends on the
##      preamble or a preceding body line, never on nothing (dependency == 0 means "root /
##      independent", which only the level-1 line may be). Repaired to the preceding line's
##      sequence. Consequence: a genuinely-dependent claim (level-1 dep > 0) no longer carries
##      a spurious dep == 0 line, so the independent selection (filter dependency == 0) stops
##      leaking it into the classified set. Flag: integ_bodyDepZero.
##
##  (2) LEVEL-1 line with dependency == 0 whose preamble refers back to a preceding claim
##      ("the apparatus of claim 1, wherein ..", "a process as claimed in claim 2 ..") -- an
##      upstream mis-set independence flag: the claim is really dependent. Repaired to the
##      referenced claim's level-1 sequence (resolved within the patent by claim number;
##      sentinel -1 when the number is unrecoverable -> excluded from the independent set and
##      from the dependent count, but still present in the claim total). Flag: integ_falseIndependent.
##
## dependency is NOT read by the classifier (fn.patccat / fn.reformatdata / splitters); it is
## used only by the independent selection and, at level == 1, by fn.dependent.claims -- so these
## repairs do not touch claim typing. Conservative and a no-op if the raw columns are absent.
fn.fixdependency <- function(data,
                             varPatentClaim = "PatentClaim",
                             varLevel       = "level",
                             varSequence    = "sequence",
                             varDependency  = "dependency",
                             varText        = "text") {
  req <- c(varPatentClaim, varLevel, varSequence, varDependency, varText)
  if (!all(req %in% names(data))) return(data)
  if (nrow(data) == 0) { data$integ_bodyDepZero <- integer(0); data$integ_falseIndependent <- integer(0); return(data) }

  d <- data
  d$.pc  <- as.character(d[[varPatentClaim]])
  d$.lv  <- suppressWarnings(as.integer(d[[varLevel]]))
  d$.sq  <- suppressWarnings(as.numeric(d[[varSequence]]))
  d$.dep <- suppressWarnings(as.numeric(d[[varDependency]]))
  d$.ord <- seq_len(nrow(d))
  d$integ_bodyDepZero      <- 0L
  d$integ_falseIndependent <- 0L

  ## stable order within claim (sequence, then input order)
  d <- d[order(d$.pc, d$.sq, d$.ord), ]

  ## ---- (1) body-line dependency == 0 -> preceding line's sequence (fallback: level-1 seq) ----
  d <- d %>% group_by(.pc) %>%
    mutate(.prevseq = dplyr::lag(.sq),
           .l1seq   = { w <- which(.lv == 1L); if (length(w)) .sq[w[1]] else NA_real_ }) %>%
    ungroup()
  fill <- ifelse(!is.na(d$.prevseq), d$.prevseq, d$.l1seq)
  bad.body <- !is.na(d$.lv) & d$.lv > 1L & !is.na(d$.dep) & d$.dep == 0 & !is.na(fill)
  d$.dep[bad.body] <- fill[bad.body]
  d$integ_bodyDepZero[bad.body] <- 1L

  ## ---- (2) level-1 dependency == 0 with a back-reference -> referenced claim's sequence ----
  t <- tolower(ifelse(is.na(d[[varText]]), "", as.character(d[[varText]])))
  t <- gsub("^[(]?[0-9a-z]{1,3}[)][ .]+", "", t, perl = TRUE)
  t <- gsub("^[0-9]{1,3}[. ]+", "", t, perl = TRUE)
  ## high-precision, OCR-tolerant back-reference to a numbered preceding claim
  br <- paste0("(?i)(",
    "\\b(of|according to|as (claimed|recited|set forth|defined|described|disclosed) in|as in|in|per|following)",
      "[\\s'.,:-]+(the[\\s'.,:-]+)?claims?[\\s'.,:-]*[0-9]",
    "|\\bany (one )?of (the )?(preceding|foregoing|above|previous|aforementioned|aforesaid|earlier|prior) claims?\\b",
    "|\\b(the|any|a|said|each|either) (immediately )?(preceding|foregoing|prior|above|previous|aforementioned|aforesaid) claims?\\b",
    "|\\bclaims?[\\s'.,:-]*[0-9]+[\\s'.,:-]*(to|through|-|or|and)[\\s'.,:-]*(claim )?[0-9]",
    ")")
  is.l1    <- !is.na(d$.lv) & d$.lv == 1L
  falseind <- is.l1 & !is.na(d$.dep) & d$.dep == 0 & grepl(br, t, perl = TRUE)

  if (any(falseind)) {
    pat  <- sub("-[^-]*$", "", d$.pc)                          # patent id part
    clno <- suppressWarnings(as.integer(sub("^.*-", "", d$.pc)))# this line's claim number
    seqmap <- tapply(d$.sq[is.l1], paste(pat[is.l1], clno[is.l1], sep = "@@"),
                     function(z) z[1])                          # (patent,claimNo) -> level-1 seq
    mm <- regexpr("(?i)claims?[\\s'.,:-]*([0-9]+)", t, perl = TRUE)
    refnum <- rep(NA_integer_, nrow(d)); got <- mm > 0
    refnum[got] <- suppressWarnings(as.integer(gsub("\\D", "", regmatches(t, mm))))
    for (i in which(falseind)) {
      ps <- NA_real_
      if (!is.na(refnum[i])) { kk <- paste(pat[i], refnum[i], sep = "@@"); if (kk %in% names(seqmap)) { v <- seqmap[[kk]]; if (!is.na(v)) ps <- v } }
      d$.dep[i] <- if (!is.na(ps) && ps != d$.sq[i]) ps else -1  # resolved parent seq, else sentinel
    }
    d$integ_falseIndependent[falseind] <- 1L
  }

  ## write back dependency, restore original row order, drop temporaries
  d[[varDependency]] <- as.integer(d$.dep)
  d <- d[order(d$.ord), ]
  d[, setdiff(names(d), c(".pc", ".lv", ".sq", ".dep", ".ord", ".prevseq", ".l1seq")), drop = FALSE]
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%

fn.dependent.claims <- function(data) {

  ### COUNT DEPENDENT CLAIMS PER INDEPENDENT CLAIM (+ their mean word length)
  ### each dependent claim is attributed to the independent claim at the ROOT of its dependency chain

  # per-claim total word count (all lines/levels of a claim), keyed by PatentClaim.
  # same word count as fn.wordcount: str_count(text, "\\w+") summed over the claim's lines.
  cw.all <- data.frame(PatentClaim = data$PatentClaim,
                       .w = stringr::str_count(ifelse(is.na(data$text), "", data$text), "\\w+"),
                       stringsAsFactors = FALSE)
  cw.all <- cw.all %>% group_by(PatentClaim) %>%
    summarise(.words = sum(.w, na.rm = TRUE), .groups = "drop")

  # only first line of each claim; only non-negative, known dependencies
  my.data.dependent <- filter(data, level==1)
  my.data.dependent <- filter(my.data.dependent, !is.na(dependency), dependency >= 0)

  patent.ids <- unique(my.data.dependent$patent_id)
  tmp.dependent.count <- data.frame()

  for (pat.i in 1:length(patent.ids)) {
    tmp.pat <- filter(my.data.dependent, patent_id == patent.ids[pat.i])

    independent.count.i <- filter(tmp.pat, dependency == 0)
    if (dim(independent.count.i)[1] == 0) {next}

    # parent lookup: claim sequence -> sequence it depends on
    parent.dep <- tmp.pat$dependency
    names(parent.dep) <- as.character(tmp.pat$sequence)

    # per-claim total words for THIS patent, keyed by the claim (level==1) sequence
    claim.words <- cw.all$.words[match(tmp.pat$PatentClaim, cw.all$PatentClaim)]
    names(claim.words) <- as.character(tmp.pat$sequence)

    dep.count   <- setNames(integer(dim(independent.count.i)[1]), as.character(independent.count.i$sequence))
    dep.wordsum <- setNames(numeric(dim(independent.count.i)[1]), as.character(independent.count.i$sequence))

    dependent.claims <- tmp.pat$sequence[tmp.pat$dependency > 0]
    for (dep.j in dependent.claims) {
      # climb the dependency chain to the root independent claim
      cur <- parent.dep[[as.character(dep.j)]]; steps <- 0L
      while (!is.na(cur) && as.character(cur) %in% names(parent.dep) &&
             parent.dep[[as.character(cur)]] != 0 && steps < length(parent.dep)) {
        cur <- parent.dep[[as.character(cur)]]; steps <- steps + 1L
      }
      # attribute this dependent claim (count + word length) to its root independent claim
      if (!is.na(cur) && as.character(cur) %in% names(dep.count)) {
        dep.count[[as.character(cur)]]   <- dep.count[[as.character(cur)]] + 1L
        w.dep.j <- claim.words[[as.character(dep.j)]]
        dep.wordsum[[as.character(cur)]] <- dep.wordsum[[as.character(cur)]] +
          ifelse(is.na(w.dep.j), 0, w.dep.j)
      }
    }

    dep.n   <- as.integer(dep.count[as.character(independent.count.i$sequence)])
    dep.wsm <- as.numeric(dep.wordsum[as.character(independent.count.i$sequence)])
    # mean word length of an independent claim's dependent claims; NA when it has none
    words.dep <- ifelse(dep.n > 0, dep.wsm / dep.n, NA_real_)

    tmp.dependent.count.i <- data.frame(
      PatentClaim     = independent.count.i$PatentClaim,
      dependentClaims = dep.n,
      wordsDependents = words.dep,
      stringsAsFactors = FALSE)
    tmp.dependent.count <- rbind(tmp.dependent.count, tmp.dependent.count.i)
  }

  ### END COUNT DEPENDENT CLAIMS PER INDEPENDENT CLAIM
  df <- tmp.dependent.count

  ### PER-PATENT CLAIM COUNTS (for the number-of-claims fields in script 08)
  ## presentClaims = distinct claim numbers present in the data; maxClaimNo = highest claim number.
  ## Taken from the claim number embedded in PatentClaim, so robust to level-structure defects.
  cc <- data.frame(pcid  = sub("-[0-9]+$", "", data$PatentClaim),
                   clnum = suppressWarnings(as.integer(sub("^.*-", "", data$PatentClaim))),
                   stringsAsFactors = FALSE)
  claiminfo <- cc %>% group_by(pcid) %>%
    summarise(maxClaimNo    = suppressWarnings(max(clnum, na.rm = TRUE)),
              presentClaims = n_distinct(clnum[!is.na(clnum)]), .groups = "drop")
  claiminfo$maxClaimNo[!is.finite(claiminfo$maxClaimNo)] <- NA_integer_
  df$pcid <- sub("-[0-9]+$", "", df$PatentClaim)
  df <- merge(df, claiminfo, by = "pcid", all.x = TRUE)
  df$pcid <- NULL

  ## MULTIPLE-DEPENDENT CLAIMS (patent-level count): a claim referring to more than one preceding claim in the alternative
  ## (35 U.S.C. 112(e); rare in US-origin filings, common in EPO/PCT-origin). Attached to independent rows; moved to patent level in 08.
  md <- data.frame(pcid = sub("-[0-9]+$", "", data$PatentClaim), PatentClaim = data$PatentClaim,
                   t = tolower(ifelse(is.na(data$text), "", data$text)), stringsAsFactors = FALSE)
  md.pat <- "any (one |1 )?of (the )?(preceding |foregoing |above |previous )?claims|one of claims [0-9]|any of claims [0-9]|either of claims|either claim [0-9]|claim [0-9]+ (or|and/or) (claim )?[0-9]|any preceding claim|(according to|as claimed in) any (one )?of (the )?(preceding |foregoing )?claims"
  md.claim <- md %>% group_by(PatentClaim) %>% summarise(pcid = dplyr::first(pcid), mdep = as.integer(any(grepl(md.pat, t))), .groups = "drop")
  md.count <- md.claim %>% group_by(pcid) %>% summarise(multipleDependentClaims = sum(mdep), .groups = "drop")
  df$pcid <- sub("-[0-9]+$", "", df$PatentClaim)
  df <- merge(df, md.count, by = "pcid", all.x = TRUE)
  df$multipleDependentClaims[is.na(df$multipleDependentClaims)] <- 0L
  df$pcid <- NULL

  return(df)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%  



## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## fn.patccat
## FUNCTION that runs all categorizer functions
## data -- dataframe with claims data
## varPatentClaim -- the column identifying the patent claim (necessary)
## varText -- the column identifying the text (necessary)
## varLevel -- the column identifying the level (if not provided, then level=1 for first line of claim, and level=2 for all other lines)
## varSequence -- the column identifying the sequence in which lines are given
## varID -- the column identifying a running id (if it does not exist, sort by PatentClaim, and assign id variable)
## extractPreambleTerms -- 
## extractPreambleText -- 
## timeOutput -- message with the time it took to run the function (in minutes)
## Output: data.frame with variables PatentClaim and all output variables
## 
fn.claimflags <- function(data) {
  ### ADDITIONAL CLAIM-TYPE FLAGS (post-classifier, text-based; do NOT feed claimType)
  ### markush, transitionType (open/closed/partial), nonTransitory, substAsDescribed, signal, singleMeans
  d <- data
  d$text  <- ifelse(is.na(d$text), "", as.character(d$text))
  d$level <- ifelse(is.na(d$level), 0L, as.integer(d$level))     # guard NA levels
  d <- d[order(d$PatentClaim, d$sequence), ]; lvl <- d$level
  pc  <- unique(d$PatentClaim); key <- as.character(pc)          # name-safe indexing (robust to numeric IDs)
  full  <- as.character(tapply(d$text, d$PatentClaim, function(x) paste(x, collapse = " "))[key])
  pre   <- as.character(tapply(seq_len(nrow(d)), d$PatentClaim, function(ix){ i1 <- ix[lvl[ix] == 1]; if (length(i1)) d$text[i1[1]] else d$text[ix[1]] })[key])
  body1 <- as.character(tapply(seq_len(nrow(d)), d$PatentClaim, function(ix){ ib <- ix[lvl[ix] >= 2]; if (length(ib)) d$text[ib[1]] else "" })[key])
  nbody <- as.integer(tapply(lvl, d$PatentClaim, function(x) sum(x >= 2))[key])
  fl <- tolower(full); pl <- tolower(pre)

  # Markush: closed alternative group within a limitation (the/a group|class; selected/chosen)
  markush <- as.integer(grepl("(selected|chosen) from (the|a) (group|class) (consisting|comprising)( essentially)? of|member of the (group|class) consisting of", fl))
  # non-transitory (Unicode-dash / spacing tolerant)
  nonTransitory <- as.integer(grepl("non[\\p{Pd}\\s]*transitory", fl, perl = TRUE))
  # omnibus / 'substantially as described'; also 'as hereinbefore described' and omnibus-without-'substantially'
  substAsDescribed <- as.integer(grepl(paste0(
      "substantially as ([a-z]+ ){0,6}(described|shown|illustrated|set forth|specified|claimed|pointed out)",
      "|as herein(before|after|above)? (described|shown|illustrated|claimed|set forth)",
      "|as (described|shown|illustrated) (herein|in the accompanying drawing)"), fl, perl = TRUE))
  # signal claim: preamble subject is a signal / carrier wave (broadened prefixes + Nuijten 'with'); excludes 'signal <device>'
  ps <- sub("^[\\[(]?\\s*\\d+[\\]).-]*\\s*", "", pl, perl = TRUE); ps <- sub("^(an?|the)\\s+", "", ps)
  signal <- as.integer(grepl(paste0(
      "^(propagated |electromagnetic |data |modulated |transitory |digital |analog(ue)? |radio |optical |rf |wireless |audio |video |acoustic |carrier |baseband |reference |communication |output |input |encoded |information[- ]bearing |computer[- ]?(readable|data) )*",
      "(signal|carrier wave)\\s*(,|;|:|comprising|comprises|embodied|encoded|representing|carrying|modulated|having|with|that|being|wherein|which|indicative|for (use|carrying|transmitting)|per se|$)"), ps, perl = TRUE))
  # single-means: exactly one body element that is a sole means-plus-function clause (unpatentable; ~absent in grants)
  nmeans <- str_count(fl, "\\bmeans\\b"); b1 <- tolower(body1)
  singleMeans <- as.integer(nbody == 1 & nmeans == 1 &
                            grepl("^\\W*means\\b (for|of|to|adapted|configured|responsive|operative|whereby|operable)", b1) & !grepl(";", b1))
  # --- batch-2 claim-type flags ---
  # hybrid & contingent dropped 2026-08-30 (validation: precision 0.07 / 0.50; IPXL and Schulhauser not reliably text-detectable).
  # step-plus-function: a CLAIMED method step "<named> step for <gerund>" (35 U.S.C. 112(f) for a method).
  # Singular "step for <gerund>" only -> excludes the preamble idiom "... to perform the method/process steps for <purpose>, the steps comprising ...".
  stepPlusFunction    <- as.integer(grepl("\\bstep for [a-z]+ing\\b", fl))
  computerImplemented <- as.integer(grepl("computer[- ]+implemented", fl))
  configuredTo        <- as.integer(grepl("\\bconfigured to\\b", fl))
  adaptedTo           <- as.integer(grepl("\\badapted (to|for)\\b", fl))
  atLeastOneOf        <- as.integer(grepl("at least one of", fl))
  whereby             <- as.integer(grepl("\\bwhereby\\b|\\bthereby\\b", fl))

  # transitionType from the preamble -> first-body-line boundary; strip Markush 'consisting of' first, then earliest transition token
  bnd  <- tolower(paste(pre, body1))
  bnd2 <- gsub("(selected|chosen) from (the|a) (group|class) consisting( essentially)? of", " ", bnd)
  p_pe <- str_locate(bnd2, "consisting essentially of")[, 1]
  p_co <- str_locate(bnd2, "consisting of|consists of|composed of")[, 1]
  p_op <- str_locate(bnd,  "comprising|comprises|comprised of|\\bincluding\\b|\\bhaving\\b|\\bcontaining\\b|characteri[sz]ed (by|in that)")[, 1]
  M    <- cbind(open = p_op, closed = p_co, partial = p_pe)
  idx  <- apply(M, 1, function(r) if (all(is.na(r))) NA_integer_ else which.min(r))
  transitionType <- ifelse(is.na(idx), "other", c("open", "closed", "partial")[idx])

  data.frame(PatentClaim = pc, markush = markush, transitionType = transitionType,
             nonTransitory = nonTransitory, substAsDescribed = substAsDescribed,
             signal = signal, singleMeans = singleMeans,
             stepPlusFunction = stepPlusFunction,
             computerImplemented = computerImplemented, configuredTo = configuredTo,
             adaptedTo = adaptedTo, atLeastOneOf = atLeastOneOf, whereby = whereby,
             stringsAsFactors = FALSE)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%


fn.patccat <- function(data,
                       varPatentClaim = "PatentClaim",
                       varText = "text",
                       varLevel = "level",
                       varSequence = "sequence",
                       varID = "id",
                       extractPreambleTerms = TRUE,
                       extractPreambleText = TRUE,
                       timeOutput = TRUE,
                       integrityCheck = TRUE,
                       params = NULL,
                       claim.rules = NULL,
                       process.words = NULL,
                       product.words = NULL,
                       processwords.simple = NULL) {
  if (is.null(.patccat$udmodel)) .patccat$udmodel <- patccat_load_model()   # [pkg build] ensure POS model
  ## [pkg build] resolve parameter overrides (NULL -> validated internal defaults)
  .patccat$params              <- if (is.null(params)) my.params else params
  .patccat$claim.rules         <- if (is.null(claim.rules)) my.claim.rules else claim.rules
  .patccat$process.words       <- if (is.null(process.words)) my.process.words else process.words
  .patccat$product.words       <- if (is.null(product.words)) my.product.words else product.words
  .patccat$processwords.simple <- if (is.null(processwords.simple)) my.processwords.simple else processwords.simple
  
  ## Beauregard claims need extractPreambleText
  if (!extractPreambleText) {
    message("With extractPreambleText = F and extractPreambleTerms = F, the classification of Beauregard claims does not work. Turn both on")
  }

  
  time.begin <- Sys.time()
  
  #
  tmp <- data
  
  # NECESSARY VARIABLES
  if (!varPatentClaim %in% names(tmp)  | !varText %in% names(tmp)) {
    stop("Patent-claim identifier or claim text are missing!")
  } else {
    colnames(tmp)[colnames(tmp) == varPatentClaim] <- "PatentClaim"
    colnames(tmp)[colnames(tmp) == varText] <- "text"
  }
  
  # ADDITIONAL VARIABLES: Not needed as input, but needed for the functions
  
  # sequence
  if (!varSequence %in% names(tmp)) {
    tmp <- arrange(tmp, PatentClaim)
    tmp["sequence"] <- c(1:length(tmp$PatentClaim))
  } else {
    colnames(tmp)[colnames(tmp) == varSequence] <- "sequence"
  }
  
  # level
  if (!varLevel %in% names(tmp)) {
    tmp <- arrange(tmp, PatentClaim)
    
    tmp.by <- group_by(tmp, PatentClaim)
    tmp.by <- summarise(tmp.by, min.seq = min(sequence, na.rm=T))
    tmp <- merge(tmp, tmp.by, all.x=T)
    
    tmp[which(tmp$sequence == tmp$min.seq),"level"] <- 1
    tmp[which(tmp$sequence > tmp$min.seq),"level"] <- 2
    tmp["min.seq"] <- NULL
    
  } else {
    colnames(tmp)[colnames(tmp) == varLevel] <- "level"
  }
  
  # id
  if (!varID %in% names(tmp)) {
    tmp <- arrange(tmp, PatentClaim)
    tmp["id"] <- c(1:length(tmp$PatentClaim))
  } else {
    colnames(tmp)[colnames(tmp) == varID] <- "id"
  }
  
  # SELECT USED VARIABLES
  tmp <- select(tmp, id, PatentClaim, text, level, sequence)
  
  ## VERY FIRST ONE!
  # Independent claim
  df.output <- fn.independent(data = tmp)
  ## VERY FIRST ONE!
  
  ## WE CONSIDER ONLY INDEPENDENT CLAIMS ##
  tmp <- filter(tmp, PatentClaim %in% filter(df.output, independent == 1)$PatentClaim)
  
  # Single-line format
  df <- fn.singleline(data = tmp)
  df.output <- merge(df.output, df, all=T)
  
  # Jepson 
  df <- fn.jepson(data = tmp, sources = FALSE)
  df.output <- merge(df.output, df, all=T)
  
  ## REFORMAT THE CLAIMS TEXT
  # Jepson Reformat
  # Single-Line Splitter
  # Begin-with-in splitter
  data.reformat <- fn.reformatdata(data = tmp, integrityCheck = integrityCheck)
  tmp <- data.reformat[["data"]]
  
  # begin with in
  df <- data.reformat[["df"]]
  df.output <- merge(df.output, df, all.x=T)
  
  # Reformating status
  df <- filter(tmp, level==1) %>% select(PatentClaim, JepsonReformat, singleReformat)
  df <- df[!duplicated(df$PatentClaim),]   # B3: one reformat-status row per claim (guard malformed multi-level-1)
  df.output <- merge(df.output, df, all=T)
  
  # # Text length: separate for preamble and body
  # df <- fn.textlength(data = tmp)
  # df.output <- merge(df.output, df, all=T)
  
  # Word count: separate for preamble and body
  df <- data.reformat[["lengths"]]   # AS-FILED length (raw pre-reformat text); see #11
  df.output <- merge(df.output, df, all=T)

  # # Count of dependent claims per independent claim
  # df <- fn.dependent.claims(data = tmp)
  # df.output <- merge(df.output, df, all=T)
  
  # Means-plus-function claims: separate for preamble and body
  df <- fn.means(data = tmp, linesOnly = F)[["bypart"]]
  df.output <- merge(df.output, df, all=T)
  
  # Additional claim-type flags (Markush, transition, non-transitory, substAsDescribed, signal, single-means)
  df <- fn.claimflags(data = tmp)
  df.output <- merge(df.output, df, all=T)
  
  # # in-combination claims: separate for preamble and body
  # df <- fn.combination(data = tmp, linesOnly = F)[["bypart"]]
  # df.output <- merge(df.output, df, all=T)
  
  ## CLAIM CATERIZER - CLAIM TYPES
  
  tmp.single <- filter(tmp, singleReformat == 3)
  tmp.regular <- filter(tmp, singleReformat %in% c(0:2))
  
  # Process simple
  # for single line and not single line sperately
  if (dim(tmp.single)[1] > 0) {
    df1 <- fn.process.simple(data = tmp.single, 
                             processwords = .patccat$processwords.simple,
                             singleLine = T)
  } else {df1 <- data.frame()}
  
  if (dim(tmp.regular)[1] > 0) {
    df2 <- fn.process.simple(data = tmp.regular, 
                             processwords = .patccat$processwords.simple,
                             singleLine = F)
  } else {df2 <- data.frame()}
  
  
  # COMBINE AND MERGE
  df <- rbind(df1, df2)
  df.output <- merge(df.output, df, all=T)
  
  
  # 
  if (dim(tmp.single)[1] > 0) {
    df1 <- fn.claimtype.single(data = tmp.single,
                               processwords = .patccat$process.words,
                               productwords = .patccat$product.words,
                               param3.nPreambleSingle = .patccat$params[3],
                               param4.nPreambleSingleByProcess = .patccat$params[4],
                               param10.nbyGap = .patccat$params[10],
                               extractPreambleTerms = extractPreambleTerms,
                               extractPreambleText = extractPreambleText)
    table(df1$claimType, useNA = "always")
    df1["bodyType"] <- NA
    df1["bodyLinesStep"] <- NA
    df1["bodyLinesElement"] <- NA
    df1["bodyLinesTotal"] <- NA
  } else {df <- data.frame()}
  
  
  #
  if (dim(tmp.regular)[1] > 0) {
    df2 <- fn.claimtype(data = tmp.regular,
                        processwords = .patccat$process.words,
                        productwords = .patccat$product.words,
                        param1.nPreamble = .patccat$params[1],
                        param2.nPreambleByProcess = .patccat$params[2],
                        param3.nPreambleSingle = .patccat$params[3],
                        param4.nPreambleSingleByProcess = .patccat$params[4],
                        param5.nBodyline = .patccat$params[5],
                        param6.nBodySteps = .patccat$params[6],
                        param7.nBodyItems = .patccat$params[7],
                        param8.stepRatio = .patccat$params[8],
                        param9.itemRatio = .patccat$params[9],
                        param10.nbyGap = .patccat$params[10],
                        claim.rules = .patccat$claim.rules,
                        extractPreambleTerms = extractPreambleTerms,
                        extractPreambleText = extractPreambleText)
    table(df2$claimType, useNA = "always")
  } else {df2 <- data.frame()}
  
  
  # COMBINE AND MERGE
  df <- rbind(df1, df2)
  df.output <- merge(df.output, df, all=T)
  
  # Labels for each preamble-body-combination
  df.output["label"] <- NA
  df.output[which(df.output$preambleType == 0  & df.output$bodyType == 0),"label"] <- "Ex"
  df.output[which(df.output$preambleType == 0  & df.output$bodyType == 1),"label"] <- "Em"
  df.output[which(df.output$preambleType == 0  & df.output$bodyType == 2),"label"] <- "Ep"
  df.output[which(df.output$preambleType == 0  & df.output$bodyType == 3),"label"] <- "Ee"
  df.output[which(df.output$preambleType == 0  & is.na(df.output$bodyType)),"label"] <- "E-"
  df.output[which(df.output$preambleType == 1  & df.output$bodyType == 0),"label"] <- "Mx"
  df.output[which(df.output$preambleType == 1  & df.output$bodyType == 1),"label"] <- "Mm"
  df.output[which(df.output$preambleType == 1  & df.output$bodyType == 2),"label"] <- "Mp"
  df.output[which(df.output$preambleType == 1  & df.output$bodyType == 3),"label"] <- "Me"
  df.output[which(df.output$preambleType == 1  & is.na(df.output$bodyType)),"label"] <- "M-"
  df.output[which(df.output$preambleType == 2  & df.output$bodyType == 0),"label"] <- "Px"
  df.output[which(df.output$preambleType == 2  & df.output$bodyType == 1),"label"] <- "Pm"
  df.output[which(df.output$preambleType == 2  & df.output$bodyType == 2),"label"] <- "Pp"
  df.output[which(df.output$preambleType == 2  & df.output$bodyType == 3),"label"] <- "Pe"
  df.output[which(df.output$preambleType == 2  & is.na(df.output$bodyType)),"label"] <- "P-"
  df.output[which(df.output$preambleType == 3  & df.output$bodyType == 0),"label"] <- "Bx"
  df.output[which(df.output$preambleType == 3  & df.output$bodyType == 1),"label"] <- "Bm"
  df.output[which(df.output$preambleType == 3  & df.output$bodyType == 2),"label"] <- "Bp"
  df.output[which(df.output$preambleType == 3  & df.output$bodyType == 3),"label"] <- "Be"
  df.output[which(df.output$preambleType == 3  & is.na(df.output$bodyType)),"label"] <- "B-"
  
  message(paste0("Duration: ",round(difftime(Sys.time(),time.begin,units="min")[[1]],digits=2), " minutes"))
  
  return(df.output)
  
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% 


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## fn.benchmarking
## FUNCTION to compare code results with benchmark results
## dataPatccat -- dataframe with results from Patccat code
## dataBenchmark -- dataframe with benchmark results (must be two columns: patent-claim identifier and benchmark category (0,1,2,3))

## Output: 
##
fn.benchmarking <- function(dataPatccat,
                            dataBenchmark) {
  
  ## dataPatccat comes from the patccat set of functions
  ## dataBenchmark is the benchmark: first column is the PatentClaim identifier, second column is the benchmark claimType
  ## 0 = empty
  ## 1 = process
  ## 2 = product
  ## 3 = product-by-process
  ## NA = possibly claims not part of the benchmark
  
  ## normalize names
  colnames(dataBenchmark) <- c("PatentClaim","benchmarkCat")
  
  dataBenchmark <- dataBenchmark[,c("PatentClaim","benchmarkCat")]
  
  tmp <- select(dataPatccat, 
                PatentClaim,
                claimType,
                singleLine,
                Jepson,
                isMeans,
                singleReformat,
                processPreamble,
                processBody,
                processSimple)
  my.data <- merge(dataBenchmark, tmp, all.x=T)
  my.data <- filter(my.data, !is.na(claimType))
  my.data["correct"] <- my.data$benchmarkCat == my.data$claimType
  
  
  amt.outcomes <- data.frame()
  amt.outcomes.i <- data.frame()
  
  # TOTAL ACCURACY and COVERAGE
  tmp <- filter(my.data, claimType!=0)
  amt.outcomes.i[1,"type"] <- "Overall results"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(my.data$claimType==0))/sum(table(my.data$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # EMPTY
  amt.outcomes.i[1,"type"] <- NA
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- NA
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # REGULAR CLAIMS (singleReformat = 0)
  tmp <- filter(my.data, singleReformat == 0, claimType!=0)
  tmp2 <- filter(my.data, singleReformat == 0)
  amt.outcomes.i[1,"type"] <- "Results for regular claims (singleReformat = 0)"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # REGULAR CLAIMS (singleReformat = 1)
  tmp <- filter(my.data, singleReformat == 1, claimType!=0)
  tmp2 <- filter(my.data, singleReformat == 1)
  amt.outcomes.i[1,"type"] <- "Results for regular claims (singleReformat = 1)"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # REGULAR CLAIMS (singleReformat = 2)
  tmp <- filter(my.data, singleReformat == 2, claimType!=0)
  tmp2 <- filter(my.data, singleReformat == 2)
  amt.outcomes.i[1,"type"] <- "Results for regular claims (singleReformat = 2)"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # SINGLE-LINE CLAIMS (singleReformat = 3)
  tmp <- filter(my.data, singleReformat == 3, claimType!=0)
  tmp2 <- filter(my.data, singleReformat == 3)
  amt.outcomes.i[1,"type"] <- "Results for single-line claims (singleReformat = 3)"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # EMPTY
  amt.outcomes.i[1,"type"] <- NA
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- NA
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # JEPSON CLAIMS
  tmp <- filter(my.data, Jepson==1, claimType!=0)
  tmp2 <- filter(my.data, Jepson==1)
  amt.outcomes.i[1,"type"] <- "Results for Jepson claims"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # NON-JEPSON CLAIMS
  tmp <- filter(my.data, Jepson==0, claimType!=0)
  tmp2 <- filter(my.data, Jepson==0)
  amt.outcomes.i[1,"type"] <- "Results for Non-Jepson claims"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # EMPTY
  amt.outcomes.i[1,"type"] <- NA
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- NA
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # SIMPLE APPROACH (a la Crouch) -- PREAMBLE ONLY
  my.data[which(my.data$processPreamble == 0),"processPreamble"] <- 2
  my.data["CrouchPreamble"] <- my.data$benchmarkCat == my.data$processPreamble
  tmp <- filter(my.data, !is.na(processPreamble))
  amt.outcomes.i[1,"type"] <- "Results for simple approach (only preamble)"
  amt.outcomes.i[1,"accuracy"] <- table(tmp$CrouchPreamble)[2]/(sum(table(tmp$CrouchPreamble)))
  amt.outcomes.i[1,"coverage"] <- 1
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # SIMPLE APPROACH (a la Crouch) -- FULL
  my.data[which(my.data$processSimple == 0),"processSimple"] <- 2
  my.data["CrouchSimple"] <- my.data$benchmarkCat == my.data$processSimple
  tmp <- filter(my.data, !is.na(processSimple))
  amt.outcomes.i[1,"type"] <- "Results for simple approach (both preamble and body)"
  amt.outcomes.i[1,"accuracy"] <- table(tmp$CrouchSimple)[2]/(sum(table(tmp$CrouchSimple)))
  amt.outcomes.i[1,"coverage"] <- 1
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # EMPTY
  amt.outcomes.i[1,"type"] <- NA
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- NA
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  amt.outcomes.i[1,"type"] <- "Conditional on manual classification"
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- NA
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # Benchmark data: Process
  tmp <- filter(my.data, benchmarkCat==1, claimType!=0)
  tmp2 <- filter(my.data, benchmarkCat==1)
  amt.outcomes.i[1,"type"] <- "Benchmark data: process"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # Benchmark data: Product
  tmp <- filter(my.data, benchmarkCat==2, claimType!=0)
  tmp2 <- filter(my.data, benchmarkCat==2)
  amt.outcomes.i[1,"type"] <- "Benchmark data: product"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # Benchmark data: Prod-by-Process
  tmp <- filter(my.data, benchmarkCat==3, claimType!=0)
  tmp2 <- filter(my.data, benchmarkCat==3)
  amt.outcomes.i[1,"type"] <- "Benchmark data: product-by-process"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # EMPTY
  amt.outcomes.i[1,"type"] <- NA
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- NA
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  amt.outcomes.i[1,"type"] <- "Conditional on automated classification"
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- NA
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # Our data: Process
  tmp <- filter(my.data, claimType==1)
  tmp2 <- filter(my.data, claimType==1)
  amt.outcomes.i[1,"type"] <- "Our data: process"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # Our data: Product
  tmp <- filter(my.data, claimType==2)
  tmp2 <- filter(my.data, claimType==2)
  amt.outcomes.i[1,"type"] <- "Our data: product"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # Our data: Prod-by-Process
  tmp <- filter(my.data, claimType==3)
  tmp2 <- filter(my.data, claimType==3)
  amt.outcomes.i[1,"type"] <- "Our data: product-by-process"
  amt.outcomes.i[1,"accuracy"] <- sum(tmp$correct, na.rm = TRUE)/(sum(table(tmp$correct)))
  amt.outcomes.i[1,"coverage"] <- 1 - length(which(tmp2$claimType==0))/sum(table(tmp2$claimType))
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # EMPTY
  amt.outcomes.i[1,"type"] <- NA
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- NA
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  # COUNTS
  tmp <- my.data
  amt.outcomes.i[1,"type"] <- "Counts: all claims"
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- dim(tmp)[1]
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  tmp <- filter(my.data, singleLine==1)
  amt.outcomes.i[1,"type"] <- "Counts: single-line claims"
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- dim(tmp)[1]
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  tmp <- filter(my.data, singleLine==0)
  amt.outcomes.i[1,"type"] <- "Counts: regular claims"
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- dim(tmp)[1]
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  tmp <- filter(my.data, Jepson==1)
  amt.outcomes.i[1,"type"] <- "Counts: Jepson claims (regular or single-line)"
  amt.outcomes.i[1,"accuracy"] <- NA
  amt.outcomes.i[1,"coverage"] <- dim(tmp)[1]
  amt.outcomes <- rbind(amt.outcomes, amt.outcomes.i)
  
  return(amt.outcomes)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## fn.diagnostics
## FUNCTION to analyze output cell-by-cell in claim.rules
## dataPatccat -- dataframe with results from Patccat code
## dataBenchmark -- dataframe with benchmark results (must be two columns: patent-claim identifier and benchmark category (0,1,2,3))

## Output: 
##
fn.diagnostics <- function(dataPatccat,
                           dataBenchmark) {
  if (nrow(dataPatccat) == 0) return(data.frame(label=character(0), preambleType=integer(0), bodyType=integer(0), claimType=integer(0), count=integer(0), accuracy=numeric(0), benchmarkProcess=numeric(0), benchmarkProduct=numeric(0), benchmarkProdByProcess=numeric(0))) # guard: empty subset (e.g. no pbp under two-way scoring)
  
  ## dataPatccat comes from the patccat set of functions
  ## dataBenchmark is the benchmark: first column is the PatentClaim identifier, second column is the benchmark claimType
  ## 0 = empty
  ## 1 = process
  ## 2 = product
  ## 3 = product-by-process
  ## NA = possibly claims not part of the benchmark
  
  ## normalize names
  colnames(dataBenchmark) <- c("PatentClaim","benchmarkCat")
  
  dataBenchmark <- dataBenchmark[,c("PatentClaim","benchmarkCat")]
  
  tmp <- select(dataPatccat, 
                PatentClaim,
                claimType,
                singleLine,
                Jepson,
                isMeans,
                singleReformat,
                processPreamble,
                processBody,
                processSimple,
                bodyType,
                preambleType)
  my.data <- merge(dataBenchmark, tmp, all.x=T)
  my.data <- filter(my.data, !is.na(claimType))
  my.data["correct"] <- my.data$benchmarkCat == my.data$claimType
  
  # add bodyType=4 to my.data
  my.data[which(is.na(my.data$bodyType)),"bodyType"] <- 4 # is a placeholder for no bodyType
  
  # Labels for each preamble-body-combination
  my.data["label"] <- NA
  my.data[which(my.data$preambleType == 0  & my.data$bodyType == 0),"label"] <- "Ex"
  my.data[which(my.data$preambleType == 0  & my.data$bodyType == 1),"label"] <- "Em"
  my.data[which(my.data$preambleType == 0  & my.data$bodyType == 2),"label"] <- "Ep"
  my.data[which(my.data$preambleType == 0  & my.data$bodyType == 3),"label"] <- "Ee"
  my.data[which(my.data$preambleType == 0  & my.data$bodyType == 4),"label"] <- "E-"
  my.data[which(my.data$preambleType == 1  & my.data$bodyType == 0),"label"] <- "Mx"
  my.data[which(my.data$preambleType == 1  & my.data$bodyType == 1),"label"] <- "Mm"
  my.data[which(my.data$preambleType == 1  & my.data$bodyType == 2),"label"] <- "Mp"
  my.data[which(my.data$preambleType == 1  & my.data$bodyType == 3),"label"] <- "Me"
  my.data[which(my.data$preambleType == 1  & my.data$bodyType == 4),"label"] <- "M-"
  my.data[which(my.data$preambleType == 2  & my.data$bodyType == 0),"label"] <- "Px"
  my.data[which(my.data$preambleType == 2  & my.data$bodyType == 1),"label"] <- "Pm"
  my.data[which(my.data$preambleType == 2  & my.data$bodyType == 2),"label"] <- "Pp"
  my.data[which(my.data$preambleType == 2  & my.data$bodyType == 3),"label"] <- "Pe"
  my.data[which(my.data$preambleType == 2  & my.data$bodyType == 4),"label"] <- "P-"
  my.data[which(my.data$preambleType == 3  & my.data$bodyType == 0),"label"] <- "Bx"
  my.data[which(my.data$preambleType == 3  & my.data$bodyType == 1),"label"] <- "Bm"
  my.data[which(my.data$preambleType == 3  & my.data$bodyType == 2),"label"] <- "Bp"
  my.data[which(my.data$preambleType == 3  & my.data$bodyType == 3),"label"] <- "Be"
  my.data[which(my.data$preambleType == 3  & my.data$bodyType == 4),"label"] <- "B-"
  table(my.data$label, useNA = "always")
  
  
  my.body.types <- sort(unique(my.data$bodyType))
  my.preamble.types <- sort(unique(my.data$preambleType)) # FIX: was claimType (masked when claimType had a 3; two-way claimType unmasks it -> B* cells were dropped)
  
  ruleCells <- expand.grid(my.body.types, my.preamble.types) 
  colnames(ruleCells) <- c("bodyType","preambleType")
  ruleCells[which(ruleCells$preambleType == 0  & ruleCells$bodyType == 0),"label"] <- "Ex"
  ruleCells[which(ruleCells$preambleType == 0  & ruleCells$bodyType == 1),"label"] <- "Em"
  ruleCells[which(ruleCells$preambleType == 0  & ruleCells$bodyType == 2),"label"] <- "Ep"
  ruleCells[which(ruleCells$preambleType == 0  & ruleCells$bodyType == 3),"label"] <- "Ee"
  ruleCells[which(ruleCells$preambleType == 0  & ruleCells$bodyType == 4),"label"] <- "E-"
  ruleCells[which(ruleCells$preambleType == 1  & ruleCells$bodyType == 0),"label"] <- "Mx"
  ruleCells[which(ruleCells$preambleType == 1  & ruleCells$bodyType == 1),"label"] <- "Mm"
  ruleCells[which(ruleCells$preambleType == 1  & ruleCells$bodyType == 2),"label"] <- "Mp"
  ruleCells[which(ruleCells$preambleType == 1  & ruleCells$bodyType == 3),"label"] <- "Me"
  ruleCells[which(ruleCells$preambleType == 1  & ruleCells$bodyType == 4),"label"] <- "M-"
  ruleCells[which(ruleCells$preambleType == 2  & ruleCells$bodyType == 0),"label"] <- "Px"
  ruleCells[which(ruleCells$preambleType == 2  & ruleCells$bodyType == 1),"label"] <- "Pm"
  ruleCells[which(ruleCells$preambleType == 2  & ruleCells$bodyType == 2),"label"] <- "Pp"
  ruleCells[which(ruleCells$preambleType == 2  & ruleCells$bodyType == 3),"label"] <- "Pe"
  ruleCells[which(ruleCells$preambleType == 2  & ruleCells$bodyType == 4),"label"] <- "P-"
  ruleCells[which(ruleCells$preambleType == 3  & ruleCells$bodyType == 0),"label"] <- "Bx"
  ruleCells[which(ruleCells$preambleType == 3  & ruleCells$bodyType == 1),"label"] <- "Bm"
  ruleCells[which(ruleCells$preambleType == 3  & ruleCells$bodyType == 2),"label"] <- "Bp"
  ruleCells[which(ruleCells$preambleType == 3  & ruleCells$bodyType == 3),"label"] <- "Be"
  ruleCells[which(ruleCells$preambleType == 3  & ruleCells$bodyType == 4),"label"] <- "B-"
  
  # add bodyType=4 to .patccat$claim.rules
  my.claim.rules.new <- rbind(.patccat$claim.rules, c(0,1,2,3))
  rownames(my.claim.rules.new) <- c(0:4)
  
  cell.diagnostics <- data.frame()
  for (cell in 1:dim(ruleCells)[1]) {
    
    cell.diagnostics.i <- data.frame()
    
    cell.diagnostics.i[1,"label"] <- ruleCells$label[cell]
    cell.diagnostics.i[1,"preambleType"] <- ruleCells$preambleType[cell]
    cell.diagnostics.i[1,"bodyType"] <- ruleCells$bodyType[cell]
    cell.diagnostics.i[1,"claimType"] <- my.claim.rules.new[as.character(ruleCells[cell,"bodyType"]),as.character(ruleCells[cell,"preambleType"])]
    
    tmp <- filter(my.data, bodyType == ruleCells$bodyType[cell], preambleType == ruleCells$preambleType[cell])
    cell.diagnostics.i[1,"count"] <- dim(tmp)[1]
    # cell.diagnostics.i[1,"coverage"] <- 1 - length(which(tmp$claimType==0))/sum(table(tmp$claimType))
    tmp2 <- filter(tmp, claimType!=0)
    
    if (dim(tmp2)[1] > 0) {
      cell.diagnostics.i[1,"accuracy"] <- table(tmp2$correct)["TRUE"]/(sum(table(tmp2$correct)))
    } else {
      cell.diagnostics.i[1,"accuracy"] <- NA
    }
    
    bm.names <- names(table(tmp$benchmarkCat))
    if ("1" %in% bm.names) {
      cell.diagnostics.i[1,"benchmarkProcess"] <- unname(table(tmp$benchmarkCat)[["1"]])/sum(table(tmp$benchmarkCat))
    } else {
      cell.diagnostics.i[1,"benchmarkProcess"] <- 0
    }
    
    if ("2" %in% bm.names) {
      cell.diagnostics.i[1,"benchmarkProduct"] <- unname(table(tmp$benchmarkCat)[["2"]])/sum(table(tmp$benchmarkCat))
    } else {
      cell.diagnostics.i[1,"benchmarkProduct"] <- 0
    }
    
    if ("3" %in% bm.names) {
      cell.diagnostics.i[1,"benchmarkProdByProcess"] <- unname(table(tmp$benchmarkCat)[["3"]])/sum(table(tmp$benchmarkCat))
    } else {
      cell.diagnostics.i[1,"benchmarkProdByProcess"] <- 0
    }
    
    # if (cell.diagnostics.i$claimType == 1) {
    #   cell.diagnostics.i[1,"inBenchmark"] <- cell.diagnostics.i[1,"benchmarkProcess"]
    # } else if (cell.diagnostics.i$claimType == 2) {
    #   cell.diagnostics.i[1,"inBenchmark"] <- cell.diagnostics.i[1,"benchmarkProduct"]
    # } else if (cell.diagnostics.i$claimType == 3) {
    #   cell.diagnostics.i[1,"inBenchmark"] <- cell.diagnostics.i[1,"benchmarkProdByProcess"]
    # } else if (cell.diagnostics.i$claimType == 0) {
    #   cell.diagnostics.i[1,"inBenchmark"] <- NA
    # }
    
    cell.diagnostics <- rbind(cell.diagnostics, cell.diagnostics.i)
  }
  
  cell.diagnostics <- select(cell.diagnostics,
                             label,
                             preambleType,
                             bodyType,
                             claimType,
                             count,
                             accuracy,
                             benchmarkProcess,
                             benchmarkProduct,
                             benchmarkProdByProcess) %>% arrange(preambleType, bodyType)
  
  return(cell.diagnostics)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   


## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   
## fn.rule.testing
## FUNCTION to test the outcome of the rules
## dataPatccat -- dataframe with results from Patccat code
## dataBenchmark -- dataframe with benchmark results (must be two columns: patent-claim identifier and benchmark category (0,1,2,3))
## claimRules -- dataframe with the claim rules

## Output: 
##
fn.rule.testing <- function(dataPatccat,
                            dataBenchmark,
                            claimRules) {
  
  tmp <-  fn.diagnostics(dataPatccat = dataPatccat,
                         dataBenchmark = dataBenchmark)
  tmp <- arrange(tmp, bodyType, preambleType)
  
  tmp <- merge(tmp, claimRules, all.x=T)
  
  tmp[which(tmp$testClaim == 1),"testBenchmark"] <- tmp[which(tmp$testClaim == 1),"benchmarkProcess"] 
  tmp[which(tmp$testClaim == 2),"testBenchmark"] <- tmp[which(tmp$testClaim == 2),"benchmarkProduct"] 
  tmp[which(tmp$testClaim == 3),"testBenchmark"] <- tmp[which(tmp$testClaim == 3),"benchmarkProdByProcess"] 
  tmp[which(tmp$testClaim == 0),"testBenchmark"] <- NA 
  
  tmp["testAccuracy"] <- tmp$count*tmp$testBenchmark
  
  my.accuracy <- sum(tmp$testAccuracy, na.rm=T)/sum(filter(tmp, testClaim!=0)$count, na.rm=T)
  my.accuracy <- paste0("Accuracy under the given claim rules is ",my.accuracy)
  
  return(my.accuracy)
}
## %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%% %%%%%   


## ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  
## SAVE
## ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  

# [pkg build] removed rda-save: save(fn.bodytype,
# [pkg build] removed rda-save:      fn.claimtype,
# [pkg build] removed rda-save:      fn.claimtype.single,
# [pkg build] removed rda-save:      fn.independent,
# [pkg build] removed rda-save:      fn.jepson,
# [pkg build] removed rda-save:      fn.jepsonreformat,
# [pkg build] removed rda-save:      fn.means,
# [pkg build] removed rda-save: #     fn.combination,
# [pkg build] removed rda-save:      fn.POStagger,
# [pkg build] removed rda-save:      fn.preambletype,
# [pkg build] removed rda-save:      fn.process.simple,
# [pkg build] removed rda-save:      fn.singleline,
# [pkg build] removed rda-save:      fn.singlesplitter,
# [pkg build] removed rda-save:      fn.beginWithIn,
# [pkg build] removed rda-save:      fn.reformatdata,
# [pkg build] removed rda-save:      fn.integrity,
# [pkg build] removed rda-save:      fn.textlength,
# [pkg build] removed rda-save:      fn.beauregard,
# [pkg build] removed rda-save:      fn.wordcount,
# [pkg build] removed rda-save:      fn.dependent.claims,
# [pkg build] removed rda-save:      fn.fixdependency,
# [pkg build] removed rda-save:      fn.claimflags,
# [pkg build] removed rda-save:      fn.patccat,
# [pkg build] removed rda-save:      fn.benchmarking,
# [pkg build] removed rda-save:      fn.diagnostics,
# [pkg build] removed rda-save:      fn.rule.testing,
# [pkg build] removed rda-save:      file=file.path("patccat-functions.rda"))


## time stamp
# [pkg build] end.time <- Sys.time();
# [pkg build] print(paste0("Begin: ", as.character(begin.time)))
# [pkg build] print(paste0("End: ", as.character(end.time)))

## 
# [pkg build] print(paste0("Elapsed runtime: ",round(difftime(end.time, begin.time, units = c("hours")),1), " hours"))


# END
