CatBins <- function(x, brk, type = c('pct', 'num', 'dollar'), acc = 1){
  
  if(missing(type)){type = 'num'}
  if(missing(brk) & type == 'num'){brk = 1000}
  if(missing(brk) & type == 'pct'){brk = 0.2}
  
  if(brk <= 0.01){acc = 0.1}
  
  brkseq <- seq(min(x, na.rm = TRUE), 
                brk * ceiling(max(x, na.rm = TRUE)/brk), 
                brk)
  
  
  if(type == 'pct'){
    Cat <- cut(x,
               breaks = brkseq,
               labels = paste(
                 scales::number(100 * (brkseq[-1] - brk) + acc,
                                accuracy = acc), 
                 "\u2014",
                 scales::percent(brkseq[-1], 
                                 accuracy = acc)))
  } else if (type == 'num') {
    Cat <- cut(x,
               breaks = brkseq,
               labels = paste(
                 scales::number(brkseq[-1] - brk + acc,
                                accuracy = acc, 
                                big.mark = ','), 
                 "\u2014",
                 scales::number(brkseq[-1], 
                                accuracy = acc,
                                big.mark = ',')))
  } else if (type == 'dollar') {
    Cat <- cut(x,
               breaks = brkseq,
               labels = paste(
                 scales::dollar(brkseq[-1] - brk + acc,
                                accuracy = acc, 
                                big.mark = ','), 
                 "\u2014",
                 scales::dollar(brkseq[-1], 
                                accuracy = acc,
                                big.mark = ',')))
    
  }
  
  CatFact <- Cat
  Cat <- as.character(Cat)
  Cat <- ifelse(x == 0, 
                as.character(levels(CatFact)[1]), 
                Cat)
  Cat <- ifelse(!is.finite(x), NA_character_, Cat)
  Cat <- gsub("^1 ", "0 ", Cat)
  Cat <-  forcats::fct_reorder(Cat, x)
  
  if(length(levels(Cat)) > 15){
    warning("More than 15 levels.. adjust the breaks")
  }
  
  if(length(levels(Cat)) == 1){
    warning("Only 1 level.. breaks too high")
  }
  
  return(Cat)
  
}


asr_ci <- function(est, lci, uci, dig = 1){
  paste0(formatC(est, format = 'f', digits = dig),
         " [", 
         formatC(lci, format = 'f', digits = dig),
         " - ",
         formatC(uci, format = 'f', digits = dig),
         "]")
}


### Function for breaking into octiles
Classify_Quantiles <- function(x, type = 'num', breaks = 8){
  zeroflag <- FALSE
  
  if(type == 'seifa'){
    x_out <- as.character(x)
    return(x_out)
  }
  
  minor_break <- 1/breaks
  
  quant_breaks <- quantile(x, probs = seq(0, 1, minor_break))
  
  if(sum(quant_breaks == 0) > 1){
    zeroflag <- TRUE
    quant_breaks <- quantile(x[x > 0.005], probs = seq(0, 1, minor_break))
  }
  
  index10 <- 10 ^ floor(min(log10(quant_breaks[-1]) - 1))
  
  if(zeroflag){
    quant_breaks[1] <- 0
  } else {
    quant_breaks[1] <- max(0, quant_breaks[1] - index10)
  }  
  
  quant_breaks[(breaks + 1)] <- ifelse(quant_breaks[(breaks + 1)] == 1,
                                       1,
                                       quant_breaks[(breaks + 1)] + index10)
  
  rounded_breaks <- plyr::round_any(quant_breaks, index10)
  
  if(type == 'pct'){
    acc <- dplyr::case_when(sum(quant_breaks < 0.01) >= 5 ~ 0.01, 
                            min(quant_breaks) >= 0.1 ~ 1,
                            TRUE ~ 0.1)
    
    breaks_out <- c(
      paste0(scales::number(100 * rounded_breaks[-(breaks + 1)],
                            accuracy = acc), 
             '-',
             c(scales::percent(rounded_breaks[c(2:breaks)] - index10, 
                               accuracy = acc),
               scales::percent(rounded_breaks[(breaks + 1)], 
                               accuracy = acc))))
    
  } else {
    breaks_out <- c(paste0(scales::number(rounded_breaks[-(breaks + 1)], 
                                          accuracy = 1, 
                                          big.mark = ','),
                           '-',
                           c(scales::number(rounded_breaks[c(2:breaks)] - index10, 
                                            accuracy = 1, 
                                            big.mark = ','),
                             scales::number(rounded_breaks[(breaks + 1)], 
                                            accuracy = 1, 
                                            big.mark = ','))))
    
  }
  
  
    x_out <- cut(x, 
                 rounded_breaks, 
                 labels = breaks_out,
                 include.lowest = TRUE,
                 right = TRUE)
    
    x_out <- as.character(x_out)

  return(x_out)
  
}

