#' make_formula_str
#'
#' an internal function for HarrellPlot
#' @export
make_formula_str <- function(y, xcols=NULL, rintcols=NULL, rslopecols=NULL, icols=NULL, covcols=NULL){
  # xcols = fixed
  # rcols = random
  # icols = interaction
  
  # fixed effects
  form_str <- paste(y, ' ~ ', paste(c(covcols, xcols), collapse=' + '), sep='')
  # fixed interaction effects
  if(!is.null(icols)){
    combos <- data.frame(t(combn(icols,2)))
    i_str <- paste(paste(combos[,1],combos[,2],sep=':'), collapse=' + ')
    form_str <- paste(form_str, i_str, sep=' + ')
  }
  # random intercepts and slopes
  if(!is.null(rintcols)){
    if(is.null(rslopecols)){slope_str <- '1'}else{slope_str <- rslopecols}
    r_str <- paste(paste('(', slope_str, '|', rintcols, ')', sep=''), collapse=' + ')
    form_str <- paste(form_str, r_str, sep=' + ')
  }
  return(form_str)
}

contrast.groups <- function(contrast_matrix, grouping, add_interaction){
  split1 <- data.frame(t(do.call("cbind", strsplit(as.character(contrast_matrix$contrast)," - "))))
  if(grouping == TRUE & add_interaction==TRUE){
    split2a <- data.frame(t(do.call("cbind", strsplit(as.character(split1$X1),","))))
    colnames(split2a) <- c('X1','G1')
    split2b <- data.frame(t(do.call("cbind", strsplit(as.character(split1$X2),","))))
    colnames(split2b) <- c('X2','G2')
    group_names <- data.table(split2a, split2b)
  }else{
    group_names <- data.table(split1)
  }
  return(group_names)
}
