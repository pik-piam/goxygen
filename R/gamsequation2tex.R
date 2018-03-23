#' gamsequation2tex
#' 
#' Convert a gams equation into latex code
#' 
#' 
#' @param x GAMS equation provided as character
#' @return GAMS equation converted to latex code
#' @author Jan Philipp Dietrich
#' @export
#' @importFrom stringi stri_extract_all_regex stri_replace_first_regex stri_replace_first_fixed stri_count_fixed
#' @seealso \code{\link{goxygen}}
#' @examples
#' 
#'   x <- "eq_1 .. v_a =e= sum(j,v_b(j)*((1-s_c)+sum(cell(i,j),v_d(i)/f_d(i))));"
#'   cat(gamsequation2tex(x))

gamsequation2tex <- function(x) {
  
  if(length(x)>1) {
    out <- NULL
    for(i in x) out <- c(out,gamsequation2tex(i))
    return(out)
  }
  
  convert_side <- function(x) {
    
    
    convert_vars <- function(v) {
      v <- sub("^AND$","\\\\&",v)
      return(gsub("\\_","\\\\_",v))
    }
    
    extract_vars <- function(x, variable, code="v", protected=c("sum","prod","power")) {
      if(length(x)!=1) stop("Works only for 1 element!")
      vars <- stri_extract_all_regex(x,variable)[[1]]
      names(vars) <- paste0("#",code,1:length(vars),"#")
      x <- stri_replace_all_regex(x,variable,"#:.INSERTHERE.:#")
      for(v in names(vars)) {
        insert <- ifelse(vars[v] %in% protected, paste0("\\",vars[v]), v)
        x <- stri_replace_first_fixed(x,"#:.INSERTHERE.:#",insert)
      }
      vars <- vars[!(vars%in% protected)]
      vars <- convert_vars(vars)
      return(list(x=x,vars=vars))
    } 
    
    convert_sumprod <- function(x) {
      type <- c(sum="\\1_{\\2}(",
                prod="\\1_{\\2}(",
                power="(\\2)^(")
      for(t in names(type)) {
        pattern <- paste0("(\\\\",t,")\\(([^(,]+|\\(.*?\\)),")
        x <- gsub(pattern,type[t],x)
      }
      x <- gsub("\\( *(#v[0-9]*#) *\\)","\\1",x)
      return(x)
    }
    
    extract_braceblocks <- function(x, level=1) {
      braceblock <- "\\([^\\)\\(]*\\)"
      y <- extract_vars(x,braceblock,paste0("b",level,"."))
      y$vars <- gsub("[\\(\\)]","",y$vars)
      y$x <- gsub("\\((#b[0-9.]*#)\\)","\\1",y$x)
      if(grepl("\\(.*\\)",y$x)) {
        tmp <- extract_braceblocks(y$x, level=level+1)
        y$x <- tmp$x
        y$vars <- c(tmp$vars,y$vars)
      }
      return(y)
    }
    
    convert_blocks <- function(x, addbraces=FALSE) {
      names <- names(x)
      # handle exponents
      x <- gsub("\\*\\*([^<>=*+/-]+)","^{\\1}",x)
      # handle divisions
      steps <- 1
      while(any(grepl("/",x)) | steps>10) {
        x <- gsub("([^<>=*+/-]+)/([^<>=*+/-]+)","\\\\frac{\\1}{\\2}",x)
        steps <- steps + 1
      }
      #handle multiplications
      x <- gsub("*", " \\cdot ", x, fixed=TRUE)
      #add braces back
      if(addbraces) x <- paste0("\\left(",x,"\\right)")
      names(x) <- names
      return(x)
    }
    
    merge_back <- function(x,vars) {
      for(i in names(vars)) x <- sub(i,vars[i],x,fixed=TRUE)
      return(x)
    }  
    
    
    variable <- "[\\w.]{1,}(\\([\\w,\"'+-]*\\)|)"
    y <- extract_vars(x,variable,"v")
    y$x <- convert_sumprod(y$x)
    
    z <- extract_braceblocks(y$x)
    
    z$x    <- convert_blocks(z$x)
    z$vars <- convert_blocks(z$vars, addbraces=TRUE)
    
    x <- merge_back(z$x,c(z$vars,y$vars))
    
    return(x)
  }
  
  fixlines <- function(x, name) {
    x <- gsub("^\n *","",x)
    x <- gsub("\n *\\}","}\n",x)
    x <- gsub("(\\\\frac\\{[^}]*\\}[^{]*?)\\n(.*?\\{)","\\1#codelinebreakonly#\\2",x)
    
    out <- strsplit(x,"\n")[[1]]
    
    #check {-bracket balance
    balance <- (stri_count_fixed(out,"{") - stri_count_fixed(out,"}"))
    #if(any(balance!=0)) warning("Fixed illegal line break in ",name,"!")
    while(any(balance!=0)) {
      i <- which(balance!=0)[1]
      out[i] <- paste(out[i],out[i+1])
      out <- out[-(i+1)]
      balance <- (stri_count_fixed(out,"{") - stri_count_fixed(out,"}"))
    }
    
    # check left/right balance
    balance <- (stri_count_fixed(out,"\\left") - stri_count_fixed(out,"\\right"))
    for(i in which(balance>0)) {
      out[i] <- paste(out[i],paste(rep("\\right.",balance[i]),collapse=""))
    }
    for(i in which(balance<0)) {
      out[i] <- paste(paste(rep("\\left.",-1*balance[i]),collapse=""),out[i])
    }
    out <- paste(out,collapse="\\\\ \n & ")
    out <- gsub("#codelinebreakonly#","\n",out,fixed=TRUE)
    out <- paste("\\begin{aligned}\n",out,"\n\\end{aligned}")
    return(out)
  }
  
  #remove spaces and line breaks
  #x <- gsub("[\n ]*","",x)
  
  # split name and equation
  pattern <- "^\n*(.*?) *\\.\\. *(.*?);?$"
  if(grepl(pattern,x)) {
    name <- sub(pattern,"\\1",x)
    eq <- sub(pattern,"\\2",x)
  } else  {
    name <- "undefined"
    eq <- x
  }
  
  
  if(grepl("(^\\$|\n\\$)",x)) {
    warning("Cannot handle equations with preceeding dollar conditions! Return original code!")
    names(x) <- paste(name,"(CONVERSION FAILED!)")
    return(x)
  }
  
  multiline <- grepl("\n",eq)
  
  #split sides
  pattern <- "^(.*)(=[lLgGeEnN]=)(.*)$"
  if(!grepl(pattern,eq)) {
    
  }
  left <- sub(pattern,"\\1",eq)
  middle <- sub(pattern,"\\2",eq)
  right <- sub(pattern,"\\3",eq)
  
  middle <- switch(tolower(middle),
                   "=e=" = "=",
                   "=l=" = "\\leq",
                   "=g=" = "\\geq",
                   "=n=" = "\\neq")
  
  if(multiline) middle <- paste(middle,"&")
  
  left <- convert_side(left)
  right <- convert_side(right)
  
  out <- paste(left,middle,right)
  out <- gsub(" +"," ",out)
  out <- gsub("([$%])","\\\\\\1",out)
  out <- gsub(">=","\\geq", out, fixed=TRUE)
  out <- gsub("<=","\\leq", out, fixed=TRUE)
  
  if(multiline) out <- fixlines(out, name)
  names(out) <- name
  
  if(grepl("#",out)) {
    warning("Equation ",name," could not be converted! Return original code!")
    names(x) <- paste(name,"(CONVERSION FAILED!)")
    return(x)
  }
  
  return(out)
}




  

