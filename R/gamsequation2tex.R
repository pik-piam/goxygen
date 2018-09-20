#' gamsequation2tex
#' 
#' Convert a gams equation into latex code
#' 
#' 
#' @param x GAMS equation provided as character
#' @return GAMS equation converted to latex code
#' @author Jan Philipp Dietrich
#' @export
#' @importFrom stringi stri_extract_all_regex stri_replace_first_regex stri_replace_first_fixed stri_count_fixed stri_extract_first_regex stri_replace_all_fixed
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
    
    extract_vars <- function(x, variable, code="v", protected=c("sum","prod","power")) {
      
      if(length(x)!=1) stop("Works only for 1 element!")
      
      vars        <- stri_extract_all_regex(x,variable)[[1]]
      names(vars) <- paste0("#",code,1:length(vars),"#")
      x           <- stri_replace_all_regex(x,variable,"#:.INSERTHERE.:#")
      
      for(v in names(vars)) {
        insert <- ifelse(vars[v] %in% protected, paste0("\\",vars[v]), v)
        x <- stri_replace_first_fixed(x,"#:.INSERTHERE.:#",insert)
      }
      
      vars <- vars[!(vars%in% protected)]
      vars <- sub("^AND$","\\\\&",vars)
      
      if(code=="v") vars <- gsub("\\_","\\\\_",vars)
      
      return(list(x=x,vars=vars))
    } 
    
    extract_braceblocks <- function(x, level=1) {
      braceblock <- "\\([^\\)\\(]*\\)"
      y <- extract_vars(x,braceblock,paste0("b",level,"."))
      y$vars <- gsub("[()]","",y$vars)
      y$x <- gsub("\\((#b[0-9.]*#)\\)","\\1",y$x)
      z <- c(y$x,y$vars)
      if(grepl("\\(.*\\)",y$x)) {
        tmp <- extract_braceblocks(y$x, level=level+1)
        z <- c(tmp,z[-1])
      }
      return(z[!is.na(z)])
    }
    
    convert_functions <- function(z) {
      if(length(z)==1) return(z)
      
      .tmp <- function(z,pattern,replacement,prefix) {
        if(is.null(z)) return(NULL)
        # Code needs to be in \\2!
        # prefixes need to be consistent to ids in replacement pattern
        # prefix "n" can be used to suppress brakets in convert_blocks 
        # (brakets only set for ids starting with "b")
        while(grepl(pattern,z[1])) {
          code <- sub("^[^#]*#([^#]*)#.*$","\\1",stri_extract_first_regex(z[1],pattern))
          id <- which(names(z)==paste0("#",code,"#"))
          if(length(id)!=1) return(NULL)
          split <- strsplit(z[id],",")[[1]]
          if(length(split)!=2) return(NULL)
          names(split) <- paste0("#",prefix,code,c("a","b"),"#")
          if(id==length(z)) {
            z <- c(z[1:(length(z)-1)],split) 
          } else {
            z <- c(z[1:(id-1)],split,z[(id+1):length(z)])
          }
          z[1] <- sub(pattern,replacement,z[1])
        } 
        return(z)
      }
      z <- .tmp(z,"\\\\(sum|prod)#([^#]*)#","\\\\\\1_{#n\\2a#}#\\2b#",c("n",""))
      z <- .tmp(z,"\\\\(power)#([^#]*)#","#\\2a#^{#n\\2b#}",c("","n"))
      if(length(z)>3) z <- c(z[1],convert_functions(z[-1]))
      return(z)
    }
    
    convert_blocks <- function(x) {
      # reduce number of blocks by substituting blocks which
      # only contain one other block
      reduce <- grep("^ *#[^v^#]*# *$",x)
      for(r in reduce) {
        id <- gsub(" ","",x[r])
        x[r] <- x[id]
        x[id] <- ".:|DELETEME|:."
      }
      x <- x[x!=".:|DELETEME|:."]
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
      #add braces back, if required
      # required, if 
      # 1) id starts with a "b" (for braket)
      # 2) block contains more than just a single variable/block
      add <- (substr(names(x),2,2)=="b")
      add <- add & !grepl("^ *#[^#]*# *$",x)
      x[add] <- paste0("\\left(",x[add],"\\right)")
      names(x) <- names
      return(x)
    }
    
    merge_back <- function(x) {
      vars <- x[-1]
      x <- x[1]
      for(i in names(vars)) x <- sub(i,vars[i],x,fixed=TRUE)
      return(x)
    }  
    
    
    variable <- "[\\w.]{1,}(\\([\\w,\"'+-]*\\)|)"
    y <- extract_vars(x,variable,"v")
    
    z <- extract_braceblocks(y$x)
    
    z <- convert_functions(z)
    if(is.null(z)) return("#FAILED#")
    z <- convert_blocks(z)
    
    x <- merge_back(c(z,y$vars))
    
    return(x)
  }
  
  fixlines <- function(x, name) {
    
    # Fix strange line breaks and set flag for codeline breaks only
    x <- gsub("^\n *","",x)
    x <- gsub("\n$","",x)
    x <- gsub("\n *\\}","}\n",x)
    x <- gsub("(\\\\frac\\{[^}]*\\}[^{]*?)\\n(.*?\\{)","\\1#codelinebreakonly#\\2",x)
    
    # Split up equation at line breaks
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
    
    # Merge equation
    out <- paste(out,collapse="\\\\ \n")
    out <- gsub("#codelinebreakonly#","\n",out,fixed=TRUE)
    #out <- paste("\\begin{aligned}\n",out,"\n\\end{aligned}")
    return(out)
  }
  
  #remove spaces and line breaks
  #x <- gsub("[\n ]*","",x)
  
  # split name and equation
  pattern <- "^\n*(.*?) *\\.\\. *(.*?);?$"
  if(grepl(pattern,x)) {
    name <- stri_replace_all_fixed(sub(pattern,"\\1",x)," ","")
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
    warning("Cannot handle equations without relational operator! Return original code!")
    names(x) <- paste(name,"(CONVERSION FAILED!)")
    return(x)
  }
  left <- sub(pattern,"\\1",eq)
  middle <- sub(pattern,"\\2",eq)
  right <- sub(pattern,"\\3",eq)
  
  middle <- switch(tolower(middle),
                   "=e=" = "=",
                   "=l=" = "\\leq",
                   "=g=" = "\\geq",
                   "=n=" = "\\neq")
  
  left <- convert_side(left)
  right <- convert_side(right)
  
  out <- paste(left,middle,right)
  out <- gsub(" +"," ",out)
  out <- gsub("([$%])","\\\\\\1",out)
  out <- gsub(">=","\\geq", out, fixed=TRUE)
  out <- gsub("<=","\\leq", out, fixed=TRUE)
  
  #if(multiline) out <- fixlines(out, name)
  out <- paste("\\begin{multline*}\n",out,"\n\\end{multline*}")
  out <- gsub("\t"," ",out)
  out <- gsub("\n[\n ]*\n","\n",out)
  names(out) <- name
  
  if(grepl("#",out)) {
    warning("Equation ",name," could not be converted! Return original code!")
    names(x) <- paste(name,"(CONVERSION FAILED!)")
    return(x)
  }
  
  return(out)
}




  

