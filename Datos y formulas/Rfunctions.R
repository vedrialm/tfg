# loads/installs libraries as needed
my.library <- function(package, ..., character.only = FALSE) {
  if (!character.only)
    package <- as.character(substitute(package))
  yesno <- require(package, ..., character.only = TRUE)
  if (!yesno) {
    try(install.packages(package, dependencies = TRUE))
    yesno <- require(package, ..., character.only = TRUE)
  }
  invisible(yesno)
} 

## Plotting
my.library("ggplot2")

## Data manipulation
my.library("dplyr")

## Information Value/ WoE
my.library("Information")

my.library("glmnet")
my.library("boot")

my.library("pROC")

## VISUALISATION
### Barplot
barp <- function(Data,x,freq="freq") {
  require(dplyr)
  X = Data[[x]]
  if (freq == "freq" | freq == "count") {
    ggplot(Data,aes(x=X, fill=X)) + geom_bar() + theme_bw() +
      labs(x=x, fill=x, y="count") + theme(legend.position="none")
  } else {
    Y<- as.data.frame(table(X))
    ggplot(Y,aes(x=X, y=Freq/sum(Y$Freq), fill=Y$X)) + geom_bar(stat="identity") + theme_bw() +
      labs(x=x, fill=x, y="Relative Frequencies") + theme(legend.position="none")
  }
}

### Class Conditional Barplot
cc_barplot <- function(Data,x,y, freq = "condProb") {
  Y <- Data[[y]]
  X <- Data[[x]]
  
  
  #require(graphics)
  # my_bar <- barplot(X, beside = TRUE, xlab ="student" , ylim=c(0,1), 
  #         col = c("red","darkblue"), 
  #         ylab = "Relative Frequency")
  
  require(ggplot2)
  if (freq == "count" | freq =="freq") {
    ggplot(Data, aes(x = X, fill = Y)) + geom_bar(position = "dodge") + 
      labs(fill=y, x=x, y="count") + theme_bw() 
  } else if (freq=="relfreq") {
    
    tab <- table(Y,X)
    cl <- colSums(tab)
    for (i in 1:ncol(tab)) {
      tab[,i] <- tab[,i]/cl[i]
    }
    Y <- as.data.frame(tab)
    ggplot(Y, aes(x=X, y=Freq, fill=Y)) + geom_col(position = "dodge") + 
      labs(fill=y, x=x, y="Relative Frequency") + theme_bw() +
      geom_text(aes(x=X, y=Freq+ 0.03, label=signif(Freq,2)), position=position_dodge(width=0.9))
    
  } else {
    tab <- table(Y,X)
    cl <- rowSums(tab)
    for (i in 1:nrow(tab)) {
      tab[i,] <- tab[i,]/cl[i]
    }
    Y <- as.data.frame(tab)
    ggplot(Y, aes(x=X, y=Freq, fill=Y)) + geom_col(position = "dodge") + 
      labs(fill=y, x=x, y=paste0("P(",x," | ",y,")")) + theme_bw() +
      geom_text(aes(x=X, y=Freq+ 0.03, label=signif(Freq,2)), position=position_dodge(width=0.9)) 
    #ggtitle(paste("Conditional Probability of ",x,"given",y))
  }
}

cc_hist <- function(Data, x, y, freq = "cond", breaks = "Sturges") {
  # class label column
  Y <- Data[[y]]
  if (is.factor(Y)) {
    l <- levels(Y)
  } else {
    l <- unique(Y)
  }
  # Predictor column
  X <- Data[[x]]
  
  rangeX <- c(min(X), max(X))
  rangeY <- c(0, 0)
  
  out <- list()
  # Compute relative frequencies
  for (i in 1:length(l)) {
    out[[i]] <- hist(X[Y == l[i]], plot = FALSE, breaks = breaks)
    
    if (freq == "cond") {
      out[[i]]$counts = out[[i]]$counts / sum(out[[i]]$counts)
    }
    
    if (max(out[[i]]$counts) > rangeY[2]) {
      rangeY[2] = max(out[[i]]$counts)
    }
  }
  
  if (freq == "cond") {
    yl <- "Relative Frequency of Conditional Distribution"
    fr <- TRUE
  } else if (freq == "freq") {
    yl <- "Frequency"
    fr <- TRUE
  } else {
    yl <- "Density"
    fr <- FALSE
  }
  
  plot(out[[1]], freq = fr, xlim = rangeX, ylim = rangeY, col = rgb(1, 0, 0, 0.5), cex = 2.5, 
       main = paste("Histogram of", x, "conditional on", y), ylab = yl, xlab = x)
  
  for (i in 2:length(l)) {
    plot(out[[i]], freq = fr, add = TRUE, xlim = rangeX, ylim = rangeY, 
         col = rgb(runif(1), runif(1), runif(1), 0.5), cex = 2.5)
  }
  
  legend("topright", l, lty = rep(1, length(l)), lwd = rep(5, length(l)), 
         col = sapply(1:length(l), function(i) rgb(runif(1), runif(1), runif(1), 0.5)))
}

### Boxplot
cc_boxplot <- function(Data,x,y) {
  X <- Data[[x]]
  Y <- Data[[y]]
  if (!is.factor(Y)) {
    stop("y has to be a categorical variable")
    
  }
  ggplot(Data,aes(x=Y,y=X,fill=Y)) + geom_boxplot(notch=TRUE) +
    labs(x=y,y=x) + theme_bw() + theme(legend.position="none") 
  
}


############## INFORMATION VALUE
# functions for WoE replacement
WOE_encode_df <- function(X,IV) {
  Y <- X
  var_names <- names(IV$Tables)
  for (i in var_names) {
    Y[i] <- WOE_encode(X[[i]], data.frame(IV$Tables[i]))
  }
  return(Y)
}

# replace single column
WOE_encode <- function(x,iv_table) {
  ny <- vector("numeric", length(x))
  # Loop over bins
  for (j in 1:nrow(iv_table)) {
    
    if (iv_table[j,1] == "NA"  | is.na(iv_table[j,1])) {
      # Missing values
      ny[which(is.na(x))] <- iv_table[j,4]
      
    } else if (is.factor(x)) {
      ny[which(x == iv_table[j,1])] <- iv_table[j,4]
      
    } else {
      if (j < nrow(iv_table)) {
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        #upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\2",iv_table[j,1]))
        upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\1",iv_table[j+1,1]))
        ny[which(x>=lower & x<upper)] <- iv_table[j,4]
      } else{
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        ny[which(x>=lower)] <- iv_table[j,4]
      }
    }
  }
  return(ny)
}

# Bin rather than substitute with WOE value
WOE_bin <- function(x,iv_table) {
  if (is.factor(x)) {
    return(x)
  }
  
  ny <- vector("numeric", length(x))
  # Loop over bins
  for (j in 1:nrow(iv_table)) {
    
    if (iv_table[j,1] == "NA"  | is.na(iv_table[j,1])) {
      # Missing values
      #ny[which(is.na(x))] <- 0 (it's already 0 so do nothing)
      
    } else {
      if (j < nrow(iv_table)) {
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        #upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\2",iv_table[j,1]))
        upper <- as.double( gsub("\\[([^,]+),([^,]+)\\]","\\1",iv_table[j+1,1]))
        ny[which(x>=lower & x<upper)] <- j
      } else{
        lower <- as.double( gsub("\\[([^,]+),[^,]+","\\1",iv_table[j,1]) )
        ny[which(x>=lower)] <- j
      }
    }
  }
  return(factor(ny))
}



WOE_bin_df <- function(X,IV) {
  Y <- X
  var_names <- names(IV$Tables)
  for (i in var_names) {
    df <- data.frame( IV$Tables[i] )
    Y[i] <- WOE_bin(X[[i]], data.frame(IV$Tables[i]))
  }
  return(Y)
}


####### Plots 2-dimensional decision boundary for logistic regression
log.dec.bound <- function(model,threshold,X)
{
  f <- function(x2, x1, model,thr) {
    out <- predict(model, newdata=data.frame("x1"=x1,"x2"=x2), type="link")
    return(out + log(1/threshold -1))
  }
  
  N <- 200
  x1 <- seq(from=min(X[,1]), to=max(X[,1]), length.out = N)
  x2.range <- range(X[,2]) + c(-1,1)
  
  Y <- data.frame("x"=x1,"y"=x1)
  for (i in 1:N) {
    #browser()
    y1 <- f(x2.range[1], x1[i], model, threshold)
    y2 <- f(x2.range[2], x1[i], model, threshold)
    #cat( y1, y2, "\n")
    if (y1 * y2 > 0) {
      
      Y[i,2] = ifelse(y1>0, x2.range[1], x2.range[2])
      
    } else {
      Y[i,2] <- uniroot(f, interval = x2.range, x1=x1[i],
                        model=model, thr=threshold, 
                        tol=1.0e-6)$root     
    }
  }
  return(Y)
}