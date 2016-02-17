library(ggplot2)

df <- read.table("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/data_directory/morph-eng.csv", header = TRUE)

sorted_df <- df[order(df[,'Child'],df[,'Age']), ]

name_list <- unique(df[,'Child'])

sorted_name_list <- sort(name_list)

column_list <- colnames(df)

column_list <- column_list[column_list != "Child"]

sorted_df[, column_list] <- sapply(sorted_df[, column_list], as.numeric)

list_child_df <- list()
for(i in 1:length(sorted_name_list)) {
  list_child_df[[i]] <- sorted_df[which(sorted_df["Child"] == as.character(sorted_name_list[[i]])), ]
}

# measurement_names <- column_list[column_list != "Age"]
measurement_names <- column_list[column_list!="H.child.I"]
measurement_names <- measurement_names[measurement_names!="H.mother.I"]

joint_length = nrow(df)

concat_series <- data.frame(matrix(ncol = 0, nrow =  joint_length + length(sorted_name_list) - 1))

# Join the series in a single data frame by measurements, with each child series separated by NA
# to find embedding dimensions of joint series
for(col in measurement_names) {
  col_data <- numeric()
  for(i in 1:length(sorted_name_list)) {
    child_df <- list_child_df[[i]]
    child_series <- child_df[[col]]
    if(i != 1) {
      col_data <- c(col_data, NA)
    }
    col_data <- c(col_data,child_series)
  }
  concat_series[[col]] <- col_data
}

panel.cor <- function(x, y, digits=2, cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  test <- cor.test(x,y)
  Signif <- ifelse(round(test$p.value,3)<0.001,"p<0.001",paste("p=",round(test$p.value,3)))  
  text(0.5, 0.65, paste("r=",txt))
  text(0.5, 0.35, Signif)
}

panel.smooth<-function (x, y, col = "grey", bg = NA, pch = 18, 
                        cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, lwd=2,...)
  #abline(lm(y[ok]~x[ok]),col=col.smooth, lwd = 2, ...)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}


# raw2 = as.data.frame(cbind(data$Lemma,data$Morph,log(data$Grammar),log(data$Dysfluencies),
#                            log(data$GrammarL),log(data$Length),data$Age))

raw2 <- as.data.frame(cbind(log(concat_series$N.child),concat_series$H.child.S,concat_series$Schild,
                           log(concat_series$N.mother),concat_series$H.mother.S,concat_series$Smother))

raw2 <- na.omit(raw2)

pdf("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/correlations.pdf",height=24,width=24)
pairs(raw2,labels = c("(log) child number of words","child lexical diversity",
                      "child MLU","(log) mother number of words",
                      "mother lexical diversity", "mother MLU"),
      lower.panel=panel.cor, upper.panel=panel.smooth,diag.panel=panel.hist)
dev.off()


