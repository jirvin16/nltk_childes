library(multispatialCCM)
library(tseriesChaos)
library(cluster)

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

measurement_names <- column_list[column_list != "Age"]

GetMinTau <- function(average_mutual_information) {
  tau = 1
  for(i in 2:(length(average_mutual_information) - 1)) {
    if(average_mutual_information[[i]] < average_mutual_information[[i+1]]) {
      tau = i - 1
      break
    }
  }
  return(tau)
}



tau_data <- data.frame(matrix(ncol = 0, nrow = length(measurement_names)))

for(i in 2:length(sorted_name_list)) {
  child_df <- list_child_df[[i]]
  child_data <- numeric()
  child <- as.character(sorted_name_list[[i]])
  for(measurement in measurement_names) {
    series <- child_df[[measurement]]
    average_mutual_information <- mutual(series, plot = FALSE, lag.max = 10)
    #     jpeg(paste("poop", measurement, ".jpg", sep=""))
    #     matplot(1:length(average_mutual_information), average_mutual_information, type="l", xlab="T", ylab="AMI") # WORTH INVESTIGATING TAUS
    #     dev.off()
    #     print(sprintf("Tau for stat %s for child %s is %d", measurement, sorted_name_list[[i]], GetMinTau(ami)))
    child_data[measurement] <- GetMinTau(average_mutual_information)
  }
  tau_data[[child]] <- child_data
}

row.names(tau_data) <- measurement_names

tau_data <- t(tau_data)

MyClust = function(x,k,...){
  # Inputs data x
  # Outputs best diana clustering using k clusters
  dv = diana(x,metric="manhattan")
  #dv = hclust(dist(x))
  
  dvk = cutree(as.hclust(dv),k)
  
  list(cluster=dvk)
  
}
# Find the optimal number of clusters from 1 to 5 of children in tau_data
# clusters <- clusGap(x, FUNcluster = MyClust, K.max = 5)

# print(maxSE(clusters$Tab[,3],clusters$Tab[,4],"Tibs2001SEmax"))

# print(clusters)

write.table(tau_data, "/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/data_directory/tau_data2.csv", sep = ",")

tau_data <- read.table("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/data_directory/tau_data2.csv", header = TRUE, sep = ",")

# average_taus <- sapply(tau_data, mean)
average_taus <- sapply(tau_data, median)

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

# Calculate optimal E

# Maximum embedding dimension E to test
maxE<-5

# Matrix for storing output
Emat<-matrix(nrow=maxE-1, ncol=length(measurement_names)); 
colnames(Emat)<-measurement_names
# Loop over potential E values and calculate predictive ability of each process for its own dynamics
# Uses defaults of looking forward one prediction step (predstep) and using time lag intervals from average_taus
for(E in 2:maxE) { 
  for(measurement in measurement_names) {
    Emat[E-1,measurement]<-SSR_pred_boot(A=concat_series[[measurement]], E=E, predstep=1, tau=average_taus[[measurement]])$rho
  } 
}

# #Look at plots to find E for each process at which predictive ability rho is maximized
# matplot(2:maxE, Emat, type="l", col=1:length(column_list), lty=1:length(column_list), xlab="E", ylab="rho", lwd=length(column_list))
# legend("bottomleft", column_list, lty=1:length(column_list), col=1:length(column_list), lwd=length(column_list), bty="n")

joint_tau_embed <- data.frame(matrix(ncol = 0, nrow =  2))

for(measurement in measurement_names) {
  d <- which(Emat[,measurement]==max(Emat[,measurement])) + 1
  joint_tau_embed[[measurement]] <- c(average_taus[[measurement]],d)
}

rownames(joint_tau_embed) <- c("T", "E")

# Flag to set if running for first time to perform large
# calculations and store the data.
# Set to false to avoid long computation again.
first_run = FALSE
options(digits=22)
CCM_boot_child_list <- list()
CCM_boot_mother_list <- list()

formal_measurement_names<- c("Child Number of Words", "Child Lexical Diversity", "Child Inflectional Diversity", "Child MLU",
                             "Mother Number of Words", "Mother Lexical Diversity", "Mother Inflectional Diversity", "Mother MLU")
if(first_run) {
  # Store to adjust after computation
  p_values <- c()
  # For names of p_values
  names <- c()
} else {
  index <- 1
  # Get CCM_boot_child_list from file
  for(i in 1:(length(formal_measurement_names)/2)) {
    for(j in 1:(length(formal_measurement_names)/2)) {
      directory_name <- paste(gsub(" ", "_",formal_measurement_names[[i]]), "and" ,gsub(" ", "_",formal_measurement_names[[j + length(formal_measurement_names)/2]]), sep = "_")
      dirname = paste("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/cross_CCM_boot_child_data/", directory_name, sep="")
      filenames <- list.files(dirname)
      CCM_boot_child_list[[index]] <- vector("list", length(filenames))
      names(CCM_boot_child_list[[index]]) <- filenames
      for(k in 1:(length(filenames))) {
        path <- paste(dirname, "/", filenames[[k]], sep = "")
        file_content <- scan(path)
        CCM_boot_child_list[[index]][[k]] <- file_content
      }
      index <- index + 1
    }
  }
  index <- 1
  # Get CCM_boot_mother_list from file
  for(i in 1:(length(formal_measurement_names)/2)) {
    for(j in 1:(length(formal_measurement_names)/2)) {
      directory_name <- paste(gsub(" ", "_",formal_measurement_names[[i]]), "and" ,gsub(" ", "_",formal_measurement_names[[j + length(formal_measurement_names)/2]]), sep = "_")
      dirname = paste("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/cross_CCM_boot_mother_data/", directory_name, sep="")
      filenames <- list.files(dirname)
      CCM_boot_mother_list[[index]] <- vector("list", length(filenames))
      names(CCM_boot_mother_list[[index]]) <- filenames
      for(k in 1:(length(filenames))) {
        path <- paste(dirname, "/", filenames[[k]], sep = "")
        file_content <- scan(path)
        CCM_boot_mother_list[[index]][[k]] <- file_content
      }
      index <- index + 1
    }
  }
  # Get fdr_p_values from file
  fdr_df = read.table("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/data_directory/cross_eng_fdr_p_values.csv", header=TRUE)
  fdr_p_values <- fdr_df[1,]
}

pic_names <- c("N_Words", "Lexical", "Inflectional", "Syntactic")
# Performs causal analysis for each measurement
# and stores the CCM_boot for child and mother
# as well as p_values for fdr
index <- 1
for(i in 1:(length(formal_measurement_names)/2)) {
  for(j in 1:(length(formal_measurement_names)/2)) {
    #Check data for nonlinear signal that is not dominated by noise
    #Checks whether predictive ability of processes declines with increasing time distance
    ptm <- proc.time()
    
    child_col <- measurement_names[[i]]
    child_series <- concat_series[[child_col]]
    child_E <- joint_tau_embed["E",child_col]
    child_T <- joint_tau_embed["T",child_col]
    
    mother_col <- measurement_names[[j + length(formal_measurement_names)/2]]
    mother_series <- concat_series[[mother_col]]
    mother_E <- joint_tau_embed["E",mother_col]
    mother_T <- joint_tau_embed["T",mother_col]
    
    child_signal <- SSR_check_signal(A=child_series, E=child_E, tau=child_T, predsteplist=1:10)
    child_signal_plot <- child_signal$predatout
    
    mother_signal <- SSR_check_signal(A=mother_series, E=mother_E, tau=mother_T, predsteplist=1:10)
    mother_signal_plot <- mother_signal$predatout
    
    # Plot the signal only on first run
    if(first_run) {
      jpeg(paste(pic_names[[i]], pic_names[[j]], "Signal.jpg", sep="_"))
      plot <- cbind(child_signal_plot$rho, mother_signal_plot$rho)
      matplot(1:10, plot, type="l", col=1:2, lty=1:2, xlab="Prediction Steps", ylab=expression(rho), lwd=2)
      title(main = paste(formal_measurement_names[[i]], "and" ,formal_measurement_names[[j + length(formal_measurement_names)/2]], sep = " "))
      legend("bottomleft", c(formal_measurement_names[[i]], formal_measurement_names[[j + length(formal_measurement_names)/2]]), lty=1:2, col=1:2, lwd=2, bty="n")
      dev.off()
    }
    
    # Does child series "cause" mother series?
    # Only perform computation once, set first_run 
    # to false after running once
    if(first_run) {
      CCM_boot_child <- CCM_boot(child_series, mother_series, child_E, tau=child_T, iterations=1000) # CHANGE THIS TO 1000
      CCM_boot_child_list[[index]] <- CCM_boot_child
    }
    else {
      CCM_boot_child <- CCM_boot_child_list[[index]]  
    }
    
    # Does mother series "cause" child series?
    if(first_run) {
      CCM_boot_mother <- CCM_boot(mother_series, child_series, mother_E, tau= mother_T, iterations=1000) # CHANGE THIS TO 1000
      CCM_boot_mother_list[[index]] <- CCM_boot_mother
    }
    else { 
      CCM_boot_mother <- CCM_boot_mother_list[[index]] 
    }
    
    # Tests for significant causal signal based on 95% 
    # confidence intervals from bootstrapping.
    if(first_run) {
      CCM_significance_test <- ccmtest(CCM_boot_child,CCM_boot_mother)
      p_values <- c(p_values, CCM_significance_test)
      names <- c(names, paste(child_col, "causes", mother_col), paste(mother_col, "causes", child_col))
    }
    
    # Plot results
    if(!first_run) {
      plotxlimits <- range(c(CCM_boot_child$Lobs, CCM_boot_mother$Lobs))
      
      jpeg(paste(pic_names[[i]], pic_names[[j]], "Cause.jpg", sep="_"))
      
      # Plot "child series causes mother series"
      plot(CCM_boot_child$Lobs, CCM_boot_child$rho, type="l", col=1, lwd=2, xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1), xlab="Library Length", ylab=expression(rho))
      title(main = paste(formal_measurement_names[[i]], "and" ,formal_measurement_names[[j + length(formal_measurement_names)/2]], sep = " "))
      
      # Add +/- 1 standard error
      matlines(CCM_boot_child$Lobs, cbind(CCM_boot_child$rho-CCM_boot_child$sdevrho, CCM_boot_child$rho+CCM_boot_child$sdevrho), lty=3, col=1)
      
      # Plot "mother inflection causes child inflection"
      lines(CCM_boot_mother$Lobs, CCM_boot_mother$rho, type="l", col=2, lty=2, lwd=2)
      
      # Add +/- 1 standard error
      matlines(CCM_boot_mother$Lobs, cbind(CCM_boot_mother$rho-CCM_boot_mother$sdevrho,CCM_boot_mother$rho+CCM_boot_mother$sdevrho), lty=3, col=2)
      c_m <- format(round(fdr_p_values[[2*index-1]],3), digits = 3)
      m_c <- format(round(fdr_p_values[[2*index]],3), digits = 3)
      
      # Change p value shown if smaller than 0.001
      c_m_legend <- if(fdr_p_values[[2*index-1]] > 0.000) paste(", p =", c_m) else ", p < 0.001"
      m_c_legend <- if(fdr_p_values[[2*index]] > 0.000) paste(", p =", m_c) else ", p < 0.001"
      legend("topleft", c(paste("Child drives Mother", c_m_legend, sep = "" ) , paste("Mother drives Child", m_c_legend, sep = "" )), lty=c(1,2), col=c(1,2), lwd=2, bty="n")
      dev.off()
    }
    print(index)
    index <- index + 1
    print(proc.time() - ptm)
  }
}

# Output CCM_boots and fdr_p_values to file
if(first_run) {
  index <- 1
  # Store CCM_boot_child_list
  dir.create("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/cross_CCM_boot_child_data", showWarnings = FALSE)
  for(i in 1:(length(formal_measurement_names)/2)) {
    for(j in 1:(length(formal_measurement_names)/2)) {
      directory_name <- paste(gsub(" ", "_",formal_measurement_names[[i]]), "and" ,gsub(" ", "_",formal_measurement_names[[j + length(formal_measurement_names)/2]]), sep = "_")
      dirname = paste("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/cross_CCM_boot_child_data/", directory_name, sep="")
      dir.create(dirname, showWarnings = FALSE)
      for(k in 1:(length(CCM_boot_child_list[[index]]))) {
        write(CCM_boot_child_list[[index]][[k]], paste(dirname, "/", names(CCM_boot_child_list[[index]])[[k]], sep = ""))
      }
      index <- index + 1
    }
  }
  index <- 1
  # Store CCM_boot_mother_list
  dir.create("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/cross_CCM_boot_mother_data", showWarnings = FALSE)
  for(i in 1:(length(formal_measurement_names)/2)) {
    for(j in 1:(length(formal_measurement_names)/2)) {
      directory_name <- paste(gsub(" ", "_",formal_measurement_names[[i]]), "and" ,gsub(" ", "_",formal_measurement_names[[j + length(formal_measurement_names)/2]]), sep = "_")
      dirname = paste("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/cross_CCM_boot_mother_data/", directory_name, sep="")
      dir.create(dirname, showWarnings = FALSE)
      for(k in 1:(length(CCM_boot_mother_list[[index]]))) {
        write(CCM_boot_mother_list[[index]][[k]], paste(dirname, "/", names(CCM_boot_mother_list[[index]])[[k]], sep = ""))
      }
      index <- index + 1
    }
  }
  # Store the fdr_p_values
  fdr_p_values <- (p.adjust(p_values, method="fdr"))
  fdr_df <- t(data.frame(fdr_p_values))
  colnames(fdr_df) <- names
  write.table(format(fdr_df, digits=22), "/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/data_directory/cross_eng_fdr_p_values.csv")
}
