library(multispatialCCM)
library(tseriesChaos)
library(cluster)

directory = "/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/"
dir.create(paste(directory, "CCM_data", sep = ""), showWarnings = FALSE)

# Set working directory 
setwd(directory)

# Obtain raw data frame of linguistic measurements
df <- read.table(paste(directory, "data_directory/morph-eng.csv", sep = ""), header = TRUE)

# Sort the data frame first by child, then by age
sorted_df <- df[order(df[,'Child'],df[,'Age']), ]

# Get the list of children
name_list <- unique(df[,'Child'])

# Sort the list of children alphabetically
sorted_name_list <- sort(name_list)

# Get all the numeric columns
column_list <- colnames(df)
column_list <- column_list[column_list != "Child"]

# Convert strings in the data frame to numerics
sorted_df[, column_list] <- sapply(sorted_df[, column_list], as.numeric)

# Create a list of data frames, one data frame for each child
list_child_df <- list()
for(i in 1:length(sorted_name_list)) {
  list_child_df[[i]] <- sorted_df[which(sorted_df["Child"] == as.character(sorted_name_list[[i]])), ]
}

# Get the list of columns for indexing into the data frame
measurement_names <- column_list[column_list != "Age"]

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

GetTauData <- function(measurement_names, list_child_df, sorted_name_list, cluster) {
  # Finds the first local min of the AMI
  GetMinTau <- function(average_mutual_information) {
    tau <- 1
    for(i in 2:(length(average_mutual_information) - 1)) {
      if(average_mutual_information[[i]] < average_mutual_information[[i + 1]]) {
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
      child_data[measurement] <- GetMinTau(average_mutual_information)
    }
    tau_data[[child]] <- child_data
  }
  row.names(tau_data) <- measurement_names
  tau_data <- t(tau_data)
  
  if(cluster) {
    MyClust <- function(x,k,...){
      # Inputs data x
      # Outputs best diana clustering using k clusters
      dv <- diana(x,metric="manhattan")
      dvk <- cutree(as.hclust(dv),k)
      list(cluster=dvk)
    }
    # Find the optimal number of clusters from 1 to 5 of children in tau_data
    clusters <- clusGap(tau_data, FUNcluster = MyClust, K.max = 5)
    # print(maxSE(clusters$Tab[,3],clusters$Tab[,4],"Tibs2001SEmax"))
    print(clusters)
  }
  write.table(tau_data, paste(directory, "CCM_data/tau_data_total.csv", sep = ""), sep = ",")
}

GetTauData(measurement_names, list_child_df, sorted_name_list, cluster=FALSE)
tau_data <- read.table(paste(directory, "CCM_data/tau_data_total.csv", sep = ""), header = TRUE, sep = ",")
median_taus <- sapply(tau_data, median)


# Calculate optimal E
GetJointTauEmbed <- function(measurement_names, concat_series, median_taus, plot) {
  # Maximum embedding dimension E to test
  maxE<-5
  
  # Matrix for storing output
  Emat<-matrix(nrow=maxE-1, ncol=length(measurement_names)); 
  colnames(Emat)<-measurement_names
  # Loop over potential E values and calculate predictive ability of each process for its own dynamics
  # Uses defaults of looking forward one prediction step (predstep) and using time lag intervals from median_taus
  for(E in 2:maxE) { 
    for(measurement in measurement_names) {
      Emat[E-1,measurement]<-SSR_pred_boot(A=concat_series[[measurement]], E=E, predstep=1, tau=median_taus[[measurement]])$rho
    } 
  }
  if(plot) {
    #Look at plots to find E for each process at which predictive ability rho is maximized
    matplot(2:maxE, Emat, type="l", col=1:length(column_list), lty=1:length(column_list), xlab="E", ylab="rho", lwd=length(column_list))
    legend("bottomleft", column_list, lty=1:length(column_list), col=1:length(column_list), lwd=length(column_list), bty="n")
  }
  joint_tau_embed <- data.frame(matrix(ncol = 0, nrow =  2))
  
  for(measurement in measurement_names) {
    d <- which(Emat[,measurement]==max(Emat[,measurement])) + 1
    joint_tau_embed[[measurement]] <- c(median_taus[[measurement]],d)
  }
  
  rownames(joint_tau_embed) <- c("T", "E")
  colnames(joint_tau_embed) <- c("child_loq", "child_lex", "child_inf", "child_syn", "mother_loq", "mother_lex", "mother_inf", "mother_syn")
  write.table(joint_tau_embed, paste(directory, "CCM_data/joint_tau_embed_total.csv", sep = ""), sep = ",")
}
GetJointTauEmbed(measurement_names, concat_series, median_taus, plot = FALSE)
joint_tau_embed <- read.table(paste(directory, "CCM_data/joint_tau_embed_total.csv", sep = ""), header = TRUE, sep = ",")

options(digits=22)
colnames(concat_series) <- colnames(joint_tau_embed)
cross_child_names <- c("child_loq", "child_lex", "child_inf", "child_syn")
cross_mother_names <- c("mother_loq", "mother_lex", "mother_inf", "mother_syn")
inner_names <- c("loq", "lex", "inf", "syn")
p_values <- c()
p_names <- c()
signal_graph_names <- c("Loquacity", "Lexical", "Inflectional", "Syntactic")
dir.create(paste(directory, "CCM_data/Signal", sep = ""), showWarnings = FALSE)
dir.create(paste(directory, "CCM_data/Causal", sep = ""), showWarnings = FALSE)

# Perform cross analysis
index <- 1
for(i in 1:(length(cross_child_names))) {
  for(j in 1:(length(cross_mother_names))) {
    ptm <- proc.time()
    
    # Get the first series variables
    name1 <- cross_child_names[[i]]
    series1 <- concat_series[[name1]]
    E1 <- joint_tau_embed[["E", name1]]
    T1 <- joint_tau_embed[["T", name1]]
    signal1 <- SSR_check_signal(A=series1, E=E1, tau=T1, predsteplist=1:10)
    signal_plot1 <- signal1$predatout
    
    # Get the second series variables
    name2 <- cross_mother_names[[j]]
    series2 <- concat_series[[name2]]
    E2 <- joint_tau_embed[["E", name2]]
    T2 <- joint_tau_embed[["T", name2]]
    signal2 <- SSR_check_signal(A=series2, E=E2, tau=T2, predsteplist=1:10)
    signal_plot2 <- signal2$predatout
    
    # Check data for nonlinear signal that is not dominated by noise
    signal_filename <- paste(paste(directory, "CCM_data/Signal/", name1, sep = ""), name2, "signal.jpg", sep="_")
    jpeg(signal_filename)
    plot <- cbind(signal_plot1$rho, signal_plot2$rho)
    matplot(1:10, plot, type="l", col=1:2, lty=1:2, xlab="Prediction Steps", ylab=expression(rho), lwd=2)
    title(main = paste("Child", signal_graph_names[[i]], "and Mother" , signal_graph_names[[j]], sep = " "))
    legend("bottomleft", c(paste("Child", signal_graph_names[[i]]), paste("Mother", signal_graph_names[[j]])), lty=1:2, col=1:2, lwd=2, bty="n")
    dev.off()
    
    # Check whether predictive ability of processes declines with increasing time distance
    CCM_boot1 <- CCM_boot(series1, series2, E1, tau=T1, iterations=5)
    CCM_boot2 <- CCM_boot(series2, series1, E2, tau=T2, iterations=5)

    # Store boot data
    directory_name1 <- paste(paste(directory, "CCM_data/Causal/", name1, sep=""), "causes", name2, sep = "_")
    dir.create(directory_name1, showWarnings = FALSE)
    for(k in 1:(length(CCM_boot1))) {
      write(CCM_boot1[[k]], paste(directory_name1, "/", names(CCM_boot1)[[k]], sep = ""))
    }
    directory_name2 <- paste(paste(directory, "CCM_data/Causal/", name2, sep=""), "causes", name1, sep = "_")
    dir.create(directory_name2, showWarnings = FALSE)
    for(k in 1:(length(CCM_boot2))) {
      write(CCM_boot2[[k]], paste(directory_name2, "/", names(CCM_boot2)[[k]], sep = ""))
    }
    
    # Tests for significant causal signal based on 95% confidence intervals from bootstrapping.
    CCM_sig <- ccmtest(CCM_boot1, CCM_boot2)
    p_values <- c(p_values, CCM_sig)
    p_names <- c(p_names, paste(name1, "causes", name2), paste(name2, "causes", name1))
    
    print(index)
    index <- index + 1
    print(proc.time() - ptm)
  }
}

# Perform inner analysis
index <- 1
for(i in 1:(length(cross_child_names))) {
  for(j in 1:(length(cross_child_names))) {
    if(i < j) {
      ptm <- proc.time()
      
      # Get the first series variables
      name1 <- cross_child_names[[i]]
      series1 <- concat_series[[name1]]
      E1 <- joint_tau_embed[["E", name1]]
      T1 <- joint_tau_embed[["T", name1]]
      signal1 <- SSR_check_signal(A=series1, E=E1, tau=T1, predsteplist=1:10)
      signal_plot1 <- signal1$predatout
      
      # Get the second series variables
      name2 <- cross_child_names[[j]]
      series2 <- concat_series[[name2]]
      E2 <- joint_tau_embed[["E", name2]]
      T2 <- joint_tau_embed[["T", name2]]
      signal2 <- SSR_check_signal(A=series2, E=E2, tau=T2, predsteplist=1:10)
      signal_plot2 <- signal2$predatout
      
      # Check data for nonlinear signal that is not dominated by noise
      signal_filename <- paste(paste(directory, "CCM_data/Signal/bootstrap", sep = ""), name1, name2, "signal.jpg", sep="_")
      jpeg(signal_filename)
      plot <- cbind(signal_plot1$rho, signal_plot2$rho)
      matplot(1:10, plot, type="l", col=1:2, lty=1:2, xlab="Prediction Steps", ylab=expression(rho), lwd=2)
      title(main = paste(signal_graph_names[[i]], "and" , signal_graph_names[[j]], sep = " "))
      legend("bottomleft", c(signal_graph_names[[i]], signal_graph_names[[j]]), lty=1:2, col=1:2, lwd=2, bty="n")
      dev.off()
      
      # Check whether predictive ability of processes declines with increasing time distance
      CCM_boot1 <- CCM_boot(series1, series2, E1, tau=T1, iterations=5)
      CCM_boot2 <- CCM_boot(series2, series1, E2, tau=T2, iterations=5)
      
      # Store boot data
      directory_name1 <- paste(paste(directory, "CCM_data/Causal/bootstrap", sep=""), name1, "causes", name2, sep = "_")
      dir.create(directory_name1, showWarnings = FALSE)
      for(k in 1:(length(CCM_boot1))) {
        write(CCM_boot1[[k]], paste(directory_name1, "/", names(CCM_boot1)[[k]], sep = ""))
      }
      directory_name2 <- paste(paste(directory, "CCM_data/Causal/bootstrap", sep=""), name2, "causes", name1, sep = "_")
      dir.create(directory_name2, showWarnings = FALSE)
      for(k in 1:(length(CCM_boot2))) {
        write(CCM_boot2[[k]], paste(directory_name2, "/", names(CCM_boot2)[[k]], sep = ""))
      }
      
      # Tests for significant causal signal based on 95% confidence intervals from bootstrapping.
      CCM_sig <- ccmtest(CCM_boot1, CCM_boot2)
      p_values <- c(p_values, CCM_sig)
      p_names <- c(p_names, paste(name1, "causes", name2), paste(name2, "causes", name1))
      
      print(index)
      index <- index + 1
      print(proc.time() - ptm)
    }
  }
}

# Write all p_values
names(p_values) <- p_names
p_df <- t(data.frame(p_values))
write.table(format(p_df, digits=22), paste(directory, "CCM_data/eng_p_values.csv", sep = ""))
