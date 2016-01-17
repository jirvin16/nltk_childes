library(multispatialCCM)
library(tseriesChaos)
library(cluster)

df <- read.table("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/NLTKCHILDES/morph-eng.csv", header = TRUE)

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
#     plot(average_mutual_information) WORTH INVESTIGATING TAUS
    #     print(sprintf("Tau for stat %s for child %s is %d", measurement, sorted_name_list[[i]], GetMinTau(ami)))
    child_data[measurement] <- GetMinTau(average_mutual_information)
  }
  tau_data[[child]] <- child_data
}

row.names(tau_data) <- measurement_names
# print(tau_data)
x <- t(tau_data)

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

write.table(x, "/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/tau_data.csv", sep = ",")

tau_data <- read.table("/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/tau_data.csv", header = TRUE, sep = ",")

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

#Calculate optimal E

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

# par(mfrow=c(10,1),mar=c(1,1,1,1))
p_values <- c()
names <- c()
for(i in 1:(length(measurement_names)/2)) {
  #Check data for nonlinear signal that is not dominated by noise
  #Checks whether predictive ability of processes declines with increasing time distance
  ptm <- proc.time()
  child_col <- measurement_names[[i]]
  child_series <- concat_series[[child_col]]
  child_E <- joint_tau_embed["E",child_col]
  child_T <- joint_tau_embed["T",child_col]
  
  mother_col <- measurement_names[[i + length(measurement_names)/2]]
  mother_series <- concat_series[[mother_col]]
  mother_E <- joint_tau_embed["E",mother_col]
  mother_T <- joint_tau_embed["T",mother_col]
  
  child_signal <- SSR_check_signal(A=child_series, E=child_E, tau=child_T, predsteplist=1:10)
  child_signal_plot <- child_signal$predatout
  
  mother_signal <- SSR_check_signal(A=mother_series, E=mother_E, tau=mother_T, predsteplist=1:10)
  mother_signal_plot <- mother_signal$predatout
  
  jpeg(paste(child_col, mother_col, "Signal.jpg", sep="_"))
  plot <- cbind(child_signal_plot$rho, mother_signal_plot$rho)
  matplot(1:10, plot, type="l", col=1:2, lty=1:2, xlab="prediction steps", ylab="rho", lwd=2)
  legend("bottomleft", c(child_col, mother_col), lty=1:2, col=1:2, lwd=2, bty="n")
  dev.off()
  
  # Does child series "cause" mother series?
  # Note - increase iterations to 100 for consistant results

  CCM_boot_child<-CCM_boot(child_series, mother_series, child_E, tau=child_T, iterations=100)
  
  # Does mother series "cause" child series?
  CCM_boot_mother<-CCM_boot(mother_series, child_series, mother_E, tau= mother_T, iterations=100)
  
  
  # Tests for significant causal signal based on 95%
  # confidence intervals from bootstrapping.
  CCM_significance_test <- ccmtest(CCM_boot_child,CCM_boot_mother) 
  names <- c(names, paste(child_col, "causes", mother_col, CCM_significance_test[[1]]), paste(mother_col, "causes", child_col, CCM_significance_test[[2]]))
  p_values <- c(p_values, CCM_significance_test)
  
  # Plot results
  plotxlimits<-range(c(CCM_boot_child$Lobs, CCM_boot_mother$Lobs))
  
  jpeg(paste(child_col, mother_col, "Cause.jpg", sep="_"))
  # Plot "child series causes mother series"
  plot(CCM_boot_child$Lobs, CCM_boot_child$rho, type="l", col=1, lwd=2, xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1), xlab="L", ylab="rho")
  
  # Add +/- 1 standard error
  matlines(CCM_boot_child$Lobs, cbind(CCM_boot_child$rho-CCM_boot_child$sdevrho, CCM_boot_child$rho+CCM_boot_child$sdevrho), lty=3, col=1)
  
  # Plot "mother inflection causes child inflection"
  lines(CCM_boot_mother$Lobs, CCM_boot_mother$rho, type="l", col=2, lty=2, lwd=2)
  
  # Add +/- 1 standard error
  matlines(CCM_boot_mother$Lobs, cbind(CCM_boot_mother$rho-CCM_boot_mother$sdevrho,CCM_boot_mother$rho+CCM_boot_mother$sdevrho), lty=3, col=2)
  legend("topleft", c(paste(child_col, "causes", mother_col), paste(mother_col, "causes", child_col)), lty=c(1,2), col=c(1,2), lwd=2, bty="n")
  dev.off()
  print(i)
  print(proc.time() - ptm)
# break
}
names(p_values) <- names
fdr_p_values <- (p.adjust(p_values, method="fdr"))
names(fdr_p_values) <- names


