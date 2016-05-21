library(multispatialCCM)
library(tseriesChaos)
library(cluster)

directory = "/Users/jeremyirvin/Desktop/SeniorThesis/Childes/nltk_childes/English/"

cross_child_names <- c("child_loq", "child_lex", "child_inf", "child_syn")
cross_mother_names <- c("mother_loq", "mother_lex", "mother_inf", "mother_syn")
formal_names <- c("Loquacity", "Lexical Diversity", "Inflectional Diversity",  "MLU")
p_df <- read.table(paste(directory, "CCM_data/eng_p_values.csv", sep = ""), header=TRUE)
fdr_p_values <- p.adjust(p_values, method="fdr")
fdr_df <- t(data.frame(fdr_p_values))
write.table(format(fdr_df, digits=22), paste(directory, "CCM_data/eng_fdr_p_values.csv", sep = ""))

dir.create(paste(directory, "CCM_data/Causal/Graphs", sep = ""), showWarnings = FALSE)

index <- 1
for(i in 1:(length(cross_child_names))) {
  for(j in 1:(length(cross_mother_names))) {
    name1 <- cross_child_names[[i]]
    name2 <- cross_mother_names[[j]]
    directory_name1 <- paste(paste(directory, "CCM_data/Causal/", name1, sep=""), "causes", name2, sep = "_")
    directory_name2 <- paste(paste(directory, "CCM_data/Causal/", name2, sep=""), "causes", name1, sep = "_")
    filenames1 <- list.files(directory_name1)
    CCM_boot1 <- vector("list", length(filenames1))
    names(CCM_boot1) <- filenames1
    for(k in 1:(length(filenames1))) {
      path1 <- paste(directory_name1, "/", filenames1[[k]], sep = "")
      file_content1 <- scan(path1)
      CCM_boot1[[k]] <- file_content1
    }
    
    filenames2 <- list.files(directory_name2)
    CCM_boot2 <- vector("list", length(filenames2))
    names(CCM_boot2) <- filenames2
    for(k in 1:(length(filenames2))) {
      path2 <- paste(directory_name2, "/", filenames2[[k]], sep = "")
      file_content2 <- scan(path2)
      CCM_boot2[[k]] <- file_content2
    }
    plotxlimits <- range(c(CCM_boot1$Lobs, CCM_boot2$Lobs))
    jpeg(paste(paste(directory, "CCM_data/Causal/Graphs/", name1, sep = ""), name2, "Cause.jpg", sep="_"))
    
    # Plot "series1 causes series2"
    plot(CCM_boot1$Lobs, CCM_boot1$rho, type="l", col=1, lwd=2, xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1), xlab="Library Length", ylab=expression(rho), cex.axis = 1.2, cex.lab = 1.3)
    title(main = paste("Child", formal_names[[i]], "and Mother" , formal_names[[j]], sep = " ")) # ****use cex.main = 1.5 for CROSS
    
    # Add +/- 1 standard error
    matlines(CCM_boot1$Lobs, cbind(CCM_boot1$rho-CCM_boot1$sdevrho, CCM_boot1$rho+CCM_boot1$sdevrho), lty=3, col=1)
    
    # Plot "series2 causes series1"
    lines(CCM_boot2$Lobs, CCM_boot2$rho, type="l", col=2, lty=2, lwd=2)
    
    # Add +/- 1 standard error
    matlines(CCM_boot2$Lobs, cbind(CCM_boot2$rho-CCM_boot2$sdevrho,CCM_boot2$rho+CCM_boot2$sdevrho), lty=3, col=2)
    c_m <- format(round(fdr_p_values[[2*index-1]],3), digits = 3)
    m_c <- format(round(fdr_p_values[[2*index]],3), digits = 3)
    
    # Change p value shown if smaller than 0.001
    c_m_legend <- if(fdr_p_values[[2*index-1]] > 0.000) paste(", p =", c_m) else ", p < 0.001"
    m_c_legend <- if(fdr_p_values[[2*index]] > 0.000) paste(", p =", m_c) else ", p < 0.001"
    legend("topleft", c(paste("Child drives Mother ", c_m_legend, sep = "") , paste("Mother drives Child ", m_c_legend, sep = "")), lty=c(1,2), col=c(1,2), lwd=2, bty="n", pt.cex = 1, cex = 1.15)
   
    dev.off()
    
    index <- index + 1
  }
}

offset <- length(cross_child_names)*length(cross_mother_names)
index <- 1
for(i in 1:(length(cross_child_names))) {
  for(j in 1:(length(cross_child_names))) {
    if(i < j) {
      name1 <- cross_child_names[[i]]
      name2 <- cross_child_names[[j]]
      directory_name1 <- paste(paste(directory, "CCM_data/Causal/bootstrap", sep=""), name1, "causes", name2, sep = "_")
      print(directory_name1)
      directory_name2 <- paste(paste(directory, "CCM_data/Causal/bootstrap", sep=""), name2, "causes", name1, sep = "_")
      filenames1 <- list.files(directory_name1)
      CCM_boot1 <- vector("list", length(filenames1))
      names(CCM_boot1) <- filenames1
      for(k in 1:(length(filenames1))) {
        path1 <- paste(directory_name1, "/", filenames1[[k]], sep = "")
        file_content1 <- scan(path1)
        CCM_boot1[[k]] <- file_content1
      }
      
      filenames2 <- list.files(directory_name2)
      CCM_boot2 <- vector("list", length(filenames2))
      names(CCM_boot2) <- filenames2
      for(k in 1:(length(filenames2))) {
        path2 <- paste(directory_name2, "/", filenames2[[k]], sep = "")
        file_content2 <- scan(path2)
        CCM_boot2[[k]] <- file_content2
      }
      plotxlimits <- range(c(CCM_boot1$Lobs, CCM_boot2$Lobs))
      jpeg(paste(paste(directory, "CCM_data/Causal/Graphs/bootstrap_", name1, sep = ""), name2, "Cause.jpg", sep="_"))
      
      # Plot "series1 causes series2"
      plot(CCM_boot1$Lobs, CCM_boot1$rho, type="l", col=1, lwd=2, xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1), xlab="Library Length", ylab=expression(rho), cex.axis = 1.2, cex.lab = 1.3)
      title(main = paste(formal_names[[i]], "and" , formal_names[[j]], sep = " "), cex.main = 1.5)
      
      # Add +/- 1 standard error
      matlines(CCM_boot1$Lobs, cbind(CCM_boot1$rho-CCM_boot1$sdevrho, CCM_boot1$rho+CCM_boot1$sdevrho), lty=3, col=1)
      
      # Plot "series2 causes series1"
      lines(CCM_boot2$Lobs, CCM_boot2$rho, type="l", col=2, lty=2, lwd=2)
      
      # Add +/- 1 standard error
      matlines(CCM_boot2$Lobs, cbind(CCM_boot2$rho-CCM_boot2$sdevrho,CCM_boot2$rho+CCM_boot2$sdevrho), lty=3, col=2)
      c1_c2 <- format(round(fdr_p_values[[2 * (index + offset) - 1]], 3), digits = 3)
      c2_c1 <- format(round(fdr_p_values[[2 * (index + offset)]], 3), digits = 3)
      
      # Change p value shown if smaller than 0.001
      c1_c2_legend <- if(fdr_p_values[[2 * (index + offset) - 1]] > 0.000) paste(", p =", c1_c2) else ", p < 0.001"
      c2_c1_legend <- if(fdr_p_values[[2 * (index + offset)]] > 0.000) paste(", p =", c2_c1) else ", p < 0.001"
      legend("topleft", c(paste(formal_names[[i]], " drives ", formal_names[[j]], c1_c2_legend, sep = "") , paste(formal_names[[j]], " drives ", formal_names[[i]], c2_c1_legend, sep = "")), lty=c(1,2), col=c(1,2), lwd=2, bty="n", pt.cex = 1, cex = 1.15)
      
      dev.off()
      
      index <- index + 1
    }
  }
}