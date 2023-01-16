# Mark - Release - Recapture script


############## 1st part ####################

# Real number of individuals
N <- 1000

# Sampling efforts (10-100% by 5% intervals)
sampling_efforts_values <- seq(0.1, 1, 0.05)

# Matrix where all estimated N are going to be stored, for each attempt
# and each sampling effort
test <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 100)
test <- as.data.frame(test)
colnames(test) <- sampling_efforts_values

# Matrix that links sampling ID with sampling effort (e.g. sampling effort 15% 
# is ID 2 in the matrix)
sampling_efforts <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 2)
sampling_efforts[1, ] <- c(1:length(sampling_efforts_values))
sampling_efforts[2, ] <- sampling_efforts_values
sampling_efforts <- as.data.frame(sampling_efforts)

# Vector that is going to store all estimated N for each sampling effort
estimated_N_vector <- rep(NA, 100)

# Matrix where the mean, sd, inferior and superior limit are going to be stored
final_results <- matrix(NA, nrow = 6 , ncol =ncol(sampling_efforts))
colnames(final_results) <- sampling_efforts[2, ]
rownames(final_results) <- c("mean", "sd", "median", "inferior limit", "superior limit", "inside 5% interval")
final_results <- as.data.frame(final_results)


for (sampling in sampling_efforts[1, ]){
  # For each sampling effort:
  
  attempts <- 1
  
  while (attempts <= 100) { 
    
    sampling_effort_1 <- N * sampling_efforts[2, sampling]
    sampling_effort_2 <- N * sampling_efforts[2, sampling]
    # We could change the sampling effort within the same sample 
    
    # Matrix that stores the marking - recapturing process
    data <- matrix(NA, ncol = 3, nrow = N)
    data <- as.data.frame(data)
    colnames(data) <- c("ID", "Captured_1", "captured_2")
    data$ID <- c(1:N)
    
    # Animals captured (and marked) on the first sample
    marked <- sample(1:N, sampling_effort_1)
    data$Captured_1 <- as.numeric(data$ID %in% marked)
    
    # Animals recaptured on the second sample
    recaptured <- sample(1:N, sampling_effort_2)
    data$captured_2 <- as.numeric(data$ID %in% recaptured)
    
    # Marked and recaptured animals
    marked_and_recaptured <- nrow(subset(data, Captured_1 == 1 & captured_2 == 1))
    
    # Estimated N for this sample
    estimated_N <- (sampling_effort_1 * sampling_effort_2)/marked_and_recaptured
    
    estimated_N_vector[attempts] <- estimated_N
    
    attempts <- attempts + 1
    
  }
  
  # Estimated N of each attempt and sampling effort are stored here
  test[, sampling] <- estimated_N_vector
  
  final_results[1, sampling] <- mean(test[, sampling])
  final_results[2, sampling] <- sd(test[, sampling])
  final_results[3, sampling] <- median(test[, sampling])
  
  # Inferior and superior limits of the CI (95%)
  final_results[4, sampling] <- final_results[1, sampling] - qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
  final_results[5, sampling] <- final_results[1, sampling] + qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
  
  # How many of the estimated N of each sampling effort are inside the N +/- 2.5%
  final_results[6, sampling] <- length(subset(estimated_N_vector, estimated_N_vector >= N - (N * 0.025) & estimated_N_vector <= N + (N * 0.025)))
  
  
}

# Boxplots
boxplot(test)

# From what sampling effort the calculated N is within N +/- 2.5%
interval <- as.numeric(final_results[6,])

plot(interval, type = "l", ylab = "mean inside N +/- 2.5%", xlab = "Sampling effort", xaxt="n")
axis(1, at = 1:19, labels =  seq(0.1, 1, 0.05))

abline(h = 95, col = "red")

interval <- as.data.frame(interval)
interval[, "samp.eff"] <- seq(0.1, 1, 0.05)

ggplot(interval, aes(samp.eff, interval)) + xlab("Sampling effort") + ylab("mean inside N +/- 2.5%") + geom_line() + scale_x_continuous(breaks=seq(0.1,1,0.05)) + scale_y_continuous(breaks=seq(0,100,10)) + theme(axis.text.x = element_text(angle=45, hjust=1)) + geom_vline(xintercept = interval$samp.eff[interval >= 95][1], col = "red") + geom_hline(yintercept =  95)
# With little sampling effort, this method of marking > N +/- 2.5% is not accurate. 



############# 2nd part ###############



# Now with more sampling effort

# Sampling efforts (10-100% by 1% intervals)
sampling_efforts_values <- seq(0.1, 1, 0.005)

# Matrix where all estimated N are going to be stored, for each attempt
# and each sampling effort
test <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 100)
test <- as.data.frame(test)
colnames(test) <- sampling_efforts_values

# Matrix that links sampling ID with sampling effort (e.g. sampling effort 15% 
# is ID 2 in the matrix)
sampling_efforts <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 2)
sampling_efforts[1, ] <- c(1:length(sampling_efforts_values))
sampling_efforts[2, ] <- sampling_efforts_values
sampling_efforts <- as.data.frame(sampling_efforts)

# Vector that is going to store all estimated N for each sampling effort
estimated_N_vector <- rep(NA, 100)

# Matrix where the mean, sd, inferior and superior limit are going to be stored
final_results <- matrix(NA, nrow = 6 , ncol =ncol(sampling_efforts))
colnames(final_results) <- sampling_efforts[2, ]
rownames(final_results) <- c("mean", "sd", "median", "inferior limit", "superior limit", "inside 5% interval")
final_results <- as.data.frame(final_results)

experiment <- function(){
  
  for (sampling in sampling_efforts[1, ]){
    # For each sampling effort:
    
    attempts <- 1
    
    while (attempts <= 100) { 
      
      sampling_effort_1 <- N * sampling_efforts[2, sampling]
      sampling_effort_2 <- N * sampling_efforts[2, sampling]
      # We could change the sampling effort within the same sample 
      
      # Matrix that stores the marking - recapturing process
      data <- matrix(NA, ncol = 3, nrow = N)
      data <- as.data.frame(data)
      colnames(data) <- c("ID", "Captured_1", "captured_2")
      data$ID <- c(1:N)
      
      # Animals captured (and marked) on the first sample
      marked <- sample(1:N, sampling_effort_1)
      data$Captured_1 <- as.numeric(data$ID %in% marked)
      
      # Animals recaptured on the second sample
      recaptured <- sample(1:N, sampling_effort_2)
      data$captured_2 <- as.numeric(data$ID %in% recaptured)
      
      # Marked and recaptured animals
      marked_and_recaptured <- nrow(subset(data, Captured_1 == 1 & captured_2 == 1))
      
      # Estimated N for this sample
      estimated_N <- (sampling_effort_1 * sampling_effort_2)/marked_and_recaptured
      
      estimated_N_vector[attempts] <- estimated_N
      
      attempts <- attempts + 1
      
    }
    
    # Estimated N of each attempt and sampling effort are stored here
    test[, sampling] <- estimated_N_vector
    
    final_results[1, sampling] <- mean(test[, sampling])
    final_results[2, sampling] <- sd(test[, sampling])
    final_results[3, sampling] <- median(test[, sampling])
    
    # Inferior and superior limits of the CI (95%)
    final_results[4, sampling] <- final_results[1, sampling] - qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
    final_results[5, sampling] <- final_results[1, sampling] + qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
    
    # How many of the estimated N of each sampling effort are inside the N +/- 2.5%
    final_results[6, sampling] <- length(subset(estimated_N_vector, estimated_N_vector >= N - (N * 0.025) & estimated_N_vector <= N + (N * 0.025)))
    
    # Data frame that stores sampling effort and N inside N +/- 2.5%
    interval <- as.numeric(final_results[6,])
    interval <- as.data.frame(interval)
    interval[, "samp.eff"] <- seq(0.1, 1, 0.005)
    
    assign("final_results", final_results, envir =  globalenv())
    assign("test", test, envir =  globalenv())
    assign("interval", interval, envir =  globalenv())
    
  }
  
  if(any(final_results[6,] == 95) == FALSE){
    
    experiment()
    
  } else {
    
    interval$samp.eff[interval == 95][1]
    
  }
  
}


experiment()

# Boxplots
boxplot(test)

# From what sampling effort the calculated N is within N +/- 2.5%
#interval <- as.numeric(final_results[6,])
#interval <- as.data.frame(interval)
#interval[, "samp.eff"] <- seq(0.1, 1, 0.005)

ggplot(interval, aes(samp.eff, interval)) + xlab("Sampling effort") + ylab("mean inside N +/- 2.5%") + geom_line() + scale_x_continuous(breaks=seq(0.1,1,0.05)) + scale_y_continuous(breaks=seq(0,100,10)) + theme(axis.text.x = element_text(angle=45, hjust=1)) + geom_vline(xintercept = interval$samp.eff[interval == 95][1], col = "red")

# This was done when I needed an accuracy of exactly 95 to plot the red line, but you can obtain better results before getting an accuracy of 95. Next version corrects that. 

############# 3rd version ################

experiment <- function(){
  
  for (sampling in sampling_efforts[1, ]){
    # For each sampling effort:
    
    attempts <- 1
    
    while (attempts <= 100) { 
      
      sampling_effort_1 <- N * sampling_efforts[2, sampling]
      sampling_effort_2 <- N * sampling_efforts[2, sampling]
      # We could change the sampling effort within the same sample 
      
      # Matrix that stores the marking - recapturing process
      data <- matrix(NA, ncol = 3, nrow = N)
      data <- as.data.frame(data)
      colnames(data) <- c("ID", "Captured_1", "captured_2")
      data$ID <- c(1:N)
      
      # Animals captured (and marked) on the first sample
      marked <- sample(1:N, sampling_effort_1)
      data$Captured_1 <- as.numeric(data$ID %in% marked)
      
      # Animals recaptured on the second sample
      recaptured <- sample(1:N, sampling_effort_2)
      data$captured_2 <- as.numeric(data$ID %in% recaptured)
      
      # Marked and recaptured animals
      marked_and_recaptured <- nrow(subset(data, Captured_1 == 1 & captured_2 == 1))
      
      # Estimated N for this sample
      estimated_N <- (sampling_effort_1 * sampling_effort_2)/marked_and_recaptured
      
      estimated_N_vector[attempts] <- estimated_N
      
      attempts <- attempts + 1
      
    }
    
    # Estimated N of each attempt and sampling effort are stored here
    test[, sampling] <- estimated_N_vector
    
    final_results[1, sampling] <- mean(test[, sampling])
    final_results[2, sampling] <- sd(test[, sampling])
    final_results[3, sampling] <- median(test[, sampling])
    
    # Inferior and superior limits of the CI (95%)
    final_results[4, sampling] <- final_results[1, sampling] - qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
    final_results[5, sampling] <- final_results[1, sampling] + qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
    
    # How many of the estimated N of each sampling effort are inside the N +/- 2.5%
    final_results[6, sampling] <- length(subset(estimated_N_vector, estimated_N_vector >= N - (N * 0.025) & estimated_N_vector <= N + (N * 0.025)))
    
    # Data frame that stores sampling effort and N inside N +/- 2.5%
    interval <- as.numeric(final_results[6,])
    interval <- as.data.frame(interval)
    interval[, "samp.eff"] <- seq(0.1, 1, 0.005)
    
    # Objects that are needed to plot the results
    assign("final_results", final_results, envir =  globalenv())
    assign("test", test, envir =  globalenv())
    assign("interval", interval, envir =  globalenv())
    
  }
  
  print("We can expect an accuracy equal to or better than 95% from a sampling effort of:")
  interval$samp.eff[interval >= 95][1]
  
}

experiment()

boxplot(test)

ggplot(interval, aes(samp.eff, interval)) + xlab("Sampling effort") + ylab("mean inside N +/- 2.5%") + geom_line() + scale_x_continuous(breaks=seq(0.1,1,0.05)) + scale_y_continuous(breaks=seq(0,100,10)) + theme(axis.text.x = element_text(angle=45, hjust=1)) + geom_vline(xintercept = interval$samp.eff[interval >= 95][1], col = "red")


############# 4th part ##################


# Automatization

# Real population (number of individuals)
N <- 1000

# Sampling effort resolution (percentage increase in sampling effort)
eff <- 0.5 # in percentage

experiment <- function(N, eff){
  
  # Sampling efforts (10-100% by 1% intervals)
  sampling_efforts_values <- seq(0.1, 1, eff/100)
  
  # Matrix where all estimated N are going to be stored, for each attempt and each sampling effort
  test <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 100)
  test <- as.data.frame(test)
  colnames(test) <- sampling_efforts_values
  
  # Matrix that links sampling ID with sampling effort
  sampling_efforts <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 2)
  sampling_efforts[1, ] <- c(1:length(sampling_efforts_values))
  sampling_efforts[2, ] <- sampling_efforts_values
  sampling_efforts <- as.data.frame(sampling_efforts)
  
  # Vector that is going to store all estimated N for each sampling effort
  estimated_N_vector <- rep(NA, 100)
  
  # Matrix where the mean, sd, inferior and superior limit are going to be stored
  final_results <- matrix(NA, nrow = 6 , ncol =ncol(sampling_efforts))
  colnames(final_results) <- sampling_efforts[2, ]
  rownames(final_results) <- c("mean", "sd", "median", "inferior limit", "superior limit", "inside 5% interval")
  final_results <- as.data.frame(final_results)
  
  for (sampling in sampling_efforts[1, ]){
    # For each sampling effort:
    
    attempts <- 1
    
    while (attempts <= 100) { 
      
      sampling_effort_1 <- N * sampling_efforts[2, sampling]
      sampling_effort_2 <- N * sampling_efforts[2, sampling]
      # We could change the sampling effort within the same sample 
      
      # Matrix that stores the marking - recapturing process
      data <- matrix(NA, ncol = 3, nrow = N)
      data <- as.data.frame(data)
      colnames(data) <- c("ID", "Captured_1", "captured_2")
      data$ID <- c(1:N)
      
      # Animals captured (and marked) on the first sample
      marked <- sample(1:N, sampling_effort_1)
      data$Captured_1 <- as.numeric(data$ID %in% marked)
      
      # Animals recaptured on the second sample
      recaptured <- sample(1:N, sampling_effort_2)
      data$captured_2 <- as.numeric(data$ID %in% recaptured)
      
      # Marked and recaptured animals
      marked_and_recaptured <- nrow(subset(data, Captured_1 == 1 & captured_2 == 1))
      
      # Estimated N for this sample
      estimated_N <- (sampling_effort_1 * sampling_effort_2)/marked_and_recaptured
      
      estimated_N_vector[attempts] <- estimated_N
      
      attempts <- attempts + 1
      
    }
    
    # Estimated N of each attempt and sampling effort are stored here
    test[, sampling] <- estimated_N_vector
    
    final_results[1, sampling] <- mean(test[, sampling])
    final_results[2, sampling] <- sd(test[, sampling])
    final_results[3, sampling] <- median(test[, sampling])
    
    # Inferior and superior limits of the CI (95%)
    final_results[4, sampling] <- final_results[1, sampling] - qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
    final_results[5, sampling] <- final_results[1, sampling] + qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
    
    # How many of the estimated N of each sampling effort are inside the N +/- 2.5%
    final_results[6, sampling] <- length(subset(estimated_N_vector, estimated_N_vector >= N - (N * 0.025) & estimated_N_vector <= N + (N * 0.025)))
    
    # Data frame that stores sampling effort and N inside N +/- 2.5%
    interval <- as.numeric(final_results[6,])
    interval <- as.data.frame(interval)
    interval[, "samp.eff"] <- sampling_efforts_values
    
    # Objects that are needed to plot the results
    assign("final_results", final_results, envir =  globalenv())
    assign("test", test, envir =  globalenv())
    assign("interval", interval, envir =  globalenv())
    
    
  }
  
  print("We can expect an accuracy equal to or better than 95% from a sampling effort of:")
  interval$samp.eff[interval >= 95][1]
  
  
}


experiment(N, eff)

boxplot(test)

ggplot(interval, aes(samp.eff, interval)) + xlab("Sampling effort") + ylab("mean inside N +/- 2.5%") + geom_line() + scale_x_continuous(breaks=seq(0.1,1,0.05)) + scale_y_continuous(breaks=seq(0,100,10)) + theme(axis.text.x = element_text(angle=45, hjust=1)) + geom_vline(xintercept = interval$samp.eff[interval >= 95][1], col = "red")





############### 5th part ###################

experiment <- function(N = 1000, eff = 0.5, rep = 10){
  
  # Sampling efforts (10-100% by 1% intervals)
  sampling_efforts_values <- seq(0.1, 1, eff/100)
  
  # Matrix where all estimated N are going to be stored, for each attempt and each sampling effort
  test <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 100)
  test <- as.data.frame(test)
  colnames(test) <- sampling_efforts_values
  
  # Matrix that links sampling ID with sampling effort
  sampling_efforts <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 2)
  sampling_efforts[1, ] <- c(1:length(sampling_efforts_values))
  sampling_efforts[2, ] <- sampling_efforts_values
  sampling_efforts <- as.data.frame(sampling_efforts)
  
  # Vector that is going to store all estimated N for each sampling effort
  estimated_N_vector <- rep(NA, 100)
  
  # Matrix where the mean, sd, inferior and superior limit are going to be stored
  final_results <- matrix(NA, nrow = 6 , ncol =ncol(sampling_efforts))
  colnames(final_results) <- sampling_efforts[2, ]
  rownames(final_results) <- c("mean", "sd", "median", "inferior limit", "superior limit", "inside 5% interval")
  final_results <- as.data.frame(final_results)
  
  
  # Data frame to plot the final results
  plot_values <- as.data.frame(t(sampling_efforts))
  colnames(plot_values) <- c("ID", "samp.eff")
  
  # Matrix that stores the sampling effort of each repetition
  mean_95 <- matrix(NA, ncol = rep, nrow = 1)
  mean_95 <- as.data.frame(mean_95)
  
  
  for(j in 1:rep) {
    
    for (sampling in sampling_efforts[1, ]){
      # For each sampling effort:
      
      attempts <- 1
      
      while (attempts <= 100) { 
        
        sampling_effort_1 <- N * sampling_efforts[2, sampling]
        sampling_effort_2 <- N * sampling_efforts[2, sampling]
        # We could change the sampling effort within the same sample 
        
        # Matrix that stores the marking - recapturing process
        data <- matrix(NA, ncol = 3, nrow = N)
        data <- as.data.frame(data)
        colnames(data) <- c("ID", "Captured_1", "captured_2")
        data$ID <- c(1:N)
        
        # Animals captured (and marked) on the first sample
        marked <- sample(1:N, sampling_effort_1)
        data$Captured_1 <- as.numeric(data$ID %in% marked)
        
        # Animals recaptured on the second sample
        recaptured <- sample(1:N, sampling_effort_2)
        data$captured_2 <- as.numeric(data$ID %in% recaptured)
        
        # Marked and recaptured animals
        marked_and_recaptured <- nrow(subset(data, Captured_1 == 1 & captured_2 == 1))
        
        # Estimated N for this sample
        estimated_N <- (sampling_effort_1 * sampling_effort_2)/marked_and_recaptured
        
        estimated_N_vector[attempts] <- estimated_N
        
        attempts <- attempts + 1
        
      }
      
      # Estimated N of each attempt and sampling effort are stored here
      test[, sampling] <- estimated_N_vector
      
      final_results[1, sampling] <- mean(test[, sampling])
      final_results[2, sampling] <- sd(test[, sampling])
      final_results[3, sampling] <- median(test[, sampling])
      
      # Inferior and superior limits of the CI (95%)
      final_results[4, sampling] <- final_results[1, sampling] - qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
      final_results[5, sampling] <- final_results[1, sampling] + qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
      
      # How many of the estimated N of each sampling effort are inside the N +/- 2.5%
      final_results[6, sampling] <- length(subset(estimated_N_vector, estimated_N_vector >= N - (N * 0.025) & estimated_N_vector <= N + (N * 0.025)))
      
      # We store the final results
      assign(paste0("exp", j), final_results[6, ])
      plot_values[, j + 2] <- assign(paste0("exp", j), as.numeric(final_results[6, ]))
      
      # We store the necessary effort to reach 95%
      mean_95[1, j] <- plot_values$samp.eff[plot_values[, j + 2] >= 95][1]
      
      # Objects that are needed to plot the results
      assign("final_results", final_results, envir =  globalenv())
      assign("test", test, envir =  globalenv())
      assign("plot_values", plot_values, envir =  globalenv())
      assign("mean_95", mean_95, envir =  globalenv())
      
      
    }
    
  }
  
  print("We can expect an accuracy equal to or better than 95% from a sampling effort of:")
  round(mean(as.numeric(mean_95[1, ])), digits = 2)
  
}

experiment(N = 5000, eff = 0.5, rep = 10)

# If the number of repetitions is modified, more "+ geom_line(aes(y = V"rep+2"))" have to be added below

ggplot(plot_values, aes(x = samp.eff)) + xlab("Sampling effort (%)") + ylab("Mean inside N ? 2.5% (%)") + scale_x_continuous(breaks=seq(0.1,1,0.05), labels = seq(10,100,5)) + scale_y_continuous(breaks=seq(0,100,10)) + theme(axis.text.x = element_text(angle=45, hjust=1)) + geom_line(aes(y = V3), col = "red") + geom_line(aes(y = V4), col = "blue") + geom_line(aes(y = V5), col = "green") + geom_line(aes(y = V6), col = "pink") + geom_line(aes(y = V7), col = "yellow") + geom_line(aes(y = V8), col = "purple")+ geom_line(aes(y = V9), col = "orange") + geom_line(aes(y = V10), col = "aquamarine") + geom_line(aes(y = V11), col = "forestgreen") + geom_line(aes(y = V12)) + geom_vline(xintercept = round(mean(as.numeric(mean_95[1, ])), digits = 2), col = "red") + theme_classic() # + geom_hline(yintercept = 95, col = "red")

# from the last repetition
boxplot(test)
abline(v = which(plot_values$samp.eff %in% round(mean(as.numeric(mean_95[1, ])), digits = 2)), col = "red")


test.L <- gather(test)

ggplot(test.L, aes(x = key, y = value)) + geom_violin() + theme_classic()

ggplot(test.L, aes(x = key, y = value)) + geom_boxplot(outlier.shape = 19, outlier.size=0.5) + theme_classic() + labs(x = "Sampling effort (%)", y = "Population size (N)") + scale_x_discrete(breaks=seq(0.1,1,0.05), labels = seq(10,100,5)) + theme(axis.text.x = element_text(angle=0, hjust=0.5)) + scale_y_continuous(breaks=seq(0,4000,500))


# + geom_hline(yintercept = (1000 + 1000*0.025), col = "red") + geom_hline(yintercept = (1000 - 1000*0.025), col = "red")

par(mfrow = c(1, 2))

experiment(N = 500, eff = 0.5, rep = 10)

plot1 <- ggplot(plot_values, aes(x = samp.eff)) + xlab("Sampling effort (%)") + ylab("Mean inside N ? 2.5% (%)") + scale_x_continuous(breaks=seq(0.1,1,0.05), labels = seq(10,100,5)) + scale_y_continuous(breaks=seq(0,100,10)) + geom_line(aes(y = V3), col = "red") + geom_line(aes(y = V4), col = "blue") + geom_line(aes(y = V5), col = "green") + geom_line(aes(y = V6), col = "pink") + geom_line(aes(y = V7), col = "yellow") + geom_line(aes(y = V8), col = "purple")+ geom_line(aes(y = V9), col = "orange") + geom_line(aes(y = V10), col = "aquamarine") + geom_line(aes(y = V11), col = "forestgreen") + geom_line(aes(y = V12)) + geom_vline(xintercept = round(mean(as.numeric(mean_95[1, ])), digits = 2), col = "red") + theme_classic() + theme(axis.text.x = element_text(angle=45, hjust=1)) # + geom_hline(yintercept = 95, col = "red")

experiment(N = 5000, eff = 0.5, rep = 10)

plot2 <- ggplot(plot_values, aes(x = samp.eff)) + xlab("Sampling effort (%)") + ylab("Mean inside N ? 2.5% (%)") + scale_x_continuous(breaks=seq(0.1,1,0.05), labels = seq(10,100,5)) + scale_y_continuous(breaks=seq(0,100,10)) + geom_line(aes(y = V3), col = "red") + geom_line(aes(y = V4), col = "blue") + geom_line(aes(y = V5), col = "green") + geom_line(aes(y = V6), col = "pink") + geom_line(aes(y = V7), col = "yellow") + geom_line(aes(y = V8), col = "purple")+ geom_line(aes(y = V9), col = "orange") + geom_line(aes(y = V10), col = "aquamarine") + geom_line(aes(y = V11), col = "forestgreen") + geom_line(aes(y = V12)) + geom_vline(xintercept = round(mean(as.numeric(mean_95[1, ])), digits = 2), col = "red") + theme_classic() + theme(axis.text.x = element_text(angle=45, hjust=1)) # + geom_hline(yintercept = 95, col = "red")


grid.arrange(plot1, plot2, ncol=2)




# 90%

experiment <- function(N = 1000, eff = 0.5, rep = 10){
  
  # Sampling efforts (10-100% by 1% intervals)
  sampling_efforts_values <- seq(0.1, 1, eff/100)
  
  # Matrix where all estimated N are going to be stored, for each attempt and each sampling effort
  test <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 100)
  test <- as.data.frame(test)
  colnames(test) <- sampling_efforts_values
  
  # Matrix that links sampling ID with sampling effort
  sampling_efforts <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 2)
  sampling_efforts[1, ] <- c(1:length(sampling_efforts_values))
  sampling_efforts[2, ] <- sampling_efforts_values
  sampling_efforts <- as.data.frame(sampling_efforts)
  
  # Vector that is going to store all estimated N for each sampling effort
  estimated_N_vector <- rep(NA, 100)
  
  # Matrix where the mean, sd, inferior and superior limit are going to be stored
  final_results <- matrix(NA, nrow = 6 , ncol =ncol(sampling_efforts))
  colnames(final_results) <- sampling_efforts[2, ]
  rownames(final_results) <- c("mean", "sd", "median", "inferior limit", "superior limit", "inside 5% interval")
  final_results <- as.data.frame(final_results)
  
  
  # Data frame to plot the final results
  plot_values <- as.data.frame(t(sampling_efforts))
  colnames(plot_values) <- c("ID", "samp.eff")
  
  # Matrix that stores the sampling effort of each repetition
  mean_95 <- matrix(NA, ncol = rep, nrow = 1)
  mean_95 <- as.data.frame(mean_95)
  
  
  for(j in 1:rep) {
    
    for (sampling in sampling_efforts[1, ]){
      # For each sampling effort:
      
      attempts <- 1
      
      while (attempts <= 100) { 
        
        sampling_effort_1 <- N * sampling_efforts[2, sampling]
        sampling_effort_2 <- N * sampling_efforts[2, sampling]
        # We could change the sampling effort within the same sample 
        
        # Matrix that stores the marking - recapturing process
        data <- matrix(NA, ncol = 3, nrow = N)
        data <- as.data.frame(data)
        colnames(data) <- c("ID", "Captured_1", "captured_2")
        data$ID <- c(1:N)
        
        # Animals captured (and marked) on the first sample
        marked <- sample(1:N, sampling_effort_1)
        data$Captured_1 <- as.numeric(data$ID %in% marked)
        
        # Animals recaptured on the second sample
        recaptured <- sample(1:N, sampling_effort_2)
        data$captured_2 <- as.numeric(data$ID %in% recaptured)
        
        # Marked and recaptured animals
        marked_and_recaptured <- nrow(subset(data, Captured_1 == 1 & captured_2 == 1))
        
        # Estimated N for this sample
        estimated_N <- (sampling_effort_1 * sampling_effort_2)/marked_and_recaptured
        
        estimated_N_vector[attempts] <- estimated_N
        
        attempts <- attempts + 1
        
      }
      
      # Estimated N of each attempt and sampling effort are stored here
      test[, sampling] <- estimated_N_vector
      
      final_results[1, sampling] <- mean(test[, sampling])
      final_results[2, sampling] <- sd(test[, sampling])
      final_results[3, sampling] <- median(test[, sampling])
      
      # Inferior and superior limits of the CI (95%)
      final_results[4, sampling] <- final_results[1, sampling] - qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
      final_results[5, sampling] <- final_results[1, sampling] + qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
      
      # How many of the estimated N of each sampling effort are inside the N +/- 2.5%
      final_results[6, sampling] <- length(subset(estimated_N_vector, estimated_N_vector >= N - (N * 0.05) & estimated_N_vector <= N + (N * 0.05)))
      
      # We store the final results
      assign(paste0("exp", j), final_results[6, ])
      plot_values[, j + 2] <- assign(paste0("exp", j), as.numeric(final_results[6, ]))
      
      # We store the necessary effort to reach 95%
      mean_95[1, j] <- plot_values$samp.eff[plot_values[, j + 2] >= 95][1]
      
      # Objects that are needed to plot the results
      assign("final_results", final_results, envir =  globalenv())
      assign("test", test, envir =  globalenv())
      assign("plot_values", plot_values, envir =  globalenv())
      assign("mean_95", mean_95, envir =  globalenv())
      
      
    }
    
  }
  
  print("We can expect an accuracy equal to or better than 95% from a sampling effort of:")
  round(mean(as.numeric(mean_95[1, ])), digits = 2)
  
}

experiment(N = 1000, eff = 0.5, rep = 10)

# If the number of repetitions is modified, more "+ geom_line(aes(y = V"rep+2"))" have to be added below

plot90 <- ggplot(plot_values, aes(x = samp.eff)) + xlab("Sampling effort (%)") + ylab("Mean inside N ? 5% (%)") + scale_x_continuous(breaks=seq(0.1,1,0.05), labels = seq(10,100,5)) + scale_y_continuous(breaks=seq(0,100,10)) + geom_line(aes(y = V3), col = "red") + geom_line(aes(y = V4), col = "blue") + geom_line(aes(y = V5), col = "green") + geom_line(aes(y = V6), col = "pink") + geom_line(aes(y = V7), col = "yellow") + geom_line(aes(y = V8), col = "purple")+ geom_line(aes(y = V9), col = "orange") + geom_line(aes(y = V10), col = "aquamarine") + geom_line(aes(y = V11), col = "forestgreen") + geom_line(aes(y = V12)) + geom_vline(xintercept = round(mean(as.numeric(mean_95[1, ])), digits = 2), col = "red") + theme_classic() + theme(axis.text.x = element_text(angle=45, hjust=1))# + geom_hline(yintercept = 95, col = "red")


# 80%

experiment <- function(N = 1000, eff = 0.5, rep = 10){
  
  # Sampling efforts (10-100% by 1% intervals)
  sampling_efforts_values <- seq(0.1, 1, eff/100)
  
  # Matrix where all estimated N are going to be stored, for each attempt and each sampling effort
  test <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 100)
  test <- as.data.frame(test)
  colnames(test) <- sampling_efforts_values
  
  # Matrix that links sampling ID with sampling effort
  sampling_efforts <- matrix(NA, ncol = length(sampling_efforts_values), nrow = 2)
  sampling_efforts[1, ] <- c(1:length(sampling_efforts_values))
  sampling_efforts[2, ] <- sampling_efforts_values
  sampling_efforts <- as.data.frame(sampling_efforts)
  
  # Vector that is going to store all estimated N for each sampling effort
  estimated_N_vector <- rep(NA, 100)
  
  # Matrix where the mean, sd, inferior and superior limit are going to be stored
  final_results <- matrix(NA, nrow = 6 , ncol =ncol(sampling_efforts))
  colnames(final_results) <- sampling_efforts[2, ]
  rownames(final_results) <- c("mean", "sd", "median", "inferior limit", "superior limit", "inside 5% interval")
  final_results <- as.data.frame(final_results)
  
  
  # Data frame to plot the final results
  plot_values <- as.data.frame(t(sampling_efforts))
  colnames(plot_values) <- c("ID", "samp.eff")
  
  # Matrix that stores the sampling effort of each repetition
  mean_95 <- matrix(NA, ncol = rep, nrow = 1)
  mean_95 <- as.data.frame(mean_95)
  
  
  for(j in 1:rep) {
    
    for (sampling in sampling_efforts[1, ]){
      # For each sampling effort:
      
      attempts <- 1
      
      while (attempts <= 100) { 
        
        sampling_effort_1 <- N * sampling_efforts[2, sampling]
        sampling_effort_2 <- N * sampling_efforts[2, sampling]
        # We could change the sampling effort within the same sample 
        
        # Matrix that stores the marking - recapturing process
        data <- matrix(NA, ncol = 3, nrow = N)
        data <- as.data.frame(data)
        colnames(data) <- c("ID", "Captured_1", "captured_2")
        data$ID <- c(1:N)
        
        # Animals captured (and marked) on the first sample
        marked <- sample(1:N, sampling_effort_1)
        data$Captured_1 <- as.numeric(data$ID %in% marked)
        
        # Animals recaptured on the second sample
        recaptured <- sample(1:N, sampling_effort_2)
        data$captured_2 <- as.numeric(data$ID %in% recaptured)
        
        # Marked and recaptured animals
        marked_and_recaptured <- nrow(subset(data, Captured_1 == 1 & captured_2 == 1))
        
        # Estimated N for this sample
        estimated_N <- (sampling_effort_1 * sampling_effort_2)/marked_and_recaptured
        
        estimated_N_vector[attempts] <- estimated_N
        
        attempts <- attempts + 1
        
      }
      
      # Estimated N of each attempt and sampling effort are stored here
      test[, sampling] <- estimated_N_vector
      
      final_results[1, sampling] <- mean(test[, sampling])
      final_results[2, sampling] <- sd(test[, sampling])
      final_results[3, sampling] <- median(test[, sampling])
      
      # Inferior and superior limits of the CI (95%)
      final_results[4, sampling] <- final_results[1, sampling] - qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
      final_results[5, sampling] <- final_results[1, sampling] + qt(0.975,df = N - 1) * final_results[2, sampling]/sqrt(N)
      
      # How many of the estimated N of each sampling effort are inside the N +/- 2.5%
      final_results[6, sampling] <- length(subset(estimated_N_vector, estimated_N_vector >= N - (N * 0.1) & estimated_N_vector <= N + (N * 0.1)))
      
      # We store the final results
      assign(paste0("exp", j), final_results[6, ])
      plot_values[, j + 2] <- assign(paste0("exp", j), as.numeric(final_results[6, ]))
      
      # We store the necessary effort to reach 95%
      mean_95[1, j] <- plot_values$samp.eff[plot_values[, j + 2] >= 95][1]
      
      # Objects that are needed to plot the results
      assign("final_results", final_results, envir =  globalenv())
      assign("test", test, envir =  globalenv())
      assign("plot_values", plot_values, envir =  globalenv())
      assign("mean_95", mean_95, envir =  globalenv())
      
      
    }
    
  }
  
  print("We can expect an accuracy equal to or better than 95% from a sampling effort of:")
  round(mean(as.numeric(mean_95[1, ])), digits = 2)
  
}

experiment(N = 1000, eff = 0.5, rep = 10)

# If the number of repetitions is modified, more "+ geom_line(aes(y = V"rep+2"))" have to be added below

plot80 <- ggplot(plot_values, aes(x = samp.eff)) + xlab("Sampling effort (%)") + ylab("Mean inside N ? 10% (%)") + scale_x_continuous(breaks=seq(0.1,1,0.05), labels = seq(10,100,5)) + scale_y_continuous(breaks=seq(0,100,10)) + geom_line(aes(y = V3), col = "red") + geom_line(aes(y = V4), col = "blue") + geom_line(aes(y = V5), col = "green") + geom_line(aes(y = V6), col = "pink") + geom_line(aes(y = V7), col = "yellow") + geom_line(aes(y = V8), col = "purple")+ geom_line(aes(y = V9), col = "orange") + geom_line(aes(y = V10), col = "aquamarine") + geom_line(aes(y = V11), col = "forestgreen") + geom_line(aes(y = V12)) + geom_vline(xintercept = round(mean(as.numeric(mean_95[1, ])), digits = 2), col = "red") + theme_classic() + theme(axis.text.x = element_text(angle=45, hjust=1)) # + geom_hline(yintercept = 95, col = "red")




grid.arrange(plot90, plot80, ncol=2)
