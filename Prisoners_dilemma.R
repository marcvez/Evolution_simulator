# Tit-for-tat game

# 1: Creating a simple version of the tit-for-tat contestant vs. random one

# decision = 0 <- defect
# decision = 1 <- cooperate

# 0-0 <- (1,1)  |   1-0 <- (5,0)  |  0-1 <- (0,5)  |  1-1 <- (3,3)

tft.vs.random <- function(rounds = 100){
  
  tit.for.tat <- matrix(NA, ncol = 2, nrow = rounds)
  tit.for.tat <- as.data.frame(tit.for.tat)
  colnames(tit.for.tat) <- c("decision", "total points")
  tit.for.tat$`total points` <- 0
  
  random.data <- matrix(NA, ncol = 2, nrow = rounds)
  random.data <- as.data.frame(random.data)
  colnames(random.data) <- c("decision", "total points")
  random.data$`total points` <- 0
  
  tit.for.tat[1, 1] <- 1 # it starts being mercyful
  
  for(i in 1:rounds){
    
    random.decision <- sample(0:1, 1)
    
    random.data[i, 1] <- random.decision
    
    if(tit.for.tat[i, 1] == 0 & random.decision == 0){
      
      tit.for.tat[i, 2] <- tit.for.tat[i, 2] + 1
      
      tit.for.tat[i + 1, 2] <- tit.for.tat[i, 2]
      
      random.data[i, 2] <- random.data[i, 2] + 1
      
      random.data[i + 1, 2] <- random.data[i, 2]
      
      tit.for.tat[i + 1, 1] <- 0
      
    } else if(tit.for.tat[i, 1] == 1 & random.decision == 0){
      
      tit.for.tat[i, 2] <- tit.for.tat[i, 2] + 0
      
      tit.for.tat[i + 1, 2] <- tit.for.tat[i, 2]
      
      random.data[i, 2] <- random.data[i, 2] + 5
      
      random.data[i + 1, 2] <- random.data[i, 2]
      
      tit.for.tat[i + 1, 1] <- 0
      
    } else if(tit.for.tat[i, 1] == 0 & random.decision == 1){
      
      tit.for.tat[i, 2] <- tit.for.tat[i, 2] + 5
      
      tit.for.tat[i + 1, 2] <- tit.for.tat[i, 2]
      
      random.data[i, 2] <- random.data[i, 2] + 0
      
      random.data[i + 1, 2] <- random.data[i, 2]
      
      tit.for.tat[i + 1, 1] <- 1
      
    } else if(tit.for.tat[i, 1] == 1 & random.decision == 1){
      
      tit.for.tat[i, 2] <- tit.for.tat[i, 2] + 3
      
      tit.for.tat[i + 1, 2] <- tit.for.tat[i, 2]
      
      random.data[i, 2] <- random.data[i, 2] + 3
      
      random.data[i + 1, 2] <- random.data[i, 2]
      
      tit.for.tat[i + 1, 1] <- 1
      
    }
    
    
  }
  print("Random final score is:")
  print(random.data[rounds, 2])
  print("Tit for tat final score is:")
  print(tit.for.tat[rounds, 2])
  
}

tft.vs.random()

# In this version, random always win or ties with tit.for.tat, bc tit.for.tat follows what random has just done, and once random defects and tit.for.tat doesn't, random has adventage. The only way tit.for.tat can tie with random is to defect when random randomly cooperates at the end of the game. Only if the game ends with random cooperating and tit.for.tat defecting when they are tied, tit.for.tat can win, but will never happen, as tft to tie with random has to defect when random cooperates, and that would make tft cooperate next round.



# 2: Adding new contestants and random enconters

all.vs.all.no.exclusive <- function(rounds = 100, rewards = c(1, 0, 5, 3)){
  
  tit.for.tat <- matrix(NA, ncol = 2, nrow = rounds)
  tit.for.tat <- as.data.frame(tit.for.tat)
  colnames(tit.for.tat) <- c("decision", "total points")
  tit.for.tat$`total points` <- 0
  
  tit.for.tat[1, 1] <- 1 # it starts being mercyful
  
  random.data <- matrix(NA, ncol = 2, nrow = rounds)
  random.data <- as.data.frame(random.data)
  colnames(random.data) <- c("decision", "total points")
  random.data$`total points` <- 0
  
  random.data[, 1] <- sample(0:1, rounds, replace = T)
  
  cooperative <- matrix(NA, ncol = 2, nrow = rounds)
  cooperative <- as.data.frame(cooperative)
  colnames(cooperative) <- c("decision", "total points")
  cooperative$`total points` <- 0
  
  cooperative[, 1] <- 1 # always cooperates
  
  defective <- matrix(NA, ncol = 2, nrow = rounds)
  defective <- as.data.frame(defective)
  colnames(defective) <- c("decision", "total points")
  defective$`total points` <- 0
  
  defective[, 1] <- 0 # always defects
  
  contestants <- list("tit.for.tat" = tit.for.tat, "random.data" = random.data, "cooperative" = cooperative, "defective" = defective)
  
  
  for(j in 1:length(contestants)){
    
    for(i in 1:rounds){
      
      random.contestant <- sample(1:4, 1)
      
      if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 0){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[1]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 0
          
        }
        
      } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 0){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[2]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 0
          
        }
        
      } else if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 1){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[3]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 1
          
        }
        
      } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 1){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[4]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 1
          
        }
        
      }
      
    }
    
  }
  
  cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#DD76DE")
  
  plot(contestants$tit.for.tat[1:rounds, 2], type = "l", lwd = 3, xlab = "Game rounds", ylab = "Points", ylim = c(min(unlist(contestants), na.rm = T), max(unlist(contestants), na.rm = T)), bg = "grey", main = "4 strategies vs. Random opponent")
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "#f7f7f7")
  abline(a = 0, b = ((rewards[1] + rewards[3])/2), lty = "dashed", col = "forestgreen", lwd = 2)
  abline(a = 0, b = (sum(rewards)/4), lty = "dashed", col = "chocolate", lwd = 2)
  abline(a = 0, b = ((rewards[2] + rewards[4])/2) , lty = "dashed", col = "dodgerblue", lwd = 2)
  lines(contestants$tit.for.tat[1:rounds, 2], type = "l", lwd = 3)
  lines(contestants$random.data[1:rounds, 2], type = "l", col = "red", lwd = 3)
  lines(contestants$cooperative[1:rounds, 2], type = "l", col = "blue", lwd = 3)
  lines(contestants$defective[1:rounds, 2], type = "l", col = "green", lwd = 3)
  legend("topleft", legend = c("Tit for tat", "Random", "Always cooperate", "Always defect", "Teoric always defect", "Teoric random", "Teoric always cooperate"), col = c("black", "red", "blue", "green", "forestgreen", "chocolate", "dodgerblue"), lty = c("solid", "solid", "solid", "solid", "dotted", "dotted", "dotted"), lwd = rep(3, 3, 3, 3, 2, 2, 2), title = "Strategy", cex = 0.6)
  
}

all.vs.all.no.exclusive(rounds = 100, rewards = c(1, 0, 5, 3)) # Rewards: (0,0), (0,1), (1,0), (1,1)

alternative.results(rounds = 50, rep = 10)
all.vs.all.no.exclusive(rounds = 50)

# In this version, each strategy can face any of the 4 possible strategies. It may happen that in round n, all 4 strategies face a tft strategy, and in all 4 games, the tft strategy is going to behave the same, as a response to what tft has faced on the previous round. Always defect sames the winner strategy

# 3: Paired games (4 contestants, random 2 vs 2) Necesary?

strategies <- sample(1:4, 4)

# 4: Exclusive all vs. all: Each strategy is going to be paired with each other strategy, so for each single strategy, we'll have 3 comparisons. Then, we'll calculate the mean final score and see which strategy is best in all situations.


mean.score <- function(rounds = 100, rewards = c(1, 0, 5, 3), rep = 10) {
  
  mean.score.1 <- function(){
    
    tit.for.tat <- matrix(NA, ncol = 2, nrow = rounds)
    tit.for.tat <- as.data.frame(tit.for.tat)
    colnames(tit.for.tat) <- c("decision", "total points")
    tit.for.tat$`total points` <- 0
    
    tit.for.tat[1, 1] <- 1 # it starts being mercyful
    
    random.data <- matrix(NA, ncol = 2, nrow = rounds)
    random.data <- as.data.frame(random.data)
    colnames(random.data) <- c("decision", "total points")
    random.data$`total points` <- 0
    
    random.data[, 1] <- sample(0:1, rounds, replace = T)
    
    cooperative <- matrix(NA, ncol = 2, nrow = rounds)
    cooperative <- as.data.frame(cooperative)
    colnames(cooperative) <- c("decision", "total points")
    cooperative$`total points` <- 0
    
    cooperative[, 1] <- 1 # always cooperates
    
    defective <- matrix(NA, ncol = 2, nrow = rounds)
    defective <- as.data.frame(defective)
    colnames(defective) <- c("decision", "total points")
    defective$`total points` <- 0
    
    defective[, 1] <- 0 # always defects
    
    contestants <- list("tit.for.tat" = tit.for.tat, "random.data" = random.data, "cooperative" = cooperative, "defective" = defective)
    
    
    final.results <- matrix(NA, ncol = 20, nrow = rep)
    final.results <- as.data.frame(final.results)
    
    pairs <- matrix(NA, ncol = 2, nrow = 16)
    pairs <- as.data.frame(pairs)
    colnames(pairs) <- c("cont.1", "cont.2")
    pairs$cont.1 <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
    pairs$cont.2 <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
    
    for (k in 1:nrow(pairs)){
      
      for(n in 1:nrow(final.results)){
        
        for(i in 1:rounds){
          
          if(contestants[[pairs[k, 1]]][i, 1] == 0 & contestants[[pairs[k, 2]]][i, 1] == 0){
            
            contestants[[pairs[k, 1]]][i, 2] <- contestants[[pairs[k, 1]]][i, 2] + rewards[1]
            
            contestants[[pairs[k, 1]]][i + 1, 2] <- contestants[[pairs[k, 1]]][i, 2]
            
            if(pairs[k, 1] == 1){
              
              contestants[[pairs[k, 1]]][i + 1, 1] <- contestants[[pairs[k, 2]]][i, 1]
              
            } else if(pairs[k, 2] == 1) {
              
              contestants[[pairs[k, 2]]][i + 1, 1] <- contestants[[pairs[k, 1]]][i, 1]
              
            }
            
          } else if(contestants[[pairs[k, 1]]][i, 1] == 1 & contestants[[pairs[k, 2]]][i, 1] == 0){
            
            contestants[[pairs[k, 1]]][i, 2] <- contestants[[pairs[k, 1]]][i, 2] + rewards[2]
            
            contestants[[pairs[k, 1]]][i + 1, 2] <- contestants[[pairs[k, 1]]][i, 2]
            
            if(pairs[k, 1] == 1){
              
              contestants[[pairs[k, 1]]][i + 1, 1] <- contestants[[pairs[k, 2]]][i, 1]
              
            } else if(pairs[k, 2] == 1) {
              
              contestants[[pairs[k, 2]]][i + 1, 1] <- contestants[[pairs[k, 1]]][i, 1]
              
            }
            
          } else if(contestants[[pairs[k, 1]]][i, 1] == 0 & contestants[[pairs[k, 2]]][i, 1] == 1){
            
            contestants[[pairs[k, 1]]][i, 2] <- contestants[[pairs[k, 1]]][i, 2] + rewards[3]
            
            contestants[[pairs[k, 1]]][i + 1, 2] <- contestants[[pairs[k, 1]]][i, 2]
            
            if(pairs[k, 1] == 1){
              
              contestants[[pairs[k, 1]]][i + 1, 1] <- contestants[[pairs[k, 2]]][i, 1]
              
            } else if(pairs[k, 2] == 1) {
              
              contestants[[pairs[k, 2]]][i + 1, 1] <- contestants[[pairs[k, 1]]][i, 1]
              
            }
            
          } else if(contestants[[pairs[k, 1]]][i, 1] == 1 & contestants[[pairs[k, 2]]][i, 1] == 1){
            
            contestants[[pairs[k, 1]]][i, 2] <- contestants[[pairs[k, 1]]][i, 2] + rewards[4]
            
            contestants[[pairs[k, 1]]][i + 1, 2] <- contestants[[pairs[k, 1]]][i, 2]
            
            if(pairs[k, 1] == 1){
              
              contestants[[pairs[k, 1]]][i + 1, 1] <- contestants[[pairs[k, 2]]][i, 1]
              
            } else if(pairs[k, 2] == 1) {
              
              contestants[[pairs[k, 2]]][i + 1, 1] <- contestants[[pairs[k, 1]]][i, 1]
              
            }
            
          }
          
        }
        
        final.results[n, k] <- contestants[[pairs[k, 1]]][rounds, 2]
        
        
        tit.for.tat <- matrix(NA, ncol = 2, nrow = rounds)
        tit.for.tat <- as.data.frame(tit.for.tat)
        colnames(tit.for.tat) <- c("decision", "total points")
        tit.for.tat$`total points` <- 0
        
        tit.for.tat[1, 1] <- 1 # it starts being mercyful
        
        random.data <- matrix(NA, ncol = 2, nrow = rounds)
        random.data <- as.data.frame(random.data)
        colnames(random.data) <- c("decision", "total points")
        random.data$`total points` <- 0
        
        random.data[, 1] <- sample(0:1, rounds, replace = T)
        
        cooperative <- matrix(NA, ncol = 2, nrow = rounds)
        cooperative <- as.data.frame(cooperative)
        colnames(cooperative) <- c("decision", "total points")
        cooperative$`total points` <- 0
        
        cooperative[, 1] <- 1 # always cooperates
        
        defective <- matrix(NA, ncol = 2, nrow = rounds)
        defective <- as.data.frame(defective)
        colnames(defective) <- c("decision", "total points")
        defective$`total points` <- 0
        
        defective[, 1] <- 0 # always defects
        
        contestants <- list("tit.for.tat" = tit.for.tat, "random.data" = random.data, "cooperative" = cooperative, "defective" = defective)
        
        assign("final.results", final.results, envir = .GlobalEnv)
        
        
      }
      
    }
    
  } # first part
  
  
  mean.score.r <- function(){
    
    all.vs.all.no.exclusive.results <- function(){
      
      tit.for.tat <- matrix(NA, ncol = 2, nrow = rounds)
      tit.for.tat <- as.data.frame(tit.for.tat)
      colnames(tit.for.tat) <- c("decision", "total points")
      tit.for.tat$`total points` <- 0
      
      tit.for.tat[1, 1] <- 1 # it starts being mercyful
      
      random.data <- matrix(NA, ncol = 2, nrow = rounds)
      random.data <- as.data.frame(random.data)
      colnames(random.data) <- c("decision", "total points")
      random.data$`total points` <- 0
      
      random.data[, 1] <- sample(0:1, rounds, replace = T)
      
      cooperative <- matrix(NA, ncol = 2, nrow = rounds)
      cooperative <- as.data.frame(cooperative)
      colnames(cooperative) <- c("decision", "total points")
      cooperative$`total points` <- 0
      
      cooperative[, 1] <- 1 # always cooperates
      
      defective <- matrix(NA, ncol = 2, nrow = rounds)
      defective <- as.data.frame(defective)
      colnames(defective) <- c("decision", "total points")
      defective$`total points` <- 0
      
      defective[, 1] <- 0 # always defects
      
      contestants <- list("tit.for.tat" = tit.for.tat, "random.data" = random.data, "cooperative" = cooperative, "defective" = defective)
      
      
      for(j in 1:length(contestants)){
        
        for(i in 1:rounds){
          
          random.contestant <- sample(1:4, 1)
          
          if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 0){
            
            contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[1]
            
            contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
            
            if(j == 1){
              
              contestants[[j]][i + 1, 1] <- 0
              
            }
            
          } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 0){
            
            contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[2]
            
            contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
            
            if(j == 1){
              
              contestants[[j]][i + 1, 1] <- 0
              
            }
            
          } else if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 1){
            
            contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[3]
            
            contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
            
            if(j == 1){
              
              contestants[[j]][i + 1, 1] <- 1
              
            }
            
          } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 1){
            
            contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[4]
            
            contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
            
            if(j == 1){
              
              contestants[[j]][i + 1, 1] <- 1
              
            }
            
          }
          
        }
        
      }
      
      final.results[n, 17] <- contestants[[1]][rounds, 2]
      final.results[n, 18] <- contestants[[2]][rounds, 2]
      final.results[n, 19] <- contestants[[3]][rounds, 2]
      final.results[n, 20] <- contestants[[4]][rounds, 2]
      assign("final.results", final.results, envir = .GlobalEnv)
      
    }
    
    
    for (n in 1:nrow(final.results)){
      
      all.vs.all.no.exclusive.results()
      
    }
    
  } # second part
  
  
  results.plot <- function(){
    
    cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#DD76DE")
    
    final.results <- final.results[, c("V1", "V2", "V3", "V4", "V17", "V5", "V6", "V7", "V8", "V18", "V9", "V10", "V11", "V12", "V19", "V13", "V14", "V15", "V16", "V20")]
    
    
    Strategy <- c(rep("TFT", 5*rep), rep("Random behaviour", 5*rep), rep("Cooperate", 5*rep), rep("Defect", 5*rep))
    Strategy <- as.data.frame(Strategy)
    
    Opponent <- c(rep("TFT", rep), rep("Random behaviour", rep), rep("Cooperate", rep), rep("Defect", rep), rep("Random opponent", rep))
    Opponent <- rep(Opponent, 4)
    Opponent <- as.data.frame(Opponent)
    
    library("tidyr")
    
    final.results.L <- gather(final.results)
    final.results.L <- final.results.L[, -1]
    final.results.L <- as.data.frame(final.results.L)
    colnames(final.results.L) <- ("Final.score")
    
    boxplot.data <- cbind(Strategy, Opponent, final.results.L)
    
    boxplot.data$Strategy <- factor(boxplot.data$Strategy, levels = c("TFT", "Random behaviour", "Cooperate", "Defect"))
    boxplot.data$Opponent <- factor(boxplot.data$Opponent, levels = c("TFT", "Random behaviour", "Cooperate", "Defect", "Random opponent"))
    
    assign("boxplot.data", boxplot.data, envir = .GlobalEnv)
    
    
    results <- split(boxplot.data, boxplot.data$Opponent)
    
    
    par(mfrow = c(2,3), mai = c(0.2, 0.5, 0.5, 0.1))
    
    plotmeans(Final.score ~ Strategy, data = results[["TFT"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Tit for tat")
    
    
    for(i in 1:4){
      
      text(i, (mean(results[["TFT"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results[["TFT"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
      
    }
    
    plotmeans(Final.score ~ Strategy, data = results[["Random behaviour"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Random behaviour")
    
    for(i in 1:4){
      
      text(i, (mean(results[["Random behaviour"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results[["Random behaviour"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
      
    }
    
    plotmeans(Final.score ~ Strategy, data = results[["Cooperate"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Cooperate")
    
    for(i in 1:4){
      
      text(i, (mean(results[["Cooperate"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results[["Cooperate"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
      
    }
    
    plotmeans(Final.score ~ Strategy, data = results[["Defect"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Defect")
    
    for(i in 1:4){
      
      text(i, (mean(results[["Defect"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results[["Defect"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
      
    }
    
    plotmeans(Final.score ~ Strategy, data = results[["Random opponent"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Random opponent")
    
    for(i in 1:4){
      
      text(i, (mean(results[["Random opponent"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results[["Random opponent"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
      
    }
    
    
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("center", legend =c('Tit for tat', 'Random behaviour', 'Cooperate',
                               'Defect'), pch=19, pt.cex=2, cex=1, bty='n',
           col = cols[1:4])
    mtext("Strategy", at=0.35, cex=1, line = -2)
    
  } # final plot
  
  mean.score.1()
  mean.score.r()
  results.plot()
  
}

library(gplots)

mean.score(rounds = 100, rep = 10, rewards = c(1, 0, 3, 5))


# In this final plot, you can see which is the best strategy to follow against each one of this 5 opponents. For example, the score you are going to obtain if you present a TFT strategy against a "Always cooperate" opponent is 300, but it's worse than the score you could obtain from a random behaviour or "always defect" strategy. It's also interesting to look the paired scores for each combination of opponents. For example, an always defect strategy scores 104 against a TFT opponent, but the TFT strategy against a always defect strategy scores 99 (because of the first benevolent round of the TFT strategy). Here we can see that presenting an always defect strategy is the best option against any of the opponents, except for a TFT strategy, where presenting a always cooperate or TFT strategy is better for both of the contestants (for a repeated interaction). If the interaction is random between contestants, an always defect strategy in a TFT population would score like against a always cooperate opponent.  



alternative.results <- function(rounds = 100, rewards = c(1, 0, 5, 3), rep = 10){
  
  cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#DD76DE")
  
  results2 <- split(boxplot.data, boxplot.data$Strategy)
  
  par(mfrow = c(2,3), mai = c(0.2, 0.5, 0.5, 0.1))
  
  plotmeans(Final.score ~ Opponent, data = results2[["TFT"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Tit for tat")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["TFT"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["TFT"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
    
  }
  
  
  plotmeans(Final.score ~ Opponent, data = results2[["Random behaviour"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Random behaviour")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["Random behaviour"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["Random behaviour"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
    
  }
  
  par(mai =  c(0.2, 0.2, 0.35, 0.2))
  
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  
  legend("topleft", legend =c('Tit for tat', 'Random behaviour', 'Cooperate',
                             'Defect', "Random opponent"), pch=19, pt.cex=1, cex=0.8, bty='n',
         col = cols, title = "Opponent")
  
  legend("bottomleft", legend = c("Tit for tat", "Random", "Always cooperate", "Always defect", "Teoric always defect", "Teoric random", "Teoric always cooperate"), col = c(cols[1:4], "forestgreen", "#AF58D4", "#D26916"), lty = c("solid", "solid", "solid", "solid", "dotted", "dotted", "dotted"), lwd = rep(3, 3, 3, 3, 2, 2, 2), title = "Strategy", cex = 0.8, bty='n')
  
  par(mai =  c(0.2, 0.5, 0.5, 0.1))
  
  plotmeans(Final.score ~ Opponent, data = results2[["Cooperate"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Cooperate")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["Cooperate"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["Cooperate"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
    
  }
  
  
  plotmeans(Final.score ~ Opponent, data = results2[["Defect"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Defect")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["Defect"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["Defect"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
    
  }
  
  all.vs.all.no.exclusive.plot <- function(){
    
    tit.for.tat <- matrix(NA, ncol = 2, nrow = rounds)
    tit.for.tat <- as.data.frame(tit.for.tat)
    colnames(tit.for.tat) <- c("decision", "total points")
    tit.for.tat$`total points` <- 0
    
    tit.for.tat[1, 1] <- 1 # it starts being mercyful
    
    random.data <- matrix(NA, ncol = 2, nrow = rounds)
    random.data <- as.data.frame(random.data)
    colnames(random.data) <- c("decision", "total points")
    random.data$`total points` <- 0
    
    random.data[, 1] <- sample(0:1, rounds, replace = T)
    
    cooperative <- matrix(NA, ncol = 2, nrow = rounds)
    cooperative <- as.data.frame(cooperative)
    colnames(cooperative) <- c("decision", "total points")
    cooperative$`total points` <- 0
    
    cooperative[, 1] <- 1 # always cooperates
    
    defective <- matrix(NA, ncol = 2, nrow = rounds)
    defective <- as.data.frame(defective)
    colnames(defective) <- c("decision", "total points")
    defective$`total points` <- 0
    
    defective[, 1] <- 0 # always defects
    
    contestants <- list("tit.for.tat" = tit.for.tat, "random.data" = random.data, "cooperative" = cooperative, "defective" = defective)
    
    
    for(j in 1:length(contestants)){
      
      for(i in 1:rounds){
        
        random.contestant <- sample(1:4, 1)
        
        if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 0){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[1]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 0
            
          }
          
        } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 0){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[2]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 0
            
          }
          
        } else if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 1){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[3]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 1
            
          }
          
        } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 1){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[4]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 1
            
          }
          
        }
        
      }
      
    }
    
    par(mai =  c(0.6, 0.4, 0.5, 0.2))
    
    cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#DD76DE")
    
    plot(contestants$tit.for.tat[1:rounds, 2], type = "l", lwd = 3, xlab = "", ylab = "", ylim = c(min(unlist(contestants), na.rm = T), max(unlist(contestants), na.rm = T)), bg = "grey", main = "4 strategies vs. \nRandom opponent")
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = "#f7f7f7")
    abline(a = 0, b = ((rewards[1] + rewards[3])/2), lty = "dashed", col = "forestgreen", lwd = 2)
    abline(a = 0, b = (sum(rewards)/4), lty = "dashed", col = "#AF58D4", lwd = 2)
    abline(a = 0, b = ((rewards[2] + rewards[4])/2) , lty = "dashed", col = "#D26916", lwd = 2)
    lines(contestants$tit.for.tat[1:rounds, 2], type = "l", col = cols[1], lwd = 3)
    lines(contestants$random.data[1:rounds, 2], type = "l", col = cols[2], lwd = 3)
    lines(contestants$cooperative[1:rounds, 2], type = "l", col = cols[3], lwd = 3)
    lines(contestants$defective[1:rounds, 2], type = "l", col = cols[4], lwd = 3)
    title(xlab = "Game rounds", line = 2)
    
  }
  
  all.vs.all.no.exclusive.plot()
  
}


alternative.results(rewards = c(1, 0, 3, 5)) # This function needs the same parameters than the mean.score function!


# Here we can see the same results as before, but from the player point view. Each plot shows the score that the player would obtain if it confronts any of these other opponents. For example, a TFT strategy scores 99 against an always defect opponent, an always cooperate strategy scores 0 against an always defect opponent



mean.score(rounds = 100, rep = 10)
alternative.results(rounds = 100, rep = 10)




all.vs.all.no.exclusive.plot(rounds = 100)


all.vs.all.no.exclusive.plot <- function(rounds = 100, rewards = c(1, 0, 5, 3)){
  
  tit.for.tat <- matrix(NA, ncol = 2, nrow = rounds)
  tit.for.tat <- as.data.frame(tit.for.tat)
  colnames(tit.for.tat) <- c("decision", "total points")
  tit.for.tat$`total points` <- 0
  
  tit.for.tat[1, 1] <- 1 # it starts being mercyful
  
  random.data <- matrix(NA, ncol = 2, nrow = rounds)
  random.data <- as.data.frame(random.data)
  colnames(random.data) <- c("decision", "total points")
  random.data$`total points` <- 0
  
  random.data[, 1] <- sample(0:1, rounds, replace = T)
  
  cooperative <- matrix(NA, ncol = 2, nrow = rounds)
  cooperative <- as.data.frame(cooperative)
  colnames(cooperative) <- c("decision", "total points")
  cooperative$`total points` <- 0
  
  cooperative[, 1] <- 1 # always cooperates
  
  defective <- matrix(NA, ncol = 2, nrow = rounds)
  defective <- as.data.frame(defective)
  colnames(defective) <- c("decision", "total points")
  defective$`total points` <- 0
  
  defective[, 1] <- 0 # always defects
  
  contestants <- list("tit.for.tat" = tit.for.tat, "random.data" = random.data, "cooperative" = cooperative, "defective" = defective)
  
  
  for(j in 1:length(contestants)){
    
    for(i in 1:rounds){
      
      random.contestant <- sample(1:4, 1)
      
      if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 0){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[1]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 0
          
        }
        
      } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 0){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[2]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 0
          
        }
        
      } else if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 1){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[3]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 1
          
        }
        
      } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 1){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[4]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 1
          
        }
        
      }
      
    }
    
  }
  
  par(mai =  c(0.6, 0.5, 0.5, 0.2))
  
  cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#DD76DE")
  
  plot(contestants$tit.for.tat[1:rounds, 2], type = "l", lwd = 3, xlab = "", ylab = "", ylim = c(min(unlist(contestants), na.rm = T), max(unlist(contestants), na.rm = T)), bg = "white", main = "4 strategies vs. \nRandom opponent")
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "#f7f7f7")
  abline(a = 0, b = ((rewards[1] + rewards[3])/2), lty = "dashed", col = "forestgreen", lwd = 2)
  abline(a = 0, b = (sum(rewards)/4), lty = "dashed", col = "#AF58D4", lwd = 2)
  abline(a = 0, b = ((rewards[2] + rewards[4])/2) , lty = "dashed", col = "#D26916", lwd = 2)
  lines(contestants$tit.for.tat[1:rounds, 2], type = "l", col = cols[1], lwd = 3)
  lines(contestants$random.data[1:rounds, 2], type = "l", col = cols[2], lwd = 3)
  lines(contestants$cooperative[1:rounds, 2], type = "l", col = cols[3], lwd = 3)
  lines(contestants$defective[1:rounds, 2], type = "l", col = cols[4], lwd = 3)
  title(xlab = "Game rounds", line = 2)
  
}



alternative.results(rounds = 100, rep = 10)
all.vs.all.no.exclusive.plot(rounds = 100)





alternative.results2 <- function(rounds = 100, rewards = c(1, 0, 5, 3), rep = 10){
  
  cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#DD76DE")
  
  results2 <- split(boxplot.data, boxplot.data$Strategy)
  
  par(mfrow = c(2,3), mai = c(0.2, 0.5, 0.5, 0.1))
  
  plotmeans(Final.score ~ Opponent, data = results2[["TFT"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Tit for tat")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["TFT"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["TFT"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 0.8)  
    
  }
  
  
  plotmeans(Final.score ~ Opponent, data = results2[["Random behaviour"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Random behaviour")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["Random behaviour"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["Random behaviour"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 0.8)  
    
  }
  
  par(mai =  c(0.2, 0.2, 0.35, 0.2))
  
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)

  legend("center", legend = c("Tit for tat", "Random", "Always cooperate", "Always defect", "Teoric always defect", "Teoric random", "Teoric always cooperate"), col = c(cols[1:4], "forestgreen", "#AF58D4", "#D26916"), lty = c("solid", "solid", "solid", "solid", "dotted", "dotted", "dotted"), lwd = rep(3, 3, 3, 3, 2, 2, 2), cex = 1, bty='n')
  
  mtext("Strategy", at=0.35, cex=1, line = -2)
  
  
  par(mai =  c(0.2, 0.5, 0.5, 0.1))
  
  plotmeans(Final.score ~ Opponent, data = results2[["Cooperate"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Cooperate")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["Cooperate"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["Cooperate"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 0.8)  
    
  }
  
  
  plotmeans(Final.score ~ Opponent, data = results2[["Defect"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Defect")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["Defect"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["Defect"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 0.8)  
    
  }
  
  all.vs.all.no.exclusive.plot <- function(){
    
    tit.for.tat <- matrix(NA, ncol = 2, nrow = rounds)
    tit.for.tat <- as.data.frame(tit.for.tat)
    colnames(tit.for.tat) <- c("decision", "total points")
    tit.for.tat$`total points` <- 0
    
    tit.for.tat[1, 1] <- 1 # it starts being mercyful
    
    random.data <- matrix(NA, ncol = 2, nrow = rounds)
    random.data <- as.data.frame(random.data)
    colnames(random.data) <- c("decision", "total points")
    random.data$`total points` <- 0
    
    random.data[, 1] <- sample(0:1, rounds, replace = T)
    
    cooperative <- matrix(NA, ncol = 2, nrow = rounds)
    cooperative <- as.data.frame(cooperative)
    colnames(cooperative) <- c("decision", "total points")
    cooperative$`total points` <- 0
    
    cooperative[, 1] <- 1 # always cooperates
    
    defective <- matrix(NA, ncol = 2, nrow = rounds)
    defective <- as.data.frame(defective)
    colnames(defective) <- c("decision", "total points")
    defective$`total points` <- 0
    
    defective[, 1] <- 0 # always defects
    
    contestants <- list("tit.for.tat" = tit.for.tat, "random.data" = random.data, "cooperative" = cooperative, "defective" = defective)
    
    
    for(j in 1:length(contestants)){
      
      for(i in 1:rounds){
        
        random.contestant <- sample(1:4, 1)
        
        if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 0){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[1]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 0
            
          }
          
        } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 0){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[2]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 0
            
          }
          
        } else if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 1){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[3]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 1
            
          }
          
        } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 1){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[4]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 1
            
          }
          
        }
        
      }
      
    }
    
    par(mai =  c(0.6, 0.5, 0.5, 0.2))
    
    cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#DD76DE")
    
    plot(contestants$tit.for.tat[1:rounds, 2], type = "l", lwd = 3, xlab = "", ylab = "", ylim = c(min(unlist(contestants), na.rm = T), max(unlist(contestants), na.rm = T)), bg = "grey", main = "4 strategies vs. \nRandom opponent")
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = "#f7f7f7")
    abline(a = 0, b = ((rewards[1] + rewards[3])/2), lty = "dashed", col = "forestgreen", lwd = 2)
    abline(a = 0, b = (sum(rewards)/4), lty = "dashed", col = "#AF58D4", lwd = 2)
    abline(a = 0, b = ((rewards[2] + rewards[4])/2) , lty = "dashed", col = "#D26916", lwd = 2)
    lines(contestants$tit.for.tat[1:rounds, 2], type = "l", col = cols[1], lwd = 3)
    lines(contestants$random.data[1:rounds, 2], type = "l", col = cols[2], lwd = 3)
    lines(contestants$cooperative[1:rounds, 2], type = "l", col = cols[3], lwd = 3)
    lines(contestants$defective[1:rounds, 2], type = "l", col = cols[4], lwd = 3)
    title(xlab = "Game rounds", line = 2)
    title(ylab = "Points", line = 2.3)
    
    
  }
  
  all.vs.all.no.exclusive.plot()
  
}


alternative.results2()



alternative.results3 <- function(rounds = 100, rewards = c(1, 0, 5, 3), rep = 10){
  
  cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#DD76DE")
  
  results2 <- split(boxplot.data, boxplot.data$Strategy)
  
  par(mfrow = c(2,3), mai = c(0.2, 0.5, 0.5, 0.1))
  
  plotmeans(Final.score ~ Opponent, data = results2[["TFT"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Tit for tat")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["TFT"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["TFT"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
    
  }
  
  
  plotmeans(Final.score ~ Opponent, data = results2[["Random behaviour"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Random behaviour")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["Random behaviour"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["Random behaviour"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
    
  }
  
  par(mai =  c(0.2, 0.2, 0.35, 0.2))
  
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  
  
  legend("center", legend =c('Tit for tat', 'Random behaviour', 'Cooperate',
                              'Defect', "Random opponent"), pch=19, pt.cex=2, cex=1, bty='n',
         col = cols)
  
  
  mtext("Opponent", at=0.35, cex=1, line = -2)
  
  
  par(mai =  c(0.2, 0.5, 0.5, 0.1))
  
  plotmeans(Final.score ~ Opponent, data = results2[["Cooperate"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Cooperate")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["Cooperate"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["Cooperate"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
    
  }
  
  
  plotmeans(Final.score ~ Opponent, data = results2[["Defect"]], connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c((min(rewards)*rounds),(max(rewards)*rounds + rounds)), xlab = "", ylab = "", xaxt="n", main = "Defect")
  
  
  for(i in 1:5){
    
    text(i, (mean(results2[["Defect"]][((i-1)*rep + 1):(i*rep), 3]) + (rounds - rounds/3)), round(mean(results2[["Defect"]][((i-1)*rep + 1):(i*rep), 3]), 1), cex = 1)  
    
  }
  
  all.vs.all.no.exclusive.plot <- function(){
    
    tit.for.tat <- matrix(NA, ncol = 2, nrow = rounds)
    tit.for.tat <- as.data.frame(tit.for.tat)
    colnames(tit.for.tat) <- c("decision", "total points")
    tit.for.tat$`total points` <- 0
    
    tit.for.tat[1, 1] <- 1 # it starts being mercyful
    
    random.data <- matrix(NA, ncol = 2, nrow = rounds)
    random.data <- as.data.frame(random.data)
    colnames(random.data) <- c("decision", "total points")
    random.data$`total points` <- 0
    
    random.data[, 1] <- sample(0:1, rounds, replace = T)
    
    cooperative <- matrix(NA, ncol = 2, nrow = rounds)
    cooperative <- as.data.frame(cooperative)
    colnames(cooperative) <- c("decision", "total points")
    cooperative$`total points` <- 0
    
    cooperative[, 1] <- 1 # always cooperates
    
    defective <- matrix(NA, ncol = 2, nrow = rounds)
    defective <- as.data.frame(defective)
    colnames(defective) <- c("decision", "total points")
    defective$`total points` <- 0
    
    defective[, 1] <- 0 # always defects
    
    contestants <- list("tit.for.tat" = tit.for.tat, "random.data" = random.data, "cooperative" = cooperative, "defective" = defective)
    
    
    for(j in 1:length(contestants)){
      
      for(i in 1:rounds){
        
        random.contestant <- sample(1:4, 1)
        
        if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 0){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[1]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 0
            
          }
          
        } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 0){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[2]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 0
            
          }
          
        } else if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 1){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[3]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 1
            
          }
          
        } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 1){
          
          contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[4]
          
          contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
          
          if(j == 1){
            
            contestants[[j]][i + 1, 1] <- 1
            
          }
          
        }
        
      }
      
    }
    
    par(mai =  c(0.6, 0.4, 0.5, 0.2))
    
    cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#DD76DE")
    
    plot(contestants$tit.for.tat[1:rounds, 2], type = "l", lwd = 3, xlab = "", ylab = "", ylim = c(min(unlist(contestants), na.rm = T), max(unlist(contestants), na.rm = T)), bg = "grey", main = "4 strategies vs. \nrandom opponent")
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = "#f7f7f7")
    abline(a = 0, b = ((rewards[1] + rewards[3])/2), lty = "dashed", col = "forestgreen", lwd = 2)
    abline(a = 0, b = (sum(rewards)/4), lty = "dashed", col = "#AF58D4", lwd = 2)
    abline(a = 0, b = ((rewards[2] + rewards[4])/2) , lty = "dashed", col = "#D26916", lwd = 2)
    lines(contestants$tit.for.tat[1:rounds, 2], type = "l", col = cols[1], lwd = 3)
    lines(contestants$random.data[1:rounds, 2], type = "l", col = cols[2], lwd = 3)
    lines(contestants$cooperative[1:rounds, 2], type = "l", col = cols[3], lwd = 3)
    lines(contestants$defective[1:rounds, 2], type = "l", col = cols[4], lwd = 3)
    title(xlab = "Game rounds", line = 2)
  }
  
  all.vs.all.no.exclusive.plot()
  
}


alternative.results3()


all.vs.all.no.exclusive.plot2 <- function(rounds = 100, rewards = c(1, 0, 5, 3)){
  
  tit.for.tat <- matrix(NA, ncol = 2, nrow = rounds)
  tit.for.tat <- as.data.frame(tit.for.tat)
  colnames(tit.for.tat) <- c("decision", "total points")
  tit.for.tat$`total points` <- 0
  
  tit.for.tat[1, 1] <- 1 # it starts being mercyful
  
  random.data <- matrix(NA, ncol = 2, nrow = rounds)
  random.data <- as.data.frame(random.data)
  colnames(random.data) <- c("decision", "total points")
  random.data$`total points` <- 0
  
  random.data[, 1] <- sample(0:1, rounds, replace = T)
  
  cooperative <- matrix(NA, ncol = 2, nrow = rounds)
  cooperative <- as.data.frame(cooperative)
  colnames(cooperative) <- c("decision", "total points")
  cooperative$`total points` <- 0
  
  cooperative[, 1] <- 1 # always cooperates
  
  defective <- matrix(NA, ncol = 2, nrow = rounds)
  defective <- as.data.frame(defective)
  colnames(defective) <- c("decision", "total points")
  defective$`total points` <- 0
  
  defective[, 1] <- 0 # always defects
  
  contestants <- list("tit.for.tat" = tit.for.tat, "random.data" = random.data, "cooperative" = cooperative, "defective" = defective)
  
  
  for(j in 1:length(contestants)){
    
    for(i in 1:rounds){
      
      random.contestant <- sample(1:4, 1)
      
      if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 0){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[1]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 0
          
        }
        
      } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 0){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[2]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 0
          
        }
        
      } else if(contestants[[j]][i, 1] == 0 & contestants[[random.contestant]][i, 1] == 1){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[3]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 1
          
        }
        
      } else if(contestants[[j]][i, 1] == 1 & contestants[[random.contestant]][i, 1] == 1){
        
        contestants[[j]][i, 2] <- contestants[[j]][i, 2] + rewards[4]
        
        contestants[[j]][i + 1, 2] <- contestants[[j]][i, 2]
        
        if(j == 1){
          
          contestants[[j]][i + 1, 1] <- 1
          
        }
        
      }
      
    }
    
  }
  
  par(mai =  c(0.6, 0.5, 0.5, 0.2))
  
  cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#DD76DE")
  
  plot(contestants$tit.for.tat[1:rounds, 2], type = "l", lwd = 3, xlab = "", ylab = "", ylim = c(min(unlist(contestants), na.rm = T), max(unlist(contestants), na.rm = T)), main = "4 strategies vs. random opponent", las = 1)
  
  abline(a = 0, b = ((rewards[1] + rewards[3])/2), lty = "dashed", col = "forestgreen", lwd = 2)
  abline(a = 0, b = (sum(rewards)/4), lty = "dashed", col = "#AF58D4", lwd = 2)
  abline(a = 0, b = ((rewards[2] + rewards[4])/2) , lty = "dashed", col = "#D26916", lwd = 2)
  lines(contestants$tit.for.tat[1:rounds, 2], type = "l", col = cols[1], lwd = 3)
  lines(contestants$random.data[1:rounds, 2], type = "l", col = cols[2], lwd = 3)
  lines(contestants$cooperative[1:rounds, 2], type = "l", col = cols[3], lwd = 3)
  lines(contestants$defective[1:rounds, 2], type = "l", col = cols[4], lwd = 3)
  title(xlab = "Game rounds", line = 2)
  
  legend("topleft", legend = c("Tit for tat", "Random behaviour", "Always cooperate", "Always defect", "Teoric always defect", "Teoric random", "Teoric always cooperate"), col = c(cols[1:4], "forestgreen", "#AF58D4", "#D26916"), lty = c("solid", "solid", "solid", "solid", "dotted", "dotted", "dotted"), lwd = rep(3, 3, 3, 3, 2, 2, 2), cex = 0.6, bty='n')
  
  
}



par(mfrow = c(1,2), mai = c(0.3, 0.4, 0.2, 0.3))

all.vs.all.no.exclusive.plot2()

all.vs.all.no.exclusive.plot2(rounds = 1000)

# old version below, do not pay attention, it might have interesting functions, but not really


res.tft <- boxplot.data[1:(5*rep), ]
res.rnd <- boxplot.data[(5*rep + 1):(2*(5*rep)), ]
res.coop <- boxplot.data[(2*(5*rep) + 1):(3*(5*rep)), ]
res.def <- boxplot.data[(3*(5*rep) + 1):(4*(5*rep)), ]

install.packages("gplots")
library(gplots)

par(mfrow = c(2,2), mai = c(0.2, 0.5, 0.5, 0.1))

plotmeans(Final.score ~ Opponent, data = res.tft, connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c(0,500), xlab = "", ylab = "", xaxt="n", main = "Tit for tat")

plotmeans(Final.score ~ Opponent, data = res.rnd, connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c(0,500), xlab = "", ylab = "", xaxt="n", yaxt = "n", main = "Random behaviour")

plotmeans(Final.score ~ Opponent, data = res.coop, connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c(0,500), xlab = "", ylab = "", xaxt="n", main = "Cooperate")

plotmeans(Final.score ~ Opponent, data = res.def, connect = FALSE, n.label = F, col = cols, pch = 19, cex = 1, barwidth = 4, barcol = cols, ylim = c(0,500), xlab = "", ylab = "", xaxt="n", yaxt = "n", main = "Defect")

layout.matrix <- matrix(c(1, 3, 2, 4, 0, 0), nrow = 3, ncol = 2)


# library("lattice")
library("ggplot2")

cols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40", "#BF72CB")

ggplot(boxplot.data, aes(x = Strategy, y = Final.score, fill = Opponent)) + geom_boxplot() + scale_fill_manual(values = cols) 



ggplot(boxplot.data, aes(x = Strategy, y = Final.score, fill = Opponent)) + 
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.45), stackratio = 0, dotsize = 0.7) + scale_fill_manual(values = cols) 

mean.data <- aggregate(boxplot.data$Final.score, by = list(boxplot.data$Strategy, boxplot.data$Opponent), FUN = mean)

sd.data <- aggregate(boxplot.data$Final.score, by = list(boxplot.data$Strategy, boxplot.data$Opponent), FUN = sd)

res.tft <- mean.data[c(1,5,9,13,17), ]
res.tft <- cbind(res.tft, sd.data[c(1,5,9,13,17), 3])

res.rnd <- mean.data[c(2,6,10,14,18), ]
res.rnd <- cbind(res.rnd, sd.data[c(2,6,10,14,18), 3])

res.coop <- mean.data[c(3,7,11,15,19), ]
res.coop <- cbind(res.coop, sd.data[c(3,7,11,15,19), 3])

res.def <- mean.data[c(4,8,12,16,20), ]
res.def <- cbind(res.def, sd.data[c(4,8,12,16,20), 3])

plot(res.tft$Group.2, res.tft$x, type = "d")



plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Tit for tat', 'Random behaviour', 'Cooperate',
                            'Defect', "Random opponent"), pch=19, pt.cex=1, cex=0.8, bty='n',
       col = cols)
mtext("Opponent", at=0.37, cex=1, line = -3)
