# Evolution: I'll try to simulate an evolutionary process. In each generation, x individuals are going to walk randomly around the canvas for a finite number of steps. After they finish walking, I'll calculate the distance of each individual from their final position to an specific point (always the same). This specific point is the goal they want to archive, but they do not know where it is. The closest y% individuals (the best suited to archive their goal) are going to pass their "genetic information" (their path) to their descendance. The copy is not going to be 100% identical, but with a mutation rate for each position. After how many generations, the individuals are going to be able to reach their goal?

library(tidyverse)

# Where the plots are going to be stored
setwd("D:/Documentos Marc/MASTER/Tonteries R/Evolution simulator")

evolution <- function(ind = 10, steps = 50, generations = 10, mut_rate = 0.1, reproduction = 0.5){
  
  # Array that stores the path of each individual of each generation
  paths <- array(numeric(),c(steps, 3, ind, generations))
  
  k <- 1
  
  # Initial position
  paths[1, 1:2, , ] <- 0
  
  # Random walk
  for(i in 1:ind){
    
    gen_1 <- sample(1:8, steps, replace = TRUE)
    
    paths[, 3, i, k] <- gen_1
    
  }
  
  # Coordinates of random walk
  for(i in 1:ind){
    
    for(j in 1:(steps - 1)){
      
      if(paths[j, 3, i, k] == 1){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k]
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
        
      } else if(paths[j, 3, i, k] == 2){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
        
      } else if(paths[j, 3, i, k] == 3){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k]
        
      } else if(paths[j, 3, i, k] == 4){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
        
      } else if(paths[j, 3, i, k] == 5){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] 
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
        
      } else if(paths[j, 3, i, k] == 6){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
        
      } else if(paths[j, 3, i, k] == 7){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k]
        
      } else if(paths[j, 3, i, k] == 8){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
        
      }
      
    }
    
  }
  
  # Inital and final points
  plot(x = c(0, steps - 1), y = c(0,0), xlim = c(-(steps/2), steps + 10), ylim = c(-(steps/2),(steps/2)), pch = 21, col = c("black", "red"), bg = c("black", "red"), main = "Evolution simulator", xlab = paste0("Individuals = ", ind, ", Generations = ", generations, ", Nº of steps = ", steps, ",\nMutation rate = ", mut_rate, ", % Reproduction = ", reproduction), ylab = "")
  mtext(paste0("Generation = ", k))
  
  # Tracks and final positions
  for(i in 1:ind){
    
    lines(paths[, 1, i, k], paths[, 2, i, k])
    
    points(paths[steps, 1, i, k], paths[steps, 2, i, k], pch = 21, col = "black", bg = "black", cex = 0.4)
    
  }
  
  # Distance between final location and goal
  final_distance <- as.data.frame(matrix(NA, ncol = 2, nrow = ind))
  
  for(i in 1:ind){
    
    final_distance[i, 1] <- i
    
    final_distance[i, 2] <- sqrt((paths[steps, 1, i, k] - steps)^2 + (paths[steps, 2, i, k])^2)
    
  }
  
  # Order ind by distance
  final_distance <- final_distance %>% arrange(final_distance$V2)
  
  # Directions taken by selected best individuals
  selected_ind <- paths[, 3, final_distance[1:(ind/(reproduction^-1)), 1], k]
  
  # Next generation sample the directions taken by the best ind from past generation in each step, with a mutation probability
  for(i in 1:ind){
    
    for(j in 1:steps){
      
      paths[j, 3, i, k + 1] <- sample(selected_ind[j, ], 1)
      
      mutation <- sample(1:1000, 1)
      
      if(mutation <= 1000*mut_rate){
        
        paths[j, 3, i, k + 1] <- sample(1:8, 1)
        
      }
      
    }
    
  }
  
  dev.copy(png, paste0("evolution", k, ".png"), width = 700, height = 460)
  dev.off()
  
  # We repeat this procedure in each generation (walk -> best -> copy with mutation) 
  for(k in 2:generations){
    
    # Coordinates of random walk
    for(i in 1:ind){
      
      for(j in 1:(steps - 1)){
        
        if(paths[j, 3, i, k] == 1){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k]
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
          
        } else if(paths[j, 3, i, k] == 2){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
          
        } else if(paths[j, 3, i, k] == 3){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k]
          
        } else if(paths[j, 3, i, k] == 4){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
          
        } else if(paths[j, 3, i, k] == 5){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] 
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
          
        } else if(paths[j, 3, i, k] == 6){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
          
        } else if(paths[j, 3, i, k] == 7){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k]
          
        } else if(paths[j, 3, i, k] == 8){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
          
        }
        
      }
      
    }
    
    # Inital and final points
    plot(x = c(0, steps - 1), y = c(0,0), xlim = c(-(steps/2), steps + 10), ylim = c(-(steps/2),(steps/2)), pch = 21, col = c("black", "red"), bg = c("black", "red"), main = "Evolution simulator", xlab = paste0("Individuals = ", ind, ", Generations = ", generations, ", Nº of steps = ", steps, ",\nMutation rate = ", mut_rate, ", % Reproduction = ", reproduction), ylab = "")
    mtext(paste0("Generation = ", k))
    
    # Tracks and final positions
    for(i in 1:ind){
      
      lines(paths[, 1, i, k], paths[, 2, i, k])
      
      points(paths[steps, 1, i, k], paths[steps, 2, i, k], pch = 21, col = "black", bg = "black", cex = 0.4)
      
    }
    
    # Distance between final location and goal
    final_distance <- as.data.frame(matrix(NA, ncol = 2, nrow = ind))
    
    for(i in 1:ind){
      
      final_distance[i, 1] <- i
      
      final_distance[i, 2] <- sqrt((paths[steps, 1, i, k] - steps)^2 + (paths[steps, 2, i, k])^2)
      
    }
    
    
    final_distance <- final_distance %>% arrange(final_distance$V2)
    
    selected_ind <- paths[, 3, final_distance[1:(ind/(reproduction^-1)), 1], k]
    
    
    for(i in 1:ind){
      
      for(j in 1:steps){
        
        paths[j, 3, i, min(k + 1, generations)] <- sample(selected_ind[j, ], 1)
        
        mutation <- sample(1:1000, 1)
        
        if(mutation <= 1000*mut_rate){
          
          paths[j, 3, i, min(k + 1, generations)] <- sample(1:8, 1)
          
        }
        
      }
      
    }
    
    dev.copy(png, paste0("evolution", k, ".png"), width = 700, height = 460)
    dev.off()
    
  }
  
}

# ind = number of individuals per generation
# generations = number of generations
# steps = number of steps to reach the goal
# mut_rate = mutation rate (from 0 to 1)
# reproduction = percentage (from 0 to 1) of individuals that reproduce in each generation and pass their "genes" to the next generation

evolution(ind = 100, generations = 100, steps = 50, mut_rate = 0, reproduction = 0.9)






evolution2 <- function(ind = 10, steps = 50, generations = 10, mut_rate = 0.1, reproduction = 0.5){
  
  # Array that stores the path of each individual of each generation
  paths <- array(numeric(),c(steps, 3, ind, generations))
  
  k <- 1
  
  # Initial position
  paths[1, 1:2, , ] <- 0
  
  # Random walk
  for(i in 1:ind){
    
    gen_1 <- sample(1:8, steps, replace = TRUE)
    
    paths[, 3, i, k] <- gen_1
    
  }
  
  # Coordinates of random walk
  for(i in 1:ind){
    
    for(j in 1:(steps - 1)){
      
      if(paths[j, 3, i, k] == 1){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k]
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
        
      } else if(paths[j, 3, i, k] == 2){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
        
      } else if(paths[j, 3, i, k] == 3){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k]
        
      } else if(paths[j, 3, i, k] == 4){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
        
      } else if(paths[j, 3, i, k] == 5){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] 
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
        
      } else if(paths[j, 3, i, k] == 6){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
        
      } else if(paths[j, 3, i, k] == 7){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k]
        
      } else if(paths[j, 3, i, k] == 8){
        
        paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
        
        paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
        
      }
      
    }
    
  }
  
  # Inital and final points
  plot(x = c(0, steps - 1), y = c(0,0), xlim = c(-steps - 10, steps + 10), ylim = c(-(steps/2),(steps/2)), pch = 21, col = c("black", "red"), bg = c("black", "red"), main = "Evolution simulator", xlab = paste0("Individuals = ", ind, ", Generations = ", generations, ", Nº of steps = ", steps, ",\nMutation rate = ", mut_rate, ", % Reproduction = ", reproduction), ylab = "")
  mtext(paste0("Generation = ", k))
  
  # Tracks and final positions
  for(i in 1:ind){
    
    lines(paths[, 1, i, k], paths[, 2, i, k])
    
    points(paths[steps, 1, i, k], paths[steps, 2, i, k], pch = 21, col = "black", bg = "black", cex = 0.4)
    
  }
  
  # Distance between final location and goal
  final_distance <- as.data.frame(matrix(NA, ncol = 2, nrow = ind))
  
  for(i in 1:ind){
    
    final_distance[i, 1] <- i
    
    final_distance[i, 2] <- sqrt((paths[steps, 1, i, k] - steps)^2 + (paths[steps, 2, i, k])^2)
    
  }
  
  # Order ind by distance
  final_distance <- final_distance %>% arrange(final_distance$V2)
  
  # Directions taken by selected best individuals
  selected_ind <- paths[, 3, final_distance[1:(ind/(reproduction^-1)), 1], k]
  
  # Next generation sample the directions taken by the best ind from past generation in each step, with a mutation probability
  for(i in 1:ind){
    
    for(j in 1:steps){
      
      paths[j, 3, i, k + 1] <- sample(selected_ind[j, ], 1)
      
      mutation <- sample(1:1000, 1)
      
      if(mutation <= 1000*mut_rate){
        
        paths[j, 3, i, k + 1] <- sample(1:8, 1)
        
      }
      
    }
    
  }
  
  dev.copy(png, paste0("evolution", k, ".png"), width = 700, height = 460)
  dev.off()
  
  # We repeat this procedure in each generation (walk -> best -> copy with mutation) 
  for(k in 2:round(generations/2)){
    
    # Coordinates of random walk
    for(i in 1:ind){
      
      for(j in 1:(steps - 1)){
        
        if(paths[j, 3, i, k] == 1){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k]
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
          
        } else if(paths[j, 3, i, k] == 2){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
          
        } else if(paths[j, 3, i, k] == 3){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k]
          
        } else if(paths[j, 3, i, k] == 4){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
          
        } else if(paths[j, 3, i, k] == 5){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] 
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
          
        } else if(paths[j, 3, i, k] == 6){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
          
        } else if(paths[j, 3, i, k] == 7){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k]
          
        } else if(paths[j, 3, i, k] == 8){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
          
        }
        
      }
      
    }
    
    # Inital and final points
    plot(x = c(0, steps - 1), y = c(0,0), xlim = c(-steps - 10, steps + 10), ylim = c(-(steps/2),(steps/2)), pch = 21, col = c("black", "red"), bg = c("black", "red"), main = "Evolution simulator", xlab = paste0("Individuals = ", ind, ", Generations = ", generations, ", Nº of steps = ", steps, ",\nMutation rate = ", mut_rate, ", % Reproduction = ", reproduction), ylab = "")
    mtext(paste0("Generation = ", k))
    
    # Tracks and final positions
    for(i in 1:ind){
      
      lines(paths[, 1, i, k], paths[, 2, i, k])
      
      points(paths[steps, 1, i, k], paths[steps, 2, i, k], pch = 21, col = "black", bg = "black", cex = 0.4)
      
    }
    
    # Distance between final location and goal
    final_distance <- as.data.frame(matrix(NA, ncol = 2, nrow = ind))
    
    for(i in 1:ind){
      
      final_distance[i, 1] <- i
      
      final_distance[i, 2] <- sqrt((paths[steps, 1, i, k] - steps)^2 + (paths[steps, 2, i, k])^2)
      
    }
    
    
    final_distance <- final_distance %>% arrange(final_distance$V2)
    
    selected_ind <- paths[, 3, final_distance[1:(ind/(reproduction^-1)), 1], k]
    
    
    for(i in 1:ind){
      
      for(j in 1:steps){
        
        paths[j, 3, i, min(k + 1, generations)] <- sample(selected_ind[j, ], 1)
        
        mutation <- sample(1:1000, 1)
        
        if(mutation <= 1000*mut_rate){
          
          paths[j, 3, i, min(k + 1, generations)] <- sample(1:8, 1)
          
        }
        
      }
      
    }
    
    dev.copy(png, paste0("evolution", k, ".png"), width = 700, height = 460)
    dev.off()
    
  }
  
  for(k in (round(generations/2) + 1):generations){
    
    # Coordinates of random walk
    for(i in 1:ind){
      
      for(j in 1:(steps - 1)){
        
        if(paths[j, 3, i, k] == 1){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k]
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
          
        } else if(paths[j, 3, i, k] == 2){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
          
        } else if(paths[j, 3, i, k] == 3){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k]
          
        } else if(paths[j, 3, i, k] == 4){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] + 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
          
        } else if(paths[j, 3, i, k] == 5){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] 
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
          
        } else if(paths[j, 3, i, k] == 6){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] - 1
          
        } else if(paths[j, 3, i, k] == 7){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k]
          
        } else if(paths[j, 3, i, k] == 8){
          
          paths[j + 1, 1, i, k] <- paths[j, 1, i, k] - 1
          
          paths[j + 1, 2, i, k] <- paths[j, 2, i, k] + 1
          
        }
        
      }
      
    }
    
    # Inital and final points
    plot(x = c(0, - steps + 1), y = c(0,0), xlim = c(-steps - 10, steps + 10), ylim = c(-(steps/2),(steps/2)), pch = 21, col = c("black", "red"), bg = c("black", "red"), main = "Evolution simulator", xlab = paste0("Individuals = ", ind, ", Generations = ", generations, ", Nº of steps = ", steps, ",\nMutation rate = ", mut_rate, ", % Reproduction = ", reproduction), ylab = "")
    mtext(paste0("Generation = ", k))
    
    # Tracks and final positions
    for(i in 1:ind){
      
      lines(paths[, 1, i, k], paths[, 2, i, k])
      
      points(paths[steps, 1, i, k], paths[steps, 2, i, k], pch = 21, col = "black", bg = "black", cex = 0.4)
      
    }
    
    # Distance between final location and goal
    final_distance <- as.data.frame(matrix(NA, ncol = 2, nrow = ind))
    
    for(i in 1:ind){
      
      final_distance[i, 1] <- i
      
      final_distance[i, 2] <- sqrt((paths[steps, 1, i, k] + steps)^2 + (paths[steps, 2, i, k])^2)
      
    }
    
    
    final_distance <- final_distance %>% arrange(final_distance$V2)
    
    selected_ind <- paths[, 3, final_distance[1:(ind/(reproduction^-1)), 1], k]
    
    
    for(i in 1:ind){
      
      for(j in 1:steps){
        
        paths[j, 3, i, min(k + 1, generations)] <- sample(selected_ind[j, ], 1)
        
        mutation <- sample(1:1000, 1)
        
        if(mutation <= 1000*mut_rate){
          
          paths[j, 3, i, min(k + 1, generations)] <- sample(1:8, 1)
          
        }
        
      }
      
    }
    
    dev.copy(png, paste0("evolution", k, ".png"), width = 700, height = 460)
    dev.off()
    
  }
  
}

# ind = number of individuals per generation
# generations = number of generations
# steps = number of steps to reach the goal
# mut_rate = mutation rate (from 0 to 1)
# reproduction = percentage (from 0 to 1) of individuals that reproduce in each generation and pass their "genes" to the next generation

evolution2(ind = 100, generations = 100, steps = 50, mut_rate = 0.001, reproduction = 0.5)
