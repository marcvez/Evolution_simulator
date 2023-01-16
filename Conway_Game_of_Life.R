# Conway's game of life

# 1: Create the environment and the shape of the cells

n.row <- 20

n.col <- 20

initial.state <- sample(0:1, (n.row*n.col), replace = T)

matrix <- matrix(initial.state, nrow = n.row, ncol = n.col)

par(mar = c(0, 0, 0, 0))

plot(NA, xlim = c(1, n.row), ylim = c(1, n.col), xlab = NA, ylab = NA, xaxt = "n", yaxt = "n")

for(i in 1:n.row){
  
  for (j in 1:n.col){
    
    if(matrix[i, j] == 0){
      
    } else if (matrix[i, j] == 1){
      
      points(i, j, pch = 22, cex = 3.5, bg = "black")
      
    }
    
  }
}


# 2: create 1 object that stores different images over time

steps <- 11

matrix.time <- array(data = NA, dim = c(n.row, n.col, steps))

initial.state <- sample(0:1, (n.row*n.col), replace = T)

matrix <- matrix(initial.state, nrow = n.row, ncol = n.col)

matrix.time[, , 1] <- matrix

# 3: create system of evolution (rules)

# rule 1: any cell with less than 2 neighbours die
# rule 2: any living cell with 2 or 3 neighbours stays alive
# rule 3: any cell with more than 3 neighbours die
# rule 4: any dead cell with exactly 3 living neghbours becomes alive

matrix.values <- array(data = NA, dim = c(n.row, n.col, steps))

for(t in 1:(steps - 1)){
  
  for (i in 1:n.row){
    
    for(j in 1:n.col){
      
      matrix.values[i, j, t] <- matrix.time[min(i + 1, n.row), j, t] + matrix.time[min(i + 1, n.row), max(j - 1, 1), t] + matrix.time[i, max(j - 1, 1), t] + matrix.time[max(i - 1, 1), max(j - 1, 1), t] + matrix.time[max(i - 1, 1), j, t] + matrix.time[max(i - 1, 1), min(j + 1, n.col), t] + matrix.time[i, min(j + 1, n.col), t] + matrix.time[min(i + 1, n.row), min(j + 1, n.col), t]
      
      if(matrix.values[i, j, t] < 2){
        
        matrix.time[i, j, t + 1] <- 0
        
      } else if(matrix.values[i, j, t] == 2 & matrix.time[i, j, t] == 0){
        
        matrix.time[i, j, t + 1] <- 0
        
      } else if(matrix.values[i, j, t] == 2 & matrix.time[i, j, t] == 1){
        
        matrix.time[i, j, t + 1] <- 1
        
      } else if(matrix.values[i, j, t] == 3 & matrix.time[i, j, t] == 0){
        
        matrix.time[i, j, t + 1] <- 1
        
      } else if(matrix.values[i, j, t] == 3 & matrix.time[i, j, t] == 1){
        
        matrix.time[i, j, t + 1] <- 1
        
      } else if(matrix.values[i, j, t] > 3){
        
        matrix.time[i, j, t + 1] <- 0
        
      }
      
    }
    
  }
  
}


# 4: plot

par(mar = c(1, 1, 1, 1))

par(mfrow = c(2,2))

for(t in 1:steps){
  
  plot(NA, xlim = c(1, n.row), ylim = c(1, n.col), xlab = NA, ylab = NA, xaxt = "n", yaxt = "n")
  
  for(i in 1:n.row){
    
    for (j in 1:n.col){
      
      if(matrix.time[i, j, t] == 0){
        
      } else if (matrix.time[i, j, t] == 1){
        
        points(i, j, pch = 22, cex = 1.5, bg = "black")
        
      }
      
    }
  }
  
}

par(mar = c(0, 0, 0, 0))

par(mfrow = c(1,1))

for(t in 1:steps){
  
  plot(NA, xlim = c(1, n.row), ylim = c(1, n.col), xlab = NA, ylab = NA, xaxt = "n", yaxt = "n")
  
  for(i in 1:n.row){
    
    for (j in 1:n.col){
      
      if(matrix.time[i, j, t] == 0){
        
      } else if (matrix.time[i, j, t] == 1){
        
        points(i, j, pch = 22, cex = 3.5, bg = "black")
        
      }
      
    }
  }
  
}

# 5: function
# Function in which the grid size (n.row x n.col) as well as the number of generations (steps) can be modified. You can also change the size of the tiles, which must be adjusted according to the grid size. 
Game.of.life <- function(n.row = 100, n.col = 100, steps = 50, size = 0.5){
  
  # Working directory where the plot for each generation are going to be saved.
  setwd("C:/Users/marc9/OneDrive/Escritorio/BIO UB/MASTER/Tonteries R/Conway steps")
  
  # Random initial state. Each cell has the same probability to be alive or to be dead.
  initial.state <- sample(0:1, (n.row*n.col), replace = T)
  
  # Grid where the initial state is going to be stored.
  matrix <- matrix(initial.state, nrow = n.row, ncol = n.col)
  
  # Array that is going to store all the generations that are going to be generated during the simulation.
  matrix.time <- array(data = NA, dim = c(n.row, n.col, steps))
  
  # Initial state.
  matrix.time[, , 1] <- matrix
  
  # Array that is going to store the number of living cells surrounding each cell at each generation. The values contained in this array are going to be used to determine if a cell is going to be dead or alive on the next generation.
  matrix.values <- array(data = NA, dim = c(n.row, n.col, steps))
  
  # In each generation, check the following for each cell [i, j]:
  for(t in 1:(steps - 1)){
    
    for (i in 1:n.row){
      
      for(j in 1:n.col){
        
        # Count the number of living cells surrounding each [i, j] cell, and store it on the corresponding array.
        matrix.values[i, j, t] <- matrix.time[min(i + 1, n.row), j, t] + matrix.time[min(i + 1, n.row), max(j - 1, 1), t] + matrix.time[i, max(j - 1, 1), t] + matrix.time[max(i - 1, 1), max(j - 1, 1), t] + matrix.time[max(i - 1, 1), j, t] + matrix.time[max(i - 1, 1), min(j + 1, n.col), t] + matrix.time[i, min(j + 1, n.col), t] + matrix.time[min(i + 1, n.row), min(j + 1, n.col), t]
        
        # Rule 1: Any cell with less than 2 neighbors die.
        if(matrix.values[i, j, t] < 2){
          
          matrix.time[i, j, t + 1] <- 0
          
          # Rule 2: Any living cell with 2 or 3 neighbours stays alive. If the cell is dead and it has only 2 living neighbors, it remains dead.
        } else if(matrix.values[i, j, t] == 2 & matrix.time[i, j, t] == 0){
          
          matrix.time[i, j, t + 1] <- 0
          
        } else if(matrix.values[i, j, t] == 2 & matrix.time[i, j, t] == 1){
          
          matrix.time[i, j, t + 1] <- 1
          
          # Rule 3: Any dead cell with exactly 3 living neghbors becomes alive.
        } else if(matrix.values[i, j, t] == 3 & matrix.time[i, j, t] == 0){
          
          matrix.time[i, j, t + 1] <- 1
          
        } else if(matrix.values[i, j, t] == 3 & matrix.time[i, j, t] == 1){
          
          matrix.time[i, j, t + 1] <- 1
          
          # RUle 4: Any cell with more than 3 neighbours die.
        } else if(matrix.values[i, j, t] > 3){
          
          matrix.time[i, j, t + 1] <- 0
          
        }
        
      }
      
    }
    
  }
  
  # Plotting parameters.
  par(mar = c(0, 0, 0, 0))
  
  par(mfrow = c(1,1))
  
  # For each generation:
  for(t in 1:steps){
    
    # Plot an empty canvas.
    plot(NA, xlim = c(1.5, n.row - 0.5), ylim = c(1.5, n.col - 0.5), xlab = NA, ylab = NA, xaxt = "n", yaxt = "n", bty = "n")
    
    # For each cell [i, j] do the following:
    for(i in 1:n.row){
      
      for (j in 1:n.col){
        
        # If the cell has a 0, do not do anything (dead).
        if(matrix.time[i, j, t] == 0){
          
          # If the cell has a 1, paint the cell with a black square (alive).
        } else if (matrix.time[i, j, t] == 1){
          
          points(i, j, pch = 22, cex = size, bg = "black")
          
        }
        
      }
      
    }
    
    # Save the resulting .png to the working directory with the corresponding generation
    dev.copy(png, paste0("conway", t, ".png"))
    dev.off()
    
  }
  
}

# Run the function!
Game.of.life(n.row = 30, n.col = 30, steps = 30, size = 2)



# 6: forms!

matrix <- matrix(0, nrow = n.row, ncol = n.col)

glider <- matrix(data = NA, nrow = 1000, ncol = 2)

glider[, 1] <- c(1,2,3,4,5,6)
glider[, 2] <- c(1,2,3,4,5,6)

for(i in 1:length(glider[, 1])){
  
  matrix[glider[i, 1], glider[i, 2]] <- 1
  
}


matrix <- matrix(0, nrow = n.row, ncol = n.col)

Gosper.glider.i <- c(5,6,5,6,15,15,15,16,16,17,17,18,18,19,20,20,21,21,21,22,25,25,25,26,26,26,27,27,29,29,29,29,39,40,39,40)
Gosper.glider.j <- c(9,9,10,10,9,10,11,8,12,7,13,7,13,10,8,12,9,10,11,10,9,8,7,9,8,7,10,6,6,10,11,5,8,8,7,7)

Gosper.glider <- matrix(c(Gosper.glider.j, Gosper.glider.i), ncol = 2)

for(i in 1:length(Gosper.glider[, 1])){
  
  matrix[Gosper.glider[i, 1], Gosper.glider[i, 2]] <- 1
  
}


# Function in which the grid size (n.row x n.col) as well as the number of generations (steps) can be modified. You can also change the size of the tiles, which must be adjusted according to the grid size. 
Game.of.life <- function(n.row = 100, n.col = 100, steps = 50, size = 0.5){
  
  # Working directory where the plot for each generation are going to be saved.
  setwd("C:/Users/marc9/OneDrive/Escritorio/BIO UB/MASTER/Tonteries R/Conway steps")
  
  matrix <- matrix(0, nrow = n.row, ncol = n.col)
  
  Gosper.glider.i <- c(5,6,5,6,15,15,15,16,16,17,17,18,18,19,20,20,21,21,21,22,25,25,25,26,26,26,27,27,29,29,29,29,39,40,39,40)
  Gosper.glider.j <- c(9,9,10,10,9,10,11,8,12,7,13,7,13,10,8,12,9,10,11,10,9,8,7,9,8,7,10,6,6,10,11,5,8,8,7,7)
  
  Gosper.glider <- matrix(c(Gosper.glider.i, Gosper.glider.j), ncol = 2)
  
  for(i in 1:length(Gosper.glider[, 1])){
    
    matrix[Gosper.glider[i, 1], Gosper.glider[i, 2]] <- 1
    
  }
  
  Gosper.glider.1 <- Gosper.glider
  
  Gosper.glider.1[, 1] <- n.col - Gosper.glider.1[, 1]
  
  for(i in 1:length(Gosper.glider.1[, 1])){
    
    matrix[Gosper.glider.1[i, 1], Gosper.glider.1[i, 2]] <- 1
    
  }
  
  Gosper.glider.2 <- Gosper.glider
  
  Gosper.glider.2[, 2] <- n.row - Gosper.glider.2[, 2]
  
  for(i in 1:length(Gosper.glider.2[, 1])){
    
    matrix[Gosper.glider.2[i, 1], Gosper.glider.2[i, 2]] <- 1
    
  }
  
  
  Gosper.glider.3 <- Gosper.glider.2
  
  Gosper.glider.3[, 1] <- n.col - Gosper.glider.3[, 1]
  
  for(i in 1:length(Gosper.glider.3[, 1])){
    
    matrix[Gosper.glider.3[i, 1], Gosper.glider.3[i, 2]] <- 1
    
  }
  
  flower.i <- c(59,59,59,60,60,61,61,62,62,62,67,67,67,68,68,69,69,70,70,70)
  flower.j <- c(64,65,66,63,67,63,67,64,65,66,64,65,66,63,67,63,67,64,65,66)
  
  flower <- matrix(c(flower.i, flower.j), ncol = 2)
  
  for(i in 1:length(flower[, 1])){
    
    matrix[flower[i, 1], flower[i, 2]] <- 1
    
  }
  
  
  # Array that is going to store all the generations that are going to be generated during the simulation.
  matrix.time <- array(data = NA, dim = c(n.row, n.col, steps))
  
  # Initial state.
  matrix.time[, , 1] <- matrix
  
  # Array that is going to store the number of living cells surrounding each cell at each generation. The values contained in this array are going to be used to determine if a cell is going to be dead or alive on the next generation.
  matrix.values <- array(data = NA, dim = c(n.row, n.col, steps))
  
  # In each generation, check the following for each cell [i, j]:
  for(t in 1:(steps - 1)){
    
    for (i in 1:n.row){
      
      for(j in 1:n.col){
        
        # Count the number of living cells surrounding each [i, j] cell, and store it on the corresponding array.
        matrix.values[i, j, t] <- matrix.time[min(i + 1, n.row), j, t] + matrix.time[min(i + 1, n.row), max(j - 1, 1), t] + matrix.time[i, max(j - 1, 1), t] + matrix.time[max(i - 1, 1), max(j - 1, 1), t] + matrix.time[max(i - 1, 1), j, t] + matrix.time[max(i - 1, 1), min(j + 1, n.col), t] + matrix.time[i, min(j + 1, n.col), t] + matrix.time[min(i + 1, n.row), min(j + 1, n.col), t]
        
        # Rule 1: Any cell with less than 2 neighbors die.
        if(matrix.values[i, j, t] < 2){
          
          matrix.time[i, j, t + 1] <- 0
          
          # Rule 2: Any living cell with 2 or 3 neighbours stays alive. If the cell is dead and it has only 2 living neighbors, it remains dead.
        } else if(matrix.values[i, j, t] == 2 & matrix.time[i, j, t] == 0){
          
          matrix.time[i, j, t + 1] <- 0
          
        } else if(matrix.values[i, j, t] == 2 & matrix.time[i, j, t] == 1){
          
          matrix.time[i, j, t + 1] <- 1
          
          # Rule 3: Any dead cell with exactly 3 living neghbors becomes alive.
        } else if(matrix.values[i, j, t] == 3 & matrix.time[i, j, t] == 0){
          
          matrix.time[i, j, t + 1] <- 1
          
        } else if(matrix.values[i, j, t] == 3 & matrix.time[i, j, t] == 1){
          
          matrix.time[i, j, t + 1] <- 1
          
          # RUle 4: Any cell with more than 3 neighbours die.
        } else if(matrix.values[i, j, t] > 3){
          
          matrix.time[i, j, t + 1] <- 0
          
        }
        
      }
      
    }
    
  }
  
  # Plotting parameters.
  par(mar = c(0, 0, 0, 0))
  
  par(mfrow = c(1,1))
  
  # For each generation:
  for(t in 1:steps){
    
    # Plot an empty canvas.
    plot(NA, xlim = c(1.5, n.row - 0.5), ylim = c(1.5, n.col - 0.5), xlab = NA, ylab = NA, xaxt = "n", yaxt = "n", bty = "n")
    
    # For each cell [i, j] do the following:
    for(i in 1:n.row){
      
      for (j in 1:n.col){
        
        # If the cell has a 0, do not do anything (dead).
        if(matrix.time[i, j, t] == 0){
          
          # If the cell has a 1, paint the cell with a black square (alive).
        } else if (matrix.time[i, j, t] == 1){
          
          points(i, j, pch = 22, cex = size, bg = "black")
          
        }
        
      }
      
    }
    
    # Save the resulting .png to the working directory with the corresponding generation
    dev.copy(png, paste0("conway", t, ".png"))
    dev.off()
    
  }
