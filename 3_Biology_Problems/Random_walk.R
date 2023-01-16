# Random walk experiment

# 1: base

steps = 1000

coord <- matrix(data = NA, ncol = 3, nrow = steps)
colnames(coord) <- c("x.coord", "y.coord", "direction")
coord <- as.data.frame(coord)

coord[1, ] <- c(0, 0, 0)

for(i in 1:steps) {
  
  direction <- sample(1:9, 1)
  
  if(direction == 1){
    
    coord[i + 1, 1] <- coord[i, 1]
    
    coord[i + 1, 2] <- coord[i, 2] + 1
    
    coord[i + 1, 3] <- "N"
     
  } else if (direction == 2) {
    
    coord[i + 1, 1] <- coord[i, 1] + 1
    
    coord[i + 1, 2] <- coord[i, 2] + 1
    
    coord[i + 1, 3] <- "NE"
    
  } else if (direction == 3) {
    
    coord[i + 1, 1] <- coord[i, 1] + 1
    
    coord[i + 1, 2] <- coord[i, 2]
    
    coord[i + 1, 3] <- "E"
    
  } else if (direction == 4) {
    
    coord[i + 1, 1] <- coord[i, 1] + 1
    
    coord[i + 1, 2] <- coord[i, 2] - 1
    
    coord[i + 1, 3] <- "SE"
    
  } else if (direction == 5) {
    
    coord[i + 1, 1] <- coord[i, 1] 
    
    coord[i + 1, 2] <- coord[i, 2] - 1
    
    coord[i + 1, 3] <- "S"
    
  } else if (direction == 6) {
    
    coord[i + 1, 1] <- coord[i, 1] - 1
    
    coord[i + 1, 2] <- coord[i, 2] - 1
    
    coord[i + 1, 3] <- "SW"
    
  } else if (direction == 7) {
    
    coord[i + 1, 1] <- coord[i, 1] - 1
    
    coord[i + 1, 2] <- coord[i, 2]
    
    coord[i + 1, 3] <- "W"
    
  } else if (direction == 8) {
    
    coord[i + 1, 1] <- coord[i, 1] - 1
    
    coord[i + 1, 2] <- coord[i, 2] + 1
    
    coord[i + 1, 3] <- "NW"
    
  } else if (direction == 9) {
    
    coord[i + 1, 1] <- coord[i, 1]
    
    coord[i + 1, 2] <- coord[i, 2]
    
    coord[i + 1, 3] <- 0
    
  }
   
}

plot(coord[,1:2], type = "l")
points(0,0, col = "red", pch = 19)
points(coord[i + 1, 1], coord[i + 1, 2], col = "blue", pch = 19)


# 2: function

random_walk <- function(steps = 10000) {
  
  coord <- matrix(data = NA, ncol = 3, nrow = steps)
  colnames(coord) <- c("x.coord", "y.coord", "direction")
  coord <- as.data.frame(coord)
  
  coord[1, ] <- c(0, 0, 0)
  
  for(i in 1:steps) {
    
    direction <- sample(1:9, 1)
    
    if(direction == 1){
      
      coord[i + 1, 1] <- coord[i, 1]
      
      coord[i + 1, 2] <- coord[i, 2] + 1
      
      coord[i + 1, 3] <- "N"
      
    } else if (direction == 2) {
      
      coord[i + 1, 1] <- coord[i, 1] + 1
      
      coord[i + 1, 2] <- coord[i, 2] + 1
      
      coord[i + 1, 3] <- "NE"
      
    } else if (direction == 3) {
      
      coord[i + 1, 1] <- coord[i, 1] + 1
      
      coord[i + 1, 2] <- coord[i, 2]
      
      coord[i + 1, 3] <- "E"
      
    } else if (direction == 4) {
      
      coord[i + 1, 1] <- coord[i, 1] + 1
      
      coord[i + 1, 2] <- coord[i, 2] - 1
      
      coord[i + 1, 3] <- "SE"
      
    } else if (direction == 5) {
      
      coord[i + 1, 1] <- coord[i, 1] 
      
      coord[i + 1, 2] <- coord[i, 2] - 1
      
      coord[i + 1, 3] <- "S"
      
    } else if (direction == 6) {
      
      coord[i + 1, 1] <- coord[i, 1] - 1
      
      coord[i + 1, 2] <- coord[i, 2] - 1
      
      coord[i + 1, 3] <- "SW"
      
    } else if (direction == 7) {
      
      coord[i + 1, 1] <- coord[i, 1] - 1
      
      coord[i + 1, 2] <- coord[i, 2]
      
      coord[i + 1, 3] <- "W"
      
    } else if (direction == 8) {
      
      coord[i + 1, 1] <- coord[i, 1] - 1
      
      coord[i + 1, 2] <- coord[i, 2] + 1
      
      coord[i + 1, 3] <- "NW"
      
    } else if (direction == 9) {
      
      coord[i + 1, 1] <- coord[i, 1]
      
      coord[i + 1, 2] <- coord[i, 2]
      
      coord[i + 1, 3] <- 0
      
    }
    
  }
  
  plot(coord[, 1:2], type = "l")
  points(0,0, col = "red", pch = 19)
  points(coord[steps + 1, 1], coord[steps + 1, 2], col = "blue", pch = 19)
  
}

random_walk()


# 3: multi-function

multi_random_walk <- function(steps = 10000, rep = 20, grid = 200){
  
  cols <- c("red", "blue", "pink", "green", "darkgreen", "purple", "black", "cyan", "firebrick", "lightcoral", "midnightblue", "palegreen3", "deepskyblue", "salmon", "orangered4", "mediumseagreen", "indianred", "darkolivegreen", "aquamarine3")
  
  random_walk <- function() {
    
    coord <- matrix(data = NA, ncol = 3, nrow = steps)
    colnames(coord) <- c("x.coord", "y.coord", "direction")
    coord <- as.data.frame(coord)
    
    coord[1, ] <- c(0, 0, 0)
    
    for(i in 1:steps) {
      
      direction <- sample(1:9, 1)
      
      if(direction == 1){
        
        coord[i + 1, 1] <- coord[i, 1]
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "N"
        
      } else if (direction == 2) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "NE"
        
      } else if (direction == 3) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- "E"
        
      } else if (direction == 4) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "SE"
        
      } else if (direction == 5) {
        
        coord[i + 1, 1] <- coord[i, 1] 
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "S"
        
      } else if (direction == 6) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "SW"
        
      } else if (direction == 7) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- "W"
        
      } else if (direction == 8) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "NW"
        
      } else if (direction == 9) {
        
        coord[i + 1, 1] <- coord[i, 1]
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- 0
        
      }
      
    }
    
    points(coord[, 1:2], type = "l", col = sample(cols))
    points(0,0, col = "red", pch = 19)
    points(coord[steps + 1, 1], coord[steps + 1, 2], col = "blue", pch = 19)
    
  }
  
  for (j in 1:rep) {
    
    if (j == 1){
      
      plot(0, xlim = c(-grid, grid), ylim = c(-grid, grid))
      
      random_walk()
      
    } else {
      
      random_walk()
    
    }
  
  }

}


multi_random_walk()


# enhanced version (simplier)

multi_random_walk <- function(steps = 10000, rep = 100, grid = 200){
  
  cols <- c("red", "blue", "pink", "green", "darkgreen", "purple", "black", "cyan", "firebrick", "lightcoral", "midnightblue", "palegreen3", "deepskyblue", "salmon", "orangered4", "mediumseagreen", "indianred", "darkolivegreen", "aquamarine3")
  
  plot(0, xlim = c(-grid, grid), ylim = c(-grid, grid), pch = 3, col = "red", cex = 0.001)
  
  random_walk <- function() {
    
    coord <- matrix(data = NA, ncol = 3, nrow = steps)
    colnames(coord) <- c("x.coord", "y.coord", "direction")
    coord <- as.data.frame(coord)
    
    coord[1, ] <- c(0, 0, 0)
    
    for(i in 1:steps) {
      
      direction <- sample(1:9, 1)
      
      if(direction == 1){
        
        coord[i + 1, 1] <- coord[i, 1]
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "N"
        
      } else if (direction == 2) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "NE"
        
      } else if (direction == 3) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- "E"
        
      } else if (direction == 4) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "SE"
        
      } else if (direction == 5) {
        
        coord[i + 1, 1] <- coord[i, 1] 
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "S"
        
      } else if (direction == 6) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "SW"
        
      } else if (direction == 7) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- "W"
        
      } else if (direction == 8) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "NW"
        
      } else if (direction == 9) {
        
        coord[i + 1, 1] <- coord[i, 1]
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- 0
        
      }
      
    }
    
    points(coord[, 1:2], type = "l", col = sample(cols))
    points(0,0, col = "red", pch = 3)
    points(coord[steps + 1, 1], coord[steps + 1, 2], col = "blue", pch = 19)
    
  }
  
  for (j in 1:rep){
    
    random_walk()
    
  }
  
}

multi_random_walk(steps = 1000, rep = 20, grid = 80)



# 4: just final location

multi_random_walk_final <- function(steps = 10000, rep = 100, grid = 200){
  
  cols <- c("red", "blue", "pink", "green", "darkgreen", "purple", "black", "cyan", "firebrick", "lightcoral", "midnightblue", "palegreen3", "deepskyblue", "salmon", "orangered4", "mediumseagreen", "indianred", "darkolivegreen", "aquamarine3")
  
  random_walk <- function() {
    
    coord <- matrix(data = NA, ncol = 3, nrow = steps)
    colnames(coord) <- c("x.coord", "y.coord", "direction")
    coord <- as.data.frame(coord)
    
    coord[1, ] <- c(0, 0, 0)
    
    for(i in 1:steps) {
      
      direction <- sample(1:9, 1)
      
      if(direction == 1){
        
        coord[i + 1, 1] <- coord[i, 1]
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "N"
        
      } else if (direction == 2) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "NE"
        
      } else if (direction == 3) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- "E"
        
      } else if (direction == 4) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "SE"
        
      } else if (direction == 5) {
        
        coord[i + 1, 1] <- coord[i, 1] 
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "S"
        
      } else if (direction == 6) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "SW"
        
      } else if (direction == 7) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- "W"
        
      } else if (direction == 8) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "NW"
        
      } else if (direction == 9) {
        
        coord[i + 1, 1] <- coord[i, 1]
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- 0
        
      }
      
    }
    
    points(0,0, col = "red", pch = 3)
    points(coord[steps + 1, 1], coord[steps + 1, 2], col = sample(colors()), pch = 19)
    
  }
  
  for (j in 1:rep) {
    
    if (j == 1){
      
      plot(0, xlim = c(-grid, grid), ylim = c(-grid, grid), pch = 3, col = "red", cex = 0)
      
      random_walk()
      
    } else {
      
      random_walk()
      
    }
    
  }
  
}


multi_random_walk_final(steps = 100, rep = 10, grid = 40)




# 5: maximum distance of each repetition

library(DescTools)

multi_random_walk <- function(steps = 10000, rep = 10, grid = 200){
  
  cols <- c("red", "blue", "pink", "green", "darkgreen", "purple", "black", "cyan", "firebrick", "lightcoral", "midnightblue", "palegreen3", "deepskyblue", "salmon", "orangered4", "mediumseagreen", "indianred", "darkolivegreen", "aquamarine3")
  
  plot(0, xlim = c(-grid, grid), ylim = c(-grid, grid), pch = 3, col = "red", cex = 0.001)
  
  random_walk <- function() {
    
    coord <- matrix(data = NA, ncol = 3, nrow = steps)
    colnames(coord) <- c("x.coord", "y.coord", "direction")
    coord <- as.data.frame(coord)
    
    coord[1, ] <- c(0, 0, 0)
    
    radius <- matrix(data = NA, ncol = 1, nrow = steps)
    
    radius[1] <- 0
    
    for(i in 1:steps) {
      
      direction <- sample(1:9, 1)
      
      if(direction == 1){
        
        coord[i + 1, 1] <- coord[i, 1]
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "N"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 2) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "NE"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 3) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- "E"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 4) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "SE"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 5) {
        
        coord[i + 1, 1] <- coord[i, 1] 
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "S"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 6) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "SW"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 7) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- "W"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 8) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "NW"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 9) {
        
        coord[i + 1, 1] <- coord[i, 1]
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- 0
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      }
      
    }
    
    points(coord[, 1:2], type = "l", col = sample(cols))
    points(0,0, col = "red", pch = 3)
    points(coord[steps + 1, 1], coord[steps + 1, 2], col = "blue", pch = 19)
    DrawCircle(x = 0, y = 0, r.out = max(radius))
    
  }
  
  for (j in 1:rep){
    
    random_walk()
    
  }
  
}

multi_random_walk(steps = 1000, rep = 10, grid = 100)



# 6: average max distance

multi_random_walk <- function(steps = 10000, rep = 10, grid = 200){
  
  cols <- c("red", "blue", "pink", "green", "darkgreen", "purple", "black", "cyan", "firebrick", "lightcoral", "midnightblue", "palegreen3", "deepskyblue", "salmon", "orangered4", "mediumseagreen", "indianred", "darkolivegreen", "aquamarine3")
  
  plot(0, xlim = c(-grid, grid), ylim = c(-grid, grid), pch = 3, col = "red", cex = 0.001, xlab = "", ylab = "", main = "Random walk patterns and \nmaximum exploration distance")
  title(ylab = "y coordinates", line = 2.5)
  title(xlab = "x coordinates", line = 2)
  title(sub = paste("n = ", rep, sep = ""), line = 3)
  
  legend("topleft", legend = c("Mean", "CI", "SD"), col = c("red", "black", "black"), lty = c("solid", "dashed", "dotted"), lwd = c(3, 1, 1), cex = 0.8, title = "Maximum distance", bty = "n")
  
  legend("topright", legend = c("Origin", "Final"), col = c("red", "black"), pt.bg = c("red", "gold"), pch = c(3, 21), cex = 0.8, title = "Location", bty = "n")
  
  max_radius <- matrix(data = NA, ncol= 1, nrow = rep)
  
  random_walk <- function() {
    
    coord <- matrix(data = NA, ncol = 3, nrow = steps)
    colnames(coord) <- c("x.coord", "y.coord", "direction")
    coord <- as.data.frame(coord)
    
    coord[1, ] <- c(0, 0, 0)
    
    radius <- matrix(data = NA, ncol = 1, nrow = steps)
    
    radius[1] <- 0
    
    for(i in 1:steps) {
      
      direction <- sample(1:9, 1)
      
      if(direction == 1){
        
        coord[i + 1, 1] <- coord[i, 1]
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "N"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 2) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "NE"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 3) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- "E"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 4) {
        
        coord[i + 1, 1] <- coord[i, 1] + 1
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "SE"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 5) {
        
        coord[i + 1, 1] <- coord[i, 1] 
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "S"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 6) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2] - 1
        
        coord[i + 1, 3] <- "SW"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 7) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- "W"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 8) {
        
        coord[i + 1, 1] <- coord[i, 1] - 1
        
        coord[i + 1, 2] <- coord[i, 2] + 1
        
        coord[i + 1, 3] <- "NW"
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      } else if (direction == 9) {
        
        coord[i + 1, 1] <- coord[i, 1]
        
        coord[i + 1, 2] <- coord[i, 2]
        
        coord[i + 1, 3] <- 0
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      }
      
    }
    
    points(coord[, 1:2], type = "l", col = sample(cols))
    points(0,0, col = "red", pch = 3)
    points(coord[steps + 1, 1], coord[steps + 1, 2], bg = "gold", col = "black", pch = 21)
    assign("j_max_radius", max(radius), envir = .GlobalEnv)

    
  }
  
  for (j in 1:rep){
    
    random_walk()
    max_radius[j] <- j_max_radius
    assign("max_radius", max_radius, envir = .GlobalEnv)
    
  }
  
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius), border = "red", lwd = 3)
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) - 1.96*((sd(max_radius))/sqrt(rep)), lwd = 1, lty = "dashed")
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) + 1.96*((sd(max_radius))/sqrt(rep)), lwd = 1, lty = "dashed")
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) - sd(max_radius), lwd = 1, lty = "dotted")
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) + sd(max_radius), lwd = 1, lty = "dotted")
  
  title(sub = paste("Mean exploration distance = ", round(mean(max_radius), 2), "   |   CI = ? ", round(1.96*((sd(max_radius))/sqrt(rep)), 2), "   |   SD = ? ", round(sd(max_radius), 2), sep = ""), line = 3.75, cex.sub = 0.8)
  
}

multi_random_walk(steps = 1000, rep = 20, grid = 100)

# 6.1: optimised model

multi_random_walk <- function(steps = 10000, rep = 10, grid = 200){
  
  cols <- c("red", "blue", "pink", "green", "darkgreen", "purple", "black", "cyan", "firebrick", "lightcoral", "midnightblue", "palegreen3", "deepskyblue", "salmon", "orangered4", "mediumseagreen", "indianred", "darkolivegreen", "aquamarine3", "royalblue2", "darkorange2", "cyan4")
  
  plot(0, xlim = c(-grid, grid), ylim = c(-grid, grid), pch = 3, col = "red", cex = 0.001, xlab = "", ylab = "", main = "Random walk patterns and \nmaximum exploration distance")
  title(ylab = "y coordinates", line = 2.5)
  title(xlab = "x coordinates", line = 2)
  title(sub = paste("n = ", rep, "  |  Number of steps = ", steps, sep = ""), line = 2.85)
  
  directions <- matrix(NA, ncol = 3, nrow = 9)
  colnames(directions) <- c("x.coord", "y.coord", "direction")
  directions <- as.data.frame(directions)
  directions$x.coord <- c(0, 1, 1, 1, 0, -1, -1, -1, 0)
  directions$y.coord <- c(1, 1, 0, -1, -1, -1, 0, 1, 0)
  directions$direction <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", 0)
  
  max_radius <- matrix(data = NA, ncol= 1, nrow = rep)
  
  final_position <- matrix(data = NA, ncol = 2, nrow = rep)
  
  random_walk <- function() {
    
    coord <- matrix(data = NA, ncol = 3, nrow = steps)
    colnames(coord) <- c("x.coord", "y.coord", "direction")
    coord <- as.data.frame(coord)
    
    coord[1, ] <- c(0, 0, 0)
    
    radius <- matrix(data = NA, ncol = 1, nrow = steps)
    
    radius[1] <- 0
    
    for(i in 1:steps) {
      
      random_dir <- sample(1:9, 1)
      
      coord[i + 1, 1] <- coord[i, 1] + directions[random_dir, 1]
      
      coord[i + 1, 2] <- coord[i, 2] + directions[random_dir, 2]
      
      coord[i + 1, 3] <- directions[random_dir, 3]
      
      radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
      
    }
    
    points(coord[, 1:2], type = "l", col = sample(cols))
    points(0,0, col = "red", pch = 3)
    points(coord[steps + 1, 1], coord[steps + 1, 2], bg = "gold", col = "black", pch = 21)
    assign("j_max_radius", max(radius), envir = .GlobalEnv)
    assign("j_final_coord", c(coord[steps + 1, 1], coord[steps + 1, 2]), envir = .GlobalEnv)
    
    
  }
  
  for (j in 1:rep){
    
    random_walk()
    max_radius[j] <- j_max_radius
    final_position[j, 1] <- j_final_coord[1]
    final_position[j, 2] <- j_final_coord[2]
    assign("max_radius", max_radius, envir = .GlobalEnv)
    assign("final_position", final_position, envir = .GlobalEnv)
    
  }
  
  for(k in 1:rep){
    
    points(final_position[k, 1], final_position[k, 2], bg = "gold", col = "black", pch = 21)
    
  }
  
  points(0,0, bg = "black", col = "black", pch = 21, cex = 1.2, lwd = 1.3)
  
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius), border = "red", lwd = 3)
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) - 1.96*((sd(max_radius))/sqrt(rep)), lwd = 1, lty = "dashed")
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) + 1.96*((sd(max_radius))/sqrt(rep)), lwd = 1, lty = "dashed")
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) - sd(max_radius), lwd = 1, lty = "dotted")
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) + sd(max_radius), lwd = 1, lty = "dotted")
  

  title(sub = paste("Mean exploration distance = ", round(mean(max_radius), 2), "   |   CI = ? ", round(1.96*((sd(max_radius))/sqrt(rep)), 2), "   |   SD = ? ", round(sd(max_radius), 2), sep = ""), line = 3.75, cex.sub = 0.8)
  
}

multi_random_walk(steps = 10000, rep = 100, grid = 230)

legend("topright", legend = c("Mean", "CI", "SD"), col = c("red", "black", "black"), lty = c("solid", "dashed", "dotted"), lwd = c(3, 1, 1), cex = 0.8, title = "Maximum distance", bty = "n")

legend("topleft", legend = c("Origin", "Final"), col = c("black", "black"), pt.bg = c("black", "gold"), pch = c(21, 21), cex = 0.8, title = "Location", bty = "n")

library(DescTools)

# I have just realised that i could have done a 3D matrix like in the SDOM matrix, and store all the results of the replicates... Well, I don't need that for the moment, but I should consider this for other scripts... Always that there is a for loop, I could (and should!) store the results!


# 7: regression steps vs. exploration

steps_vs_dist_function <- function(rep = 50){
  
  effort <- c(1, 2, 3, 5, 7, 10^seq(1, 4, 0.1)) # 3 = 1000, 4 = 10000
  
  steps_vs_dist <- matrix(data = NA, ncol = 4, nrow = length(effort))
  colnames(steps_vs_dist) <- c("steps", "mean", "SD", "CI")
  steps_vs_dist <- as.data.frame(steps_vs_dist)
  steps_vs_dist$steps <- effort
  
  p <- 1
  
  directions <- matrix(NA, ncol = 3, nrow = 9)
  colnames(directions) <- c("x.coord", "y.coord", "direction")
  directions <- as.data.frame(directions)
  directions$x.coord <- c(0, 1, 1, 1, 0, -1, -1, -1, 0)
  directions$y.coord <- c(1, 1, 0, -1, -1, -1, 0, 1, 0)
  directions$direction <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", 0)
  
  multi_random_walk <- function(steps = k){
    
    cols <- c("red", "blue", "pink", "green", "darkgreen", "purple", "black", "cyan", "firebrick", "lightcoral", "midnightblue", "palegreen3", "deepskyblue", "salmon", "orangered4", "mediumseagreen", "indianred", "darkolivegreen", "aquamarine3")
    
    max_radius <- matrix(data = NA, ncol= 1, nrow = rep)
    
    random_walk <- function() {
      
      coord <- matrix(data = NA, ncol = 3, nrow = steps)
      colnames(coord) <- c("x.coord", "y.coord", "direction")
      coord <- as.data.frame(coord)
      
      coord[1, ] <- c(0, 0, 0)
      
      radius <- matrix(data = NA, ncol = 1, nrow = steps)
      
      radius[1] <- 0
      
      for(i in 1:steps) {
        
        random_dir <- sample(1:9, 1)
        
        coord[i + 1, 1] <- coord[i, 1] + directions[random_dir, 1]
        
        coord[i + 1, 2] <- coord[i, 2] + + directions[random_dir, 2]
        
        coord[i + 1, 3] <- directions[random_dir, 3]
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      }
      
      assign("j_max_radius", max(radius), envir = .GlobalEnv)
      
    }
    
    for (j in 1:rep){
      
      random_walk()
      max_radius[j] <- j_max_radius
      assign("max_radius", max_radius, envir = .GlobalEnv)
      
    }
    
  }
  
  
  for(k in effort){
    
    multi_random_walk()
    steps_vs_dist[p, 2] <- mean(max_radius)
    steps_vs_dist[p, 3] <- sd(max_radius)
    steps_vs_dist[p, 4] <- 1.96*((sd(max_radius))/sqrt(rep))
    assign("steps_vs_dist", steps_vs_dist, envir = .GlobalEnv)
    p <- p + 1
    
  }
  
  plot(steps_vs_dist$steps, steps_vs_dist$mean, type = "l", xlab = "", ylab = "", lwd = 3, col = "red", ylim = c(0,200))
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$SD, lty = "dotted")
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$SD, lty = "dotted")
  
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$CI, lty = "dashed")
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$CI, lty = "dashed")
  points(steps_vs_dist$steps, steps_vs_dist$mean, pch = 21, col = "black", bg = "black")
  
  lm_steps_vs_dist_log <- lm(log(mean)~log(steps), data = steps_vs_dist)
  summary(lm_steps_vs_dist_log)
  
  lines(steps_vs_dist$steps,steps_vs_dist$steps^lm_steps_vs_dist_log$coefficients[2], col = "blue")
  
  title(main = "Mean maximum exploration distance \nas a function of the number of steps")
  title(xlab = "N? of steps", line = 2.5)
  title(ylab = "Mean maximum exploration distance", line = 2.5)
  title(sub = paste("n = ", rep, sep = ""), line = 3.5)
  
}

steps_vs_dist_function(rep = 50)

plot(steps_vs_dist$steps, steps_vs_dist$mean, type = "l", xlab = "", ylab = "", lwd = 3, col = "red", ylim = c(0,200))
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$SD, lty = "dotted")
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$SD, lty = "dotted")

lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$CI, lty = "dashed")
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$CI, lty = "dashed")
points(steps_vs_dist$steps, steps_vs_dist$mean, pch = 21, col = "black", bg = "black")

lm_steps_vs_dist_log <- lm(log(mean)~log(steps), data = steps_vs_dist)
summary(lm_steps_vs_dist_log)

lines(steps_vs_dist$steps,steps_vs_dist$steps^lm_steps_vs_dist_log$coefficients[2], col = "blue")

title(main = "Mean maximum exploration distance \nas a function of the number of steps")
title(xlab = "N? of steps", line = 2.5)
title(ylab = "Mean maximum exploration distance", line = 2.5)
title(sub = paste("n = ", rep, sep = ""), line = 3.5)


lm_steps_vs_dist_log <- lm(log(mean)~log(steps), data = steps_vs_dist)
summary(lm_steps_vs_dist_log)

legend("topleft", legend = c("Mean", "CI", "SD", as.expression(bquote("y = " ~ x^0.516 ~ ""))), col = c("red", "black", "black", "blue"), lty = c("solid", "dashed", "dotted", "solid"), lwd = c(3, 1, 1, 1), cex = 0.8, title = "Maximum distance", bty = "n")



plot(steps_vs_dist$steps, steps_vs_dist$mean, type = "l", xlab = "", ylab = "", lwd = 3, col = "red", log = "xy")
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$SD, lty = "dotted")
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$SD, lty = "dotted")
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$CI, lty = "dashed")
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$CI, lty = "dashed")
abline(a = 0, b = lm_steps_vs_dist_log$coefficients[2], col = "blue")
points(steps_vs_dist$steps, steps_vs_dist$mean, pch = 21, col = "black", bg = "black")
legend("topleft", legend = c("Mean", "CI", "SD", "y = 0.516x"), col = c("red", "black", "black", "blue"), lty = c("solid", "dashed", "dotted", "solid"), lwd = c(3, 1, 1, 1), cex = 0.8, title = "Maximum distance", bty = "n")

title(main = "Mean maximum exploration distance \nas a function of the number of steps")
title(xlab = "log(N? of steps)", line = 2.5)
title(ylab = "log(Mean maximum exploration distance)", line = 2.5)
title(sub = paste("n = ", rep, sep = ""), line = 3.5)





# Plot

plot1 <- function(){
  plot(steps_vs_dist$steps, steps_vs_dist$mean, type = "l", xlab = "", ylab = "", lwd = 3, col = "red", ylim = c(0,200))
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$SD, lty = "dotted")
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$SD, lty = "dotted")
  
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$CI, lty = "dashed")
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$CI, lty = "dashed")
  points(steps_vs_dist$steps, steps_vs_dist$mean, pch = 21, col = "black", bg = "black")
  
  lm_steps_vs_dist_log <- lm(log(mean)~log(steps), data = steps_vs_dist)
  summary(lm_steps_vs_dist_log)
  
  lines(steps_vs_dist$steps, steps_vs_dist$steps^(lm_steps_vs_dist_log$coefficients[2]), col = "blue")
  
  title(xlab = "N? of steps", line = 2)
  title(ylab = "MMED", line = 2)
  title(sub = paste("n = ", rep, sep = ""), line = 3)
  
  lm_steps_vs_dist_log <- lm(log(mean)~log(steps), data = steps_vs_dist)
  summary(lm_steps_vs_dist_log)
  
  legend("topleft", legend = c("Mean", "CI", "SD", as.expression(bquote("y = " ~ x^0.518 ~ ""))), col = c("red", "black", "black", "blue"), lty = c("solid", "dashed", "dotted", "solid"), lwd = c(3, 1, 1, 1), cex = 0.8, title = "Maximum distance", bty = "n")
  
  print(lm_steps_vs_dist_log$coefficients[2])
  
}


plot2 <- function(){
  
  plot(steps_vs_dist$steps, steps_vs_dist$mean, type = "l", xlab = "", ylab = "", lwd = 3, col = "red", log = "xy")
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$SD, lty = "dotted")
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$SD, lty = "dotted")
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$CI, lty = "dashed")
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$CI, lty = "dashed")
  abline(a = 0, b = lm_steps_vs_dist_log$coefficients[2], col = "blue")
  points(steps_vs_dist$steps, steps_vs_dist$mean, pch = 21, col = "black", bg = "black")
  legend("topleft", legend = c("Mean", "CI", "SD", "y = 0.518x"), col = c("red", "black", "black", "blue"), lty = c("solid", "dashed", "dotted", "solid"), lwd = c(3, 1, 1, 1), cex = 0.8, title = "Maximum distance", bty = "n")
  
  title(xlab = "log(N? of steps)", line = 2)
  title(ylab = "log(MMED)", line = 2)
  title(sub = paste("n = ", rep, sep = ""), line = 3)
  
  
}


par(mfrow = c(1,2),  mai = c(0.9, 0.7, 0.2, 0.2))

plot1()

plot2()






# Extra: no 9th postion (stay)


multi_random_walk_no_9 <- function(steps = 10000, rep = 10, grid = 200){
  
  cols <- c("red", "blue", "pink", "green", "darkgreen", "purple", "black", "cyan", "firebrick", "lightcoral", "midnightblue", "palegreen3", "deepskyblue", "salmon", "orangered4", "mediumseagreen", "indianred", "darkolivegreen", "aquamarine3")
  
  plot(0, xlim = c(-grid, grid), ylim = c(-grid, grid), pch = 3, col = "red", cex = 0.001, xlab = "", ylab = "", main = "Random walk patterns and \nmaximum exploration distance")
  title(ylab = "y coordinates", line = 2.5)
  title(xlab = "x coordinates", line = 2)
  title(sub = paste("n = ", rep, sep = ""), line = 3)
  
  legend("topleft", legend = c("Mean", "CI", "SD"), col = c("red", "black", "black"), lty = c("solid", "dashed", "dotted"), lwd = c(3, 1, 1), cex = 0.8, title = "Maximum distance", bty = "n")
  
  legend("topright", legend = c("Origin", "Final"), col = c("red", "black"), pt.bg = c("red", "gold"), pch = c(3, 21), cex = 0.8, title = "Location", bty = "n")
  
  directions <- matrix(NA, ncol = 3, nrow = 8)
  colnames(directions) <- c("x.coord", "y.coord", "direction")
  directions <- as.data.frame(directions)
  directions$x.coord <- c(0, 1, 1, 1, 0, -1, -1, -1 )
  directions$y.coord <- c(1, 1, 0, -1, -1, -1, 0, 1)
  directions$direction <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  
  max_radius <- matrix(data = NA, ncol= 1, nrow = rep)
  
  random_walk <- function() {
    
    coord <- matrix(data = NA, ncol = 3, nrow = steps)
    colnames(coord) <- c("x.coord", "y.coord", "direction")
    coord <- as.data.frame(coord)
    
    coord[1, ] <- c(0, 0, 0)
    
    radius <- matrix(data = NA, ncol = 1, nrow = steps)
    
    radius[1] <- 0
    
    for(i in 1:steps) {
      
      random_dir <- sample(1:8, 1)
      
      coord[i + 1, 1] <- coord[i, 1] + directions[random_dir, 1]
      
      coord[i + 1, 2] <- coord[i, 2] + + directions[random_dir, 2]
      
      coord[i + 1, 3] <- directions[random_dir, 3]
      
      radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
      
    }
    
    points(coord[, 1:2], type = "l", col = sample(cols))
    points(0,0, col = "red", pch = 3)
    points(coord[steps + 1, 1], coord[steps + 1, 2], bg = "gold", col = "black", pch = 21)
    assign("j_max_radius", max(radius), envir = .GlobalEnv)
    
    
  }
  
  for (j in 1:rep){
    
    random_walk()
    max_radius[j] <- j_max_radius
    assign("max_radius", max_radius, envir = .GlobalEnv)
    
  }
  
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius), border = "red", lwd = 3)
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) - 1.96*((sd(max_radius))/sqrt(rep)), lwd = 1, lty = "dashed")
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) + 1.96*((sd(max_radius))/sqrt(rep)), lwd = 1, lty = "dashed")
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) - sd(max_radius), lwd = 1, lty = "dotted")
  DrawCircle(x = 0, y = 0, r.out = mean(max_radius) + sd(max_radius), lwd = 1, lty = "dotted")
  
  title(sub = paste("Mean exploration distance = ", round(mean(max_radius), 2), "   |   CI = ? ", round(1.96*((sd(max_radius))/sqrt(rep)), 2), "   |   SD = ? ", round(sd(max_radius), 2), sep = ""), line = 3.75, cex.sub = 0.8)
  
}

multi_random_walk_no_9(steps = 10000, rep = 50, grid = 200)



steps_vs_dist_function_no_9 <- function(rep = 50){
  
  effort <- c(1, 10^seq(1, 4, 0.1))
  
  steps_vs_dist <- matrix(data = NA, ncol = 4, nrow = length(effort))
  colnames(steps_vs_dist) <- c("steps", "mean", "SD", "CI")
  steps_vs_dist <- as.data.frame(steps_vs_dist)
  steps_vs_dist$steps <- effort
  
  p <- 1
  
  directions <- matrix(NA, ncol = 3, nrow = 8)
  colnames(directions) <- c("x.coord", "y.coord", "direction")
  directions <- as.data.frame(directions)
  directions$x.coord <- c(0, 1, 1, 1, 0, -1, -1, -1)
  directions$y.coord <- c(1, 1, 0, -1, -1, -1, 0, 1)
  directions$direction <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  
  multi_random_walk <- function(steps = k){
    
    cols <- c("red", "blue", "pink", "green", "darkgreen", "purple", "black", "cyan", "firebrick", "lightcoral", "midnightblue", "palegreen3", "deepskyblue", "salmon", "orangered4", "mediumseagreen", "indianred", "darkolivegreen", "aquamarine3")
    
    max_radius <- matrix(data = NA, ncol= 1, nrow = rep)
    
    random_walk <- function() {
      
      coord <- matrix(data = NA, ncol = 3, nrow = steps)
      colnames(coord) <- c("x.coord", "y.coord", "direction")
      coord <- as.data.frame(coord)
      
      coord[1, ] <- c(0, 0, 0)
      
      radius <- matrix(data = NA, ncol = 1, nrow = steps)
      
      radius[1] <- 0
      
      for(i in 1:steps) {
        
        random_dir <- sample(1:8, 1)
        
        coord[i + 1, 1] <- coord[i, 1] + directions[random_dir, 1]
        
        coord[i + 1, 2] <- coord[i, 2] + + directions[random_dir, 2]
        
        coord[i + 1, 3] <- directions[random_dir, 3]
        
        radius[i + 1] <- sqrt((coord[i + 1, 1])^2 + (coord[i + 1, 2])^2)
        
      }
      
      assign("j_max_radius", max(radius), envir = .GlobalEnv)
      
    }
    
    for (j in 1:rep){
      
      random_walk()
      max_radius[j] <- j_max_radius
      assign("max_radius", max_radius, envir = .GlobalEnv)
      
    }
    
  }
  
  
  for(k in effort){
    
    multi_random_walk()
    steps_vs_dist[p, 2] <- mean(max_radius)
    steps_vs_dist[p, 3] <- sd(max_radius)
    steps_vs_dist[p, 4] <- 1.96*((sd(max_radius))/sqrt(rep))
    assign("steps_vs_dist", steps_vs_dist, envir = .GlobalEnv)
    p <- p + 1
    
  }
  
  plot(steps_vs_dist$steps, steps_vs_dist$mean, type = "l", xlab = "", ylab = "", lwd = 3, col = "red", ylim = c(0,200))
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$SD, lty = "dotted")
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$SD, lty = "dotted")
  
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$CI, lty = "dashed")
  lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$CI, lty = "dashed")
  points(steps_vs_dist$steps, steps_vs_dist$mean, pch = 21, col = "black", bg = "black")
  
  lm_steps_vs_dist_log <- lm(log(mean)~log(steps), data = steps_vs_dist)
  summary(lm_steps_vs_dist_log)
  
  lines(steps_vs_dist$steps,steps_vs_dist$steps^lm_steps_vs_dist_log$coefficients[2], col = "blue")
  
  legend("topleft", legend = c("Mean", "CI", "SD", as.expression(bquote("y = " ~ x^0.515 ~ ""))), col = c("red", "black", "black", "blue"), lty = c("solid", "dashed", "dotted", "solid"), lwd = c(3, 1, 1, 1), cex = 0.8, title = "Maximum distance", bty = "n")
  
  title(main = "Mean maximum exploration distance \nas a function of the number of steps")
  title(xlab = "N? of steps", line = 2.5)
  title(ylab = "Mean maximum exploration distance", line = 2.5)
  title(sub = paste("n = ", rep, sep = ""), line = 3.5)
  
}

steps_vs_dist_function_no_9(rep = 20)



lm_steps_vs_dist_log <- lm(log(mean)~log(steps), data = steps_vs_dist)
summary(lm_steps_vs_dist_log)

plot(steps_vs_dist$steps, steps_vs_dist$mean, type = "l", xlab = "", ylab = "", lwd = 3, col = "red", log = "xy")
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$SD, lty = "dotted")
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$SD, lty = "dotted")
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean + steps_vs_dist$CI, lty = "dashed")
lines(x = steps_vs_dist$steps, y = steps_vs_dist$mean - steps_vs_dist$CI, lty = "dashed")
abline(a = 0, b = lm_steps_vs_dist_log$coefficients[2], col = "blue")
points(steps_vs_dist$steps, steps_vs_dist$mean, pch = 21, col = "black", bg = "black")
legend("topleft", legend = c("Mean", "CI", "SD", "y = 0.521x"), col = c("red", "black", "black", "blue"), lty = c("solid", "dashed", "dotted", "solid"), lwd = c(3, 1, 1, 1), cex = 0.8, title = "Maximum distance", bty = "n")

title(main = "Mean maximum exploration distance \nas a function of the number of steps")
title(xlab = "N? of steps", line = 2.5)
title(ylab = "Mean maximum exploration distance", line = 2.5)
title(sub = paste("n = ", rep, sep = ""), line = 3.5)
