# Created by: Michael Ha and Mohammed Tanvir

# 2.1 Playing with lists (warming up)
V = list()

for(i in 1:100) 
{
  V[[i]] <- vector() # assigns an empty vector to the ith element of list V
}

for (n in 1:200) 
{
  i <- sample(1:100, 1)
  aux <- c(sample(1:10, 2, replace = FALSE))
  V[[i]] <- union(V[[i]], aux)
}
barplot(table(sapply(V, length)), ylim = c(0, 40), 
        main = "Figure 1: Barplot of the frequency of the vectors' lengths in the list", 
        xlab = "Vector length", 
        ylab = "Frequency")



# 2.2 Generating generations (warming up)
N <- 500
generation_0 <- list()
generation_1 <- list()
generation_2 <- list()
generation_3 <- list()

# GENERATION 1
for (i in 1:N) # This for loop assigns incremented vectors to the list generation_0 and null vectors to the generation lists
{
  generation_0[[i]] <- c(i)
  generation_1[[i]] <- vector()
  generation_2[[i]] <- vector()
  generation_3[[i]] <- vector()
}

for (i in 1:N) # This for loop assigns the children to the their corresponding random parents in generation_1
{
  parents <- c(sample(1:N, 2, replace = FALSE))
  generation_1[[parents[1]]] <- union(generation_0[[i]], generation_1[[parents[1]]]) # The first parent
  generation_1[[parents[2]]] <- union(generation_0[[i]], generation_1[[parents[2]]]) # The second parent
}

# GENERATION 2
for (i in 1:N) # This for loop assigns the descendants of the children to the their corresponding random parents in generation_2
{
  parents <- c(sample(1:N, 2, replace = FALSE))
  generation_2[[parents[1]]] <- union(generation_1[[i]], generation_2[[parents[1]]]) # The first parent
  generation_2[[parents[2]]] <- union(generation_1[[i]], generation_2[[parents[2]]]) # The second parent
}

# GENERATION 3
for (i in 1:N) # This for loop assigns the descendants of the children to the their corresponding random parents in generation_3
{
  parents <- c(sample(1:N, 2, replace = FALSE))
  generation_3[[parents[1]]] <- union(generation_2[[i]], generation_3[[parents[1]]]) # The first parent
  generation_3[[parents[2]]] <- union(generation_2[[i]], generation_3[[parents[2]]]) # The second parent
}

# COUNT OF DESCENDANTS AND HISTOGRAM
descendants_length_generation_3 <- sapply(generation_3, length) # The lengths of each vector in the list generation_3 form a new vector
hist(descendants_length_generation_3, 
     ylim = c(0, 150), 
     xlim = c(0,50), 
     breaks = seq(0, 50, 1), 
     xaxp = c(0, 50, 50),
     main = "Figure 2: Histogram of the number of descendants per each member of generation 3", 
     xlab = "Number of descendants", 
     ylab = "Frequency of individuals of generation 3")



# 2.3 Basic function to simulate the MRCA model
# Without replacement
TMRCA_F <- function(N)
{
  generation_X <- list()
  generation_temp <- list()
  generation_counter <- 0
  
  for (i in 1:N) # This for loop assigns incremented vectors to the list generation_temp
  {
    generation_temp[[i]] <- c(i)
  }
  
  while (!N %in% sapply(generation_X, length)) # This while loop loops while no vectors in the list generation_X have a length of N
  {
    for (i in 1:N) # This for loop assigns null vectors to the list generation_X
    {
      generation_X[[i]] <- vector()
    }
    
    for (i in 1:N) # This for loop assigns the descendants to the their corresponding random parents in generation_X
    {
      parents <- c(sample(1:N, 2, replace = FALSE))
      generation_X[[parents[1]]] <- union(generation_temp[[i]], generation_X[[parents[1]]]) # The first parent
      generation_X[[parents[2]]] <- union(generation_temp[[i]], generation_X[[parents[2]]]) # The second parent
    }
    
    generation_temp <- generation_X # The list generation_X temporarily assigns its vectors of descendants to the vectors in the list generation_temp
    generation_counter <- generation_counter + 1 # 1 generation is added after each loop
  }
  return (generation_counter)
}

# With replacement
TMRCA_T <- function(N)
{
  generation_X <- list()
  generation_temp <- list()
  generation_counter <- 0
  
  for (i in 1:N) # This for loop assigns incremented vectors to the list generation_temp
  {
    generation_temp[[i]] <- c(i)
  }
  
  while (!N %in% sapply(generation_X, length)) # This while loop loops while no vectors in the list generation_X have a length of N
  {
    for (i in 1:N) # This for loop assigns null vectors to the list generation_X
    {
      generation_X[[i]] <- vector()
    }
    
    for (i in 1:N) # This for loop assigns the descendants to the their corresponding random parents in generation_X
    {
      parents <- c(sample(1:N, 2, replace = TRUE))
      generation_X[[parents[1]]] <- union(generation_temp[[i]], generation_X[[parents[1]]]) # The first parent
      generation_X[[parents[2]]] <- union(generation_temp[[i]], generation_X[[parents[2]]]) # The second parent
    }
    
    generation_temp <- generation_X # The list generation_X temporarily assigns its vectors of descendants to the vectors in the list generation_temp
    generation_counter <- generation_counter + 1 # 1 generation is added after each loop
  }
  return (generation_counter)
}



# 2.4 Simulating the TMRCA_F
N_is_10 <- replicate(50, TMRCA_F(10)) 
N_is_100 <- replicate(50, TMRCA_F(100)) 
N_is_250 <- replicate(50, TMRCA_F(250))
N_is_500 <- replicate(50, TMRCA_F(500))
N_is_1000 <- replicate(50, TMRCA_F(1000))
N_is_2000 <- replicate(50, TMRCA_F(2000))
N_is_3000 <- replicate(50, TMRCA_F(3000))
N_is_4000 <- replicate(50, TMRCA_F(4000))
N_is_5000 <- replicate(50, TMRCA_F(5000))
population_sizes <- c(10, 100, 250, 500, 1000, 2000, 3000, 4000, 5000)



# 2.5 Presenting the results of the simulation
TMRCA_F_data_frame <- data.frame(N_is_10, N_is_100, N_is_250, N_is_500, N_is_1000, N_is_2000, N_is_3000, N_is_4000, N_is_5000)
TMRCA_F_data_frame

TMRCA_F_means <- sapply(TMRCA_F_data_frame, mean) # This calculates the TMRCA_F mean of 50 simulations for each population size
TMRCA_F_means_data_frame <- with(TMRCA_F_data_frame, setNames(TMRCA_F_means, population_sizes)) # This creates a data frame with the means

barplot(TMRCA_F_means_data_frame,  
        ylim = c(0, 15),
        yaxp = c(0, 15, 15),
        main = "Figure 3: Barplot of the TMRCA_F mean of 50 simulations\n for each population size of the simulations", 
        xlab = "Population size (N)", 
        ylab = "TMRCA_F mean of 50 simulations (generation)")

plot(population_sizes, TMRCA_F_means_data_frame,
     ylim = c(0, 15),
     yaxp = c(0, 15, 15),
     main = "Figure 4: Graph of the TMRCA_F mean of 50 simulations\n vs the population size of the simulations", 
     xlab = "Population size (N)", 
     ylab = "TMRCA_F mean of 50 simulations (generation)")



# 2.6 You can choose your parents: alternate sampling methods
N_is_100_F <- replicate(1000, TMRCA_F(100))
N_is_100_T <- replicate(1000, TMRCA_T(100))
N_is_1000_F <- replicate(1000, TMRCA_F(1000))
N_is_1000_T<- replicate(1000, TMRCA_T(1000))

# Small population size hypothesis testing
t.test(N_is_100_F, N_is_100_T,
       alternative = c("two.sided"),
       mu = 0, var.equal = FALSE)

# Big population size hypothesis testing
t.test(N_is_1000_F, N_is_1000_T,
       alternative = c("two.sided"),
       mu = 0, var.equal = FALSE)
