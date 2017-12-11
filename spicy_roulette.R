# Simulation -----

library(tidyverse)
library(doParallel)
library(boot)
cores <- 8
cl <- makeCluster(cores)
registerDoParallel(cl)

max_rounds <- 20
max_gamesize <- 1000 # defines how many times you will play spicy roulette with a round size specified above

#store for each round size, chances of getting all spicy
#r_results <- data_frame(round_number = numeric(max_rounds), all_spicy_chance = numeric(max_rounds) , game_size = numeric(max_rounds))

swapping_positions <- function(number,min,max){
  runif(n = number,min,max) %>% round(digits = 0)
}

#Simulate for a given game size for vector of round sizes
simulation_results <- foreach(round_size = 1:max_rounds, .combine=rbind ,.multicombine = TRUE,.export = c("r_results","swapping_positions")) %dopar%{
  library(tidyverse)
  #define results data frame for each set of games to be played at a given round size
  #Temporary variable, that stores all_spicy instances
  results <- data_frame(game_index=numeric() , player_1 = numeric()  , all_spicy = logical() )
  #We also want to see the effect of different game size on the variance of results
  
  #Variable that each thread will return
  returning_df <- data_frame(round_number = numeric(max_rounds), all_spicy_chance = numeric(max_rounds) , game_size = numeric(max_rounds)) 
  for (games in 1:max_gamesize) {
      for (game in 1:games){
        #Define vector of plates
        #A true indicates the spicy burger
        plate_1 <- c(TRUE,FALSE,FALSE)
        plate_2 <- c(TRUE,FALSE,FALSE)
        plate_3 <- c(TRUE,FALSE,FALSE)
        for(i in 1:round_size){
        #get swapping positions
          one_and_two <- swapping_positions(2,1,3)
          tmp <- plate_1[one_and_two[1]] #store value in plate 1 to be swapped
          plate_1[one_and_two[1]]  <- plate_2[one_and_two[2]]
          plate_2[one_and_two[2]] <- tmp
          
          two_and_three <- swapping_positions(2,1,3)
          tmp <- plate_2[two_and_three[1]] #store value in plate 2 to be swapped
          plate_2[two_and_three[1]]  <- plate_3[two_and_three[2]]
          plate_3[two_and_three[2]] <- tmp
          
          
          three_and_one <- swapping_positions(2,1,3)
          tmp <- plate_3[three_and_one[1]] #store value in plate 3 to be swapped
          plate_3[three_and_one[1]]  <- plate_1[three_and_one[2]]
          plate_1[three_and_one[2]] <- tmp
        }
        results[game,"game_index"] <- game
        results[game,"player_1"] <- sum(plate_1)
        if(sum(plate_1)==3){results[game,"all_spicy"] = TRUE }else{results[game,"all_spicy"] = FALSE }
      }
    #once we have finsihed all games at a given round size, record % chance of getting all spicy
    #Add results to df that will be returned
    returning_df <- bind_rows(
      returning_df,
      data_frame(round_number = round_size , all_spicy_chance = (sum(results$all_spicy)/dim(results)[1])*100  , game_size = games) 
    )
    # c(round_size ,
    #   (sum(results$all_spicy)/dim(results)[1])*100 , games )
    }
    returning_df
}
stopCluster(cl)


#Simulate for a given ROUND size and one given GAME size , i.e. create distribution of 3 round games where you play 30games
outlier_simulation <- function(round_size, game_size , R = 1000){
  
  foreach(repetition = 1:R, .combine=rbind ,.multicombine = TRUE,.export = c("swapping_positions")) %dopar%{
    library(tidyverse)
    #define results data frame for each set of games to be played at a given round size
    #Temporary variable, that stores all_spicy instances
    results <- data_frame(game_index=numeric() , player_1 = numeric()  , all_spicy = logical() )
    #We also want to see the effect of different game size on the variance of results
    
    #Variable that each thread will return
    returning_df <- data_frame(round_number = numeric(), all_spicy_chance = numeric() , game_size = numeric()) 
    
      for (game in 1:game_size){
        #Define vector of plates
        #A true indicates the spicy burger
        plate_1 <- c(TRUE,FALSE,FALSE)
        plate_2 <- c(TRUE,FALSE,FALSE)
        plate_3 <- c(TRUE,FALSE,FALSE)
        for(i in 1:round_size){
          #get swapping positions
          one_and_two <- swapping_positions(2,1,3)
          tmp <- plate_1[one_and_two[1]] #store value in plate 1 to be swapped
          plate_1[one_and_two[1]]  <- plate_2[one_and_two[2]]
          plate_2[one_and_two[2]] <- tmp
          
          two_and_three <- swapping_positions(2,1,3)
          tmp <- plate_2[two_and_three[1]] #store value in plate 2 to be swapped
          plate_2[two_and_three[1]]  <- plate_3[two_and_three[2]]
          plate_3[two_and_three[2]] <- tmp
          
          
          three_and_one <- swapping_positions(2,1,3)
          tmp <- plate_3[three_and_one[1]] #store value in plate 3 to be swapped
          plate_3[three_and_one[1]]  <- plate_1[three_and_one[2]]
          plate_1[three_and_one[2]] <- tmp
        }
        results[game,"game_index"] <- game
        results[game,"player_1"] <- sum(plate_1)
        if(sum(plate_1)==3){results[game,"all_spicy"] = TRUE }else{results[game,"all_spicy"] = FALSE }
      }
      #once we have finsihed all games at a given round size, record % chance of getting all spicy
      #Add results to df that will be returned
      returning_df <- bind_rows(
        returning_df,
        data_frame(rep_index = repetition,round_number = round_size , all_spicy_chance = (sum(results$all_spicy)/dim(results)[1])*100  , game_size = game_size) 
      )
      # c(round_size ,
      #   (sum(results$all_spicy)/dim(results)[1])*100 , games )
    
    returning_df
  }
  
}

outlier_r_16_g_1 <- outlier_simulation(round_size = 16 , game_size = 1 , R= 10000)
outlier_r_20_g_2 <- outlier_simulation(round_size = 20 , game_size = 2 , R= 10000)



# Plots ------

#Exclude 0 game size entries. I think they are created as a result of the multithreading
simulation_results <- simulation_results %>% filter(game_size != 0)
saveRDS(simulation_results ,"results/naive_simulation.rds")

#Define results for plotting
plotting_with_outliers <- simulation_results #include all game sizes

simulation_plot_with_outliers <- ggplot(plotting_with_outliers) + aes(x = factor(round_number) , y = all_spicy_chance , col = game_size) + scale_color_gradient(low="green",high="red") +
                  xlab("Round Size") + ylab("% of games where one player had all 3 spicy burgers") + labs(title ="Results with outliers",color ="Game Size") + geom_point(size=5)  
simulation_plot_with_outliers
saveRDS(simulation_plot_with_outliers,"plots/simulation_plot_with_outliers.rds")

plotting_without_outliers <- simulation_results %>% filter(game_size > 3) #include game sizes from 3 upwards

simulation_plot_without_outliers <- ggplot(plotting_without_outliers) + aes(x = factor(round_number) , y = all_spicy_chance , col = game_size) + scale_color_gradient(low="green",high="red") + scale_y_continuous(breaks = seq(1,20,1)) +
  xlab("Round Size") + ylab("% of games where one player had all 3 spicy burgers") + labs(title ="Results without extreme outliers",color ="Game Size") + geom_point(size =5)
simulation_plot_without_outliers
saveRDS(simulation_plot_without_outliers,"plots/simulation_plot_without_outliers.rds")

simulation_box_plot <- ggplot(plotting_without_outliers) + aes(x = factor(round_number) , y = all_spicy_chance )+  scale_y_continuous(breaks = round(seq(min(plotting_without_outliers$all_spicy_chance), max(plotting_without_outliers$all_spicy_chance), by = 1),1)) +
  xlab("Round Size") + ylab("% of games where one player had all 3 spicy burgers") + labs(title ="Boxplot of results without extreme outliers") +geom_boxplot()
simulation_box_plot
saveRDS(simulation_box_plot,"plots/simulation_box_plot.rds")




#TODO bootstrap of two extreme outlier points... calculate chances of that happening FOR THOSE GAME SIzes!
#PERHPAS GENERATE A DISTRIBUTION?

#Plot of round_number vs all_spicy chance for ALL game sizes
line_plot_overall <- ggplot(plotting_without_outliers) + aes(x = round_number , y = all_spicy_chance ) + geom_smooth()
line_plot_overall


#all spicy vs game size... let's see how variation of success changes with game size


line_gamesize_round_number <- ggplot(plotting_without_outliers) + aes(x = game_size , y = all_spicy_chance, col = factor(round_number) , alpha =all_spicy_chance) + geom_point() + labs(title = "Getting all 3 Spicy vs Game Size" ) + xlab("Game Size") + ylab("% of games where one player had all 3 spicy burgers")
line_gamesize_round_number




#If i need to play one game????/


#After what game size do things stablillise?
