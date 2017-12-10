library(tidyverse)
library(doParallel)
#Question. If three people were to randomly swap burgers r times, what are the chances that one person
# will get all 3 spicy burgers?
#To make this simple, Player 1 and 2 will swap, then 2 and 3, then 3 and 1

cores <- 8
cl <- makeCluster(cores)
registerDoParallel(cl)

max_rounds <- 180
games <- 1000 # defines how many times you will play spicy roulette with a round size specified above

#store for each round size, chances of getting all spicy
r_results <- data_frame(round_number = numeric(max_rounds), all_spicy_chance = numeric(max_rounds))

swapping_positions <- function(number,min,max){
  runif(n = number,min,max) %>% round(digits = 0)
}

#loop this for r swap rounds
simulation_results <- foreach(round_size = 1:max_rounds, .combine=rbind ,.multicombine = TRUE,.export = c("r_results","swapping_positions")) %dopar%{
  library(tidyverse)
  #define results data frame for each set of games to be played at a given round size
  results <- data_frame(game_index=numeric() , player_1 = numeric()  , all_spicy = logical() )
  
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
 c(round_size ,
 (sum(results$all_spicy)/dim(results)[1])*100 )
}

stopCluster(cl)

#put results into a dataframe
r_results$round_number <- simulation_results[1:max_rounds,1] 
r_results$all_spicy_chance <- simulation_results[1:max_rounds,2]
saveRDS(r_results ,"naive_simulation.rds")
simulation_plot <- ggplot(r_results) + aes(x = round_number , y = all_spicy_chance) + geom_point() + geom_smooth(col="red",method="lm")
saveRDS(simulation_plot,"simulation_plot.rds")
