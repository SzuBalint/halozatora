library(data.table)

ratings <- fread("C:/Users/Ildiko/Documents/Python/Network project/GyulaDora.csv",
              sep=';', encoding='UTF-8')

rest_weight <- fread("C:/Users/Ildiko/Documents/Python/Network project/restaurant_network_name.csv",
                 sep=';', encoding='UTF-8')

getRec <- function(user_rating, weights){

  user_rating <- na.omit(user_rating)
  
  avg_rat <- mean(user_rating$RATING) - 1
  print(avg_rat)
  
  weights <- weights[c(weights$First) %in% c(user_rating$NAME)]
  
  user_rat_weights <- merge(weights, user_rating, by.x = c('First'), by.y = c('NAME'))
  
  user_rat_weights$RATING <- user_rat_weights$RATING - avg_rat
  
  predicted_rat <- user_rat_weights$RATING * user_rat_weights$Weight
  
  predicted_rest <- cbind(user_rat_weights, predicted_rat)
  
  predicted_rest_sum <- predicted_rest[, pred_score:=sum(predicted_rat), by=Second][order(-pred_score)] 

  predicted_rest_sum <- unique(predicted_rest_sum, by='Second')
  
  return(predicted_rest_sum)
}

enyem <- getRec(ratings, rest_weight)
