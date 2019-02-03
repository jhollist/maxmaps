#' Capital quiz
#'
#' This function gives a quiz for each of the capitals based on region or regions
#' @param regions a list of brms regions to use for quiz.  Included in
#'                `states_caps`` data frame.
#' @param type Which type of test to take, can be "mc" of "fill".  Only "fill"
#'             is currently implemented.
#' @import dplyr USAboundaries ggplot2
#' @export
captial_quiz <- function(regions = c("all","northeast","southeast", "midwest",
                                     "southwest","west"), type = c("fill", "mc")
                         ){
  regions <- match.arg(regions)
  type <- match.arg(type)
  #browser()
  quiz_data <- states_caps %>%
    filter(brms_regions %in% regions) %>%
    arrange(sample(1:nrow(.)))
  score <- vector("numeric", nrow(quiz_data))
  user_answers <- vector("character", nrow(quiz_data))
  for(i in seq_along(quiz_data[,1])){
    region_states <- quiz_data %>%
      filter(brms_regions == quiz_data[i,4]) %>%
      pull(state)
    region <- us_boundaries(resolution = "high") %>%
      filter(name %in% region_states)
    state <- us_boundaries(resolution = "high") %>%
      filter(name %in% quiz_data[i,1])
    map <- ggplot(region) +
      geom_sf() +
      geom_sf(data = state, fill = "black")

    if(type == "fill"){
      print(map)
      user_answer <- readline("What is the state capital of the state shown in black? ")
      user_answers[i] <- user_answer
      if(user_answer == quiz_data[i,3]){
        score[i] <- 1
        message(praise::praise(template = "Correct!, you are ${adjective}"))
      } else {
        score[i] <- 0
        message("Sorry, not quite right")
        message(paste0("You typed ", user_answer, ' and the correct answer is ',
                      quiz_data[i,3], '.'))
        message("Can you see the difference?")
      }
    } else if(type == "mc"){}
    #continue <- ""
    #while(continue == ""){
    #  continue <- tolower(readline('Type "y" to continue: '))
    #y  if(continue != "y"){return()}
    #}
  }
  message("You are done!")
  message(paste("You got", sum(score), "out of", length(score), "correct."))
  message(paste0("That's ", round(100*(sum(score)/length(score)), 1), "%."))
  message("Here are the ones you didn't get this time.")
  wrong <- data.frame(your_answers = user_answers[!score],
             correct_answers = quiz_data[!score,3])
  wrong
}
