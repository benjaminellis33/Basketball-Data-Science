


# fun with functions


input_percent <- 0.3 # this notation will come in handy as we move to creating a shiny app
percent <- 0.3

make_2pt_shot <- function(.percent){
    
    print(paste0("percent = ", percent))
    print(paste0(".percent = ", .percent))
    
    shot <- runif(1)
    
    score <- ifelse(shot < .percent, 2, 0)
    
    score
    
}

make_2pt_shot(.percent = input_percent)
make_2pt_shot(.percent = 0.5)


# global environment
percent <- 0.3

make_2pt_shot <- function(percent){ # percent here is in the function environment

    # function (local) environment
    print(paste0("percent = ", percent))
    
    shot <- runif(1)
    
    score <- ifelse(shot < percent, 2, 0)
    
    score
    
}

# confusion alert; sometimes you may get errors!
make_2pt_shot(percent)
make_2pt_shot(percent = 0.01)
make_2pt_shot(percent = percent)

percent <- 0.1
make_2pt_shot(percent = percent)
