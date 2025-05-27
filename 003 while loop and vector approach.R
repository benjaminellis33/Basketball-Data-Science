
library(tidyverse)

# caution - while loops can explode!

possession <- TRUE
clock <- 0
eop <- 0.2 # end of possession


while(possession == TRUE){ # run until team no longer has possession
    
    # simulate
    random_number <- runif(n = 1)
    
    
    if(random_number < eop) possession <- FALSE
    
    # track clock; possession ends after some # of steps
    clock <- clock + 1
    if(clock > 20) possession <- FALSE
  
    print(paste0(clock, " ", possession))
    
}



# Q: is there a better approach?  Vectorized approach?  Matrix approach?


input_n_poss <- 60 # number of possessions
input_pr_to <- 0.25 # prob turn over
input_pr_take <- 0.6 # 60% 2 pt; 40% 3 pt
input_pr_make <- 0.30


df_simple <- 
    
    tibble(possession = 1:input_n_poss) %>% 
    
    # turnover
    mutate(rand_to = runif(n = input_n_poss)) %>% 
    mutate(to = rand_to < input_pr_to) %>% 
    
    # shot (2 vs 3)
    mutate(rand_take = runif(n = input_n_poss)) %>% 
    mutate(take = ifelse(rand_take < input_pr_take, 2, 3)) %>% 
    
    # make
    mutate(rand_make = runif(n = input_n_poss)) %>% 
    mutate(make = rand_make < input_pr_make) %>% 
    
    # didn't turn the ball over AND made a shot
    # i.e. scored (pts)
    mutate(score = !to & make) %>% 
    mutate(pts = score * take)





df_simple




df_simple %>% 
    
    summarise(
        
        to = sum(to),
        pts = sum(pts)
        
    )
















