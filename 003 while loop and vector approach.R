
# libraries ----

library(tidyverse)



# while loop ----

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


# vector approach ----

# shiny app inputs input_xyz
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

df_simple %>% summarise(to = sum(to), pts = sum(pts))


# would need to know if team got rebound; continue possession
df_simple %>% filter(!to & !make)

# vector function ----

# let's make a function for the simple approach

# shiny app inputs
input_n_poss <- 60 # number of possessions
input_pr_to <- 0.25 # prob turn over
input_pr_take2 <- 0.6 # 60% 2 pt; 40% 3 pt
input_pr_make2 <- 0.40
input_pr_make3 <- 0.20
input_pr_reb <- 0.37

# function inputs (for testing and developing)
# in the final app, this will be in the function as below
.n_poss = input_n_poss
.pr_to = input_pr_to
.pr_take2 = input_pr_take2
.pr_make2 = input_pr_make2
.pr_make3 = input_pr_make3
.pr_reb = input_pr_reb

sim_simple <- 
    
    function(
        
        .poss = 1:input_n_poss,
        .pr_to = input_pr_to,
        .pr_take2 = input_pr_take2,
        .pr_make2 = input_pr_make2,
        .pr_make3 = input_pr_make3,
        .pr_reb = input_pr_reb
        
    ){
       
        .n_poss <- length(.poss)
        
        .df <- 
            
            # tibble(possession = 1:.n_poss) %>% 
            tibble(possession = .poss) %>% 
            
            # turnover
            mutate(rand_to = runif(n = .n_poss)) %>% 
            mutate(to = rand_to < .pr_to) %>% 
            
            # shot (2 vs 3)
            mutate(rand_take = runif(n = .n_poss)) %>% 
            mutate(take = ifelse(rand_take < .pr_take2, 2, 3)) %>% 
            
            # make
            mutate(rand_make = runif(n = .n_poss)) %>% 
            mutate(make = ifelse(rand_take == 2, rand_make < .pr_make2, rand_make < .pr_make3)) %>% 
            
            # didn't turn the ball over AND made a shot
            # i.e. scored (pts)
            mutate(score = !to & make) %>% 
            mutate(pts = score * take) %>% 
            
            # didn't turn ball over but missed shot
            mutate(rand_reb = runif(n = .n_poss)) %>% 
            mutate(reb = rand_reb < .pr_reb)
        
        return(.df)
        
    }



df1 <- sim_simple()

df1
df1 %>% summarise(to = sum(to), pts = sum(pts))

# continue poss ----

# would need to know if team got rebound; continue possession
df1 %>% filter(!to & !make & reb)

# need to extend possession if reb
df1.2 <- sim_simple(.poss = df1 %>% filter(!to & !make & reb) %>% pull(possession))

# repeat until no longer have ball ...
df1.2 %>% filter(!to & !make & reb)
df1.3 <- sim_simple(.poss = df1.2 %>% filter(!to & !make & reb) %>% pull(possession))
df1.3 %>% filter(!to & !make & reb)

# combine
df1 <- bind_rows(df1, df1.2, df1.3)

# Note: will not know how many times required to do this!   Make generic...

# some outputs

df1 %>% 
    filter(!to) %>% 
    summarise(
        .by = take,
        n = n(),
        make = sum(make),
        percent = make / n
    )

df1 %>% 
    
    summarise(
        to = sum(to),
        pts = sum(pts),
        reb = sum(reb)
    )
    


# NOTE: this is for one team for one game!  
# Will need for both teams for ex: 1000 games!













