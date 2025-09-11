require(tidyverse)
require(deSolve)

#### Task 1 ####
simulate_seir <- function(init = NULL, params = NULL, time = NULL) {
  
  
  if (is.null(params)) {
    stop("undefined 'params'")
  }
  if (is.null(init)) {
    stop("undefined 'init'")
  }
  if (is.null(time)) {
    stop("undefined 'time'")
  }

  states <- data.frame(time = seq(time+1), 
                       S = 0, 
                       E = 0, 
                       I = 0, 
                       R = 0, 
                       N = 0, 
                       inc = 0)
  
  states[1, "S"] <- init$S
  states[1, "E"] <- init$E
  states[1, "I"] <- init$I
  states[1, "R"] <- init$R
  states[1, "N"] <- sum(unlist(init))
  
  mu <- params$mu
  sigma <- params$sigma
  gamma <- params$gamma
  beta <- params$beta

  for (t in seq(time)) {
    states[t+1, "S"] <- states[t, "S"] + mu*states[t, "N"] - beta*states[t, "S"]*states[t, "I"]/states[t, "N"] - mu*states[t, "S"]
    states[t+1, "E"] <- states[t, "E"] + beta*states[t, "S"]*states[t, "I"]/states[t, "N"] - sigma*states[t, "E"] - mu*states[t, "E"]
    states[t+1, "I"] <- states[t, "I"] + sigma*states[t, "E"] - gamma*states[t, "I"] - mu*states[t, "I"]
    states[t+1, "R"] <- states[t, "R"] + gamma*states[t, "I"] - mu*states [t, "R"]
    states[t+1, "N"] <- states[t+1, "S"] + states[t+1, "E"] + states[t+1, "I"] +states[t+1, "R"] 
    states[t+1, "inc" ] <- states[t,"E"]*sigma
    }

  diff_N <- unique(round(states$N/states[1,"N"],2))
  if(diff_N != 1) stop("Your population change is unexpectedly varying!")
  
  return(states)
  
}

task1_run_1 <- simulate_seir(init = list(S = 1000, E = 1, I = 1, R = 8998),
                        params = list(sigma = 1/14, gamma = 1/7, 
                                      mu = 1/(70*365), beta = 1.43),
                        time = 365*5)

task1_run_2 <- simulate_seir(init = list(S = 8000, E = 1, I = 1, R = 1998),
                        params = list(sigma = 1/14, gamma = 1/7, 
                                      mu = 1/(70*365), beta = 1.43),
                        time = 365*5)

#### Task 2 ####
simulate_seir_sto <- function(init = NULL, params = NULL, time = NULL) {
  
  
  if (is.null(params)) {
    stop("undefined 'params'")
  }
  if (is.null(init)) {
    stop("undefined 'init'")
  }
  if (is.null(time)) {
    stop("undefined 'time'")
  }
  
  states <- data.frame(time = seq(time+1), 
                       S = 0, 
                       E = 0, 
                       I = 0, 
                       R = 0, 
                       N = 0, 
                       inc = 0,
                       beta_t = params$beta) # new line
  
  states[1, "S"] <- init$S
  states[1, "E"] <- init$E
  states[1, "I"] <- init$I
  states[1, "R"] <- init$R
  states[1, "N"] <- sum(unlist(init))
  
  mu <- params$mu
  sigma <- params$sigma
  gamma <- params$gamma
  beta <- params$beta
  
  for (t in seq(time)) {
    beta_tmp <- rnorm(1, 1.43, 0.287)*runif(1, 0.9, 1.1) # new line
    states[t+1, "S"] <- states[t, "S"] + mu*states[t, "N"] - beta_tmp*states[t, "S"]*states[t, "I"]/states[t, "N"] - mu*states[t, "S"] # modified
    states[t+1, "E"] <- states[t, "E"] + beta_tmp*states[t, "S"]*states[t, "I"]/states[t, "N"] - sigma*states[t, "E"] - mu*states[t, "E"] # modified
    states[t+1, "I"] <- states[t, "I"] + sigma*states[t, "E"] - gamma*states[t, "I"] - mu*states[t, "I"]
    states[t+1, "R"] <- states[t, "R"] + gamma*states[t, "I"] - mu*states [t, "R"]
    states[t+1, "N"] <- states[t+1, "S"] + states[t+1, "E"] + states[t+1, "I"] +states[t+1, "R"] 
    states[t+1, "inc" ] <- states[t,"E"]*sigma
    states[t+1, "beta_t" ] <- beta_tmp
  }
  
  diff_N <- unique(round(states$N/states[1,"N"],2))
  if(diff_N != 1) stop("Your population change is unexpectedly varying!")
  
  return(states)
  
}

task2_run_1 <- simulate_seir_sto(init = list(S = 1000, E = 1, I = 1, R = 8998),
                                 params = list(sigma = 1/14, gamma = 1/7, 
                                               mu = 1/(70*365), beta = 1.43),
                                 time = 365*5)

task2_run_1 %>% 
  mutate(run = "task2_run_1") %>% 
  bind_rows(task1_run_1 %>% mutate(run = "task1_run_1")) %>% 
  pivot_longer(cols = c("S", "E", "I", "R", "inc", "beta_t")) %>%
  mutate(name = factor(name, levels = c("S", "E", "I", "R", "inc", "beta_t"))) %>% 
  ggplot(., aes(x = time, y = value, group = run, color = run)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

#### Task 3 ####
simulate_seir_con <- function(init = NULL, params = NULL, time = NULL) {
  
  if (is.null(params)) {
    stop("undefined 'params'")
  }
  if (is.null(init)) {
    stop("undefined 'init'")
  }
  if (is.null(time)) {
    stop("undefined 'time'")
  }
  
  # new line
  if (!requireNamespace("deSolve", quietly = TRUE))
    stop("Package 'deSolve' is required. Please install it.")

  # we need to save the trajectories differently for the diffential equation 
  # solver.
  y0 <- c(S = init$S, E = init$E, I = init$I, R = init$R)
  # states <- data.frame(time = seq(time+1), 
  #                      S = 0, 
  #                      E = 0, 
  #                      I = 0, 
  #                      R = 0, 
  #                      N = 0, 
  #                      inc = 0)
  # 
  # states[1, "S"] <- init$S
  # states[1, "E"] <- init$E
  # states[1, "I"] <- init$I
  # states[1, "R"] <- init$R
  # states[1, "N"] <- sum(unlist(init))
  
  mu <- params$mu
  sigma <- params$sigma
  gamma <- params$gamma
  beta <- params$beta
  
  # how compartments are linked together also need to be defined differently, 
  # as a function. Instead of going through a loop, we now need to save track 
  # time separately. The old ones are commented out so you can see the difference.
  
  times <- if (length(time) == 1L) seq(0, time, by = 1) else as.numeric(time)
  seir_ode <- function(t, y, p) {
    S <- y["S"]; E <- y["E"]; I <- y["I"]; R <- y["R"]
    N <- S + E + I + R
    dS <- mu * N - mu * S - beta * S * I / N
    dE <- beta * S * I / N - sigma * E - mu * E
    dI <- sigma * E - gamma * I - mu * I
    dR <- gamma * I - mu * R
    list(c(dS, dE, dI, dR))
  }
  
  # for (t in seq(time)) {
  #   states[t+1, "S"] <- states[t, "S"] + mu*states[t, "N"] - beta*states[t, "S"]*states[t, "I"]/states[t, "N"] - mu*states[t, "S"]
  #   states[t+1, "E"] <- states[t, "E"] + beta*states[t, "S"]*states[t, "I"]/states[t, "N"] - sigma*states[t, "E"] - mu*states[t, "E"]
  #   states[t+1, "I"] <- states[t, "I"] + sigma*states[t, "E"] - gamma*states[t, "I"] - mu*states[t, "I"]
  #   states[t+1, "R"] <- states[t, "R"] + gamma*states[t, "I"] - mu*states [t, "R"]
  #   states[t+1, "N"] <- states[t+1, "S"] + states[t+1, "E"] + states[t+1, "I"] +states[t+1, "R"] 
  #   states[t+1, "inc" ] <- states[t,"E"]*sigma
  # }
  
  # actually running the model
  out <- deSolve::ode(y = y0, times = times, func = seir_ode, parms = NULL)
  out <- as.data.frame(out)
  out$N <- out$S + out$E + out$I + out$R
  out$inc <- c(0, sigma*head(out$E, -1))
  
  diff_N <- unique(round(out$N/sum(unlist(init)),2))
  if(diff_N != 1) stop("Your population change is unexpectedly varying!") # modified
  
  return(out) # modified
  
}

task3_run_1 <- simulate_seir_con(init = list(S = 1000, E = 1, I = 1, R = 8998),
                                 params = list(sigma = 1/14, gamma = 1/7, 
                                               mu = 1/(70*365), beta = 1.43),
                                 time = 365*5)

task3_run_1 %>% 
  mutate(run = "task3_run_1") %>% 
  bind_rows(task1_run_1 %>% mutate(run = "task1_run_1")) %>% 
  pivot_longer(cols = c("S", "E", "I", "R", "inc")) %>%
  mutate(name = factor(name, levels = c("S", "E", "I", "R", "inc"))) %>% 
  ggplot(., aes(x = time, y = value, group = run, color = run)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

#### Task 4 ####
simulate_seir_gillespie <- function(init = NULL, params = NULL, time = NULL) {
  
  
  if (is.null(params)) {
    stop("undefined 'params'")
  }
  if (is.null(init)) {
    stop("undefined 'init'")
  }
  if (is.null(time)) {
    stop("undefined 'time'")
  }
  
  states <- data.frame(time = seq(time+1), 
                       S = 0, 
                       E = 0, 
                       I = 0, 
                       R = 0, 
                       N = 0, 
                       inc = 0)
  
  states[1, "S"] <- init$S
  states[1, "E"] <- init$E
  states[1, "I"] <- init$I
  states[1, "R"] <- init$R
  states[1, "N"] <- sum(unlist(init))
  
  mu <- params$mu
  sigma <- params$sigma
  gamma <- params$gamma
  beta <- params$beta
  
  # for (t in seq(time)) {
  #   states[t+1, "S"] <- states[t, "S"] + mu*states[t, "N"] - beta*states[t, "S"]*states[t, "I"]/states[t, "N"] - mu*states[t, "S"]
  #   states[t+1, "E"] <- states[t, "E"] + beta*states[t, "S"]*states[t, "I"]/states[t, "N"] - sigma*states[t, "E"] - mu*states[t, "E"]
  #   states[t+1, "I"] <- states[t, "I"] + sigma*states[t, "E"] - gamma*states[t, "I"] - mu*states[t, "I"]
  #   states[t+1, "R"] <- states[t, "R"] + gamma*states[t, "I"] - mu*states [t, "R"]
  #   states[t+1, "N"] <- states[t+1, "S"] + states[t+1, "E"] + states[t+1, "I"] +states[t+1, "R"] 
  #   states[t+1, "inc" ] <- states[t,"E"]*sigma
  # }

  t <- 0
  S <- states[1, "S"]; E <- states[1, "E"]; I <- states[1, "I"]; R <- states[1, "R"]
  inc_bucket <- 0
  next_row <- 2
  
  while (t < time) {
    
    N <- S + E + I + R
    a_inf <- beta * S * I / N
    a_prog <- sigma * E                               
    a_rec  <- gamma * I
    a_rep  <- mu * N
    
    a0 <- a_inf + a_prog + a_rec + a_rep
    # define the time that the next event is expected to happen
    tau <- rexp(1, rate = a0)
    t <- t + tau
    
    # when no events happened, S, E, I, and R should be like the last time point
    # before.
    while (next_row <= nrow(states) && (next_row - 1) <= t) {
      states[next_row, c("S","E","I","R")] <- c(S, E, I, R)
      states[next_row, "N"]   <- N
      states[next_row, "inc"] <- inc_bucket
      inc_bucket <- 0
      next_row <- next_row + 1
    }
    
    r <- runif(1) * a0
    if (r <= a_inf) {
      # Infection S->E
      if (S > 0) { S <- S - 1; E <- E + 1 }
    } else if (r <= a_inf + a_prog) {
      # Progression E->I  (this is what we count as incidence)
      if (E > 0) { E <- E - 1; I <- I + 1; inc_bucket <- inc_bucket + 1 }
    } else if (r <= a_inf + a_prog + a_rec) {
      # Recovery I->R
      if (I > 0) { I <- I - 1; R <- R + 1 }
    } else {
      # Replacement: pick a compartment to die (∝ size), then birth to S
      if (N > 0) {
        u <- runif(1) * N
        if (u <= S) { if (S > 0) S <- S - 1
        } else if (u <= S + E) { if (E > 0) E <- E - 1
        } else if (u <= S + E + I) { if (I > 0) I <- I - 1
        } else { if (R > 0) R <- R - 1 }
        S <- S + 1  # birth into S
      }
    }
  }
  
  # If we ended early, carry forward final state and flush remaining incidence once
  while (next_row <= nrow(states)) {
    states[next_row, c("S","E","I","R")] <- c(S, E, I, R)
    states[next_row, "N"]   <- S + E + I + R
    states[next_row, "inc"] <- if (next_row == which.max(states$time)) 0 else 0
    next_row <- next_row + 1
  }
  
  
  diff_N <- unique(round(states$N/states[1,"N"],2))
  if(diff_N != 1) stop("Your population change is unexpectedly varying!")
  
  return(states)
  
}

task4_run_1 <- simulate_seir_gillespie(init = list(S = 1000, E = 1, I = 1, R = 8998),
                             params = list(sigma = 1/14, gamma = 1/7, 
                                           mu = 1/(70*365), beta = 1.43),
                             time = 365*5)

task4_run_1 %>% 
  mutate(run = "task4_run_1") %>% 
  bind_rows(task1_run_1 %>% mutate(run = "task1_run_1")) %>% 
  pivot_longer(cols = c("S", "E", "I", "R", "inc")) %>%
  mutate(name = factor(name, levels = c("S", "E", "I", "R", "inc"))) %>% 
  ggplot(., aes(x = time, y = value, group = run, color = run)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

task4_run_2 <- list()

for(i in 1:100){
  task4_run_2[[i]] <- simulate_seir_gillespie(init = list(S = 1000, E = 1, I = 1, R = 8998),
                                                params = list(sigma = 1/14, gamma = 1/7, 
                                                              mu = 1/(70*365), beta = 1.43),
                                                time = 365*5)
  
}

task4_run_2 %>% 
  bind_rows(.id = "round") %>% 
  mutate(run = "task4_run_2") %>% 
  bind_rows(task1_run_1 %>% mutate(run = "task1_run_1")) %>% 
  pivot_longer(cols = c("S", "E", "I", "R", "inc")) %>%
  mutate(name = factor(name, levels = c("S", "E", "I", "R", "inc"))) %>% 
  ggplot(., aes(x = time, y = value, group = interaction(run, round), color = run)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  theme_minimal()
