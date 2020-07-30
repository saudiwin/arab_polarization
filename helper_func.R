# Helper functions

# generate time series data using environments as counters

gen_ts_data <- function(t,adj_in,adj_out,this_beta,alpha_int,sigma,init_sides,country) {
  current_val <- new.env()
  current_val$t1 <- 0
  current_val$t2 <- 0
  
  out_vec2 <- lapply(1:t,function(t_1) {
    
    if(t_1<(t/2)) {
      this_beta <- rep(0,sides)
    } else {
      this_beta <- this_beta
    }
    if(t_1==1) {
      t_11 <- alpha_int[1]
      t_12 <- alpha_int[2]
      current_val$t1 <- t_11
      current_val$t2 <- t_12
      return(data_frame(t_11,t_12))
    } else {
      t_11 <- alpha_int[1] + adj_in[1]*current_val$t1 +  adj_out[1]*current_val$t2 + this_beta[1] +
        rnorm(n=1,sd=sigma[1])
      t_12 <- alpha_int[2] + adj_in[2]*current_val$t2 + adj_out[2]*current_val$t1 + this_beta[2] + rnorm(n=1,sd=sigma[2])
    }
    current_val$t1 <- t_11
    current_val$t2 <- t_12
    return(data_frame(t_11,t_12))
  })  %>% bind_rows
  return(out_vec2)
}

gen_ts_data_mvn <- function(t,adj_in1,adj_out1,
                            adj_in2,adj_out2,
                            this_beta1,
                            this_beta2,
                            alpha_int1,
                            alpha_int2,
                            sigma1,
                            sigma2,
                            init_sides1,
                            init_sides2,country) {
  current_val <- new.env()
  current_val$t1d1 <- 0
  current_val$t2d1 <- 0
  current_val$t1d2 <- 0
  current_val$t2d2 <- 0
  
  out_vec2 <- lapply(1:t,function(t_1) {
    
    if(t_1<(t/2)) {
      this_beta1 <- rep(0,sides)
      this_beta2 <- rep(0,sides)
    } else {
      this_beta1 <- this_beta1
      this_beta2 <- this_beta2
    }
    if(t_1==1) {
      t_11d1 <- init_sides1[1]
      t_12d1 <- init_sides1[2]
      t_11d2 <- init_sides2[1]
      t_12d2 <- init_sides2[2]
      current_val$t1d1 <- t_11d1
      current_val$t2d1 <- t_12d1
      current_val$t1d2 <- t_11d2
      current_val$t2d2 <- t_12d2
      return(data_frame(t_1d1,t_2d1,
                        t_1d2,t_2d2))
    } else {
      t_11d1 <- alpha_int1[1] + adj_in1[1]*current_val$t1d1 +  adj_out1[1]*current_val$t2d1 + this_beta1[1]
      t_12d1 <- alpha_int1[2] + adj_in1[2]*current_val$t2d1 + adj_out1[2]*current_val$t1d1 + this_beta1[2]
      t_11d2 <- alpha_int2[1] + adj_in2[1]*current_val$t1d2 +  adj_out2[1]*current_val$t2d2 + this_beta2[1]
      t_12d2 <- alpha_int2[2] + adj_in2[2]*current_val$t2d2 + adj_out2[2]*current_val$t1d2 + this_beta2[2]
      # now generate errors via mvrnorm across dimensions

      t_1 <- MASS::mvrnorm(n=1,mu=c(t_11d1,
                                   t_11d2),
                          sigma=sigma1)
      
      t_2 <- MASS::mvrnorm(n=1,mu=c(t_11d1,
                                   t_11d2),
                          sigma=sigma1)
      
      
    }
    current_val$t1d1 <- t_1[1]
    current_val$t2d1 <- t_2[1]
    current_val$t1d2 <- t_1[2]
    current_val$t2d2 <- t_2[2]
    return(data_frame(t_1d1=t_1[1],t_2d1=t_2[1],
                      t_1d2=t_1[2],t_2d2=t_2[2]))
  })  %>% bind_rows
  return(out_vec2)
}


# generate IRFs

irf <- function(time=1,shock=1,
                adj_in=NULL,
                adj_out=NULL,
                y_1=0,
                x_1=0,
                xshock=TRUE,
                total_t=10,
                old_output=NULL) {
  
  # set up the exogenous shock
  # unless the shock comes from an exogenous covariate beta_x
  if(time==1) {
    if(xshock) {
      x_1 <- shock
    } else {
      y_1 <- shock
    }
    
    output <- tibble(y_shock= y_1,
                         x_shock=x_1,
                         time=time,
                         iter=1:nrow(adj_in))
     
  } else {
    
  }
  
  print(paste0('Now processing time point ',time))
  
  # Calculate current values of y and x given posterior uncertainty
  
  output <- tibble(y_shock= adj_in[,1]*y_1 + adj_out[,1]*x_1,
                   x_shock=adj_in[,2]*x_1 + adj_out[,2]*y_1,
                   time=time,
                   iter=1:nrow(adj_in))
  
  
  
  if(!is.null(old_output)) {
    new_output <- bind_rows(old_output,output)
  } else {
    new_output <- output
  }
  
  # run function recursively until time limit is reached
  
  if(time<total_t) {
    irf(time=time+1,
        shock=shock,
        adj_in=adj_in,
        adj_out=adj_out,
        y_1=output$y_shock,
        x_1=output$x_shock,
        total_t=total_t,
        old_output=new_output)
  } else {
    return(new_output)  
  }
  
}

irf_2shock <- function(time=1,shock=cbind(1,1),
                adj_in=NULL,
                adj_out=NULL,
                y_1=0,
                x_1=0,
                xshock=TRUE,
                total_t=10,
                old_output=NULL) {
  
  # set up the exogenous shock
  # unless the shock comes from an exogenous covariate beta_x
  if(time==1) {
      x_1 <- shock[,2]
      y_1 <- shock[,1]
      
      output <- tibble(y_shock= y_1,
                           x_shock=x_1,
                           time=time,
                           iter=1:nrow(adj_in))
  } else {
    
  }
  
  print(paste0('Now processing time point ',time))
  
  # Calculate current values of y and x given posterior uncertainty
  
  output <- tibble(y_shock= adj_in[,1]*y_1 + adj_out[,1]*x_1,
                   x_shock=adj_in[,2]*x_1 + adj_out[,2]*y_1,
                   time=time,
                   iter=1:nrow(adj_in))
  

  
  
  if(!is.null(old_output)) {
    new_output <- bind_rows(old_output,output)
  } else {
    new_output <- output
  }
  
  # run function recursively until time limit is reached
  
  if(time<total_t) {
    irf(time=time+1,
        shock=shock,
        adj_in=adj_in,
        adj_out=adj_out,
        y_1=output$y_shock,
        x_1=output$x_shock,
        total_t=total_t,
        old_output=new_output)
  } else {
    return(new_output)  
  }
  
}

hdr <- function(datap) {
  data_frame(y=mean(datap),
             ymin=quantile(datap,0.1),
             ymax=quantile(datap,0.9))
}

#' Helper function for sampling from ordinal cutpoints
.sample_cut <- function(pr_vote=NULL,cutpoints=NULL,n_outcomes=NULL) {
  
  # Given a raw ideal position of a bill-legislator combination, select the most likely outcome from all ordinal categories
  
  cuts <- sapply(cutpoints,function(y) {
    pr_vote - y
  })
  
  
  # Now we pick votes as a function of the number of categories
  # This code should work for any number of categories
  
  pr_bottom <- 1 - plogis(cuts[1])
  
  mid_prs <- sapply(1:(length(cuts)-1), function(c) {
    plogis(cuts[c]) - plogis(cuts[c+1])
  })
  
  pr_top <- plogis(cuts[length(cuts)])
  
  return(as.integer(sample(1:(length(cuts)+1),size=1,prob=c(pr_bottom,mid_prs,pr_top))))
  
}