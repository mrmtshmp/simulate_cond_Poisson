#'Simulate data from Poisson distribution and calculate coefficients
#'
#'@import dplyr
#'@import plyr
#'@import tibble
#'
#'@omportFrom lme4 glmer
#'@importFrom bindata rmvbin
#'
#'@param vect.margprob_GS_1 <object; input>
#'@param vect.margprob_GS_0 <object; input>
#'@param mat.cov_GS_1  <object; input>
#'@param mat.cov_GS_0  <object; input>
#'@param n.sim  <numeric; proccessing>
#'@param sim.N_1  <numeric; proccessing>
#'@param sim.N_0  <numeric; proccessing>
#'
#'@export
#'


# mf for Simulation -------------------------------------------------------

mf.sim.glmerPoisson <- function(
  vect.margprob_GS_1,
  vect.margprob_GS_0,
  mat.cov_GS_1,
  mat.cov_GS_0,
  n.sim,
  sim.N_1,
  sim.N_0
  ){
  
  .data <- data
  
  df.pmt <- data.frame(
    i=seq(1:n.sim),
    j=seq(1:n.sim)
  )
  
  output <- ddply(
    df.pmt,
    .(i),
    function(itt){
      i.numb <- unique(itt$i)
      D <- .data
      df.sim_GS_1 <- rmvbin(
        sim.N_1, 
        margprob = vect.margprob_GS_1[c(2,3)], 
        bincorr = mat.cov_GS_1
      ) %>%
        data.frame() %>%
        mutate(GS=1)
      
      df.sim_GS_0 <- rmvbin(
        sim.N_0, 
        margprob = vect.margprob_GS_0[c(2,3)], 
        bincorr = mat.cov_GS_0
      ) %>%
        data.frame() %>%
        mutate(GS=0)
      
      df.sim <- df.sim_GS_0 %>%
        rbind(df.sim_GS_1)
      
      colnames(df.sim) <- cov.names[c(2,3,1)]
      
      df.sim$ID <- seq(1:nrow(df.sim))
      
      df.sim.False <- df.sim %>%
        mutate(
          TotalFalse_Test_1 = abs(GS-Test_1),
          TotalFalse_Test_2 = abs(GS-Test_2)
        ) %>%
        dplyr::select(
          ID,
          starts_with(
            'TotalFalse'
          )
        ) %>%
        gather(var, val, -ID)
      print(
        c(
          sum(df.sim.False[df.sim.False$var=='TotalFalse_Test_1','val']), 
          sum(df.sim.False[df.sim.False$var=='TotalFalse_Test_2','val'])
        )
      )
      m1a <- glmer(
        val ~ 1 + var + (1 | ID), data = df.sim.False, family = poisson(link = "log")
      ) %>% 
        summary() %>%
        coef() %>%
        data.frame()%>%
        rownames_to_column('terms')
      
      return(m1a)
    }
  )
  return(output)
}