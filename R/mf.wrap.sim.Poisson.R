#'Power analysis of conditional Poisson or mixed-effect Poisson
#'
#'@import dplyr
#'@import plyr
#'@import tibble
#'
#'@importFrom gnm gnm
#'
#'@param data <object; input> A data.frame-class object .
#'@param dir.output <character string; output> Directory to output results.
#'@param fn.output.pdf <character string; output> Filename to output results.
#'@param sim.N <numeric; processing> N of observation for simulation.
#'@param n.sim <numeric; processing> Itteration number.
#'@param cov.name <character vector; input>
#'@param method <character; proccessing; preset="cPoisson"> cPoisson or glmerPoisson
#'
#'@export
#'

mf.wrap.sim.Poisson <- function(

  data,
  dir.sub     = "./sub",
  dir.output = "../Output/190803",
  fn.output.pdf = "Result",
  sim.N      = 640,
  n.sim      = 200,
  cov.names  = c('GS','Test_1','Test_2'),
  method     = "cPoisson",
  prevalance = seq(0.3, 0.4, 0.02)
  ){

  if(method=="cPoisson"){
    formula.test <- mf.sim.condPoisson
    }else formula.test <- mf.sim.glmerPoisson



  data_GS_1 <- data %>% filter(get(cov.names[1])==1)
  data_GS_0 <- data %>% filter(get(cov.names[1])==0)

  data_GS_1[
    nrow(data_GS_1) + 1,
    cov.names
    ] <- c(1,0.5,0.5)

  data_GS_0[
    nrow(data_GS_0) + 1,
    cov.names
    ] <- c(1,0.5,0.5)


  # calculation of cov matrix -----------------------------------------------

  mat.cov_GS_1 <-
    data_GS_1[,cov.names[c(2,3)]] %>%
    cor()

  mat.cov_GS_0 <-
    data_GS_0[,cov.names[c(2,3)]] %>%
    cor()

  vect.margprob_GS_1 <-
    c(
      sum(data_GS_1[,cov.names[1]]),
      sum(data_GS_1[,cov.names[2]]),
      sum(data_GS_1[,cov.names[3]])
      )/ (nrow(data_GS_1))

  vect.margprob_GS_0 <-
    c(
      sum(data_GS_0[,cov.names[1]]),
      sum(data_GS_0[,cov.names[2]]),
      sum(data_GS_0[,cov.names[3]])
      )/ (nrow(data_GS_0))

  pdf(sprintf("%s/%s.pdf", dir.output, fn.output.pdf))
  for(i in 1:length(prevalance)){

    prev <- prevalance[i]

    test <-  formula.test(
      vect.margprob_GS_1,
      vect.margprob_GS_0,
      mat.cov_GS_1,
      mat.cov_GS_0,
      n.sim,
      sim.N_1 = floor(sim.N*prev),
      sim.N_0 = floor(sim.N*(1-prev))
      )

    p.val_beta1 <-
      test %>% filter(terms=='varTotalFalse_Test_2')

    plot(
      ecdf(p.val_beta1[,'Pr...z..']),
      main=sprintf(
        'GS:1, n=%s; GS:0, n=%s',
        floor(sim.N*prev),
        floor(sim.N*(1-prev))
      )
    )
    abline(
      h=0.8,
      v=c(0.10,0.05,0.025)
    )
    }
  dev.off()
  }

# Endrant -----------------------------------------------------------------


