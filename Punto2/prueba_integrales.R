h <- function(x) (cos(50*x)+sin(20*x))^2
p <- function(x) (1/(pi*(1+x^2)))

MonteCarloInt <- function(func,lim1,lim2){
  g <- curve(func, lim1, lim2, n = 10000)
  r <- seq(length=10000, from=lim1, to=lim2)
  f <- func(r)
  max_y = max(f)
  min_y= min(f)
  rand_x <- runif(10000, 0, 1) * (lim2-lim1) + lim1
  rand_y <- runif(10000, 0, 1) * (max_y-min_y) + min_y
  plot(rand_x, rand_y)
  delta <- func(rand_x) - rand_y
  i = 0
  below <- (NULL)
  for(n in delta){i = i+1
                  if(n>0.0){below <- c(below,i)}}
  plot(rand_x[below], rand_y[below])
  interval_integral = (max_y-min_y) * (lim2 - lim1)
  integral  = interval_integral * (length(below)/(1.0*length(rand_y)))
  integral
}

MonteCarloInt(p,2.0,30.0)


