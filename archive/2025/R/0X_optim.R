?optim

?nls

?lm

a <- seq(from = 5,
         to = 10,
         by = .1)
b <- exp(x = a) * rnorm(n = length(x = a),
                        m = 8,
                        sd = 2)

plot(x = a,
     y = b)

dta <- data.frame(a, b)

fun <- function(x, a, b) {
  
  y <-  a^x + b
  y
}

opt_fun <- function(x, obs, ...) {
  
  sim <- fun(x = a,
             a = x[1],
             b = x[2])
  
  mse <- mean((sim - obs)^2,
             na.rm = TRUE)
  mse
}

opt_fun(x = c(1, 1),
        obs = b)


fit <- optim(par = c(1, 1),
             fn = opt_fun,
             obs = dta$b)

fit_nls <- nls(formula = b ~ par_a^a + par_b,
               data = dta,
               start = c(par_a = 3, 
                         par_b = 5000))
fit_lm <- lm(formula = log(x = b) ~ a,
             data = dta)

dta$c <- fun(x = a,
             a = fit$par[1],
             b = fit$par[2])

dta$d <- predict(object = fit_nls,
                 newdata = dta$a)

dta$e <- exp(x = predict(object = fit_lm,
                         newdata = data.frame(a = dta$a)))

plot(x = dta$a,
     y = dta$b)

lines(x = dta$a,
      y = dta$c, 
      col = "red")

lines(x = dta$a,
      y = dta$d, 
      col = "blue",
      lty = 2)

lines(x = dta$a,
      y = dta$d, 
      col = "green",
      lty = 3)
