# Define log modulus transformation for plotting (for negative (and positive) log axis)
log_modulus_trans <- function(base = exp(1)) {
  scales::trans_new(name = "log_modulus",
                    transform = function(x) sign(x) * log(abs(x) + 1, base = base),
                    inverse   = function(x) sign(x) * ( base^(abs(x)) - 1 ))
}
log_modulus_rev_trans <- function(base = exp(1)) {
  scales::trans_new(name = "log_modulus_rev",
                    transform = function(x) -sign(x) * log(abs(x) + 1, base = base),
                    inverse   = function(x) -sign(x) * ( base^(abs(x)) - 1 ))
}
scale_y_logModulus <- function(...) {
  major_breaks <- c(1) # or use c(1,3)
  scale_y_continuous(trans = "log_modulus",
                     labels=function(n) format(n, scientific=FALSE),
                     breaks = c((+1)*(10^(0:10) %*% t(major_breaks) ),
                                0,
                                (-1)*(10^(0:10) %*% t(major_breaks) )),
                     minor_breaks = c((+1)*(10^(-0:10) %*% t(c(1:9)) ),
                                      (-1)*(10^(-0:10) %*% t(c(1:9)) )),
                     ...)
}
