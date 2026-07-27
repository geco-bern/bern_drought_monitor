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


general_plot_theme <- list(
  theme_classic(base_size = 13),
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      colour = "grey30",
      margin = margin(b = 10)
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    )),
  theme(
    strip.background = element_blank(),
    panel.spacing = grid::unit(1, "lines"),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    plot.caption = element_text(
      colour = "grey40",
      hjust = 0
    )),
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.justification = "left"
  )
)
