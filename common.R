library(methods)
set.seed(2899)

knitr::opts_chunk$set(fig.align = "center", fig.retina = 3,
                      fig.width = 5.5, fig.height = (5.5 * 0.618),
                      out.width = "90%", collapse = TRUE, 
                      cache = TRUE, comment = "#|")


options(digits = 3, 
        width = 300)

options(
  rlang_trace_top_env = rlang::current_env(),
  rlang__backtrace_on_error = "none")


