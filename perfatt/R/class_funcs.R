# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


perfattr <- function(port_wt = data.frame(),
                     bench_wt = data.frame(),
                     port_ret = data.frame(),
                     bench_ret = data.frame(),
                     total_port = data.frame(),
                     total_bench = data.frame(),
                     ret_dates = vector)
{

  # input validation
  if ((nrow(port_wt) != nrow(bench_wt)) |
      (nrow(bench_wt) != nrow(port_ret)) |
      (nrow(port_ret) != nrow(bench_ret)) |
      (nrow(bench_ret) != length(total_port)) |
      (length(total_port) != length(total_bench)))
    stop('All data must have the same row count')

  if ((ncol(port_wt) != ncol(bench_wt)) |
      (ncol(bench_wt) != ncol(port_ret)) |
      (ncol(port_ret) != ncol(bench_ret)))
    stop('Returns data and weights data must have the same number of columns')

  # computations
  allocation <- (port_wt - bench_wt) * (bench_ret - total_bench)
  selection <- bench_wt * (port_ret - bench_ret)
  select_interact <- (port_wt - bench_wt) * (port_ret - bench_ret)
  total_alpha <- total_port - total_bench
  unexplained <- total_alpha - apply(allocation + selection + select_interact, 1, sum)
  calc_alpha <- apply(allocation + selection + select_interact, 1, sum) + unexplained

  me <- list(allocation = allocation,
             selection = selection,
             select_interact = select_interact,
             unexplained = unexplained,
             port_wt = port_wt,
             bench_wt = bench_wt,
             port_ret = port_ret,
             bench_ret = bench_ret,
             total_port = total_port,
             total_bench = total_bench,
             calc_alpha = calc_alpha,
             ret_dates = ret_dates)
  class(me) <- append(class(me), "perfattr")
  return(me)
}

attrSummary <- function(perfatt){
  if (!"perfattr" %in% class(perfatt)) stop('attrSummary takes a perfattr object')

  }
