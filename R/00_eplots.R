library(data.table)
library(echarts4r)

# generate nice eplot
# x must behave nicely as a factor... recommend to sort beforehands
# y must be a numeric
eplot_bar_groupby <- function(x, y, groupby, stack=T, x_lab=NULL, y_lab=NULL, decimals=3, magic=T) {
  stopifnot(!"x__" %in% c(y, groupby)) # we'll create x__, we f*ck it up if it means something already
  if (is.null(y_lab)) y_lab <- y
  if (is.null(x_lab)) x_lab <- x
  dt_p <- data.table(dt)[, x__ := factor(get(x))]   # copy input data, create x__ variable as a factor
  dt_p[, (y) := round(get(y), decimals)]   # round datato help display / hover text
  p <- dt_p |> group_by(get(groupby)) |> e_charts(x__)
  if (stack) p <- p |> e_bar_(y, stack="total", areaStyle=list())
  else p <- p |> e_bar_(y, areaStyle=list())  # dodge
  p <- p |>  e_axis(x__, axis="y", name=y_lab, nameLocation="middle", nameGap=30) |> 
    e_axis(x__, axis="x", name=x_lab, nameLocation="middle", nameGap=25, axisPointer=list(show=TRUE))
  p <- p |> e_tooltip(trigger="axis", axisPointer = list(type="cross"))
  p <- p |> e_legend(bottom=0) |> e_toolbox_feature(feature=c("dataView", "saveAsImage"))
  if (magic) p <- e_toolbox_feature(p, feature = "magicType", type=list("line", "bar"))
  p
}

dt <- melt(data.table(mtcars)[, .(avg_mpg = mean(mpg), avg_hp = mean(hp)), by=cyl], id.var="cyl")

eplot_bar_groupby("cyl", "value", "variable", x_lab = "") 
eplot_bar_groupby("cyl", "value", "variable", stack = F)



# x <- dt
# 
# x
# 
# x <- x |> mutate(x_ = factor(!!rlang::sym(xvar)))
# # x <- x |> mutate(x_ = !!rlang::sym(xvar))
# x |> group_by(!!rlang::sym(groupvar)) |> e_charts(x_) |>
#   e_bar_(yvar, stack="total", areaStyle=list())
# 
# x |> group_by(!!rlang::sym(groupvar)) |> e_charts(x_) |>
#   e_bar_(yvar, areaStyle=list())
# 
# x |> group_by(!!rlang::sym(groupvar)) |> e_charts(x_) |>
#   e_bar_(yvar, stack="total")
# 

# #####
# xvar <- "cyl"
# yvar <- "value"
# groupvar <- "variable"
# ylab <- NULL
# decimals <- 3
# magic <- TRUE
# #####
# 
# 
# # copy input data, create x variable as a factor
# stopifnot(!"x__" %in% c(yvar, groupvar)) # we'll create x__, we f*ck it up if it means something already
# if (is.null(ylab)) ylab <- yvar
# dt_p <- data.table(dt)[, x__ := factor(get(xvar))]
# dt_p[, (yvar) := round(get(yvar), decimals)]
# p <- dt_p |> group_by(get(groupvar)) |> e_charts(x__) |>
#   e_bar_(yvar, stack="total", areaStyle=list())
# p <- p |>  e_axis(x__, axis="y", name=ylab, nameLocation="middle", nameGap=30) |> 
#   e_axis(x__, axis="x", axisPointer=list(show=TRUE))
# p <- p |> e_tooltip(trigger="axis", axisPointer = list(type="cross"))
# p <- p |> e_legend(bottom=10) |>
#   e_toolbox_feature(feature=c("dataView", "saveAsImage"))
# if (magic) p <- e_toolbox_feature(p, feature = "magicType", type=list("line", "bar"))


# ?e_title
# ?e_legend
# 
# p
# p |> e_theme(name="roma")
# p |> e_theme(name="infographic")
# 
# ?e_theme
# x |> group_by(get(groupvar)) |> e_charts_(xvar) |>
#   e_bar_(yvar, stack="total", areaStyle=list())
# 
# x |> group_by(!!rlang::sym(groupvar)) |> e_charts(x_) |>
#   e_bar_(yvar, stack="total", areaStyle=list())
# 
# 
# e_charts(dt) |>
#   e_bar_(yvar, stack="total", areaStyle=list())
# 
# 
# 
# 
# stacked_bar <- function(x, xvar, yvar, groupvar, ylab) {
#   x <- x |> mutate(x_ = factor(!!rlang::sym(xvar)))
#   p <- x |> group_by(!!rlang::sym(groupvar)) |> e_charts(x_) |>
#     e_bar_(yvar, stack="total", areaStyle=list())
# }
# 
# 
