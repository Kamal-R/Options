### Source: https://stackoverflow.com/questions/35633239/add-curly-braces-to-ggplot2-and-then-use-ggsave
bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}

options_theme <- function() {
  theme_classic() + 
    theme(plot.title = element_text(hjust = 0, face = "bold", size = 13, 
                                    colour = "grey33", lineheight = 1.2,
                                    margin = margin(0,0,0,0)),
          plot.subtitle = element_text(face = "plain", colour = "grey33", size = 11,
                                       lineheight = 1.25, margin = margin(0,0,0,0)),
          legend.position = "none", 
          text = element_text(size=16), 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = plot_bg, color = plot_bg),
          plot.background = element_rect(fill = plot_bg, color = plot_bg),
          axis.line = element_line(colour = plot_bg),
          axis.title.x = element_text(margin = unit(c(4, 0, 0, 0), "mm"), 
                                      colour = plot_fg),
          axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm"), 
                                      angle = 90),
          axis.text.x = element_text(colour = "black"), 
          axis.ticks = element_blank() )
}


stock_payoff <- function(data, main_title, sub_title = "") {
  ggplot() + 
    geom_line(data = data, aes(stock_price_x, stock_profit_y, color = Profitable), linetype = "solid") +   
    geom_segment(data = data,
                 aes(x = min(stock_price_x)+1, xend = min(stock_price_x), 
                     y = min(stock_profit_y)+1, yend = min(stock_profit_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = "red") + 
    geom_segment(data = data,
                 aes(x = max(stock_price_x)-1, xend = max(stock_price_x), 
                     y = max(stock_profit_y)-1, yend = max(stock_profit_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = "green3") + 
    scale_y_continuous(label = scales::dollar) +
    scale_x_continuous(label = scales::dollar) + 
    labs(title = main_title, subtitle = sub_title, x = "", y = "") +
    scale_color_manual(values = c("Profit" = "green3", "Loss" = "red"),
                       labels = c("", "", ""), guide = "legend") + 
    guides(color = guide_legend(override.aes = list(color = c("white", "white","white"),
                                                    linetype = c("dashed","solid","solid")))) + 
    options_theme() + 
    theme( plot.title = element_text(vjust = -1), 
           plot.subtitle = element_text(vjust = -5) )
}


call_payoff <- function(data, main_title, sub_title = "") {
  ggplot() + 
    geom_line(data = data, aes(Call_x, Call_y, color = Profitable),
              linetype = "solid") +   
    geom_segment(data = data,
                 aes(x = min(Call_x), xend = min(Call_x), y = min(Call_y), yend = min(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "dotted") + 
    geom_segment(data = data,
                 aes(x = max(Call_x)-1, xend = max(Call_x), y = max(Call_y)-1, yend = max(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = "green3", linetype = "dotted") + 
    scale_y_continuous(label = scales::dollar) +
    scale_x_continuous(label = scales::dollar) + 
    labs(title = main_title, subtitle = sub_title, x = "", y = "") +
    scale_color_manual(values = c("Profit" = "green3", "Loss" = "red"),
                       labels = c("", ""), guide = "legend") + 
    guides(color = guide_legend(override.aes = list(color = c("white","white"),
                                                    linetype = c("solid","solid")))) + 
    options_theme() + 
    theme( plot.title = element_text(vjust = -1), 
           plot.subtitle = element_text(vjust = -5) )
}


stock_and_call_payoff <- function(data_stock, data_call, main_title, sub_title = "") { 
  ggplot() + 
    geom_line(data = data_stock, aes(stock_price_x, stock_profit_y, color = Profitable), 
              linetype = "dotdash") +   
    geom_segment(data = data_stock,
                 aes(x = min(stock_price_x)+1, xend = min(stock_price_x), 
                     y = min(stock_profit_y)+1, yend = min(stock_profit_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = adjustcolor("red", alpha.f = 0.5), 
                 linetype = "dotted") + 
    geom_segment(data = data_stock, 
                 aes(x = max(stock_price_x)-1, xend = max(stock_price_x), 
                     y = max(stock_profit_y)-1, yend = max(stock_profit_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = adjustcolor("green3", alpha.f = 0.5),
                 linetype = "dotted") + 
    geom_line(data = data_call, aes(Call_x, Call_y, color = Profitable), linetype = "solid") +   
    geom_segment(data = data_call,
                 aes(x = min(Call_x), xend = min(Call_x), y = min(Call_y), yend = min(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "dotted") + 
    geom_segment(data = data_call,
                 aes(x = max(Call_x)-1, xend = max(Call_x), y = max(Call_y)-1, yend = max(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = "green3", linetype = "dotted") + 
    labs(title = main_title, subtitle = sub_title, x = "", y = "") +
    scale_y_continuous(label = scales::dollar) +
    scale_x_continuous(label = scales::dollar) + 
    scale_color_manual(values = c( "Profit_call" = "green3", "Loss_call" = "red",
                                   "Profit_stock" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_stock" = adjustcolor("red", alpha.f = 0.5)),
                       labels = c("", "", "", "", ""), guide = "legend") + 
    guides(color = guide_legend(override.aes = list(color = c("white","white"),
                                                    linetype = c("solid","solid")))) + 
    options_theme() + 
    theme( plot.title = element_text(vjust = -1), 
           plot.subtitle = element_text(vjust = -5) )
}


put_payoff <- function(data, main_title, sub_title = "") {
  ggplot() + 
    geom_line(data = data, aes(Put_x, Put_y, color = Profitable), linetype = "solid") +
    geom_segment(data = data,
                 aes(x = min(Put_x), xend = min(Put_x)-1, y = max(Put_y), yend = max(Put_y)+1), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open"), 
                 show.legend = FALSE, col = "green3", linetype = "solid") + 
    geom_segment(data = data,
                 aes(x = max(Put_x), xend = max(Put_x), y = min(Put_y), yend = min(Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 210, type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "solid") + 
    scale_y_continuous(label = scales::dollar) +
    scale_x_continuous(label = scales::dollar) + 
    labs(title = main_title, subtitle = sub_title, x = "", y = "") +
    scale_color_manual(values = c( "Profit" = "green3", "Loss" = "red"),
                       labels = c("", ""), guide = "legend") + 
    options_theme() + 
    theme( plot.title = element_text(vjust = -1), 
           plot.subtitle = element_text(vjust = -5) )
}


stock_and_put_payoff <- function(data_stock, data_put, main_title, sub_title = "") { 
  ggplot() + 
    geom_line(data = data_stock, aes(stock_price_x, stock_profit_y, color = Profitable), 
              linetype = "dotdash") +   
    geom_segment(data = data_stock, 
                 aes(x = min(stock_price_x)+1, xend = min(stock_price_x), 
                     y = min(stock_profit_y)+1, yend = min(stock_profit_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = adjustcolor("red", alpha.f = 0.5), 
                 linetype = "dotted") + 
    geom_segment(data = data_stock,
                 aes(x = max(stock_price_x)-1, xend = max(stock_price_x), 
                     y = max(stock_profit_y)-1, yend = max(stock_profit_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = adjustcolor("green3", alpha.f = 0.5),
                 linetype = "dotted") + 
    geom_line(data = data_put, aes(Put_x, Put_y, color = Profitable), linetype = "solid") +
    geom_segment(data = data_put,
                 aes(x = min(Put_x), xend = min(Put_x)-1, y = max(Put_y), yend = max(Put_y)+1), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open"), 
                 show.legend = FALSE, col = "green3", linetype = "dotted") + 
    geom_segment(data = data_put,
                 aes(x = max(Put_x), xend = max(Put_x), y = min(Put_y), yend = min(Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 210, type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "dotted") + 
    labs(title = main_title, subtitle = sub_title, x = "", y = "") +
    scale_y_continuous(label = scales::dollar) +
    scale_x_continuous(label = scales::dollar) + 
    scale_color_manual(values = c( "Profit_put" = "green3", "Loss_put" = "red",
                                   "Profit_stock" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_stock" = adjustcolor("red", alpha.f = 0.5)),
                       labels = c("", "", "", "", ""), guide = "legend") + 
    guides(color = guide_legend(override.aes = list(color = c("white","white"),
                                                    linetype = c("solid","solid")))) + 
    options_theme() + 
    theme( plot.title = element_text(vjust = -1), 
           plot.subtitle = element_text(vjust = -5, margin=margin(0,0,20,0)) )
}

synthetic_long_payoff <- function(data_synthetic_long, data_put, data_call, main_title, sub_title = "") { 
  ggplot() + 
    geom_line(data = data_synthetic_long, aes(Synthetic_Long_x, Synthetic_Long_y, color = Profitable), 
              linetype = "solid") +   
    geom_segment(data = data_synthetic_long, linetype = "solid",
                 aes(x = min(Synthetic_Long_x)+1, xend = min(Synthetic_Long_x), 
                     y = min(Synthetic_Long_y)+1, yend = min(Synthetic_Long_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = adjustcolor("red", alpha.f = 0.5)) + 
    geom_segment(data = data_synthetic_long, linetype = "solid",
                 aes(x = max(Synthetic_Long_x)-1, xend = max(Synthetic_Long_x), 
                     y = max(Synthetic_Long_y)-1, yend = max(Synthetic_Long_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = adjustcolor("green3", alpha.f = 0.5)) + 
    geom_line(data = data_call, aes(Call_x, Call_y, color = Profitable), linetype = "dotdash") +
    geom_segment(data = data_call,
                 aes(x = min(Call_x), xend = min(Call_x), y = min(Call_y), yend = min(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "dotted") + 
    geom_segment(data = data_call,
                 aes(x = max(Call_x)-1, xend = max(Call_x), y = max(Call_y)-1, yend = max(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = "green3", linetype = "dotted") + 
    geom_line(data = data_put, aes(Put_x, Put_y, color = Profitable), linetype = "dotdash") +
    geom_segment(data = data_put, linetype = "dotted",
                 aes(x = min(Put_x)+1, xend = min(Put_x), 
                     y = min(Put_y)+1, yend = min(Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = "red") + 
    geom_segment(data = data_put,
                 aes(x = max(Put_x), xend = max(Put_x), 
                     y = max(Put_y), yend = max(Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 210, type = "open"), 
                 show.legend = FALSE, col = "green3", linetype = "dotted") + 
    labs(title = main_title, subtitle = sub_title, x = "", y = "") +
    scale_y_continuous(label = scales::dollar) +
    scale_x_continuous(label = scales::dollar) + 
    scale_color_manual(values = c( "Profit_stock" = "green3", "Loss_stock" = "red",
                                   "Profit_put" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_put" = adjustcolor("red", alpha.f = 0.5),
                                   "Profit_call" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_call" = adjustcolor("red", alpha.f = 0.5) ),
                       labels = c("", "", "", "", "", "", ""), guide = "legend") + 
    guides(color = guide_legend(override.aes = list(color = c("white","white"),
                                                    linetype = c("solid","solid")))) + 
    options_theme() + 
    theme( plot.title = element_text(vjust = -1), 
           plot.subtitle = element_text(vjust = -5) )
}


protective_put_payoff <- function(data_protective_put, data_stock, data_put, main_title, sub_title = "") { 
  ggplot() + 
    geom_line(data = data_protective_put, aes(Protective_Put_x, Protective_Put_y, color = Profitable), 
              linetype = "solid") +   
    geom_segment(data = data_protective_put, linetype = "solid",
                 aes(x = min(Protective_Put_x), xend = min(Protective_Put_x), 
                     y = min(Protective_Put_y), yend = min(Protective_Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open"), 
                 show.legend = FALSE, col = adjustcolor("red", alpha.f = 0.5)) + 
    geom_segment(data = data_protective_put, linetype = "solid",
                 aes(x = max(Protective_Put_x)-1, xend = max(Protective_Put_x), 
                     y = max(Protective_Put_y)-1, yend = max(Protective_Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = adjustcolor("green3", alpha.f = 0.5)) + 
    geom_line(data = data_stock, aes(stock_price_x, stock_profit_y, color = Profitable), 
              linetype = "dotdash") +   
    geom_segment(data = data_stock,
                 aes(x = min(stock_price_x)+1, xend = min(stock_price_x), 
                     y = min(stock_profit_y)+1, yend = min(stock_profit_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = adjustcolor("red", alpha.f = 0.5), 
                 linetype = "dotdash") + 
    geom_segment(data = data_stock, 
                 aes(x = max(stock_price_x)-1, xend = max(stock_price_x), 
                     y = max(stock_profit_y)-1, yend = max(stock_profit_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = adjustcolor("green3", alpha.f = 0.5),
                 linetype = "dotdash") + 
    geom_line(data = data_put, aes(Put_x, Put_y, color = Profitable), linetype = "dotdash") +
    geom_segment(data = data_put, 
                 aes(x = min(Put_x), xend = min(Put_x)-1, y = max(Put_y), yend = max(Put_y)+1), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = "green3", linetype = "dotted") + 
    geom_segment(data = data_put,
                 aes(x = max(Put_x), xend = max(Put_x), y = min(Put_y), yend = min(Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 210, type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "dotted") + 
    labs(title = main_title, subtitle = sub_title, x = "", y = "") +
    scale_y_continuous(label = scales::dollar) +
    scale_x_continuous(label = scales::dollar) + 
    scale_color_manual(values = c( "Profit_protective_put" = "green3", "Loss_protective_put" = "red",
                                   "Profit_put" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_put" = adjustcolor("red", alpha.f = 0.5),
                                   "Profit_stock" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_stock" = adjustcolor("red", alpha.f = 0.5) ),
                       labels = c("", "", "", "", "", "", ""), guide = "legend") + 
    guides(color = guide_legend(override.aes = list(color = c("white","white"),
                                                    linetype = c("solid","solid")))) + 
    options_theme() + 
    theme( plot.title = element_text(vjust = -1), 
           plot.subtitle = element_text(vjust = -5) )
}


covered_call_payoff <- function(data_covered_call, data_stock, data_call, main_title, sub_title = "") { 
  ggplot() + 
    geom_line(data = data_covered_call, aes(Covered_Call_x, Covered_Call_y, color = Profitable), 
              linetype = "solid") +   
    geom_segment(data = data_covered_call, linetype = "solid",
                 aes(x = max(Covered_Call_x), xend = max(Covered_Call_x), 
                     y = max(Covered_Call_y), yend = max(Covered_Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 210, type = "open"), 
                 show.legend = FALSE, col = adjustcolor("green3", alpha.f = 0.5)) + 
    geom_segment(data = data_covered_call, linetype = "solid",
                 aes(x = min(Covered_Call_x)+1, xend = min(Covered_Call_x), 
                     y = min(Covered_Call_y)+1, yend = min(Covered_Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = adjustcolor("red", alpha.f = 0.5)) + 
    geom_line(data = data_stock, aes(stock_price_x, stock_profit_y, color = Profitable), 
              linetype = "dotdash") +   
    geom_segment(data = data_stock,
                 aes(x = min(stock_price_x)+1, xend = min(stock_price_x), 
                     y = min(stock_profit_y)+1, yend = min(stock_profit_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = adjustcolor("red", alpha.f = 0.5), 
                 linetype = "dotdash") + 
    geom_segment(data = data_stock, 
                 aes(x = max(stock_price_x)-1, xend = max(stock_price_x), 
                     y = max(stock_profit_y)-1, yend = max(stock_profit_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = adjustcolor("green3", alpha.f = 0.5),
                 linetype = "dotdash") + 
    geom_line(data = data_call, aes(Call_x, Call_y, color = Profitable), linetype = "dotdash") +
    geom_segment(data = data_call, 
                 aes(x = min(Call_x), xend = min(Call_x), 
                     y = max(Call_y), yend = max(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open"), 
                 show.legend = FALSE, col = "green3", linetype = "dotted") + 
    geom_segment(data = data_call,
                 aes(x = max(Call_x)-1, xend = max(Call_x), 
                     y = min(Call_y)+1, yend = min(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "dotted") + 
    labs(title = main_title, subtitle = sub_title, x = "", y = "") +
    scale_y_continuous(label = scales::dollar) +
    scale_x_continuous(label = scales::dollar) + 
    scale_color_manual(values = c( "Profit_covered_call" = "green3", "Loss_covered_call" = "red",
                                   "Profit_call" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_call" = adjustcolor("red", alpha.f = 0.5),
                                   "Profit_stock" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_stock" = adjustcolor("red", alpha.f = 0.5) ),
                       labels = c("", "", "", "", "", "", ""), guide = "legend") + 
    guides(color = guide_legend(override.aes = list(color = c("white","white"),
                                                    linetype = c("solid","solid")))) + 
    options_theme() + 
    theme( plot.title = element_text(vjust = -1), 
           plot.subtitle = element_text(vjust = -5) )
}


collar_payoff_left <- function(data_collar, data_stock, data_put,  
                               data_call, main_title, sub_title = "") { 
  ggplot() + 
    geom_rect(data = fill_color_left, fill = "aliceblue", alpha = 0.7,
              aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = 50)) +
    geom_line(data = data_collar, aes(Collar_x, Collar_y, color = Profitable), 
              linetype = "solid") + 
    geom_segment(data = data_collar, linetype = "solid",
                 aes(x = min(Collar_x)+1, xend = min(Collar_x), 
                     y = min(Collar_y), yend = min(Collar_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = "red") + 
    geom_segment(data = data_collar, linetype = "solid",
                 aes(x = max(Collar_x)-1, xend = max(Collar_x), 
                     y = max(Collar_y), yend = max(Collar_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = "green3") + 
    geom_line(data = data_stock, aes(Stock_x, Stock_y, color = Profitable), 
              linetype = "dotdash") +   
    geom_segment(data = data_stock, linetype = "dotdash",
                 aes(x = min(Stock_x)+1, xend = min(Stock_x), 
                     y = min(Stock_y)+1, yend = min(Stock_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = adjustcolor("red", alpha.f = 0.5)) + 
    geom_segment(data = data_stock, linetype = "dotdash",
                 aes(x = max(Stock_x)-1, xend = max(Stock_x), 
                     y = max(Stock_y)-1, yend = max(Stock_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = adjustcolor("green3", alpha.f = 0.5)) + 
    geom_line(data = data_call, aes(Call_x, Call_y, color = Profitable), linetype = "dotdash") +
    geom_segment(data = data_call,
                 aes(x = min(Call_x), xend = min(Call_x), 
                     y = max(Call_y), yend = max(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open"), 
                 show.legend = FALSE, col = "green3", linetype = "dotted") + 
    geom_segment(data = data_call,
                 aes(x = max(Call_x)-1, xend = max(Call_x), 
                     y = min(Call_y)+1, yend = min(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "dotted") + 
    geom_line(data = data_put, aes(Put_x, Put_y, color = Profitable), linetype = "dotdash") +
    geom_segment(data = data_put, linetype = "dotted",
                 aes(x = min(Put_x)+1, xend = min(Put_x), 
                     y = max(Put_y)-1, yend = max(Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = "green3") + 
    geom_segment(data = data_put,
                 aes(x = max(Put_x), xend = max(Put_x), 
                     y = min(Put_y), yend = min(Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 210, type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "dotted") + 
    labs(title = main_title, subtitle = sub_title, x = "", y = "") +
    scale_y_continuous(label = scales::dollar) +
    scale_x_continuous(label = scales::dollar) + 
    scale_color_manual(values = c( "Profit_collar" = "green3", "Loss_collar" = "red",
                                   "Profit_put" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_put" = adjustcolor("red", alpha.f = 0.5),
                                   "Profit_call" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_call" = adjustcolor("red", alpha.f = 0.5),
                                   "Profit_stock" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_stock" = adjustcolor("red", alpha.f = 0.5) ),
                       labels = c("", "", "", "", "", "", "", "", ""), guide = "legend") + 
    guides(color = guide_legend(override.aes = list(color = c("white","white"),
                                                    linetype = c("solid","solid")))) + 
    options_theme() + 
    theme( plot.title = element_text(vjust = -1), 
           plot.subtitle = element_text(vjust = -5) )
}


collar_payoff_right <- function(data_collar, data_stock, data_put, 
                                data_call, main_title, sub_title = "") { 
  ggplot() + 
    geom_rect(data = fill_color_right, fill = "aliceblue", alpha = 0.7,
              aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = 85)) +
    geom_line(data = data_collar, aes(Collar_x, Collar_y, color = Profitable), 
              linetype = "solid") + 
    geom_segment(data = data_collar, linetype = "solid",
                 aes(x = min(Collar_x)+1, xend = min(Collar_x), 
                     y = min(Collar_y), yend = min(Collar_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = "red") + 
    geom_segment(data = data_collar, linetype = "solid",
                 aes(x = max(Collar_x)-1, xend = max(Collar_x), 
                     y = max(Collar_y), yend = max(Collar_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = "green3") + 
    geom_line(data = data_stock, aes(Stock_x, Stock_y, color = Profitable), 
              linetype = "dotdash") +   
    geom_segment(data = data_stock, linetype = "dotdash",
                 aes(x = min(Stock_x)+1, xend = min(Stock_x), 
                     y = min(Stock_y)+1, yend = min(Stock_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = adjustcolor("red", alpha.f = 0.5)) + 
    geom_segment(data = data_stock, linetype = "dotdash",
                 aes(x = max(Stock_x)-1, xend = max(Stock_x), 
                     y = max(Stock_y)-1, yend = max(Stock_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = adjustcolor("green3", alpha.f = 0.5)) + 
    geom_line(data = data_call, aes(Call_x, Call_y, color = Profitable), linetype = "dotdash") +
    geom_segment(data = data_call,
                 aes(x = min(Call_x), xend = min(Call_x), 
                     y = max(Call_y), yend = max(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open"), 
                 show.legend = FALSE, col = "green3", linetype = "dotted") + 
    geom_segment(data = data_call,
                 aes(x = max(Call_x)-1, xend = max(Call_x), 
                     y = min(Call_y)+1, yend = min(Call_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 30, type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "dotted") + 
    geom_line(data = data_put, aes(Put_x, Put_y, color = Profitable), linetype = "dotdash") +
    geom_segment(data = data_put, linetype = "dotted",
                 aes(x = min(Put_x)+1, xend = min(Put_x), 
                     y = max(Put_y)-1, yend = max(Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), type = "open", angle = 30), 
                 show.legend = FALSE, col = "green3") + 
    geom_segment(data = data_put,
                 aes(x = max(Put_x), xend = max(Put_x), 
                     y = min(Put_y), yend = min(Put_y)), 
                 arrow = arrow(length = unit(0.50, "cm"), angle = 210, type = "open"), 
                 show.legend = FALSE, col = "red", linetype = "dotted") + 
    labs(title = main_title, subtitle = sub_title, x = "", y = "") +
    scale_y_continuous(label = scales::dollar) +
    scale_x_continuous(label = scales::dollar) + 
    scale_color_manual(values = c( "Profit_collar" = "green3", "Loss_collar" = "red",
                                   "Profit_put" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_put" = adjustcolor("red", alpha.f = 0.5),
                                   "Profit_call" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_call" = adjustcolor("red", alpha.f = 0.5),
                                   "Profit_stock" = adjustcolor("green3", alpha.f = 0.5),
                                   "Loss_stock" = adjustcolor("red", alpha.f = 0.5) ),
                       labels = c("", "", "", "", "", "", "", "", ""), guide = "legend") + 
    guides(color = guide_legend(override.aes = list(color = c("white","white"),
                                                    linetype = c("solid","solid")))) + 
    options_theme() + 
    theme( plot.title = element_text(vjust = 0), 
           plot.subtitle = element_text(vjust = -5) )
}
