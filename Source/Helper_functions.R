# Extract participant means
means <- function(var, grp) {
  return(tapply(var, grp, mean, na.rm = T)[grp])
}
#-------------------------------------------------------------------------------------------------------------------------------------
# groupe centering 
group_center <- function(var, grp) {
  return(var - tapply(var, grp, mean, na.rm = T)[grp])
}
#-------------------------------------------------------------------------------------------------------------------------------------
# refit LMM with a bettert start position
refit_LMM <- function (model){
  re_model <- update(model, .~., start = getME(model, 'theta'))
  return (re_model)
}
#-------------------------------------------------------------------------------------------------------------------------------------
# Bar graph function
bar_plot <- function (means, palette){
  plot <- ggplot(means, aes(x = Conditions, y = emmean)) +
    geom_bar(stat = 'identity', width=0.4, fill=c(palette[1], palette[2], palette[3]), alpha = 0.5) +
    theme_minimal() +
    ylab("Willingness to help") +
    xlab ("Conditions") +
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = .15, position = position_dodge(.9)) +
    coord_cartesian(ylim = c(1, 7)) + 
    expand_limits(y = c(1, 7)) + 
    scale_y_discrete(limits=c(1,2,3,4,5,6,7)) +
    theme_classic()
  return (plot)
}
#-------------------------------------------------------------------------------------------------------------------------------------
# Plot interaction effects
int_plot <- function(data, DV_name, x_name, palette = c(palette[1], palette[2])){
  ggplot(data, aes(x, predicted, colour = group)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, color = NULL), alpha = .4) +
    theme_classic() +
    labs(title = "", x = x_name, y = DV_name, fill = "Condition") +
    scale_color_manual(values = palette, 
                       labels = c("Conceptual Helping", "Imagine Helping")) +
    scale_fill_manual(values = palette, 
                      labels = c("Conceptual Helping", "Imagine Helping")) +
    coord_cartesian(ylim = c(1, 7)) + 
    expand_limits(y = c(1, 7)) + 
    scale_y_discrete(limits = c(1, 2, 3, 4, 5, 6, 7)) 
}
#-------------------------------------------------------------------------------------------------------------------------------------
# Moderated mediation plots
medmod_plot <- function (data){
  ggplot(data, aes(x = IE, y = Condition)) +
    geom_point() +
    geom_errorbarh(aes(xmin = l_CI, xmax = u_CI), height = 0.3) +
    theme_classic() +
    xlab("Indirect effect") +
    ylab ("Condition") +
    xlim(-0.1, 0.5)
}
