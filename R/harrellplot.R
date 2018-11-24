#' harrellplot
#'
#' a wrapper to ggplot2 that generates a Harrel (or Horizontal) dot plot, with an upper panel of model contrasts and a lower panel of treatment distributions
#' @import ggplot2
#' @import cowplot
#' @import Hmisc
#' @import broom
#' @import data.table
#' @param y contains the name of the column with the response variable
#' @param x contains the name of the column of Treatment 1 -- will be ploted on the X-axis
#' @param g contains the name of the column of Treatment 2 -- will be the "grouping" variable for the plot
#' @param data is the data frame or data.table
#' @param fit.model at present, only "lm" and 'lmm' (using lme4) are implemented
#' @param error at present, only "Normal" is implemented
#' @param add_interaction if TRUE, include interaction effect
#' @param interaction.group if TRUE, plots effects across levels of Treatment 2
#' @param interaction.treatment if TRUE, plots effects across levels of Treatment 1
#' @param mean_intervals.method method for computing confidence intervals of the treatment means. "raw" commputes the intervals based on the treatment SE, "lm" is based on the model SE from the fit model, "boot" computes bootstrap intervals
#' @param conf.mean is the percentile level for the treatment CIs. Possible values are 0.9, 0.95, and 0.99
#' @param contrasts.method "coefficients" are the model coefficients. "trt.vs.ctrl1" and "revpairwise" are from the emmeans (formerly lsmeans) package.
#' @param contrasts.scaling Scaling of the effects (if contrasts). Can be 'raw', 'percent', or 'standardized'
#' @param conf.contrast is percentile level for the contrast CIs. Possible values are 0.9, 0.95, and 0.99
#' @param adjust is interval adjustment for contrast CIs, TRUE is default adjustment, which is Tukey for pairwise or modified Dunnets for treatment vs. control
#' @param display.treatment can be "box", "ci"
#' @param short if TRUE, treatment levels are shortened using abbreviate()
#' @param show.mean if TRUE, plot mean
#' @param show.dots if TRUE, plot dots
#' @param show.zero if TRUE, includes 0.0 on in the forest plot of effects
#' @param horizontal if TRUE, the plot is flipped into horizontal geometry
#' @param color_palette can be "ggplot", "Greys", "Blues", "Accent", "Dark2", "Set1", "Set2", "Set3"
#' @param rel_height Aspect ratio of forest plot vs. box/dot plot components. E.g .66 or 1.5. For default, either do not pass this parameter or us "rel_height=0".
#' @param y_label user supplied lable for Y axis
#' @param jtheme can be "gray", "bw", "classic", "minimal"
#' @export
harrellplot <- function(
  # function for Harrell or Horizontal dot plot after Harrell's Hmisc
  x,
  y,
  g=NULL,
  covcols=NULL,
  rintcols=NULL,
  rslopecols=NULL,
  data, # data frame or table
  fit.model='lm', # lm, lmm, glm
  REML=TRUE, # for lmm models, if false then fit with ML
  error='Normal', # normal, lognormal, logistic, poisson
  add_interaction=TRUE,
  interaction.group=TRUE,
  interaction.treatment=TRUE,
  mean_intervals.method='raw', # model for CI of mean
  conf.mean=0.95, # confidence level for CI of mean
  contrasts.method='revpairwise', # which contrasts to show
  contrasts.scaling='raw', # 'percent', 'standardized'
  conf.contrast=0.95,
  adjust=FALSE,
  show.contrasts=TRUE,
  show.treatments=TRUE,
  display.treatment='box',
  short=FALSE,
  show.mean=TRUE,
  show.dots=TRUE,
  zero=TRUE,
  horizontal=TRUE,
  color_palette='Greys',
  jtheme='minimal',
  rel_height=0, # Aspect ratio of forest plot vs. box/dot plot components (0 is default)
  y_label=NULL # user supplied lable for Y axis
){
  # subset data
  if(is.null(g)){
    xcols <- x
    grouping <- FALSE
    add_interaction <- FALSE
    interaction.group <- FALSE
  }else{
    xcols <- c(x,g)
    grouping <- TRUE
  }
  data <- data.table(data)
  dt <- data[, .SD, .SDcols=unique(c(xcols, y, rintcols, rslopecols, covcols))]
  dt <- na.omit(dt) # has to be after groups read in

  # add empty grouping variable column if grouping == FALSE to make subsequent code easier
  if(grouping == FALSE){
    g <- 'dummy_g'
    dt[, (g):='dummy']
  }

  # abbreviate levels if TRUE
  if(short==TRUE){
    dt[, (x):=abbreviate(get(x))]
    dt[, (g):=abbreviate(get(g))]
  }
  x_order <- dt[,.(i=min(.I)),by=get(x)][, get]
  dt[, (x):=factor(get(x), x_order)]
  g_order <- dt[,.(i=min(.I)),by=get(g)][, get]
  dt[, (g):=factor(get(g), g_order)]

  res <- fit_model(x, y, g, covcols, rintcols, rslopecols, dt, fit.model, REML, error, add_interaction, interaction.group, interaction.treatment, mean_intervals.method, conf.mean, contrasts.method, contrasts.scaling, conf.contrast, adjust)

  ci_means <- res$ci_means
  ci_diffs <- res$ci_diffs
  tables <- res$tables

  # temp for debug
  # tables$contrasts <- ci_diffs

  # plot it
  base.size <- 18

  gg_contrasts <- NULL
  gg_treatments <- NULL
  if(show.contrasts == TRUE){
    gg_contrasts <- ggplot(data=ci_diffs, aes_string(x=x, y=y)) +

      # draw line at y=0 first

      # draw effects + CI
      geom_linerange(aes(ymin = lower, ymax = upper), color='black', size=2) +
      geom_point(size=3, color='white') +
      geom_point(size=2, color='black')

    # re-label Y
    if(contrasts.scaling=='raw'){contrast_axis_name <- 'Effect'}
    if(contrasts.scaling=='percent'){contrast_axis_name <- 'Percent Effect'}
    if(contrasts.scaling=='standardized'){contrast_axis_name <- 'Standardized Effect'}
    gg_contrasts <- gg_contrasts + ylab(contrast_axis_name)

    # re-label X
    contrast_txt <- ifelse(contrasts.method=='coefficients', 'Coefficient', 'Contrast')
    gg_contrasts <- gg_contrasts + xlab(contrast_txt)

    # set theme and gridlines first as background
    if(jtheme=='grey'){
      gg_contrasts <- gg_contrasts + theme_grey(base_size = base.size)
    }
    if(jtheme=='gray'){
      gg_contrasts <- gg_contrasts + theme_gray(base_size = base.size)
    }
    if(jtheme=='bw'){
      gg_contrasts <- gg_contrasts + theme_bw(base_size = base.size)
    }
    if(jtheme=='classic'){
      gg_contrasts <- gg_contrasts + theme_classic(base_size = base.size)
    }
    if(jtheme=='minimal'){
      gg_contrasts <- gg_contrasts + theme_minimal(base_size = base.size)
    }
    if(jtheme=='cowplot'){
      gg_contrasts <- gg_contrasts + theme_cowplot(font_size = base.size)
    }

    # include zero in axis?
    y_range <- range(pretty(c(ci_diffs[, lower], ci_diffs[, upper])))
    ylims <- y_range
    if(min(y_range) > 0){
      ylims <-c(0, max(y_range))
    }
    if(max(y_range) < 0){
      ylims <- c(min(y_range), 0)
    }

    if(horizontal==TRUE){
      gg_contrasts <- gg_contrasts +
        scale_y_continuous(position = "right") +
        theme(plot.margin = margin(0, 0, 0, 0, "cm"))
      if(zero==TRUE){
        gg_contrasts <- gg_contrasts + coord_flip(ylim=ylims)
      }else{
        gg_contrasts <- gg_contrasts + coord_flip()
      }

    }

  }

  if(show.treatments == TRUE){
    gg_treatments <- ggplot(data=dt, aes_string(x=x, y=y))
    dodge_width <- 0.75

    # show box plot
    if(display.treatment=='box'){ # plot before dots
      if(show.dots==TRUE){outlier_color <- NA}else{outlier_color <- NULL}
      gg_treatments <- gg_treatments + geom_boxplot(data=dt, aes_string(fill=g), outlier.colour = outlier_color)
    }

    if(display.treatment=='ci'){
      gg_treatments <- gg_treatments + geom_linerange(data=ci_means, aes_string(ymin = 'lower', ymax = 'upper', group=g), size=2, position=position_dodge(dodge_width))
      gg_treatments <- gg_treatments + geom_point(data=ci_means, aes_string(x=x, y=y, shape=g), size=5, color='white', position=position_dodge(dodge_width))
      gg_treatments <- gg_treatments + geom_point(data=ci_means, aes_string(x=x, y=y, shape=g), size=3, color='black', position=position_dodge(dodge_width))
      gg_treatments
    }

    # show dots
    if(show.dots==TRUE){
      #gg_treatments <- gg_treatments + geom_jitter(aes_string(group=g), width=0.1, height = 0.0, size=1, alpha=0.5)
      if(is.null(rintcols)){
        gg_treatments <- gg_treatments + geom_point(aes_string(fill=g), size=1, alpha=0.5, position=position_jitterdodge())
      }else{
        gg_treatments <- gg_treatments + geom_point(aes_string(fill=g), size=1, alpha=0.5, position=position_jitterdodge())
        # gg_treatments <- ggplot(data=dt, aes_string(x=x, y=y))
        # gg_treatments <- gg_treatments + geom_boxplot(data=dt, aes_string(fill=g), outlier.colour = outlier_color)
        # gg_treatments <- gg_treatments + geom_line(aes(group=Time, color=ID), position=position_dodge(dodge_width))
        # gg_treatments
      }
    }

    # show mean
    if(show.mean==TRUE & display.treatment=='box'){
      dot_color <- ifelse(color_palette=='Greys','black','black')
      gg_treatments <- gg_treatments + geom_point(data=ci_means, aes_string(x=x, y=y, group=g), size=3, color='white', position=position_dodge(width=dodge_width))
      gg_treatments <- gg_treatments + geom_point(data=ci_means, aes_string(x=x, y=y, group=g), size=2, color=dot_color, position=position_dodge(width=dodge_width))
    }

    # set colors
    if(color_palette != 'ggplot'){
      gg_treatments <- gg_treatments + scale_color_brewer(palette = color_palette)
      gg_treatments <- gg_treatments + scale_fill_brewer(palette = color_palette)
    }

    # set theme and gridlines first as background
    if(jtheme=='grey'){
      gg_treatments <- gg_treatments + theme_grey(base_size = base.size)
    }
    if(jtheme=='gray'){
      gg_treatments <- gg_treatments + theme_gray(base_size = base.size)
    }
    if(jtheme=='bw'){
      gg_treatments <- gg_treatments + theme_bw(base_size = base.size)
    }
    if(jtheme=='classic'){
      gg_treatments <- gg_treatments + theme_classic(base_size = base.size)
    }
    if(jtheme=='minimal'){
      gg_treatments <- gg_treatments + theme_minimal(base_size = base.size)
    }
    if(jtheme=='cowplot'){
      gg_treatments <- gg_treatments + theme_cowplot(font_size = base.size)
    }

    legend_postion <- ifelse(grouping==TRUE,'bottom','none')
    gg_treatments <- gg_treatments +
      theme(plot.margin = margin(0, 0, 0, 0, "cm"), legend.position = legend_postion)

    if(horizontal==TRUE){
      gg_treatments <- gg_treatments +
        coord_flip()
    }

    if(!is.null(y_label)){
      gg_treatments <- gg_treatments + ylab(y_label)
    }
  }

  if(rel_height==0){
    ar <- nrow(ci_diffs)/nrow(ci_means)
  }else{
    ar <- rel_height
  }
  if(show.contrasts==TRUE & show.treatments==TRUE){
    gg <- plot_grid(gg_contrasts, gg_treatments, nrow=2, align = "v", rel_heights = c(1*ar, 1))
  }
  if(show.contrasts==TRUE & show.treatments==FALSE){
    gg <- gg_contrasts
  }
  if(show.contrasts==FALSE & show.treatments==TRUE){
    gg <- gg_treatments
  }

  return(list(gg=gg, gg_contrasts=gg_contrasts, gg_treatments=gg_treatments, tables=tables))
}
