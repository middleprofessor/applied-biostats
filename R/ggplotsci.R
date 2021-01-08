# Plotting functions (-)

# wrangling packages
library(here)
library(janitor)
library(readxl)
library(data.table)
library(stringr)
library(forcats)

# analysis packages
library(emmeans)

# graphics packages
library(ggsci)
library(ggpubr)
library(cowplot)
library(lazyWeave) #pvalstring
library(dabestr) # Garner-Altman plots
library(ggforce) # autopoint, sina jitter



#r plot-functions-palettes}
# palettes

# http://mkweb.bcgsc.ca/colorblind/palettes.mhtml#page-container
pal_nature <- c(
  "#2271B2", # honolulu blue
  "#3DB7E9", # summer sky
  "#F748A5", # barbi pink
  "#359B73", # ocean green
  "#d55e00", # bamboo
  "#e69f00", # gamboge, squash, buttercup
  "#f0e442" # holiday, 
)

pal_nature_black <- c(
  "#000000", # black
  "#2271B2", # honolulu blue
  "#3DB7E9", # summer sky
  "#F748A5", # barbi pink
  "#359B73", # ocean green
  "#d55e00", # bamboo
  "#e69f00", # gamboge, squash, buttercup
  "#f0e442" # holiday, 
)

pal_nature_black_mod <- c(
  "#000000", # black
  "#3DB7E9", # summer sky
  "#e69f00", # gamboge, squash, buttercup
  "#359B73", # ocean green
  "#2271B2", # honolulu blue
  "#f0e442", # holiday, 
  "#F748A5", # barbi pink
  "#d55e00" # bamboo
)

pal_nature_mod <- c(
  "#3DB7E9", # summer sky
  "#e69f00", # gamboge, squash, buttercup
  "#359B73", # ocean green
  "#2271B2", # honolulu blue
  "#f0e442", # holiday, 
  "#F748A5", # barbi pink
  "#d55e00" # bamboo
)

pal_nature_alt <- c(
  "#AA0DB4", # barney
  "#FF54ED", # light magenta
  "#00B19F", # strong opal
  "#EB057A", # vivid rose
  "#F8071D", # vivid red
  "#FF8D1A", # dark orange
  "#9EFF37" # french lime, 
)


# https://clauswilke.com/dataviz/color-pitfalls.html
# https://mikemol.github.io/technique/colorblind/2018/02/11/color-safe-palette.html
# https://thenode.biologists.com/data-visualization-with-flying-colors/research/

pal_okabe_ito <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)



#r plot-functions-odd-even}
odd <- function(x) x%%2 != 0
even <- function(x) x%%2 == 0


amc_qq = function(m1,
                  line = "robust",
                  n_boot = 200){
  n <- nobs(m1)
  m1_res <- residuals(m1)
  #sigma_m1_res <- sigma(m1)
  
  normal_qq <- ppoints(n) %>%
    qnorm()
  sample_qq <- m1_res[order(m1_res)]
  
  # mean + sd
  parametric_slope <- sd(sample_qq)
  parametric_intercept <- mean(sample_qq)
  
  # quartiles
  m1_quartiles <- quantile(m1_res, c(.25, .75))
  qnorm_quartiles <- qnorm( c(.25, .75))
  m1_diff <- m1_quartiles[2] - m1_quartiles[1]
  qnorm_diff <- qnorm_quartiles[2] - qnorm_quartiles[1] # = 1.349
  quartile_slope <- m1_diff/qnorm_diff
  quartile_intercept <- median(m1_quartiles) # median of quartiles not quantiles
  
  # robust uses MASS:rlm (default arguments?)
  qq_observed <- data.table(normal_qq = normal_qq,
                            sample_qq = sample_qq)
  m2 <- rlm(sample_qq ~ normal_qq, data = qq_observed)
  robust_intercept <- coef(m2)[1]
  robust_slope <- coef(m2)[2]
  
  # re-sample ribbon
  set.seed(1)
  resample_qq_model <- numeric(n_boot*n)
  Y <- simulate(m1, n_boot)
  fd <- model.frame(m1) %>%
    data.table
  inc <- 1:n
  for(sim_i in 1:n_boot){
    # parametric bound
    fd[, c_peptide := Y[,sim_i]]
    m1_class <- class(m1)[1]
    if(m1_class == "lm"){
      ff <- lm(formula(m1), data = fd) 
    }
    if(m1_class == "lmerModLmerTest" | m1_class == "lmerMod"){
      ff <- lmer(formula(m1), data = fd)
    }
    y_res <- residuals(ff)
    resample_qq <- y_res[order(y_res)]
    resample_qq_model[inc] <- resample_qq
    inc <- inc + n
    
    # robust bound
    qq_resampled <- data.table(normal_qq = normal_qq,
                               resample_qq = resample_qq)
    m2_resample <- rlm(resample_qq ~ normal_qq, data = qq_resampled)
    
  }
  
  qq_sim <- data.table(normal_qq = normal_qq,
                       resample_qq_model = resample_qq_model)
  
  qq_ci_model <- qq_sim[, .(median = median(resample_qq_model),
                            lower = quantile(resample_qq_model, 0.025),
                            upper = quantile(resample_qq_model, 0.975)),
                        by = normal_qq]
  m2_boot <- rlm(median ~ normal_qq, data = qq_ci_model)
  robust_intercept_boot <- coef(m2_boot)[1]
  robust_slope_boot <- coef(m2_boot)[2]
  
  ggplot(data = qq_observed,
         aes(x = normal_qq, y = sample_qq)) +
    
    # ribbon
    geom_ribbon(data = qq_ci_model,
                aes(ymin = lower,
                    ymax = upper,
                    y = median,
                    fill = "band"),
                fill = "gray",
                alpha = 0.6) +
    # draw points
    geom_point() +
    
    # robust
    geom_abline(aes(intercept = robust_intercept,
                    slope = robust_slope,
                    color = "robust"),
                show.legend = FALSE,
                size = 0.75) +
    # robust_boot
    # geom_abline(aes(intercept = robust_intercept_boot,
    #                 slope = robust_slope_boot,
    #                 color = "robust boot"),
    #             show.legend = TRUE,
    #             size = 0.75) +
    xlab("Normal Quantiles") +
    ylab("Sample Quantiles") +
    
    scale_color_manual(values = pal_okabe_ito[c(1:2,5:6)]) +
    theme_minimal_grid() +
    NULL
  
}

amc_spreadlevel <- function(m1,
                            n_boot = 200){
  n <- nobs(m1)
  m1_res <- residuals(m1)
  m1_scaled <- m1_res/sd(m1_res)
  m1_root <- sqrt(abs(m1_scaled))
  m1_fitted <- fitted(m1)
  
  m2 <- lm(m1_root ~ m1_fitted)
  m2_intercept <- coef(m2)[1]
  m2_slope <- coef(m2)[2]
  
  plot_data <- data.table(
    m1_res = sqrt(abs(m1_scaled)),
    fitted = m1_fitted
  )
  
  ggplot(data = plot_data,
         aes(x = fitted, y = m1_res)) +
    
    # ribbon
    # geom_ribbon(data = qq_ci_model,
    #             aes(ymin = lower,
    #                 ymax = upper,
    #                 y = median,
    #                 fill = "band"),
    #             fill = "gray",
    #             alpha = 0.6) +
    # # draw points
    geom_point() +
    
    geom_smooth(method = lm) +
    # robust 
    # geom_abline(aes(intercept = robust_intercept,
    #                 slope = robust_slope,
    #                 color = "robust"),
    #             show.legend = TRUE,
    #             size = 0.75) +
    # robust_boot
    # geom_abline(aes(intercept = robust_intercept_boot,
    #                 slope = robust_slope_boot,
    #                 color = "robust boot"),
    #             show.legend = TRUE,
  #             size = 0.75) +
  xlab("Fitted") +
    ylab("root abs-scaled-residual") +
    
    
    scale_color_manual(values = pal_okabe_ito[c(1:2,5:6)]) +
    theme_minimal_grid() +
    NULL
}

amc_model_check <- function(m1){
  gg1 <- amc_qq(m1)
  gg2 <- amc_spreadlevel(m1)
  cowplot::plot_grid(gg1, gg2, nrow = 1)
}
#r plot-functions-estimate}
estimate <- function(fit, # model fit
                     specs, # factor(s)
                     method = "revpairwise", # revpairwise
                     type = "response", # "link", "response"
                     adjust = "none" # p-value
){
  # response table
  fit_emm <- emmeans(fit, specs = specs, type = type)
  response_table <- emm_table(fit_emm)
  
  # effect table
  fit_pairs <- summary(contrast(fit_emm,
                                method = method,
                                type = type,
                                adjust = adjust,
                                simple = "each",
                                combine = TRUE),
                       infer = c(TRUE, TRUE)) %>%
    data.table()
  effect_table <- pairs_table(fit_pairs)
  
  return(list(
    response = response_table,
    effect = effect_table))
}


#r plot-functions-emm-table}
emm_table <- function(fit_emm){
  table_out <- data.table(summary(fit_emm))
  if("response" %in% colnames(table_out)){
    # if the lm with log(y) or glm with log link, use " / "
    setnames(table_out,
             old = c("response"),
             new = c("emmean"))
  }
  if("asymp.LCL" %in% colnames(table_out)){
    # if the lm with log(y) or glm with log link, use " / "
    setnames(table_out,
             old = c("asymp.LCL", "asymp.UCL"),
             new = c("lower.CL", "upper.CL"))
  }
  return(table_out)
}



#r plot-functions-pairs-table}
pairs_table <- function(fit_pairs){
  
  if("ratio" %in% colnames(fit_pairs)){
    # if the lm with log(y) or glm with log link, use " / "
    groups <- unlist(str_split(fit_pairs$contrast, " / "))
    setnames(fit_pairs, old = "ratio", new = "estimate")
  }else{
    # if lm use " - "
    groups <- unlist(str_split(fit_pairs$contrast, " - "))
  }
  
  if("asymp.LCL" %in% colnames(fit_pairs)){
    # if the lm with log(y) or glm with log link, use " / "
    setnames(fit_pairs,
             old = c("asymp.LCL", "asymp.UCL"),
             new = c("lower.CL", "upper.CL"))
  }
  
  # add the group1 and group 2 columns
  fit_pairs[, group1 := groups[odd(1:length(groups))]]
  fit_pairs[, group2 := groups[even(1:length(groups))]]
  
  # for simple = each, beautify contrast column and group1, group2
  if(which(colnames(fit_pairs)=="contrast") == 3){
    # replace "." with blank
    g_col <- colnames(fit_pairs)[1]
    x_col <- colnames(fit_pairs)[2]
    for(col in names(fit_pairs)[1:2]){
      set(fit_pairs,
          i=which(fit_pairs[[col]]=="."),
          j=col, value="")
    }
    
    fit_pairs[, contrast := paste0(get(names(fit_pairs)[1]),
                                   get(names(fit_pairs)[2]),
                                   ": ",
                                   contrast)]
    fit_pairs[get(g_col) == "", group1 := paste0(get(names(fit_pairs)[1]),
                                                 get(names(fit_pairs)[2]),
                                                 ",",
                                                 group1)]
    fit_pairs[get(g_col) == "", group2 := paste0(get(names(fit_pairs)[1]),
                                                 get(names(fit_pairs)[2]),
                                                 ",",
                                                 group2)]
    fit_pairs[get(x_col) == "", group1 := paste0(group1,
                                                 ",",
                                                 get(names(fit_pairs)[1]),
                                                 get(names(fit_pairs)[2]))]
    fit_pairs[get(x_col) == "", group2 := paste0(group2,
                                                 ",",
                                                 get(names(fit_pairs)[1]),
                                                 get(names(fit_pairs)[2]))]
    
  }
  
  # create a column of nicely formatted p-values for display.
  fit_pairs[, p := pvalString(p.value)]
}




#r plot-functions-gg_mean_error}
gg_mean_error <- function(data,
                          fit, # model fit from lm, lmer, nlme, glmmTMB
                          fit_emm,
                          fit_pairs,
                          x_col,
                          y_col,
                          g_col=NULL,
                          wrap_col=NULL,
                          x_label = "none",
                          y_label = "Response (units)",
                          g_label = NULL,
                          dots = "jitter",
                          dodge_width = 0.8,
                          adjust = 0.5,
                          p_show = 0,
                          p_pos = NULL){
  
  
  dt <- data.table(data)
  gg <- NULL
  
  if(is.null(g_col)){
    g_col <- x_col
  }
  if(is.factor(dt[, get(x_col)]) == FALSE){
    dt[, (x_col) := factor(get(x_col))]
  }
  if(is.factor(dt[, get(g_col)]) == FALSE){
    dt[, (g_col) := factor(get(g_col))]
  }
  
  fit_emm_dt <- emm_table(fit_emm)
  fit_pairs_dt <- pairs_table(data.table(fit_pairs))
  
  if(g_col == x_col | is.null(g_col)){
    pd <- position_dodge(width = 0)
    fit_pairs_dt[, x_min_col := group1]
    fit_pairs_dt[, x_max_col := group2]
  }else{
    pd <- position_dodge(width = dodge_width)
    fit_pairs_dt[, x_min_col := NA]
    fit_pairs_dt[, x_max_col := NA]
    if(is.null(g_label)){
      g_label <- g_col
    }
  }
  
  
  gg <- ggplot(data=dt, aes(x = get(x_col),
                            y = get(y_col),
                            color = get(g_col)))
  # plot points
  if(dots == "sina"){
    gg <- gg + geom_sina(alpha = 0.5,
                         position = pd,
                         adjust = adjust)
  }
  if(dots == "jitter"){
    gg <- gg + geom_point(alpha = 0.5,
                          position = "jitter")
  }
  if(dots == "dotplot"){
    gg <- gg + geom_dotplot(binaxis='y',
                            stackdir='center',
                            alpha = 0.5,
                            position = pd)
    
  }
  
  # plot means and CI
  gg <- gg +
    geom_errorbar(data = fit_emm_dt, aes(y = emmean,
                                         ymin = lower.CL,
                                         ymax = upper.CL,
                                         color = get(g_col)),
                  width = 0,
                  position = pd
    ) +
    geom_point(data = fit_emm_dt, aes(y = emmean,
                                      color = get(g_col)),
               size = 3,
               position = pd
    ) +
    
    # aesthetics
    ylab(y_label) +
    scale_color_manual(values=pal_nature_mod,
                       name = g_col) +
    theme_pubr() +
    theme(legend.position="top") +
    
    NULL
  
  
  if(g_col == x_col){
    gg <- gg + theme(legend.position="none")
  }
  
  if(is.null(g_label)){
    gg <- gg + guides(color = guide_legend(title=NULL))
  }else{
    gg <- gg + guides(color = guide_legend(title=g_label))
  }
  
  if(sum(p_show) > 0){ # show p-values
    # get x positions for p-values
    # [[3]] is 3rd layer which is means
    if(is.na(fit_pairs_dt[1, x_min_col])){
      gg_data <- cbind(fit_emm_dt,
                       ggplot_build(gg)$data[[3]])
      
      gg_data[, cell := paste(get(x_col), get(g_col), sep=",")]
      match_it_ref <- match(fit_pairs_dt$group1, gg_data$cell)
      
      fit_pairs_dt[, rowa := match(group1, gg_data$cell)]
      fit_pairs_dt[, rowb := match(group2, gg_data$cell)]
      fit_pairs_dt[, x_min_col := gg_data[rowa, x]]
      fit_pairs_dt[, x_max_col := gg_data[rowb, x]]
    }
    
    if(is.null(p_pos)){
      p_pos <- 1:length(p_show)
    }
    max_y <- max(dt[, get(y_col)], na.rm=TRUE)
    min_y <- min(dt[, get(y_col)], na.rm=TRUE)
    increment <- 0.1*(max_y - min_y)
    for(i in 1:length(p_pos)){
      pos <- p_pos[i]
      y_position <- max_y + increment*pos
      row <- p_show[i]
      gg <- gg +
        stat_pvalue_manual(fit_pairs_dt[row],
                           label = "p",
                           y.position=y_position,
                           xmin = "x_min_col",
                           xmax = "x_max_col",
                           tip.length = 0.01)
    }
    
    # make sure ylim includes p-value
    y_hi <- max_y + 0.05*(max_y - min_y) +
      increment*max(p_pos)
    y_lo <- min_y - 0.05*(max_y - min_y)
    gg <- gg + coord_cartesian(ylim = c(y_lo, y_hi))
  }
  
  
  # remove x axis title
  if(x_label == "none"){
    gg <- gg + theme(axis.title.x=element_blank())
  }else{
    gg <- gg + xlab(x_label)}
  
  gg
  
  return(gg)
}




#r plot-functions-gg_ancova}
gg_ancova <- function(data,
                      fit, # model fit from lm, lmer, nlme, glmmTMB
                      fit_emm,
                      fit_pairs,
                      x_col,
                      y_col,
                      cov_col,
                      cov_label = "covariate (units)",
                      y_label = "Response (units)",
                      add_ci = TRUE,
                      add_p = TRUE,
                      p_show = NULL,
                      p_pos = "center",
                      p_adjust = "none"){
  
  dt <- data.table(data)[, .SD, .SDcols = c(x_col, y_col, cov_col)]
  
  dt <- data.table(fit$model)
  if(is.factor(dt[, get(x_col)]) == FALSE){
    dt[, (x_col) := factor(get(x_col))]
  }
  
  
  fit_emm_dt <- emm_table(fit_emm)
  fit_pairs_dt <- pairs_table(data.table(fit_pairs))
  
  g <- x_col
  y <- y_col
  x <- cov_col
  
  new_wide <- dt[, .(xmin = min(get(x)), xmax = max(get(x))), by = get(g)]
  new_long <- melt(new_wide,
                   measure.vars = c("xmin", "xmax"),
                   value.name = "x")
  setnames(new_long, old = c("get", "x"), new = c(g, x))
  yhat <- predict(fit,
                  new_long,
                  interval = "confidence")
  new_long <- cbind(new_long, yhat)
  setnames(new_long, old = c("fit"), new = c(y))
  
  gg <- ggplot(dt, aes(
    x = get(x),
    y = get(y),
    color = get(g)
  )) +
    
    geom_point(size = 3) +
    labs(x = x,
         y = y,
         color = g) +
    NULL
  
  g_levels <- levels(dt[, get(g)])
  new_long[, get := get(g)] # not sure how to avoid this
  
  # add ci ribbons
  if(add_ci == TRUE){
    for(g_i in g_levels){
      gg <- gg +
        geom_ribbon(data = new_long[get == g_i,],
                    aes(ymin = lwr,
                        ymax = upr,
                        #                     fill = get(g),
                        linetype = NA
                    ),
                    alpha = 0.2,
                    show.legend = FALSE)
    }
  }
  
  # add regression lines
  for(g_i in g_levels){
    gg <- gg +
      geom_path(data = new_long[get == g_i,],
                aes(x = get(x),
                    y = get(y),
                    color = get(g)
                ))
  }
  
  
  gg <- gg + # aesthetics
    xlab(cov_label) +
    ylab(y_label) +
    scale_color_manual(values=pal_nature_mod) +
    theme_pubr() +
    theme(legend.position="top") +
    
    NULL
  
  if(is.null(p_show)){p_show := 1:nrow(fit_pairs_dt)}
  if(sum(p_show) > 0){
    if(p_pos == "center"){
      x_p <- mean(range(dt[, get(cov_col)], na.rm = TRUE))
    }
    if(p_pos == "left"){
      x_p <- min(dt[, get(cov_col)], na.rm = TRUE) + diff(range(dt[, get(cov_col)], na.rm = TRUE))/10
    }
    if(p_pos == "right"){
      x_p <- max(dt[, get(cov_col)], na.rm = TRUE) - diff(range(dt[, get(cov_col)], na.rm = TRUE))/10
    }
    
    pos_p <- 1:length(p_show)
    max_y <- max(dt[, get(y_col)], na.rm = TRUE)
    min_y <- min(dt[, get(y_col)], na.rm = TRUE)
    increment <- 0.075*(max_y - min_y)
    
    for(i in pos_p){
      y_p <- max_y + increment*i
      row <- p_show[i]
      gg <- gg + annotate("text",
                          x = x_p,
                          y = y_p,
                          label = paste0(fit_pairs_dt[row, contrast],
                                         ": ",
                                         fit_pairs_dt[row, p]))
    }
    
    
  }
  
  
  
  
  
  gg
  
  return(gg)
}




#r plot-functions-response-plot}
gg_mean_ci_ancova <- function(data,
                              x_col,
                              y_col,
                              cov_col,
                              x_label = "none",
                              y_label = "Response (units)",
                              dots = "sina",
                              dodge_width = 0.8,
                              adjust = 0.5,
                              p_adjust = "none"){
  
  dt <- data.table(data)[, .SD, .SDcols = c(x_col, y_col, cov_col)]
  ref_group <- levels(dt[, get(x_col)])[1]
  
  # center the covariate by the mean of the reference, 
  # which means the intercept of the model will be EXP[Y]_ref
  # at the average value of the covariate
  mean_cov_ref <- mean(dt[get(x_col)==ref_group, get(cov_col)])
  dt[, cov_col_c := get(cov_col) - mean_cov_ref]
  cov_col_c <- paste0(cov_col, "_c")
  setnames(dt, old = "cov_col_c", new = cov_col_c)
  
  # two ways for computing adjusted y
  # "xside_xxx" is the "x side" of the formula
  # xside_1 doesn't use centered covariate but uses predicted value at mean covariate
  # xside_2 uses the centered covariate
  xside_1 <- paste(c(cov_col, x_col), collapse = " + ")
  xside_2 <- paste(c(cov_col_c, x_col), collapse = " + ")
  
  form1 <- formula(paste(y_col, "~", xside_1))
  form2 <- formula(paste(y_col, "~", xside_2))
  
  m1 <- lm(form1, data = dt)
  m2 <- lm(form2, data = dt)
  
  # using model 2 - centered covariate in model
  b <- coef(m2)
  dummy <- as.integer(dt[, get(x_col)]) - 1
  dt[, y_cond := b[1] + b[3]*dummy + residuals(m2)]
  
  # using model 1 - prediction at mean covariate
  new_data <- copy(dt)
  new_data[, bw_02_05_18 := mean_cov_ref]
  dt[, y_cond2 := predict(m1, new_data) + residuals(m1)]
  
  # emmeans
  temp_emm <- emmeans(m1,
                      specs = x_col) %>%
    summary() %>%
    data.table()
  emm_offset <- b[1] - temp_emm[get(x_col) == ref_group, emmean]
  
  fit_emm <- emmeans(m1,
                     specs = x_col,
                     offset = emm_offset)
  
  fit_pairs <- contrast(fit_emm,
                        method = "revpairwise",
                        adjust = p_adjust) %>%
    summary(infer = c(TRUE, TRUE))
  
  fit_emm_dt <- data.table(summary(fit_emm))
  fit_pairs_dt <- pairs_table(data.table(fit_pairs))
  
  x_col1 <- x_col[1]
  if(length(x_col)==2){
    g_col <- x_col[2]
  }else{
    g_col <- x_col[1]
  }
  
  if(g_col == x_col1){
    pd <- position_dodge(width = 0)
  }else{
    pd <- position_dodge(width = dodge_width)
  }
  
  gg <- ggplot(data=dt, aes(x = get(x_col1),
                            y = y_cond,
                            color = get(g_col)))
  # plot points
  if(dots == "sina"){
    gg <- gg + geom_sina(alpha = 0.5,
                         position = pd,
                         adjust = adjust)
    
  }
  if(dots == "jitter"){
    gg <- gg + geom_dotplot(alpha = 0.5,
                            position = pd)
    
  }
  
  # plot means and CI
  gg <- gg +
    geom_errorbar(data = fit_emm_dt, aes(y = emmean,
                                         ymin = lower.CL,
                                         ymax = upper.CL,
                                         color = get(g_col)),
                  width = 0,
                  position = pd
    ) +
    geom_point(data = fit_emm_dt, aes(y = emmean,
                                      color = get(g_col)),
               size = 3,
               position = pd
    ) +
    
    # aesthetics
    ylab(y_label) +
    scale_color_manual(values=pal_nature_mod) +
    theme_pubr() +
    theme(legend.position="none") +
    
    NULL
  
  # add p-value
  y_positions <- max(dt[, y_cond]) + 
    0.075*(max(dt[, y_cond]) - min(dt[, y_cond]))
  gg <- gg +
    stat_pvalue_manual(fit_pairs_dt, # only show sox effects
                       label = "p", 
                       y.position=y_positions)
  
  # make sure ylim includes p-value
  ymax <- max(dt[, y_cond])
  ymin <- min(dt[, y_cond])
  y_hi <- ymax + 0.115*(ymax - ymin)
  y_lo <- ymin - 0.05*(ymax - ymin)
  gg <- gg + coord_cartesian(
    ylim = c(y_lo, y_hi),
  )  
  
  # remove x axis title
  if(x_label == "none"){
    gg <- gg + theme(axis.title.x=element_blank())
  }else{
    gg <- gg + xlab(x_label)}
  
  gg
  
  return(gg)
}





#r plot-functions-gg_effect-plot}
gg_effects <- function(data,
                       x_col,
                       y_col,
                       cov_col,
                       x_label = "none",
                       y_label = "contrast",
                       p_adjust = "none"){
  
  dt <- data.table(data)[, .SD, .SDcols = c(x_col, y_col, cov_col)]
  
  xside_1 <- paste(c(cov_col, x_col), collapse = " + ")
  
  form1 <- formula(paste(y_col, "~", xside_1))
  
  m1 <- lm(form1, data = dt)
  
  fit_emm <- emmeans(m1,
                     specs = x_col)
  
  fit_pairs <- contrast(fit_emm,
                        method = "revpairwise",
                        adjust = p_adjust) %>%
    summary(infer = c(TRUE, TRUE))
  
  fit_emm_dt <- data.table(summary(fit_emm))
  fit_effect <- pairs_table(data.table(fit_pairs))
  
  if(y_label == "contrast"){
    y_label <- fit_effect[1, contrast]
  }
  
  fit_effect[, contrast := x_label]
  # # fit_effect[, contrast_pretty := paste(group1, "\n-", group2)]
  # # fit_effect[, contrast_pretty := paste(group1, "\n minus \n", group2)]
  # if(is.null(y_label)){
  #   y_label <- "Difference in means (units)"
  # }
  
  min_bound <- min(fit_effect[, lower.CL])
  max_bound <- min(fit_effect[, upper.CL])
  y_lo <- min(min_bound+min_bound*0.2,
              -max_bound)
  y_hi <- max(max_bound + max_bound*0.2,
              -min_bound)
  y_lims <- c(y_lo, y_hi)
  
  gg <- ggplot(data=fit_effect, aes(x = fct_rev(contrast),
                                    y = estimate)) +
    geom_errorbar(aes(ymin=lower.CL, 
                      ymax=upper.CL),
                  width=0, 
                  color="black") +
    geom_point(size = 3) +
    geom_hline(yintercept=0, linetype = 2) +
    coord_flip(ylim = y_lims) + 
    #coord_flip() + 
    #scale_y_continuous(position="right") +
    theme_pubr() +
    ylab(y_label) +
    theme(axis.title.y = element_blank()) +
    NULL
  gg
  
  return(gg)
}



