#' Plots COG from SDM
#' 
#' plots the center of gravity versus time
#' 
#' @param x SDM object
#' @param ... Additional SDM objects to compare

#' @export
plot_cog <- function(x, ...) {
  sdms <- list(...)
  if(!missing(...)){
    sdms <- c(list(x), sdms)
  }else{
    sdms <- list(x)
  }
  cl <- deparse( sys.call() )
  cl <- stringr::str_split(cl, "[(]")[[1]][2]; cl <- stringr::str_split(cl, "[)]")[[1]][1]
  mod.names <- stringr::str_trim(stringr::str_split(cl, ",")[[1]])

  pred.all <- NULL
  nm <- length(sdms)
  for(i in 1:nm){
    mod <- sdms[[i]]
    for(j in c("gam", "brt", "mlp")) if(inherits(mod, j)) themod <- j
    pred <- predict(mod, model=themod)
    pred$model <- mod.names[i]
    pred.all <- dplyr::bind_rows(pred.all, pred)
  }

  
  
  # First compute the true cog
  cog_lat <- x %>% dplyr::group_by(year) %>% 
    dplyr::summarize(cog=weighted.mean(x=lat, w=abundance))
  cog_lat <- cbind(model="true", cog_lat, stringsAsFactors = FALSE)
  
  # Now add the model cogs
  tmp <- pred.all %>% dplyr::group_by(model, year) %>% 
    dplyr::summarize(cog=weighted.mean(x=lat, w=pred))
  
  # dplyr uses tibbles and they return a matrix when you use rbind(). ug.
  # so use 
  cog_lat <- dplyr::bind_rows(cog_lat, tmp)

  p <- ggplot(cog_lat, aes(x=year, y=cog, color=model)) + 
    geom_line() +
    ggtitle("Comparison of lnorm_low models") +
    ylab("Centre of Gravity (deg lat)") +
    geom_vline(xintercept=2020) +
    annotate("text", x=2020, y=max(cog_lat$cog), label="  forecast", hjust=0) +
    annotate("text", x=2020, y=max(cog_lat$cog), label="hindcast  ", hjust=1)
  
  p
}
