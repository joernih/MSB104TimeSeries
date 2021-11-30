#' This is a function for producing Covid-19 time series plot
#' @importFrom magrittr %>%
#' @export
plot19ts <- function(sel_cou=NULL,covid19df=NULL, yvar='hosp'){
	
	c_cdf <- sel_cou %>% purrr::map(function(x,df_c=covid19df){
		out <- df_c %>% dplyr::filter(iso_alpha_3==x)
		gout <- ggplot(out, aes_string(x='dayofyear',y=yvar, color='year')) + 
			geom_point() + 
		geom_line() + 
			labs(title=x,"Day during the year", y=yvar)
	})
}

#' @export
k <- function(){
    df <-  clipr::write_last_clip()
}

#' @export
l <- function(){
    open_command <- switch(Sys.info()[['sysname']],
                           Windows= 'open',
                           Linux  = 'xdg-open',
                           Darwin = 'open')

    #temp_file <- paste0('tmp/abc', '.xlsx')
    temp_file <- paste0(tempfile(), '.xlsx')
    df <-  clipr::write_last_clip()
    openxlsx::write.xlsx(df, file = temp_file)
    invisible(system(paste(open_command, temp_file),
                     ignore.stdout = TRUE, ignore.stderr = TRUE))
}

