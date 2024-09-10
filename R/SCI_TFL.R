#' Make Table1
#'
#' This function is used to make Table1 and return excel file.
#' @title Table1
#' @rdname Table1
#' @param df a data.frame
#' @param ycol a grouping variable
#' @param xcol variables to be compared
#' @param xlabels levels of ycol
#' @param result_dir directory to save the result
#' @param verbose logical, controlling the output
#'
#' @return excel file
#' @author Zhen Lu
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @import boot
#' @export
#'
#' @examples
#' \donttest{
#' data("melanoma", package = "boot")
#' melanoma2 <- melanoma
#' # Factor the basic variables that
#' # we're interested in
#' melanoma2$status <-
#'   factor(melanoma2$status,
#'          levels=c(2,1,3),
#'          labels=c("Alive", # Reference
#'                   "Melanoma death",
#'                   "Non-melanoma death"))
#' test= Table1(
#'   df= melanoma2,
#'   xcol= setdiff(names(melanoma2), "status"),
#'   ycol= "status",
#'   result_dir= tempdir()
#' )
#' }
Table1<-function(df,ycol,xcol,xlabels,result_dir,verbose=TRUE){
  if(any(c(missing(df),missing(xcol),missing(ycol)))){
    return("parameters missing! arg1 data.frame, arg2 ycol, arg3 xcol")
  }
  else if(!all(c(is.data.frame(df),is.character(xcol),is.character(ycol)))==TRUE){
    return("parameter type error! arg1 data.frame, arg2,arg3 character vector")
  }

  if(nrow(df)>5000){
    shapiro.test= function(x) stats::ks.test(x, 'pnorm')
  }

  xlabels= levels(df[[ycol]])
  df[[ycol]]= factor(df[[ycol]],levels= c(xlabels,'P-value'), labels=c(xlabels,'P-value'))
  levels= (nlevels(df[[ycol]])-1) %>% as.numeric()
  rndr <- function(x, name, ...) {
    if (length(x) == 0) {
      y <- df[[name]]
      s1 <- rep("", length(table1::render.default(x=y, name=name, ...)))
      if (is.numeric(y)) {
        if ((levels-2)==0){
          if(shapiro.test(y[df[[ycol]]==xlabels[[1]]])$p.value > .05 &
             shapiro.test(y[df[[ycol]]==xlabels[[2]]])$p.value > .05)
          {
            if(car::leveneTest(y~df[[ycol]]) %>% `$`('Pr(>F)') %>% `[[`(1) > .05){
              p= stats::t.test(y ~ df[[ycol]],paired = FALSE, var.equal = T,
                        alternative="two.sided",conf.level=0.95)$p.value
              v= stats::t.test(y ~ df[[ycol]],paired = FALSE, var.equal = T,
                        alternative="two.sided",conf.level=0.95)$statistic
            }
            else{
              p= stats::t.test(y ~ df[[ycol]],paired = FALSE, var.equal = FALSE,
                        alternative="two.sided",conf.level=0.95)$p.value
              v= stats::t.test(y ~ df[[ycol]],paired = FALSE, var.equal = FALSE,
                        alternative="two.sided",conf.level=0.95)$statistic
            }
          }else{
            p= stats::wilcox.test(y ~ df[[ycol]],conf.level=0.95)$p.value
            v= stats::wilcox.test(y ~ df[[ycol]],conf.level=0.95)$statistic
          }
        }else{
          p1= logical(levels)
          for (i in seq(levels)) {
            p_try= try(shapiro.test(y[df[[ycol]]==xlabels[[i]]])$p.value)
            if('try-error' %in% class(p_try)){
              p1[i]= FALSE
            }else{
              p1[i]= shapiro.test(y[df[[ycol]]==xlabels[[i]]])$p.value > .05
            }
          }
          if(all(isTRUE(p1))){
            p2= (car::leveneTest(y~df[[ycol]]) %>% `$`('Pr(>F)') %>% `[[`(1)) > .05
            if(p2){
              p= stats::aov(y~df[[ycol]]) %>% summary() %>% `[[`(1) %>%
                `$`('Pr(>F)') %>% `[[`(1)
              v= stats::aov(y~df[[ycol]]) %>% summary() %>% `[[`(1) %>%
                `$`('F value') %>% `[[`(1)
            }else{
              p= stats::kruskal.test(y~df[[ycol]])$p.value
            }
          }else{
            p= stats::kruskal.test(y~df[[ycol]])$p.value
          }
        }
      } else {
        if((levels-2)==0){
          T= function(a){
            A= matrix(0, nrow(a), ncol(a))
            for(i in 1:nrow(a)){
              for(j in 1:ncol(a)){
                t1= sum(a[i,])
                t2= sum(a[,j])
                A[i,j] = t1*(t2/sum(a))
              }
            }
            A
          }
          mytable= stats::xtabs(~droplevels(y) + droplevels(df[[ycol]]))
          if(nrow(df) < 40){
            p= stats::fisher.test(mytable)$p.value
          }else{
            if(nrow(mytable)>=3){
              p= stats::chisq.test(mytable,correct = FALSE)$p.value
            }else{
              if(any(T(mytable)<1)){
                p= stats::fisher.test(mytable)$p.value
              }else{
                if(any(T(mytable)<5)){
                  p= try(stats::chisq.test(mytable,correct = T)$p.value)
                  if('try-error' %in% class(p)){
                    p= stats::chisq.test(mytable,correct = FALSE)$p.value
                  }
                }else{
                  p= stats::chisq.test(mytable,correct = FALSE)$p.value
                }
              }
            }
          }
        }else{
          chisq= descr::CrossTable(df[[ycol]], y,drop.levels = T,
                                   prop.chisq = T, chisq = T)
          # browser()
          if(dim(chisq$prop.row)[2] != 1){
            p= chisq$CST$p.value
            v= chisq$CST$statistic}
          else{
            chisq= descr::CrossTable(df[[ycol]], y,drop.levels = FALSE,
                                     prop.chisq = T, chisq = T)
            p= NA
            v= NA
          }
        }
      }
      s1[1]= p %>% round(., 3) %>% format(.,nsmall = 3) %>%
        dplyr::if_else(.=='0.000','<0.001',.)
      s1
    } else {
      table1::render.default(x=x, name=name, ...)
    }
  }

  rndr.strat <- function(label, n, ...) {
    ifelse(n==0, label, table1::render.strat.default(label, n, ...))
  }

  my.render.cont <- function(x) {
    with(table1::stats.default(x),
         if(length(table(x))==1){
           c('',`Mean &plusmn; SD`= sprintf("%.02f &plusmn; %.02f", MEAN, SD),
             `Median (Q1, Q3)` = sprintf("%.02f (%.02f, %.02f)",
                                         MEDIAN, Q1, Q3))}
         else if(shapiro.test(x)$p.value > .05){
           c('',`Mean &plusmn; SD`= sprintf("%.02f &plusmn; %.02f", MEAN, SD),
             `Median (Q1, Q3)` = sprintf("%.02f (%.02f, %.02f)",
                                         MEDIAN, Q1, Q3))
         }else{
           c('',
             `Median (Q1, Q3)` = sprintf("%.02f (%.02f, %.02f)",
                                         MEDIAN, Q1, Q3),
             `Mean &plusmn; SD`= sprintf("%.02f &plusmn; %.02f", MEAN, SD))
         })
  }

  my.render.cat <- function(x) {
    c("", sapply(table1::stats.default(x), function(y) with(y,
                                                    sprintf("%d (%0.2f)", FREQ, PCT))))
  }

  result= table1::table1(stats::as.formula(paste("~",paste(xcol,collapse = "+"),"|",ycol)),
         data=df, droplevels=FALSE, render=rndr, render.strat=rndr.strat,
         render.continuous=my.render.cont,
         render.categorical=my.render.cat,
         overall= 'Total')
  result %<>% as.data.frame() %>%
    sapply(function(x) x %>% stringr::str_replace_all(., '&plusmn;', "\u00B1")) %>%
    as.data.frame() 
  result %>%
    openxlsx::write.xlsx(
      file= file.path(result_dir,'Table1.xlsx'),
      asTable= TRUE
    )
  if(verbose){
    cat(sprintf('Table1.xlsx has been saved in your specified folder of:\n%s\n',result_dir))
  }
  return(result)
}
