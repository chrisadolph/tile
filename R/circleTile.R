"circleTile" <-
function(...){  
  args <- list(...,graphic="circle")
  class(args) <- c(class(args),"tileTrace","circle")
  args
}

