#there is no easy way to flatten such a list that is vectorizable
#so do checking of tileTrace type here
flattenTileTraces <- function(traces) {
  newtraces <- list()
  for (i in 1:length(traces)) {
    if (any(class(traces[[i]]) == "tileTrace")) {
      newtraces[[length(newtraces) + 1]] <- traces[[i]]
    } else if (any(class(traces[[i]]) == "list")) {
      for (j in 1:length(traces[[i]])) {
        newtraces[[length(newtraces) + 1]] <- traces[[i]][[j]]
      }
    } else if (any(class(traces[[i]]) != "list")) {
      stop("You have provided an object that is not a tile trace")
    }
  }
  return(newtraces)
}
