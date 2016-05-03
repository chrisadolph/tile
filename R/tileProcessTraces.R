tileProcessTraces <- function(traces) {

    # Unbundle,  # Make new flat list and check for mimimal requirements
    newtraces <- flattenTileTraces(traces)  
    ntraces <- length(newtraces)
    
    # Loop over all traces; fill in defaults; determine number of trace-plots    
    ntp <- 0
    for (i in 1:ntraces) {

        # Set trace default options
        tracedefault <- eval(call(paste0(newtraces[[i]]$graphic,
                                         "TileTraceDefaults")))

        # Fillout trace options
        newtraces[[i]] <- eval(call(paste0(newtraces[[i]]$graphic,
                                          "TileTraceFillout"),
                                    newtraces[[i]], tracedefault))

        if (is.null(newtraces[[i]]$plot)) {
            newtraces[[i]]$plot <- i
        }
        
        ntp <- ntp + length(newtraces[[i]]$plot)
    }
    tracesout <- vector("list",ntp)
    
    # Loop over traces; process for plotting
    ip <- 0
    for (i in 1:ntraces) {

        # Optional special treatment based on graphic type
        newtraces[[i]] <- eval(call(paste0(newtraces[[i]]$graphic,
                                          "TilePrep"),
                                    newtraces[[i]]))
        
        # Create one copy of the trace for each plot it appears in
        currplots <- newtraces[[i]]$plot
        for (k in 1:length(currplots)) {
            ip <- ip+1
            tracesout[[ip]] <- newtraces[[i]]
            tracesout[[ip]]$plot <- currplots[k]
        }
    }
    return(tracesout)
}
