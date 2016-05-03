\name{circleTile}
\alias{circleTile}
\title{ Add circles to a tile plot }
\description{
  Initializes a trace which plots circles to a tile graphic.  Useful for making bubbleplots with known circle areas.}

\usage{
circleTile(\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Any
  number of arguments given below.   Must include exactly one horizontal
  dimension (\code{x} or \code{top}), exactly one vertical dimension
  (\code{y} or \code{right}), and should include a vector of radii \code{r}.  All inputs should be identified by
  appropriate tags; i.e., use \code{circleTile(x=myxvar, y=myyvar, r=myradii)},
  \emph{not} \code{circleTile(myxvar,myyvar,myradii)}}
}
\details{

This function does no plotting; instead, it creates a \code{circleTile}
  object, or \dfn{trace} of plotting data, to be drawn on one or more
  plots in a tiled arrangement of plots.  To complete the drawing
  include the object as an input to \code{\link{tile}}.  From
  \code{tile}, it is possible to set further options including plot
  and axis titles, axis ranges and labels, logged axes, and
  annotations to the plot.

  \code{circleTile} simply adds a circle at a specific location or locations
  to a \code{\link{tile}} plot.  It serves as the \pkg{tile}
  equivalent to the grid graphics function
  \code{circleGrob}, and is primarily useful for annotating plots which
  also use other traces or for constructing bubble plots.  Note that \code{circleTile} traces must be
  created in advance and included in the call to \code{tile}, rather
  than added afterward.

  To plot symbols to a location outside the plotting area, users might
  try combining \code{clip="off"}, with coordinates
  placing the symbols in the desired spot.

}

\section{circleTile-specific parameters}{

 A call to \code{pointsTile} \strong{must} provide an orthogonal pair of the
  following inputs:

  \describe{
  \item{\code{x}}{coordinate vector of circles to plot, attached to the \var{x}
   axis.}
  \item{\code{y}}{coordinate vector of circles to plot, attached to the \var{y}
  axis.}
  \item{\code{top}}{coordinate vector of circles to plot, attached to the \var{top}
  axis.}
  \item{\code{right}}{coordinate vector of circles to plot, attached to
    the \var{right} axis.}
  \item{\code{r}}{radius vector of plotted circles.}
  }

Users will often wish to provide the following input:

\describe{
  \item{\code{plot}}{scalar or vector, the plot(s) in which this trace
    will be drawn; defaulting to the first plot.  Plots are numbered
    consecutively from the top left, row-by-row.  Thus in a 2 x 3
    tiling, the first plot in the second row is plot number 4.}  
}



In addition to these \code{circleTile}-specific parameters, users may provide any of the generic tile parameters documented in \code{\link{pointsTile}}.

}
  

\value{
  A \code{circleTile} object, used only as an input to \code{\link{tile}}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Christopher Adolph \email{cadolph@u.washington.edu}}

\seealso{\link{tile}, \link{pointsTile}, \link{scatter}}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{list}
