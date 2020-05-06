\name{panel.addScales}
\alias{panel.addScales}
\title{
Default panelFUN For addScales.trellis
}
\description{
Adds Labeled Midline and (Unlabeled) Scale Lines to Trellis Panels.
}
\usage{
   panel.addScales(
   scaleline = c(0,0),
   ndig.midline = c(h = 2, v = 2),
   col.midline = "red",
   adj.midline = c(-0.1, -0.25),
   midline.aes = list(lwd = 1.5),
   midline.label.aes = list(fontface = "bold", cex = 0.8),
   scaleline.aes = list(lty = "dashed", col = "purple"),
   \dots
   )
}
\arguments{
  \item{scaleline}{
      numeric: A numeric vector of length 1 or 2 giving the distance from the scalelines
      to the midline(s). Signs are ignored. If two values are given, the first is the
      (vertical) distance to the horizontal midline and the second is the (horizontal)
      distance to the vertical midline. If a single value is given,it is assumed to be the
      first and the second is 0. 0 or NA mean: don't add that scaleline and midline.
   }
  \item{ndig.midline}{
Named or unnamed pair of integer arguments, or a single integer that will be replicated. The names must be (and are assumed to be if unnamed) "h" and "v" \emph{in that order} and give the number of \emph{significant} digits to show in the midline labels for the corresponding midlines. Non-integer values are rounded to integer, and values outside the range of 0 to 15
digits are converted to 2. However, see the \bold{Details} section below for a caveat.
}
  \item{col.midline}{
      Midline color. Can be given in any form suitable for the base \code{col}
      parameter of \code{par}.
   }
  \item{adj.midline}{
      numeric: The \code{adj} vector of length two that will be fed to \code{panel.text}
      to position the midline label with respect to the midline. \code{c(.5,.5)} centers
      the label on the midline and lower limit of the relevant axis
      (x-axis for a horizontal midline, so left-center; and y-axis for a vertical
      midline, so bottom-center). See the base \code{\link{text}} function for details.
   }
  \item{midline.aes}{
      List of \emph{aesthetics} of midline: \code{lwd, lty, alpha}, but
      \bold{not} \code{col}, as this is already specified in the
      \code{col.midline} argument.
   }
  \item{midline.label.aes}{
      List of \emph{aesthetics} of the midline label: \code{cex, font,
      fontface, etc.}, but not \code{col} or \code{adj}, as these are
      handled separately. See \code{\link{panel.text}} for details.
   }
  \item{scaleline.aes}{
      List of \emph{aesthetics} of the scale lines. Same as for
      \code{midline.aes} except that \code{col} can be specified.
   }
   \item{\dots}{
   Needed only to avoid panel error messages for unused arguments
   passed down to the \code{panelFUN} function from the
   \code{addScales} call.
   }
}

\value{
   No value is returned. This panel function is added to the existing panel
   function component of the \code{trellis} object on which \code{addScales}
   dispatches. It adds midline(s) and scalelines to the panels when they are
   plotted.
}
\details{
   Midline values with significant digits that change precision by
   \dQuote{small} amounts relative to the \code{scaleline} values are
   \emph{zapped} by the \code{zapsmall()} function to remove extra
   digits in the display to improve readability. For example,
   a \code{midline} value of 1.23 would be shown as 1 when
\code{ndig.midline = 2} and \code{scaleline} = 100.
}

\author{
Bert Gunter \email{bgunter.4567@gmail.com}
}

\note{
   For the \code{xxx.aes} arguments, when the user explicitly specifies the list, any component not specified will default to its formal panel.addScales argument value if that exists, or to the relevant panel function default if not, i.e. \code{panel.refline} for the lines and \code{panel.text} for the labels.
}
\seealso{
\code{\link{colors}}, \code{\link{par}}, \code{\link{panel.refline}},
\code{\link{panel.text}}, \code{\link{text}}
}

\keyword{ hplot }
\keyword{ aplot }
