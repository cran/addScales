\name{update.scaledTrellis}
\alias{update.scaledTrellis}

\title{
   Update Method for scaledTrellis Objects
}
\description{
   Updates both \code{addScale} parameters, including those passed to the \code{panelFUN},
   and components of the original \code{trellis} object.
}
\usage{
   \method{update}{scaledTrellis}(object, \dots)
}

\arguments{
  \item{object}{
\code{scaledTrellis} object: The object on which method dispatch is carried out.
}
  \item{\dots}{
  Any number of name = value pairs giving arguments that will be used to update \code{object}
   }
}
\details{
   Arguments to \code{addScales} and those to (the most recent version of) the original
   \code{trellis} object are separated and \code{update.trellis} is first called on the latter.
   This means that any \code{trellis} argument changes must satisfy the restrictions
   on what \code{update.trellis} can change, basically, anything but
   the data used for plotting. \emph{All} \code{addScales} and \code{panelFUN}
   parameters can be changed as long as such changes are possible
   (e.g. no scale lines can be added for factors.
}

\value{
   The updated object of class \code{c("scaledTrellis","trellis")} if successful.\cr
   The unmodified object if an error occurs.
}
\author{
   Bert Gunter \email{bgunter.4567@gmail.com}
}

\seealso{
   \code{\link{update.trellis}}, \code{\link{addScales}},
   \code{\link{panel.addScales}}
}
\examples{
## Replicate the USAcrimes example in ?addScales
##
data(USAcrime)
state.smpl <- c("CA","TX","GA","CO","VA","FL","NY","OH","MO","UT","MA","TN")
wh <- USAcrime$State \%in\% state.smpl
pcols <- hcl.colors(n = 55, rev = TRUE)
crm <-xyplot(allViolent ~ allProperty|State, data = USAcrime[wh,],
             subscripts = TRUE,
             as.table = TRUE,
             layout = c(4,3), type = c("p", "g"),
             cex= .75,  pch = 19,
             col = pcols[USAcrime[wh,'Year'] -1959],
             par.strip.text = list(lines = .7, cex = .7),
             between = list(x = 1),
             scales = list(axs="i",relation = "free", draw = FALSE),
             xlab = "Property Crime Rate (incidents/100,000 population)",
             ylab = "Violent Crime Rate (incidents/100,000 population)",
             main = paste0("Violent vs. Property Crime Rates from 1960-2014 For 12 States"),
             sub = "Point Darkness Codes Years: Darker = Later Years",
             panel = function(subscripts,col,...)
                panel.xyplot(col = col[subscripts],...)
   )
crm
ads.1 <- addScales(crm, scaleline = TRUE)
ads.1 ## plot it

## Change the plotting symbol, add a fitted line to the panel,
## remove the grid, change the layout,
## make the midline color black, thicken the scaleline dashes,
## and put the legend on the right.
##
## Note that the arguments can be given in any order.
## (automatically plotted since no return value)
update(ads.1, pch = 19,layout = c(3,4), type = "p",
       col.midline = "black", legend.loc = "right",
       panel = function(x, y,  ...){
          panel.xyplot(x, y,...)
          panel.abline(reg = lm(y ~ x), col = "darkred", lwd = 2)
       },
       scaleline.aes = list(lwd = 2)
)
## cleanup
rm(ads.1, crm, pcols, wh, state.smpl)
}
\keyword{hplot}
\keyword{aplot}
\keyword{methods}
