##' @title Create 2-d pdf movie of lagged points
##'
##' @description Create 2-d movie of x_t (on the y-axis) against x_{t-1}, with colours to
##' show the latest points as it grows.
##' @param x Vector of values
##' @param pdf.filename Filename (including .pdf) to save the movie
##' @param start First value to use when plotting
##' @param end Final value to use when plotting
##' @param late.num How many points to have colour 'late.col'
##' @param late.col Colour for the late points (the last 'late.num' points) for
##'   each frame
##' @param early.col Colour for the early points
##' @param early.col.lines Colour for the early lines
##' @param pt.type Type of point (the usual plot(..., type= ) option, but do not
##'   use "o".
##' @param x.lab Label for x axis
##' @param y.lab Label for y axis
##' @param figheight Figure height
##' @param figwidth Figure width
##' @param ...
##' @return Movie as a .pdf then can then be easily input into Sweave file using
##'   the animategraphics latex pacakge. [Update this regarding after testing knitr etc.]
##' @author Andrew Edwards
##' @export
plotLag2d = function(x,
                     pdf.filename,
                     start = 1,
                     end = length(x),
                     late.col = "red",
                     early.col = "black",
                     early.col.lines = "lightgrey",
                     late.num = 10,
                     pt.type = "p",
                     x.lab = expression("x"[t-1]),
                     y.lab = expression("x"[t]),
                     figheight = figheight,
                     figwidth = figwidth,
                     ...)
  {
  pdf(pdf.filename, height = figheight, width = figwidth)

  x.tmin1 = x[start:(end-1)]                    # x_{t-1}
  x.t = x[(start+1):end]                        # x_{t}
  axes.range = range(c(x.tmin1, x[end]))
  num.frames = length(x.t)
  for(i in 1:num.frames)
    {
      col.plot = c( rep(early.col, max(c(0, i-late.num))),
                    rep(late.col, min(c(i, late.num))) )   # colours of points
      #col.plot.lines = (col.plot == early.col) * early.col.lines +
      #                   (col.plot == late.col) * late.col
      col.plot.lines = col.plot                            # colours of lines
      col.plot.lines[col.plot.lines == early.col] = early.col.lines
      pch.plot = (col.plot == early.col) * 1 + (col.plot == late.col) * 16
                                          # filled circles for latest
      plot(x.tmin1[1:i], x.t[1:i],
        xlab = x.lab, ylab = y.lab,
        xlim = axes.range, ylim = axes.range,
        type = pt.type, pch = pch.plot,
        col = col.plot)
      if(i > 1.5)
        {  segments(x.tmin1[1:(i-1)], x.t[1:(i-1)],
               x.tmin1[2:i], x.t[2:i],
               col = col.plot.lines)       # lines() will not use vector of col
        }
      legend("topleft", legend=paste("sample", i), border = NULL)
    }
  dev.off() # close pdf device
}
