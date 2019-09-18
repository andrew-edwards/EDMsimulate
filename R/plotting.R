##' @title Create 2-d pdf movie of lagged points
##'
##' @description Create 2-d movie of x_t (on the y-axis) against x_{t-1}, with colours to
##' show the latest points as it grows.
##' @param x Vector of values
##' @param pdf_filename Filename (including .pdf) to save the movie
##' @param start First value to use when plotting
##' @param end Final value to use when plotting
##' @param late_num How many points to have colour 'late_col'
##' @param late_col Colour for the late points (the last 'late_num' points) for
##'   each frame
##' @param early_col Colour for the early points
##' @param early_col_lines Colour for the early lines
##' @param pt_type Type of point (the usual plot(..., type= ) option, but do not
##'   use "o".
##' @param x_lab Label for x axis
##' @param y_lab Label for y axis
##' @param figheight Figure height
##' @param figwidth Figure width
##' @param ...
##' @return Movie as a .pdf then can then be easily input into Sweave file using
##'   the animategraphics latex pacakge. [Update this regarding after testing knitr etc.]
##' @author Andrew Edwards
##' @export
##' @examples
##' \dontrun{
##'   plotLag2d(x = default_sim_seed_42$p_t3, pdf_filename = "hello.pdf")  # not
##'   a time series
##' }
plotLag2d = function(x,
                     pdf_filename,
                     start = 1,
                     end = length(x),
                     late_col = "red",
                     early_col = "black",
                     early_col_lines = "lightgrey",
                     late_num = 10,
                     pt_type = "p",
                     x_lab = expression("x"[t-1]),
                     y_lab = expression("x"[t]),
                     figheight = figheight,
                     figwidth = figwidth,
                     ...)
  {
  pdf(pdf_filename, height = figheight, width = figwidth)

  x_tmin1 = x[start:(end-1)]                    # x_{t-1}
  x_t = x[(start+1):end]                        # x_{t}
  axes_range = range(c(x_tmin1, x[end]))
  num_frames = length(x_t)
  for(i in 1:num_frames)
    {
      col_plot = c( rep(early_col, max(c(0, i-late_num))),
                    rep(late_col, min(c(i, late_num))) )   # colours of points
      #col_plot_lines = (col_plot == early_col) * early_col_lines +
      #                   (col_plot == late_col) * late_col
      col_plot_lines = col_plot                            # colours of lines
      col_plot_lines[col_plot_lines == early_col] = early_col_lines
      pch_plot = (col_plot == early_col) * 1 + (col_plot == late_col) * 16
                                          # filled circles for latest
      plot(x_tmin1[1:i], x_t[1:i],
        xlab = x_lab, ylab = y_lab,
        xlim = axes_range, ylim = axes_range,
        type = pt_type, pch = pch_plot,
        col = col_plot)
      if(i > 1.5)
        {  segments(x_tmin1[1:(i-1)], x_t[1:(i-1)],
               x_tmin1[2:i], x_t[2:i],
               col = col_plot_lines)       # lines() will not use vector of col
        }
      legend("topleft", legend=paste("sample", i), border = NULL)
    }
  dev.off() # close pdf device
}
