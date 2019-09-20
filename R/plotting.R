##' @title Create 2-d pdf movie of lagged points. Now trying gifski approach instead.
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




# Copying over, probably need some bits divided out into smaller functions, but
# this does everything so start with it.
plotPanelMovie.X = function(Nx.lags, pdf.filename, rhoForE = NULL,
                     Evec = NULL, Ecols = NA,
                     includeTimeSeries = TRUE,
                     start = 1,
                     end = nrow(Nx.lags),
                     only.final.plot = FALSE,
                     cobwebbing = TRUE,
                     late.col = "red", early.col = "black",
                     early.col.lines = "lightgrey",
                     late.num = 10,
                     pt.type = "p",
                     x.lab = expression("X(t-2)"),
                     y.lab = expression("X(t-1)"),
                     z.lab = expression("X(t)"),
                     figheight = figheight, figwidth = figwidth,
                     open.pdf = TRUE,
                     ...)
  {
  # Create figure with six panels as a movie, using newer X(t) notation.
  # Will be:
  #  N(t) vs time and X(t) vs time (optional)
  #  N(t) vs N(t-1), X(t) vs X(t-1) (like plotLag2d),
  #  X(t) vs X(t-1) vs X(t-2) (like plotLag3d) and
  #    pred vs obs (for given values of E, if provided).
  #  Don't think can just modify those functions to call them with wrapper
  #   functions, so copy the code here and use in each panel.
  #  With colours to show the latest points, and star to highlight latest point.
  # Args:
  #   Nx.lags: dataframe with time indexing the row, and columns named
  #         Nt, Ntmin1, Xt, Xtmin1, Xtmin2 and, if E!=0, columns XtPredEeq1,
  #         XtPredEeq2, etc.
  #       This allows Xt to equal Nt if want to plot figures
  #         without the first differencing (basically the 3d plot and the pred
  #         vs obs would be the only new ones). Nt is the original data,
  #         Xt is the first difference, and Xt.est is the estimate for Xt from
  #         EDM. Row numbers are time, and start at 1.
  #      Could maybe extend to have multiple Xt.est for the different
  #         embedding dimensions. ***
  #   pdf.filename: filename (including .pdf) to save the movie as
  #   Evec: vector of E values used to construct columns in Nx.lags called
  #     XtPredEeq1, XtPredEeq2 etc. (Xt predicted with E=1, etc.). If NULL
  #     then no predicted vs observed plot is drawn.
  #   rhoForE: values of rho corrsponding to each E in Evec
  #   Ecols: colour coding for E points in predicted vs observed plot
  #   includeTimeSeries: TRUE to include the time series at the top (FALSE hasn't
  #     been tested yet**)
  #   start: first time value (row) to use when plotting - not fully implemented
  #     as the colours aren't correct. Gives warning if != 1.
  #   end: last time value (row) to use when plotting (so will have end-start
  #     points plotted)
  #   only.final.plot: if TRUE then only plot the final plot
  #   cobwebbing: whether to join up points in 2d lag plots via cobwebbing;
  #     if false then joins consecutive plots
  #   late.col: colour for the late points (the last 'late.num' points)
  #     for each frame
  #   early.col: colour for the early points
  #   early.col.lines: colour for the early lines
  #   pt.type: usual 'type' option (but don't use "o")
  #   late.num: how many points to have colour 'late.col'
  #   x.lab, y.lab, z.lab: labels for axes for 3d lagged plots - for 2d
  #     lagged plots z.lab is the y label and y.lab is the x label.
  #   figheight, figwidth: height and width of .pdf, but ignored if
  #     open.pdf == FALSE.
  #   open.pdf: whether or not to open a new .pdf file for the figure. Use
  #     FALSE if .pdf is being opened automatically in knitr (then figheight
  #     and figwidth get ignored, and should be set in <<..>>=.
  # Returns:
  #   Creates movie as a .pdf then can then be easily input into Sweave file
  #     using the animategraphics latex pacakge.
  #  max.time = nrow(Nx.lags.df)
  #  time = 1:max.time
  if(start != 1) warning("Not properly implemented yet for start > 1;
                                colours won't work.")
  Nx.lags = Nx.to.NX.format(Nx.lags)    # converts older format xt headings to Xt
  Nx.lags = Nx.lags    # keep as Nx.lags for now - columns need to be correct

  # Axes ranges:
  Nt.max.abs = max( abs( range(Nx.lags[start:end, "Xt"], na.rm=TRUE) ) )
  Nt.axes.range = c(0, Nt.max.abs*1.04)         # Expand else points can hit edge

  Xt.max.abs = max( abs( range(Nx.lags[start:end, "Xt"], na.rm=TRUE) ) )
  Xt.axes.range = c(-Xt.max.abs, Xt.max.abs)    # Make axes symmetric, though
                                                #  axs="i" doesn't work for 3d
  if(open.pdf) pdf(pdf.filename, height = figheight, width = figwidth)

  first.fig = ifelse( only.final.plot, end, start)
                                                # if only doing final plot then
                                                #  make just one figure in loop:
  for(iii in first.fig:end)    # Use iii here, which is now the row number
    {
      # Loops: iii=1 will only have pred vs obs,
      #        iii=2 will only have pred vs obs and 2d plots
      #        iii=3 onwards will have all plots
      ifelse(includeTimeSeries, par(mfrow = c(3,2)), par(mfrow = c(2,2)))
#      par.mai = c(0.7, 0.8, 0.1, 0.1)
#      par(mai = par.mai)# Usually affects all figures but scatterplot3d resets
                        #  so have to set this again later. Actually it affects
                        #  mar
      # Set mar, the numbers of lines of margins, default c(5, 4, 4, 2) + 0.1.
      par.mar.ts = c(3, 3, 1, 1)         # For time series
      par.mar.phase = c(3, 0, 1, 0)      # For phase plots (3d sets it anyway)
      par.mar.3d = c(3, 0, 0, 0)

      par.mgp.3d = c(3, 10, 0)
      par.mgp = c(1.5, 0.5, 0)
      par("mgp" = par.mgp) # first val sets axis title dist (mex units)
                           #  to axes, second sets labels


      i = iii-1       # to not have to change indexing from plotLag2d()

      # Colour vector for all plots - it will correspond
      #  to the last late.num times, not points (since different plots have
      #  different numbers of points). So these now correspond to times from
      #  start:iii , and so each needs to have length iii-start+1 (maybe not
      #  lines all get used):
      col.plot = c( rep(early.col, max(c(0, iii-late.num-start+1))),
                    rep(late.col, min(c(iii, late.num))) )   # colours of points
      col.plot.lines = col.plot                            # colours of lines
      col.plot.lines[col.plot.lines == early.col] = early.col.lines
      pch.plot = (col.plot == early.col) * 1 + (col.plot == late.col) * 16
                                         # filled circles for latest
      pch.plot[length(pch.plot)] = 8     # latest one a star

      if(includeTimeSeries)
        {
          par(pty = "m")                 # maximal plotting region, not square
                                         #  like for phase plots
          par(mar = par.mar.ts)
          # N(t) vs t:
          plot(0, 0,
               xlab = expression("Time, t"),
               ylab = expression("N(t)"),
               xlim = c(start,end), ylim = Nt.axes.range,
               type = "n",
               main = paste0("Time t=", iii))       # empty plot
          if(iii > 1.5)
            {
               segments( start:(iii-1), pull(Nx.lags[start:(iii-1), "Nt"]),
                       (start+1):iii, pull(Nx.lags[(start+1):iii, "Nt"]),
                        col = col.plot.lines) # lines() will not use vector col
            }
           points(start:iii, pull(Nx.lags[start:iii, "Nt"]),
                  type = pt.type, pch = pch.plot,
                  col = col.plot)
           # X(t) vs t:
          XtLoc = -0.05 * end        # location to plot Xt on a vertical line,
                                     #  needs correcting if start>1
          plot(0, 0,
               xlab = expression("Time, t"),
               ylab = expression("X(t)"),
               xlim = c(XtLoc,end), ylim = Xt.axes.range,
               type = "n")                           # empty plot
          abline(v = 0.5*XtLoc, col="black")
          if(iii > 1.5)
            {
               segments( start:(iii-1), pull(Nx.lags[start:(iii-1), "Xt"]),
                       (start+1):iii, pull(Nx.lags[(start+1):iii, "Xt"]),
                        col = col.plot.lines) # lines() will not use vector col
            }
           points(start:iii, pull(Nx.lags[start:iii, "Xt"]),
                  type = pt.type, pch = pch.plot,
                  col = col.plot)
                                                     # '1d phase plot':
           points(rep(XtLoc, iii-start+1), pull(Nx.lags[start:iii, "Xt"]),
                  type = pt.type, pch = pch.plot,
                  col = col.plot)

        }                                            # end if(includeTimeSeries)

      par(pty="s")             # set following plot types to be square
                               #  (without this the axes don't seem to
                               #  be the same, even with the settings below)
      par(mar = par.mar.phase) # margins
      # N(t) vs N(t-1):
      # Empty plot to get started, that's it for iii=0:
      plot(0, 0,
           xlab = expression("N(t-1)"),
           ylab = expression("N(t)"),
           xlim = Nt.axes.range, ylim = Nt.axes.range,
           type = "n")
      if(cobwebbing) abline(0, 1, col="darkgrey")
      # Draw lines first so they get overdrawn by points
      if(iii > 2.5)
        {
          if(cobwebbing)
            {
              # Do lines for cobwebbin
              Nvals = rep( pull(Nx.lags[start:iii, "Nt"]), each = 2)
              Nvals = Nvals[-1]
              Nvals = Nvals[-length(Nvals)]
              len = length(Nvals)
              col.cobweb.lines = rep(early.col.lines, len)
              col.cobweb.lines[(max( (len - 2*late.num + 1), 1)):len] = late.col
              segments(Nvals[1:(len-2)],
                       Nvals[2:(len-1)],
                       Nvals[2:(len-1)],
                       Nvals[3:len],
                       col = col.cobweb.lines)
            } else
            {
              # Join each point to the next
              segments(pull(Nx.lags[start:(iii-1), "Ntmin1"]),
                       pull(Nx.lags[start:(iii-1), "Nt"]),
                       pull(Nx.lags[(start+1):iii, "Ntmin1"]),
                       pull(Nx.lags[(start+1):iii, "Nt"]),
                       col = col.plot.lines) # lines() will not use vector of col
            }
        }
      if(iii > 1.5)
        {
          points(pull(Nx.lags[start:iii, "Ntmin1"]),
               pull(Nx.lags[start:iii, "Nt"]),
               type = pt.type, pch = pch.plot,
               col = col.plot)          # start row has NA's, gets ignored
        }
      # legend("topright", legend=paste0("Time t=", iii), box.col = "white",
      #        inset = 0.01)  # inset to stop white overwriting outer box


      # X(t) vs x{t-1}:

      # Empty plot to get started:
      plot(0, 0,
           xlab = y.lab, ylab = z.lab,
           xlim = Xt.axes.range, ylim = Xt.axes.range,
           type = "n")
      if(cobwebbing) abline(0, 1, col="darkgrey")
      if(iii > 2.5)
        {
          if(cobwebbing)
            {
               xvals = rep( pull(Nx.lags[start:iii, "Xt"]), each = 2)
               xvals = xvals[-1]
               xvals = xvals[-length(xvals)]
               lenx = length(xvals)
               segments(xvals[1:(lenx-2)],
                        xvals[2:(lenx-1)],
                        xvals[2:(lenx-1)],
                        xvals[3:lenx],
                        col = col.cobweb.lines)
            } else
            {  # Just join consecutive points with lines
               segments(pull(Nx.lags[start:(iii-1), "Xtmin1"]),
                        pull(Nx.lags[start:(iii-1), "Xt"]),
                        pull(Nx.lags[(start+1):iii, "Xtmin1"]),
                        pull(Nx.lags[(start+1):iii, "Xt"]),
                        col = col.plot.lines)
            }
        }
      if(iii > 1.5)
        {
          points(pull(Nx.lags[start:iii, "Xtmin1"]),
                 pull(Nx.lags[start:iii, "Xt"]),
                 type = pt.type, pch = pch.plot,
                 col = col.plot)           # start row has NA's, get ignored
        }
      # legend("topleft", legend=paste("Time", iii), border = NULL)

      # 3d plot of X(t) vs X(t-1) vs X(t-2):
      # Empty plot to get started
      par(mgp = par.mgp.3d)
      par(mai = c(0.1, 0.1, 0.1, 0.1)) # scat..3d resets mar, think mai still has an effect
      scat = scatterplot3d(0, 0, 0,
                xlab = x.lab, ylab = y.lab, zlab = z.lab,
                xlim = Xt.axes.range, ylim = Xt.axes.range, zlim = Xt.axes.range,
                type = "n", box = FALSE,
                angle = 40 + iii,
                mar = par.mar.3d)
                                              # mar=c(5,3,4,3)+0.1 is default,set
                                              #  within scatterplot3d
                                              # Add axes so can see origin:
      actual.axes.ranges = gets3dusr(scat)    # Get the actual values used
      scat$points3d(actual.axes.ranges[1:2], c(0,0), c(0,0), type = "l",
                    col = "lightgrey")
      scat$points3d(c(0,0), actual.axes.ranges[3:4], c(0,0), type = "l",
                    col = "lightgrey")
      scat$points3d(c(0,0), c(0,0), actual.axes.ranges[5:6], type = "l",
                    col = "lightgrey")
      # Obtain x-y co-ords of points for segments:
      proj.pts = scat$xyz.convert(
                                  pull(Nx.lags[start:iii, "Xtmin2"]),
                                  pull(Nx.lags[start:iii, "Xtmin1"]),
                                  pull(Nx.lags[start:iii, "Xt"]) )
      if(iii > 3.5)
        {   # Think the indexing will now be 1:(iii-start), need start value also
            segments(proj.pts$x[1:(iii-start)], proj.pts$y[1:(iii-start)],
                    proj.pts$x[2:(iii-start+1)], proj.pts$y[2:(iii-start+1)],
                    col = col.plot.lines) # lines() will not use vector
                                          #  of col
        }
      # The points
      if(iii > 2.5)
        {
          scat$points3d(pull(Nx.lags[start:iii, "Xtmin2"]),
                        pull(Nx.lags[start:iii, "Xtmin1"]),
                        pull(Nx.lags[start:iii, "Xt"]),
                        type = pt.type, pch = pch.plot,
                        col = col.plot)
        }


#      par(scat$par.mar)    # should do the same as:
      par(mar = par.mar.phase)   # scatterplot3d changes mar
      par(mgp = par.mgp)   # back to usual for 2d figures
      # Predictions vs observations for E values in Evec
      if(!is.null(Evec))
        {
          all.pred = select(Nx.lags, starts_with("XtPredEeq"))
          pred.max.abs = max( abs( range(all.pred, na.rm=TRUE) ) )
          pred.max.abs = max(pred.max.abs, Xt.max.abs)  # Latter is observed
          predObs.axes.range = c(-pred.max.abs, pred.max.abs)

          plot(0, 0,
               xlab = expression("Observation of X(t)"),
               ylab = expression("Prediction of X(t)"),
               xlim = predObs.axes.range,
               ylim = predObs.axes.range,
               asp = 1,
               type = "n")
          abline(0, 1, col="grey")
          leg = vector()
          for(j in 1:length(Evec))
            {
               points(select(Nx.lags[start:iii,], Xt, paste0("XtPredEeq", j)),
                      pch=pch.plot, col=Ecols[j])
               leg = c(leg,
                       paste0("E=", j, ", rho=", round(rhoForE[j], 2)))
            }
          legend("topleft", pch=c(20, 20, 20),
                 leg, col=Ecols, cex=0.7)
       }                           # if(Evec != 0)
  }                                # for(iii in start:end)
  if(open.pdf) dev.off() # close pdf device
}
