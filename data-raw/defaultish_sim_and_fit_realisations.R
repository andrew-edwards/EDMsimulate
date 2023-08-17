# Defaultish values for sim_and_fit_realisations(). Want to save an output to
# then check future changes still give same results. Andy has to run this in R
# console.


# Running at commit 2719d39 before doing nu_t changes (even though this and nu_t
# changes will get rolled into the next commit).

three_sim_fits <- sim_and_fit_realisations(M = 3,
                                           larkin_fit = TRUE,
                                           ricker_fit = TRUE)

usethis::use_data(three_sim_fits,
                  overwrite = TRUE)
