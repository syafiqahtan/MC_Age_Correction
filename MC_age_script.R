  # code created by Syaf, cleaned up for checking on 4Jan 2024

# MC Correction for Tekukor and Lazarus

library(tidyr)
library(hdrcde)

# Opening and creating files to save results ---
  # reading the parameters (measured from cores)
fossil_params<- readxl::read_xlsx(file.path(".", "montecarlo_params.xlsx"))
  # creating new columns
  fossil_params$mean <- 0
  fossil_params$width_1sig <- 0 
  # Creating a blank dataframe to save results
results <- as.data.frame(matrix(0, ncol = nrow(fossil_params), nrow = 1000000))

# correcting for diplo -----

for (p in 1:1000000) {
  for (i in 1:5) {
    repeat {
      
      # defining the variables
      
      # growth rate
      # this stays constant for all the cores
      # choosing a random number from a normal distribution (4,1)
      # published growth rates for diplo are 2-6 mm/yr
      
      GR <- rnorm(1, 4, 1) # units are in mm/yr
      # Check the condition
      if (GR >= 0.5) {
        # continue with rest of calculation and break the repeat loop
        # U-Th ages
        sample_age_depth_median <- as.numeric(fossil_params[i,5]) 
        sample_age_depth_stdev <- as.numeric(fossil_params[i,7])
        # choosing a random age from the normal distribution
        sample_age_depth <- rnorm(1, sample_age_depth_median, sample_age_depth_stdev)
        
        # coral growth direction (this follows a uniform distribution)
        angle_min_degree <- as.numeric(fossil_params[i,2]) #[row,column]
        angle_max_degree <- as.numeric(fossil_params[i,3]) #[row,column]
        # convert degree to radian for the cosine function
        angle_min_rad <- angle_min_degree * 0.0174533
        angle_max_rad <- angle_max_degree * 0.0174533
        # choosing a random angle from the uniform distribution
        angle <- runif(1, angle_min_rad, angle_max_rad)
        
        # sample depth
        # this is a constant and never changes
        depth <- as.numeric(fossil_params[i,4]) * 10 # to convert from cm to mm
        
        sample_age_top <- sample_age_depth - ((depth*cos(angle))/GR)
        results[p,i] <- sample_age_top
        break
      }
    }
  }
}

# correcting for porites ----

# change for min max correction and LZRS-F5 IN

for (p in 1:1000000) {
  for(i in 6:19) { 
    repeat {
      # Growth rate
      GR <- rnorm(1, 15, 5) # mm/yr
      
      if (GR >= 0.5) {
        # U-Th ages
        sample_age_depth_median <- as.numeric(fossil_params[i, 5])
        sample_age_depth_stdev <- as.numeric(fossil_params[i, 7])
        sample_age_depth <- rnorm(1, sample_age_depth_median, sample_age_depth_stdev)
        
        # Coral growth direction
        # since the growth bands cannot be seen in the few corals mentioned, assume the full correction i.e. angle = 0deg
        angle_min_degree <- as.numeric(fossil_params[i, 2])
        angle_max_degree <- as.numeric(fossil_params[i, 3])
        angle_min_rad <- angle_min_degree * 0.0174533
        angle_max_rad <- angle_max_degree * 0.0174533
        angle <- runif(1, angle_min_rad, angle_max_rad)
        
        # Sample depth
        depth <- as.numeric(fossil_params[i, 4]) * 10 # mm
        
        # Calculate sample_age_top
        sample_age_top <- sample_age_depth - ((depth * cos(angle)) / GR)
        
        # Store the result and exit the repeat loop
        results[p, i] <- sample_age_top
        break
      }
    }
  }
}

# from the results df, getting the mean and 95% prob in the HDR

for (q in 6:19) {
 
  hdr_results <- hdr(results[,q], prob = 95)
  fossil_params[q,8] <- (hdr_results[["hdr"]][,1] + hdr_results[["hdr"]][,2])/2 # this is the mean of the HDR
  fossil_params[q,9] <- (hdr_results[["hdr"]][,2] - hdr_results[["hdr"]][,1])/4 # 1 sigma
}

# saving the results df as .Rda to extract pd if needed ---
# save(results,file="MC_results.Rda")

# saving fossil_params with the extrapolated ages ----
# write_xlsx(fossil_params, "sample_age_correction.xlsx")

# loading file from previous runs ----
# load("MC_results_v4.Rda") #opens a file called results_copy
#
