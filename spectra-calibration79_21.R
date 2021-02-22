# PACKAGES
require(deSolve)
require(reshape2)
require(ggplot2)

setwd("/Users/brandonpae/Desktop/ISEF_Code") #set working directory

# Load the full dataset of spectra data
# Nomenclature: First number is the percentage of PET, second number is the percentage of LDPE (in sample)

spectra0_0 <- read.csv("spectra0_0.csv")
spectra0_100 <- read.csv("spectra0_100.csv")
spectra15_0 <- read.csv("spectra15_0.csv")
spectra16_5 <- read.csv("spectra16_5.csv")
spectra75_25 <- read.csv("spectra75_25.csv")
spectra100_0 <- read.csv("spectra100_0.csv")

# test cases
test0_0 <- read.csv("test0_0.csv")
test0_100 <- read.csv("test0_100.csv")
test10_0 <- read.csv("test10_0.csv")
test28_7 <- read.csv("test28_7.csv")
test79_21 <- read.csv("test79_21.csv")
test100_0 <- read.csv("test100_0.csv")

#input all previously obtained spectra datasets into a list
spectra_values = list(spectra0_0, spectra0_100, spectra15_0, 
                      spectra16_5, spectra75_25, spectra100_0)

new_spectra = test79_21 #set the unknown spectra to test for

sum_of_squares <- function(new_data, stored_data) { #input the new and stored spectra data to compare
  
  #select from stored_data$Reflectance where the wavelength matches the wavelength in the new_data
  #sum of squares calculation (taking the difference in the functions for each wavelength and squaring it)
  deltas2 <- (stored_data$Reflectance[stored_data$Wavelength %in% new_data$Wavelength] - new_data$Reflectance)^2   
  
  SSQ <- sum(deltas2) #return the sum of squares
  
  return(SSQ)
}

#once we get the SSQ (sum of squares) value, we need to compare it with the SSQ values from
#the rest of the graphs and find the smallest value
least_squares <- function() {
  
  least_SSQ = 10^10 #start with a large SSQ value
  closest_spectra = spectra_values[1] #default spectra
  
  for(spectra in spectra_values) { #for each spectra dataset
    
    SSQ_value = sum_of_squares(new_spectra, spectra) #using the SSQ function to compare new graph to stored graphs
    
    if (SSQ_value < least_SSQ) { #if the graph is a closer fit
      least_SSQ = SSQ_value 
      closest_spectra = spectra #set this graph as the new closest spectra
    }
  }

  return(closest_spectra) #return the stored spectra graph that is the closest fit to the current test one
}

best_spectra = least_squares() #returns the closest spectra, from which we can determine the percent composition

#blue is the new data
#black is the stored data

plot = ggplot() +
  geom_line(data = new_spectra, aes(x = Wavelength, y = Reflectance), colour = "blue") + 
  geom_line(data = best_spectra, aes(x = Wavelength, y = Reflectance), colour = "black") + 
  xlab("Wavelength (nm)") +
  ylab("Reflectance") +
  labs(title = "Wavelength vs. Reflectance Spectra for 79% PET, 21% LDPE, 0% KOMBU") + #add a title
  labs(color = "Compartments") +
  theme(legend.position = "bottom")

plot
