#!/usr/bin/env Rscript

source("renv/activate.R")
library(callr)
library(kiwiDecoder)

#index_root <- "N:/Projects/Stage2 Clonal Trials/Images/A Auto-Naming/1 Stage 2 Base Photos/1. Orchard photos/2021_22/Flower Buds/1_Superceded/Phil Copy"
index_root <- "C:/Users/Kathy.Ruggiero/OneDrive - The Kiwifruit Breeding Centre/Documents/kbc/projects/other/rename-images/images"

bg_job <- callr::r_bg(
  func = function(root) {
    source("renv/activate.R")
    library(kiwiDecoder)

    resolve_folder_sequence(root)
  },
  args = list(root = index_root)
)

print(bg_job)   # Shows PID, running status
