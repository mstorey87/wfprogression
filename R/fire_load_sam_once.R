# In zzz.R or a new file like R/sam_loader.R

# Package-level environment to store session state
.fire_env <- new.env(parent = emptyenv())
.fire_env$sam_loaded <- FALSE

#' Internal: Load SAM model once
#'
#' Loads the Python SAM2 model using reticulate. This runs only once per R session.
#' It sets a flag in a package-private environment.
#'
#' @return Invisibly returns TRUE.
#' @noRd
fire_load_sam_once <- function() {
  if (!.fire_env$sam_loaded) {

    message("loading SAM module")
    #reticulate::use_python("C:/Users/mstorey/AppData/Local/Programs/Python/Python313/python.exe", required = TRUE)
    #reticulate::use_condaenv(condaenv = "sam2_env")
    reticulate::py_run_string("
import torch
import numpy as np
import cv2
import os
import sys

from sam2.build_sam import build_sam2
from sam2.sam2_image_predictor import SAM2ImagePredictor


# --- Find the conda environment folder ---
conda_prefix = sys.prefix
checkpoints_dir = os.path.join(conda_prefix, 'checkpoints')

# --- Define model paths ---
sam2_checkpoint = os.path.join(checkpoints_dir, 'sam2.1_hiera_tiny.pt')
model_cfg = os.path.join(checkpoints_dir, 'sam2.1_hiera_tiny.yaml')

#sam2_checkpoint = r'C:\\Users\\mstorey\\sam2\\checkpoints\\sam2.1_hiera_base_plus.pt'
#model_cfg = r'C:\\Users\\mstorey\\sam2\\checkpoints\\sam2.1_hiera_b+.yaml'

if torch.cuda.is_available():
    device = torch.device('cuda')
elif torch.backends.mps.is_available():
    device = torch.device('mps')
else:
    device = torch.device('cpu')

sam2_model = build_sam2(model_cfg, sam2_checkpoint, device=device)
predictor = SAM2ImagePredictor(sam2_model)
")
    .fire_env$sam_loaded <- TRUE
  }
  invisible(TRUE)
}
