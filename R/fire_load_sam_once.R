# In zzz.R or a new file like R/sam_loader.R


# Package-level environment to store session state
.fire_env <- new.env(parent = emptyenv())
.fire_env$sam_loaded <- FALSE

#' Internal: Load SAM model once
#' @param checkpoints_dir Directory with model checkpoint pt and config yaml
#'
#' Loads the Python SAM2 model using reticulate. This runs only once per R session.
#' It sets a flag in a package-private environment.
#'
#' @return Invisibly returns TRUE.
#' @noRd
fire_load_sam_once <- function(checkpoints_dir=NULL) {
  if (!.fire_env$sam_loaded) {

    message("loading SAM module")


    if(is.null(checkpoints_dir)){
      checkpoints_dir <- tempdir()
    }

    sam2_checkpoint <- file.path(checkpoints_dir,"sam2.1_hiera_tiny.pt")
    sam2_config <- file.path(checkpoints_dir,"sam2.1",'sam2.1_hiera_t.yaml')

    if (!file.exists(sam2_checkpoint)) {

      message(paste0("checkpoints not found, downloading to ",checkpoints_dir))
      if(!file.exists(checkpoints_dir)|!file.exists(sam2_config)){
        dir.create(checkpoints_dir)
        dir.create(file.path(checkpoints_dir,"sam2.1"))
      }

      sam2_checkpoint_loc <- file.path(checkpoints_dir, "sam2.1_hiera_tiny.pt")
      download.file(  'https://dl.fbaipublicfiles.com/segment_anything_2/092824/sam2.1_hiera_tiny.pt',
                      sam2_checkpoint_loc,
                      mode = "wb")

      sam2_config_loc <- file.path(checkpoints_dir,"sam2.1", "sam2.1_hiera_t.yaml")
      download.file(  'https://raw.githubusercontent.com/facebookresearch/sam2/refs/heads/main/sam2/configs/sam2.1/sam2.1_hiera_t.yaml',
                      sam2_config_loc,
                      mode = "wb")


          reticulate::py_run_string(glue::glue("
model_cfg = r'{sam2_config_loc}'
sam2_checkpoint = r'{sam2_checkpoint_loc}'
"))


    }else{
        reticulate::py_run_string(glue::glue("
model_cfg = r'{sam2_config}'
sam2_checkpoint = r'{sam2_checkpoint}'
"))





    }

    reticulate::py_run_string("
import torch
import numpy as np
import cv2
import os
import sys
import sam2

from sam2.build_sam import build_sam2
from sam2.sam2_image_predictor import SAM2ImagePredictor

if torch.cuda.is_available():
    device = torch.device('cuda')
elif torch.backends.mps.is_available():
    device = torch.device('mps')
else:
    device = torch.device('cpu')

")

    #copy yaml to config path
    reticulate::py_run_string(glue::glue("
# Destination folder inside the installed SAM2 package
dest_dir = os.path.join(os.path.dirname(sam2.__file__))
"))
    dest_dir <- reticulate::py$dest_dir
    copy_to <- file.path(dest_dir,basename(sam2_config))
    file.copy(sam2_config,copy_to)




          reticulate::py_run_string("
sam2_model = build_sam2(r'sam2.1_hiera_t.yaml', sam2_checkpoint, device=device)
predictor = SAM2ImagePredictor(sam2_model)
")


    .fire_env$sam_loaded <- TRUE
  }
  invisible(TRUE)
}
