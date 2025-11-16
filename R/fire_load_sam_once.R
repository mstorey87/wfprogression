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
    #reticulate::use_python("C:/Users/mstorey/AppData/Local/Programs/Python/Python313/python.exe", required = TRUE)
    #reticulate::use_condaenv(condaenv = "sam2_env")
    #reticulate::py$checkpoints_dir <- checkpoints_dir

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

          #reticulate::py$model_cfg <- sam2_config_loc
          #reticulate::py$sam2_checkpoint <- sam2_checkpoint_loc
          reticulate::py_run_string(glue::glue("
model_cfg = r'{sam2_config_loc}'
sam2_checkpoint = r'{sam2_checkpoint_loc}'
"))


    }else{
        reticulate::py_run_string(glue::glue("
model_cfg = r'{sam2_config}'
sam2_checkpoint = r'{sam2_checkpoint}'
"))
      # reticulate::py$model_cfg <- sam2_config
      # reticulate::py$sam2_checkpoint <- sam2_checkpoint


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

#    if(docker==FALSE){
          reticulate::py_run_string("
sam2_model = build_sam2(model_cfg, sam2_checkpoint, device=device)
predictor = SAM2ImagePredictor(sam2_model)
")

#    }else{


#       reticulate::py_run_string(glue::glue("
# # --- Create custom Hydra config path ---
# custom_cfg_dir = r'{checkpoints_dir}'
# os.environ['HYDRA_CONFIG_PATH'] = custom_cfg_dir
# os.environ['HYDRA_FULL_ERROR'] = '1'
#
# # if r'{checkpoints_dir}' not in sys.path:
# #     sys.path.append(r'{checkpoints_dir}')
# #print(f'Added config path to sys.path: {checkpoints_dir}')
#
# # --- Build SAM2 model ---
# config_file = r'sam2.1/sam2.1_hiera_t.yaml'  # relative to the custom config folder
# checkpoint = r'{file.path(checkpoints_dir, 'sam2.1_hiera_tiny.pt')}'
#
# sam2_model = build_sam2(
#     config_file=config_file,  # relative to sam2 package
#     checkpoint=checkpoint,
#     device=device
# )
#
# predictor = SAM2ImagePredictor(sam2_model)
# print('SAM2 model loaded successfully.')"))


#       reticulate::py_run_string(glue::glue("
# import shutil
# import sam2
#
# # --- Paths --- Need to copy to config path on decoker
# src_yaml = r'{file.path(checkpoints_dir, 'sam2.1_hiera_t.yaml')}'
# dest_dir = os.path.join(os.path.dirname(sam2.__file__), 'configs', 'sam2.1')
# #dest_dir = os.path.join(os.path.dirname(sam2.__file__))
# #os.makedirs(dest_dir, exist_ok=True)
# dest_yaml = os.path.join(dest_dir, 'sam2.1_hiera_t.yaml')
#
# # --- Copy YAML into package ---
# shutil.copy2(src_yaml, dest_yaml)
# print('Not Copied YAML to:', dest_yaml)
#
# checkpoint = r'{file.path(checkpoints_dir, 'sam2.1_hiera_tiny.pt')}'
#
# config_path_rel = 'configs/sam2.1/sam2.1_hiera_t.yaml'
# #config_path_rel = 'sam2.1_hiera_t.yaml'
# print('Entering YAML path to SAM:', config_path_rel)
# # --- Build SAM2 model ---
# sam2_model = build_sam2(
#     config_file=config_path_rel,  # relative to sam2 package
#     checkpoint=checkpoint,
#     device=device
# )
#
# predictor = SAM2ImagePredictor(sam2_model)
# print('SAM2 model loaded successfully.')
# "))

 #   }

    .fire_env$sam_loaded <- TRUE
  }
  invisible(TRUE)
}
