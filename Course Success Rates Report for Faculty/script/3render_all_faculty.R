# --------------------------------------------------
# render_faculty_reports.R
# --------------------------------------------------

library(quarto)
library(readr)
library(fs)
library(stringr)
library(tidyverse)

# -----------------------------------------------
# Paths
# -----------------------------------------------

qmd_file <- "faculty_template-withQ.qmd"
output_dir <- "output"
dir_create(output_dir)

log_file <- file.path(output_dir, "render_log.csv")


# -----------------------------------------------
# Add a row to the log immediately
# -----------------------------------------------
# Adding first to indicate process of rendering is/has started

log_row <- function(loop_num, total_reports, faculty_id, file_name,
                     render_time, elapsed_sec, file_created, status) {

  write.table(
    data.frame(
      loop_num = loop_num,
      total_reports = total_reports,
      faculty_id = faculty_id,
      file_name = file_name,
      render_time = render_time,
      elapsed_sec = elapsed_sec,
      file_created = file_created,
      status = status
    ),
    file = log_file,
    sep = ",",
    row.names = FALSE,
    col.names = FALSE,
    append = TRUE
  )
}


# -----------------------------------------------
# Read faculty IDs
# -----------------------------------------------

faculty_ids <- read_csv(
 #"data/course_data_oie.csv",
 "data/course_data.csv",
  show_col_types = FALSE
) |>
  pull(FACULTY_ID) |>
  unique() |>
  na.omit()


# -----------------------------------------------
# OPTIONAL: render only selected faculty
# -----------------------------------------------

#change render_subset to FALSE if want all faculty
# change overwrite_existing to TRUE to replace existing files

###################################
####### IMPORTANT ##############
#####################################

# TRUE if you want subset
# FALSE = if you want ALL faculty
render_subset <- FALSE
subset_faculty_ids <- c("@9999999")

if (render_subset) {
  faculty_ids <- subset_faculty_ids
 # faculty_ids <- head(faculty_ids, 1) # first few
}

# TRUE  = re-render and replace existing PDFs
# FALSE = skip faculty who already have a PDF
overwrite_existing <- TRUE

# -----------------------------------------------
# Loop and render
# -----------------------------------------------

#########CHANGE TERM HERE########################


for (i in seq_along(faculty_ids)) {

  fid <- faculty_ids[i]

  safe_id <- str_replace_all(fid, "[^A-Za-z0-9]", "")

  out_name <- paste0(
    "Course_Success_2026-27_",
    safe_id,
    ".pdf"
  )

  dest <- file.path(output_dir, out_name)

  # -------------------------------------------
  # Skip if already exists
  # -------------------------------------------

  if (file.exists(dest) && !overwrite_existing) {

    message(
      sprintf(
        "[%s/%s] Skipping existing file: %s",
        i,
        length(faculty_ids),
        fid
      )
    )

    log_row(
      loop_num = i,
      total_reports = length(faculty_ids),
      faculty_id = fid,
      file_name = out_name,
      render_time = Sys.time(),
      elapsed_sec = 0,
      file_created = TRUE,
      status = "Skipped - Already Exists"
    )

    next
  }

  if (file.exists(dest) && overwrite_existing) {
    message(
      sprintf(
        "[%s/%s] Overwriting existing file: %s",
        i,
        length(faculty_ids),
        fid
      )
    )
  }

  # -------------------------------------------
  # Log "Started" BEFORE rendering, so an
  # interrupted run leaves a trace of exactly
  # which faculty was in-flight when it died.
  # -------------------------------------------

  message(
    sprintf(
      "[%s/%s] Rendering faculty: %s",
      i,
      length(faculty_ids),
      fid
    )
  )

  start_time <- Sys.time()

  log_row(
    loop_num = i,
    total_reports = length(faculty_ids),
    faculty_id = fid,
    file_name = out_name,
    render_time = start_time,
    elapsed_sec = NA,
    file_created = FALSE,
    status = "Started"
  )

  # -------------------------------------------
  # Render
  # -------------------------------------------

  status <- "Success"

  tryCatch({

    quarto_render(
      input = qmd_file,
      execute_params = list(
        faculty_id = fid
      ),
      output_file = out_name
    )

    if (file.exists(out_name)) {
      file_move(out_name, dest)
    }

  }, error = function(e) {

    status <<- paste0("ERROR: ", e$message)

  })

  elapsed_sec <- round(
    as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    ),
    1
  )

  file_created <- file.exists(dest)

  if (!file_created && status == "Success") {
    status <- "ERROR: Render completed but PDF not found"
  }

  # -------------------------------------------
  # Log final outcome (Success / Error) AFTER
  # rendering. This is a second row for this
  # faculty_id — the "Started" row above stays
  # in the log as a record of the attempt.
  # -------------------------------------------

  log_row(
    loop_num = i,
    total_reports = length(faculty_ids),
    faculty_id = fid,
    file_name = out_name,
    render_time = Sys.time(),
    elapsed_sec = elapsed_sec,
    file_created = file_created,
    status = status
  )

  message(
    sprintf(
      " → %s (%.1f sec)",
      status,
      elapsed_sec
    )
  )

}

message("All faculty reports processed.")