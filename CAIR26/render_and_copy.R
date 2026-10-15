# ============================================================
# render_and_copy.R
# Package:  fctools
#
# Renders every .qmd/.Rmd in scripts/. Quarto's own `output-dir`
# setting (in _quarto.yml) places the rendered file in this
# project's local output/ folder automatically — this script does
# NOT duplicate that step.
#
# This script's only job is optional: if `publish_dir` is set
# below, it ALSO copies the rendered output (and any *_files/
# asset folder) to an external location, e.g. a public docs repo.
# Leave `publish_dir` as NULL to skip external publishing.
#
# Run this from the template root (the folder containing scripts/,
# data/, figures/, output/, template/) — not from inside scripts/.
# ============================================================

library(quarto)
library(fs)

# ---------------------------------------------------------------
# User configuration — edit per project
# ---------------------------------------------------------------

# Must match `project.output-dir` in this project's _quarto.yml.
local_output_dir <- "output"

# External publish folder. Set to NULL (default) to skip.
#
# Example — CAMP-style public docs repo:
# publish_dir <- "C:/Users/vwu/OneDrive - Fullerton College/Documents/vwu-Code/fc-oie/docs/R-Tutorial/CAMP"
publish_dir <- NULL

# ---------------------------------------------------------------
# Render
# ---------------------------------------------------------------

cat(
  "\n🔧 Rendering all Quarto (.qmd) and R Markdown (.Rmd) files in scripts/ ...\n"
)

qmd_files <- dir_ls(
  "scripts",
  regexp = "\\.(qmd|Rmd)$",
  recurse = FALSE,
  type = "file"
)

if (length(qmd_files) == 0) {
  cat("⚠️  No .qmd or .Rmd files found in scripts/. Nothing to render.\n")
}

if (!is.null(publish_dir)) {
  dir_create(publish_dir)
}

for (qmd in qmd_files) {
  cat("\n📄 Processing:", path_file(qmd), "\n")

  render_ok <- TRUE

  tryCatch(
    quarto_render(path_norm(qmd)),
    error = function(e) {
      cat("❌ Rendering failed for", path_file(qmd), ":", e$message, "\n")
      render_ok <<- FALSE
    }
  )

  if (!render_ok) {
    next
  }

  if (is.null(publish_dir)) {
    cat(
      "ℹ️  Rendered — placed in",
      local_output_dir,
      "by Quarto's output-dir setting.\n"
    )
    next
  }

  # Only reached if external publishing is configured. Quarto's
  # output-dir already placed the file in local_output_dir — look
  # for it there rather than next to the .qmd, and check both
  # extensions since this script is shared across html/pdf/revealjs.
  base_name <- path_ext_remove(path_file(qmd))

  candidate_outputs <- path(
    local_output_dir,
    paste0(base_name, c(".html", ".pdf"))
  )

  src_output <- candidate_outputs[file_exists(candidate_outputs)]

  if (length(src_output) == 0) {
    cat(
      "⚠️  Skipped publish — no rendered output found in",
      local_output_dir,
      "for:",
      path_file(qmd),
      "\n"
    )
    next
  }

  src_output <- src_output[1]
  file_name <- path_file(src_output)

  file_copy(src_output, path(publish_dir, file_name), overwrite = TRUE)
  cat("✅ Published to:", path(publish_dir, file_name), "\n")

  # Accompanying *_files/ asset folder, if one was produced
  src_files <- path(local_output_dir, paste0(base_name, "_files"))

  if (dir_exists(src_files)) {
    dir_copy(
      src_files,
      path(publish_dir, path_file(src_files)),
      overwrite = TRUE
    )
    cat(
      "✅ Published assets to:",
      path(publish_dir, path_file(src_files)),
      "\n"
    )
  }
}

cat("\n🏁 All .qmd and .Rmd files in scripts/ processed.\n")
