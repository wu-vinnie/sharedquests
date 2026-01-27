# --- render_and_copy.R ---
library(quarto)
library(fs)

cat(
  "\n🔧 Rendering all Quarto (.qmd) and R Markdown (.Rmd) reports in /scripts ...\n"
)

# 1. List only .qmd and .Rmd files in scripts/, non-recursive
qmd_files <- dir_ls(
  "script",
  regexp = "\\.(qmd|Rmd)$",
  recurse = FALSE,
  type = "file"
)

# 2. Render each file and copy its output
for (qmd in qmd_files) {
  cat("\n📄 Processing:", path_file(qmd), "\n")

  # Render the document (handles both .qmd and .Rmd)
  tryCatch(
    {
      quarto_render(path_norm(qmd))
    },
    error = function(e) {
      cat("❌ Rendering failed for", qmd, ":", e$message, "\n")
      next
    }
  )

  # Build source and destination paths
  src_html <- path_ext_set(qmd, "html")
  file_name <- path_file(src_html)

  dest_html <- path(
    "C:/Users/vwu/OneDrive - Fullerton College/Documents/vwu-Code/fc-oie/docs/HealthyMinds/2025",
    file_name
  )

  # Ensure destination folder exists
  dir_create(path_dir(dest_html))

  # Copy file if found
  if (file_exists(src_html)) {
    file_copy(src_html, dest_html, overwrite = TRUE)
    cat("✅ Copied to:", normalizePath(dest_html), "\n")
  } else {
    cat("⚠️ Skipped — no output found for:", file_name, "\n")
  }
}

cat("\n🏁 All .qmd and .rmd documents in /scripts processed.\n")
