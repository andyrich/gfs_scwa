f <- fs::dir_ls(here::here("code"), regexp = "02|03|04")

st <- Sys.time()
for(i in seq_along(f)){
  
  cat("RUNNING:", basename(f[i]), "\n\n")
  
  source(f[i])
  
  cat("\n\nCOMPLETED:", basename(f[i]), "\n\n")

}
Sys.time() - st

# render markdown report
rmarkdown::render(
  here::here("code/06_report.Rmd"), 
  output_file = here::here("code/06_report.html")
)

rmarkdown::render(
  here::here("code/07_sanity_check.Rmd"), 
  output_file = here::here("code/07_sanity_check.html")
)
