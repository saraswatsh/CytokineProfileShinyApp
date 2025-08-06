# Load built-in data files
load(file.path("data", "ExampleData1.rda"))
load(file.path("data", "ExampleData2.rda"))
load(file.path("data", "ExampleData3.rda"))
load(file.path("data", "ExampleData4.rda"))
load(file.path("data", "ExampleData5.rda"))

# A vector of built-in dataset names; update as needed.
builtInList <- c(
  "ExampleData1",
  "ExampleData2",
  "ExampleData3",
  "ExampleData4",
  "ExampleData5"
)

## Sourcing all functions to be used in the app from R/ directory
files.sources <- list.files("R", full.names = TRUE, pattern = "\\.R$")
sapply(files.sources, source)
