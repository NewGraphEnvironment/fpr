# Script to generate hex sticker for fpr package
# Uses hexSticker package to create a simple black and white design
# with NG logo icon and package name

# Install hexSticker if not already installed
if (!requireNamespace("hexSticker", quietly = TRUE)) {
  install.packages("hexSticker")
}

library(hexSticker)

# Configuration parameters (easy to tweak)
logo_url <- "https://raw.githubusercontent.com/NewGraphEnvironment/new_graphiti/main/assets/logos/logo_newgraph/WHITE/PNG/nge-icon_white.png"
logo_file <- "data-raw/nge-icon_white.png"
output_file <- "man/figures/logo.png"
package_name <- "fpr"

# Download the logo if it doesn't exist locally
if (!file.exists(logo_file)) {
  dir.create(dirname(logo_file), recursive = TRUE, showWarnings = FALSE)
  download.file(logo_url, logo_file, mode = "wb")
  message("Downloaded NG logo to ", logo_file)
} else {
  message("Using existing logo at ", logo_file)
}

# Create output directory if it doesn't exist
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

# Generate hex sticker
sticker(
  subplot = logo_file,
  package = package_name,
  # Logo positioning and size
  s_x = 1,           # Logo x position (center)
  s_y = 0.75,        # Logo y position (slightly above center)
  s_width = 0.4,     # Logo width
  s_height = 0.4,    # Logo height (will maintain aspect ratio)
  # Package name styling
  p_size = 20,       # Package name font size
  p_x = 1,           # Package name x position (center)
  p_y = 1.45,        # Package name y position (below logo)
  p_color = "white", # Package name color
  # Hex sticker styling
  h_fill = "black",  # Background fill color
  h_color = "white", # Border color
  h_size = 1.5,      # Border thickness
  # Output settings
  filename = output_file,
  dpi = 300          # High resolution for quality
)

message("Hex sticker generated successfully at ", output_file)

# Optional: Also create a smaller version for README
readme_logo <- "man/figures/logo_small.png"
sticker(
  subplot = logo_file,
  package = package_name,
  s_x = 1,
  s_y = 0.75,
  s_width = 0.4,
  s_height = 0.4,
  p_size = 20,
  p_x = 1,
  p_y = 1.45,
  p_color = "white",
  h_fill = "black",
  h_color = "white",
  h_size = 1.5,
  filename = readme_logo,
  dpi = 150  # Lower DPI for web display
)

message("Small version created at ", readme_logo)
