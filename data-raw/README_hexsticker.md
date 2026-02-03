# Hex Sticker Generation

This directory contains the script to generate the hex sticker for the `fpr` package.

## Quick Start

To generate the hex sticker, simply run:

```r
source("data-raw/make_hexsticker.R")
```

This will:
1. Download the NG logo icon (if not already present)
2. Generate a high-resolution hex sticker at `man/figures/logo.png`
3. Generate a web-optimized version at `man/figures/logo_small.png`

## Customization

All customizable parameters are at the top of `make_hexsticker.R`:

- **Logo URL**: Where to download the NG icon from
- **Logo positioning**: `s_x`, `s_y` - adjust the icon position
- **Logo size**: `s_width`, `s_height` - adjust the icon size
- **Package name styling**: `p_size`, `p_x`, `p_y`, `p_color` - adjust text appearance
- **Hex sticker colors**: `h_fill` (background), `h_color` (border)
- **Border thickness**: `h_size`
- **Output resolution**: `dpi` - higher values = better quality but larger files

## Design Specifications

The current design features:
- Black background with white border
- NG logo icon in white, centered in the upper portion
- Package name "fpr" in white, centered below the icon
- 300 DPI for print quality, 150 DPI for web use

## Template for Other Packages

This script can be easily adapted for other packages (dff, dfp) by:
1. Copying `make_hexsticker.R` to the other package
2. Updating the `package_name` variable
3. Adjusting any other parameters as needed
