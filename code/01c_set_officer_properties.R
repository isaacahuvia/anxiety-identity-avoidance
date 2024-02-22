# ---------------------------------------------------------------------------- #
# Set "officer" Package Properties
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# Set section and text properties that can be sourced in other scripts

psect_prop <- prop_section(page_size(orient = "portrait", width = 8.5, height = 11),
                           type = "nextPage")
lsect_prop <- prop_section(page_size(orient = "landscape", width = 8.5, height = 11),
                           type = "nextPage")

text_prop <- fp_text_lite(color = "black", font.size = 12, font.family = "Times New Roman")
text_prop_bold   <- update(text_prop, bold = TRUE)
text_prop_italic <- update(text_prop, italic = TRUE)