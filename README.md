# codytools

A warm literary ggplot2 theme, and helpers for sending plots from analysis projects to a Quarto website.

```r
pak::pak("Codyvanzandt/codytools")
options(codytools.site = "C:/Users/codya/Desktop/personal-website")   # in ~/.Rprofile
```

## Plotting

```r
library(codytools)
cody_use()      # fonts, theme, default palettes and geom styles
```

Plain ggplot code then picks up the theme and palettes. Choices that stay explicit:
`scale_fill_cody_div()` for values centred on zero, and
`scale_colour_cody(values = c(Control = "burgundy", Treatment = "blue"))` to keep a
category's colour fixed across figures.

## Posts

```r
to_post(my_fig, "my-slug")      # from an analysis project; saved as posts/my-slug/my_fig.rds
new_post("My *title*")          # creates posts/my-title/index.qmd (or pass slug =)
from_post("my_fig")             # inside the post
build_site()                    # quarto render into docs/
publish_site("Add post")        # build, then commit and push
```
