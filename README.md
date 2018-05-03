# ussc_draft
A repository for drafting USSC ggplot2 themes.


## Notes:
### Opinionated themes...
The user can set grids within the theme function itself by writing grid=TRUE/FALSE or grid="XYxy" -- or any combination thereof. Themes default to showing major grid lines only. 
Applying parameters within the theme also applies to the axis lines, ticks, and plot border. 
### Default colours for monochromatic plots (not working)
Line charts default to the USSC red colour.
Bar charts = light blue.
Points = dark blue.
If you are plotting multiple groups, set the fill or colour by running ussc::scale_colour_ussc() or ussc::scale_fill_ussc().
