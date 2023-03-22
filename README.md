# recolony_shiny_demo
Example of a Shiny app.

Takes a very specific rds file as an input, derived from code found at https://github.com/zellerlab/commensal_clostridiales - this app has no general purpose utility; it is a tech demo. The data is included in the repository.

The app produces an overview of the relative abundances of four Clostridiales species of interest in 

The first output is a summary table reporting average log relative abundances of the four taxa.
The second is a recreation of box plots found in [Montalban-Arques et al 2021](https://doi.org/10.1016/j.chom.2021.08.001) (Fig. 2), based on code from the aforementioned repository.
The third output is an alternative visualization of the distributions, using overlapping density plots instead of box plots.
The fourth output is a UMAP embedding to see to which extent control and CRC samples cluster by the log relative abundances of the four Clostridiales species (not very much).
