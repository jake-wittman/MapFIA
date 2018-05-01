# Map FIA

The purpose of this app is to provide an easy-to-use interface for anyone who is interested in mapping the distribution of tree species as well as their basal area per acre in the US. The US Forest Service collects data yearly (the Forest Inventory Analysis, or FIA) from plots across the country on the distribution of tree species in the US.  USFS personnel have created models that take this data along with environmental data and generate distributions and basal areas for over 300 species of trees.

## App Functions

   At it's most basic, this app allows a user to select a tree species for which they want to see the distribution and basal area for and plot that information on any of the 48 contiguous states in the US. Beyond that users can:
   
   * select a species from either it's scientific name or common name. The two input fields are reactive to each other, so they will both update once the user makes a choice. Species can be selected by typing in their names or by scrolling through a drop down menu.
   * select any combination of the 48 contiguous US states for mapping. There are specific options to easily select all 48 ("Contiguous US") or to select by different regions (e.g. "Pacific West" or "Northeast"). States can be selected by typing in their names or by scrolling through a drop down menu.
   * adjust the numbers of pixels used in the production of each plot. Each raster starts with over 200 million pixels which takes a long time to process for plotting. By adjusting the numbers of pixels used, the app will produce plots more quickly but at a courser resolution. The allows the user to balance performance/quality based on their needs.
   * select more than one species of tree. If users select more than one species, the app will aggregate the rasters for all the species selected and plot their combined distribution and basal area. Alternatively, if users select more than one species they can choose to have the plot display only areas where the species occur together. This option shows up once users select more than one tree species.
   * adjust the color palette showing the basal area per acre in each pixel. There are three different palette range options, each with their own specific color settings the user can adjust.
   * save the map in one of three file formats (.png, .tiff, or .pdf), at the desired resolution, and for a desired height/width.
   * if the user selects more than one tree species, the app also produces a proportional bar chart which shows the relative proportion of total basal area each species makes up over the entire mapped region as well as each individual state mapped. Users also have the option of saving this plot if they want.
