# lawstPost
Post processing routines for Logistics Analysis and Wargame Support Tool simulation.

## Getting started

Check out `test/testPost.R` for an overview of functionality and how to use the various functions. 


## To do's

See individual source code files for some focused to-do's of smaller scope. This list is somewhat overarching.

* Need a projector / powerpoint friendly ggplot theme which features good contrast, larger fonts, and clearer (perhaps less dense) gridlines and markers. This theme should be applied to all graphics.

* Structure game data for comparison of multiple games with arbitrary differences (e.g. different units, different supply types, different transports). Refactor plots to leverage this as either a facet_grid modification, or use some aesthetic as appropriate.

* Shortcut outputs grouping supply types by class of supply groups: Class III and Other.

* Define a data set for product movement across zones. This would be used to produce a Sankey diagram or similar. Must preserve transport category (incl pipe) as a dimension. Zone definition could be by geographic region, location clustering, unit list membership, log tree crawling, etc. Will need careful handling of lognodes within the zone, assumed that they service the units within the zone and perhaps some beyond. Subtasks: 1 Define function to build desired data given unit-zone association. 2 Define basic zone associations: log tree and location cluster

* 