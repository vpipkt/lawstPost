# Branch notes

##24 JUl
Working some improvments to the combined deliveries which led to vague improvments in the related reading functions. One issue here is setting the factor levels. Think thru this - what if stacking two games with different sets of levels/// hmm

 see main readme/todo for inspiration. 

 also review some of the lab work done that ignored the combined deliveries function; instead rolled my own function to handle transport deliveries.

perhaps the read transportDeliveries (and related) should as a matter of course, read and merge the requests.

and the readCombined would just stack these, correcting for for common variables...


## 27 July thoughts ####

makes more sense to attack the overarching to-do regarding data structure for multiple games / cases

then refactor the Xdeliveries readers to merge requests always; then refactor combined. then follow up the chain such as flowData flowMap that depend on this.

## Design thought

How to arrange the data structure to do case stacking and acheive other tidy data design objectives. See scanned paper in this repo/branch.

Goals:
1. for an arbitray set of games; allow tidy data frames of key outputs, with factor levels working well
2. for any game in a set, convenient cross reference to object particulars 
3. for all games in a set, arbitrary cross reference

