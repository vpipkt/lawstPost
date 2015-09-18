
#run and save  the default game 
shell(cmd = 'lawst.exe k\\_kdemo.lconfig')

#Read the unit scripts.
kdemo <- readLawstConfig('k/_kdemo.lconfig')
us <- readUnitScript(kdemo)

#write out the game's scripts to a new file
writeUnitScript('k/DATA/unitScriptsMachined.csv',us)
#read the new file into  lawst, run the game and save. 
shell(cmd = 'Lawst.exe k\\_kdemoMachined.lconfig')

#now diff the machine created file, which lawst has overwritten, with the original. ALso diff the output data.

# In current run 18 Sept, the Unit scripts end up the same
# Game outputs are not binary identical, two small diffs in supply request write order, but overall result is effectively the same.
