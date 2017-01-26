# ShinyDryMatterSorter
Sorts data from the Kakapo data subset. Based from ShinyPSASorter

I'll write more stuff here as the program becomes more complete and as Kakapo is deprecated.

Columns to note (change when Kakapo output changes):

breeder_cross_code: Should make enough sense. Codes like rOJ_M101, Zi232 etc.

group_code: A substitute to breeder_cross_code for where such code doesn't exist for that row: K10.01-01-01b

Concept: Fruit type, green, gold, red, whatever. Please refer to line ( testParents <- testParents[grep("Green", testParents$Concept), ] )

data_type: Type of data, predictive or destructive. Please see line ( testParents <- testParents[grep("Destructive", testParents$data_type), ] )

harvest_dm: Percentage of dry matter to a fruit ( 18.36 )

To change these variable names upon Kakapo's change of output, find the columns in the new table where these data exist, and perform and find/replace from the previous variable name to the new variable name.