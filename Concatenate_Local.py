# -*- coding: utf-8 -*-
"""
Created on Mon Sep 27 16:30:02 2021

@author: NoteboomM

Short routine to gather the high/normal/low categories for all the 'local_entry'
csv files into a single summary output for IID.

"""

import os

outstring = "Year,Station,Low,Normal,High,Flow_Type\n"

for year in range(2000,2020):
    print(year)
    os.chdir("C://Users//noteboomm//Documents//CESI//CESI_WQI_Calculator_V_3//Output//"+str(year))
    with open(str(year)+"_local_entry_HYDAT_2021-07.csv", 'r') as infile:
        instring = infile.readlines()
    for line in instring[1:]:
        outline = ','.join(line.split(',')[:5])
        outstring = outstring + str(year) + ',' + outline + "\n"

os.chdir("C://Users//noteboomm//Documents//CESI//CESI_WQI_Calculator_V_3//Output")
with open("Local_summary.csv", 'w') as outfile:
    outfile.write(outstring)
    
