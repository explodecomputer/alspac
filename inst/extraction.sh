#!/bin/bash

stata="/Applications/Stata/StataSE.app/Contents/MacOS/StataSE"
rootdir="/Users/explodecomputer/Documents/data/ALSPAC/Current"
dofile="extraction.do"
outdir="raw_data"

find ${rootdir} -iname "*.dta" > ${outdir}/filelist.txt
find ${rootdir} -iname "*.sav" > ${outdir}/filelist_spss.txt

while read filename
do
	nom=$(basename "${filename}")
	echo "${nom}"
	# ${stata} -e do ${dofile} ${filename} ${outdir}/${nom}
	if [ -f "${outdir}/${nom}.txt" ]
	then
		cat ${outdir}/${nom}.txt | /usr/local/bin/sed -e 's/^[ \t]*//' | /usr/local/bin/sed ':a;N;$!ba;s/\n> //g' | /usr/local/bin/sed -n '/3. }/,$p' | /usr/local/bin/sed 1d | /usr/local/bin/sed -e :a -e '$d;N;2,7ba' -e 'P;D' | LANG=C tr -cd '\11\12\15\40-\176' | tr -s ' ' > temp1

		awk '{print $1}' temp1 > temp2
		awk '{$1=""; print $0}' temp1 > temp3
		paste temp2 temp3 | tr -s ' ' > "${outdir}/${nom}.parsed.txt"
		rm temp*
	fi

done < ${outdir}/filelist.txt

