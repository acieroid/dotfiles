#!/bin/sh -x

INFILE=$1
OUTFILE=$2
FIRSTPAGE=$3
LASTPAGE=$4

for i in $(seq $FIRSTPAGE 24 $LASTPAGE);
do
    qpdf --empty --pages $INFILE $i-$((i+23)) -- $OUTFILE-$i.pdf
    pdfbook2 $OUTFILE-$i.pdf
done
