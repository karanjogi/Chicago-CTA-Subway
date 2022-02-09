awk -v lines="120000" -v pre="cta_part_" '
     NR==1 { header=$0; next}
     (NR-1) % lines ==1 { fname=pre c++; print header > fname}
     {print > fname}' CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.tsv