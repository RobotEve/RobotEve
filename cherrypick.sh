!/bin/sh

echo "Step 1 of 7: Starting EveActivity R"
echo $(date +"%T")
R --vanilla --slave < EveActivity.r
echo "Completed EveActivity R"

echo "Step 2 of 7: Starting Smiles R"
echo $(date +"%T")
R --vanilla --slave < smiles.r
echo "Completed Smiles R"

echo "Step 3 of 7: Starting OpenBabel fingerprint maker"
echo $(date +"%T")
babel AllSMILES.smi AllSMILES.fpt -xfFP2 -xN1024 -xh
echo "Completed OpenBabel fingerprint maker"

echo "Step 4 of 7: Starting Java fingerprint ascii matrix maker"
echo $(date +"%T")
java -jar fingerprintconverter.jar AllSMILES.fpt AllSMILES_fptmatrix.ascii
echo "Completed Java fingerprint ascii matrix maker"

echo "Step 5 of 7: Starting octave binary matrix maker"
echo $(date +"%T")
octave --silent SMILES_bin_matrix.m
echo "Completed octave binary matrix maker"

#echo "Step 6 of 7: Starting octave multiSelOpt"
#this version creates a printed list of compound index ids
#octave --silent --eval
"multiSelOpt('AllSMILES_fptmatrix.bin','measurements.ma\
t','AvailMols.mat',64, 0.001,1.0)"
#echo "Completed octave multiSelOpt"

echo "Step 6 of 7: Starting octave multiSelOptToFile"
echo $(date +"%T")
#this version creates .csv file outputs of selected compound index ids
octave --silent --eval
"multiSelOptToFile('AllSMILES_fptmatrix.bin','measuremen\
ts.mat','AvailMols.mat',64,0.001,1.0,'chosenlist.csv','chosenpreds.csv')"
echo "Completed octave multiSelOptToFile"

echo "Step 7 of 7: Starting new compound list in R"
echo $(date +"%T")
R --vanilla --slave < new_compounds.r
echo "Completed new compound list in R"
echo $(date +"%T")
