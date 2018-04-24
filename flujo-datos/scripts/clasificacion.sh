#!/bin/bash

for i in `seq 1 30`;
    do
        eval "nombreNB=nb$i.txt"
        eval "nombreHT=ht$i.txt"
        java -cp moa.jar -javaagent:sizeofag-1.0.0.jar moa.DoTask "EvaluateInterleavedTestThenTrain -l bayes.NaiveBayes -s (generators.RandomTreeGenerator -i $i) -i 1000000 -f 10000" >  $nombreNB
        java -cp moa.jar -javaagent:sizeofag-1.0.0.jar moa.DoTask "EvaluateInterleavedTestThenTrain -l trees.HoeffdingTree -s (generators.RandomTreeGenerator -i $i) -i 1000000 -f 10000" >  $nombreHT                                                                  
    done
