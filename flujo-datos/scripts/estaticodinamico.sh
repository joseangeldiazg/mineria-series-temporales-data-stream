#!/bin/bash

for i in `seq 1 30`;
    do
        eval "estatico=estatico$i.txt"
        eval "dinamico=dinamico$i.txt"
        java -cp moa.jar -javaagent:sizeofag-1.0.0.jar moa.DoTask  "EvaluateModel -m (LearnModel -l bayes.NaiveBayes -s (generators.SEAGenerator -f 2 -i 2) -m 100000) -s (ConceptDriftStream -s (generators.SEAGenerator -f 2) -d (generators.SEAGenerator -f 3 -i $i) -p 20000 -w 100) -i 100000" >  $estatico
        java -cp moa.jar -javaagent:sizeofag-1.0.0.jar moa.DoTask  "EvaluateInterleavedTestThenTrain -l drift.SingleClassifierDrift -s (ConceptDriftStream -s (generators.SEAGenerator -f 2) -d (generators.SEAGenerator -f 3 -i $i) -p 20000 -w 100) -i 100000" >  $dinamico
    done
