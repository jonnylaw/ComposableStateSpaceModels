#!/bin/bash

sbt assembly
cp target/scala-2.12/ComposableModels-assembly-0.6.1.jar NegativeBinomial.jar
scp NegativeBinomial.jar maths:/home/a9169110/.

ssh airy -t /home/a9169110/jdk/jdk1.8.0_121/bin/java -cp NegativeBinomial.jar com.github.jonnylaw.examples.SimulateNegativeBinomial
ssh airy -f screen -S NegativeBinomial -dm /home/a9169110/jdk/jdk1.8.0_121/bin/java -cp NegativeBinomial.jar com.github.jonnylaw.examples.OnlineFiltering

ssh airy -t screen -ls
