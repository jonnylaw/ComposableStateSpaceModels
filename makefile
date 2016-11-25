plots:
	Rscript Rvisualisation/Bernoulli.R
	Rscript Rvisualisation/Composed.R
	Rscript Rvisualisation/StudentT.R
	Rscript Rvisualisation/VisualiseLGCP.R

storvik:
	sbt assembly
	scp target/scala-2.11/ComposableModels-assembly-0.1.jar maths:/home/a9169110/.
	ssh struve -t "java -cp ComposableModels-assembly-0.1.jar com.github.jonnylaw.examples.SimLinear"
	ssh struve -t "java -cp ComposableModels-assembly-0.1.jar com.github.jonnylaw.examples.StorvikFilter"
	scp maths:/home/a9169110/LinearModelSims.csv .
	scp maths:/home/a9169110/StorvikGaussianUnknownPrecision.csv .
