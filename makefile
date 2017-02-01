plots:
	Rscript Rvisualisation/Bernoulli.R
	Rscript Rvisualisation/Composed.R
	Rscript Rvisualisation/StudentT.R
	Rscript Rvisualisation/VisualiseLGCP.R

linearModel:
	sbt assembly
	ssh struve -t "rm -f LinearModelPMMH.csv LinearModelSims.csv"
	scp target/scala-2.11/ComposableModels-assembly-0.1.jar maths:/home/a9169110/.
	ssh struve -t "java -cp ComposableModels-assembly-0.1.jar com.github.jonnylaw.examples.SimLinear"
	ssh struve -t "java -cp ComposableModels-assembly-0.1.jar com.github.jonnylaw.examples.MultipleChains"
	scp maths:/home/a9169110/LinearModelSims.csv .
<<<<<<< HEAD
	scp maths:/home/a9169110/LinearModelPMMH-1.csv .	
	scp maths:/home/a9169110/LinearModelPMMH-2.csv .	
	scp maths:/home/a9169110/LinearModelPMMH-3.csv .	
	scp maths:/home/a9169110/LinearModelPMMH-4.csv .	
=======
	scp maths:/home/a9169110/StorvikGaussianUnknownPrecision.csv .

linearModel:
	sbt assembly
	ssh struve -t "rm -f LinearModelPMMH.csv LinearModelSims.csv"
	scp target/scala-2.11/ComposableModels-assembly-0.1.jar maths:/home/a9169110/.
	ssh struve -t "java -cp ComposableModels-assembly-0.1.jar com.github.jonnylaw.examples.SimLinear"
	ssh struve -t "java -cp ComposableModels-assembly-0.1.jar com.github.jonnylaw.examples.ErrorHandlingMCMC"
	scp maths:/home/a9169110/LinearModelSims.csv .
	scp maths:/home/a9169110/LinearModelPMMH.csv .	
>>>>>>> 4296ee6f4cac6a717a3d51e112472903c7076fbf
