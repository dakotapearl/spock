package metricDomain.metrics;

public class NativeLinearNeuralNetwork implements TrainableNetwork<Integer> {

	@Override
	public boolean isTrained() {
		
		return false;
	}

	@Override
	public void train(Integer[] trainingArray, Integer[] desiredOutputs) {
		
	}

	@Override
	public Integer[] exploit(Integer[] input) {
		
		return null;
	}

}

/*
Process p = Runtime.getRuntime().exec("C:\\tc\\bin\\" + prog_name , null);
BufferedReader stdError = new BufferedReader(new InputStreamReader(p.getInputStream()));
System.out.println("Here is the standard op :\n");
s1=stdError.readLine();
*/