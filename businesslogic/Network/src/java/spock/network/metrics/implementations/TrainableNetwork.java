package spock.network.metrics.implementations;

public interface TrainableNetwork<E> {
	
	public boolean isTrained();
	public void train(E[] trainingArray, E[] desiredOutputs);
	public E[] exploit(E[] input);
	
}
