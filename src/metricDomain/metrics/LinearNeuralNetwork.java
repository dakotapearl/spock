package metricDomain.metrics;

import metricDomain.goals.flavours.NotifyGoal;

/**
 *  Needs to attach to the (some) perceptions of a network. Then builds a linear layered network
 *  of the inputs. Connections are fixed within a short time period, exponential or linear decrease
 *  in signal building. Degree of match determine degree of metric match.
 */
public class LinearNeuralNetwork extends NotifyGoal implements TrainableNetwork<Integer>{

	private int inputs, outputs, hiddenLayers;
	private layer[] layers;
	
	private class layer {
		public node[] nodes;
		public layer(int size) {
			nodes = new node[size];
		}
	}
	
	private class outputLayer extends layer {
		public outputLayer(int size) { super(size); }
	}
	
	private class node {
		layer nextLayer;
		public void setNextLayer(layer nextLayer) {
			this.nextLayer = nextLayer;
		}
	}
	
	private class outputNode extends node {
		public Integer getOutput() {
			return 0;
		}
	}
	
	public LinearNeuralNetwork(int inputs, int outputs, int hiddenLayers, int hiddenLayerSize) {
		int i;
		
		this.inputs = inputs;
		this.outputs = outputs;
		
		layers = new layer[hiddenLayers + 2];
		layers[0] = new layer(inputs);
		layers[hiddenLayers + 1] = new layer(outputs);
		for (i = 1; i < hiddenLayers; i++) {
			layers[i] = new layer(hiddenLayerSize);
		}
		
		// Connect layers
		for (i = 0; i < layers.length; i++) {
			//layers[i]
		}
		
	}
	
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
