package metricDomain.metrics;

import metricDomain.goals.flavours.NotifyGoal;

/**
 *  Needs to attach to the (some) perceptions of a network. Then builds a linear layered network
 *  of the inputs. Connections are fixed within a short time period, exponential or linear decrease
 *  in signal building. Degree of match determine degree of metric match.
 */
public class LinearLayeredNetworkMetric extends NotifyGoal {

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
		public node(layer nextLayer) {
			this.nextLayer = nextLayer;
		}
	}
	
	private class outputNode extends node {
		public outputNode(layer nextLayer) { super(nextLayer); }
	}
	
	public LinearLayeredNetworkMetric(int depth) {
		
	}
	
	private void connectLayers(layer First, layer Second) {
		
	}
	

}
