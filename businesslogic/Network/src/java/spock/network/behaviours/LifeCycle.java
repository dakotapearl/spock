package spock.network.behaviours;

import networkDomain.NetworkBehaviour;
import networkDomain.NetworkNode;

/**
 * @author Loren Chorley
 */
public abstract class LifeCycle extends Thread implements NetworkBehaviour {
	
	protected NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") LifeCycle newFunction;
		public replicator(LifeCycle newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(LifeCycle newFunction) { (new replicator(newFunction)).start(); }
	public abstract LifeCycle replicate();
	public abstract void run();

	// cell death condition
	// neurogenic mitosis condition
	
}
