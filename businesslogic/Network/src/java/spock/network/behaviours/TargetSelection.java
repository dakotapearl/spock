package spock.network.behaviours;

import networkDomain.NetworkBehaviour;
import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;
import networkDomain.NetworkTargetable;

/**
 * @author Loren Chorley
 */
public abstract class TargetSelection extends Thread implements NetworkBehaviour {
	
	protected NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") TargetSelection newFunction;
		public replicator(TargetSelection newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(TargetSelection newFunction) { (new replicator(newFunction)).start(); }
	public abstract TargetSelection replicate();
	
	public abstract NetworkTargetable selectTarget(NetworkSignal signal);
	public abstract void run();
	
}
