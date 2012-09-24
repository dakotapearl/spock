package networkDomain.extensions;

import networkDomain.NetworkNode;

public abstract class LifeCycle extends Thread {
	
	protected NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	// cell death condition
	// neurogenic mitosis condition
	
}
