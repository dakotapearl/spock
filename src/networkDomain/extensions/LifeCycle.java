package networkDomain.extensions;

import networkDomain.NetworkNode;

public abstract class LifeCycle {
	
	NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	// cell death condition
	// neurogenic mitosis condition
	
}
