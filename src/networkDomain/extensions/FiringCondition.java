package networkDomain.extensions;

import networkDomain.NetworkNode;

/**
 * Instantiated for each node
 * @author Loren
 *
 */
public abstract class FiringCondition {
	
	NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	public abstract boolean readyToFire();
	public abstract boolean continueFiring();
	public abstract void notifyNodeBecameInactive();
	
}
