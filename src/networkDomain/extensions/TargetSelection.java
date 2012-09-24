package networkDomain.extensions;

import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;

/**
 * Instantiated once for whole network
 * @author Loren
 *
 */
public abstract class TargetSelection extends Thread {
	
	protected NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	public abstract NetworkNode selectTarget(NetworkSignal signal);
	
}
