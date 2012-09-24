package networkDomain.extensions;

import networkDomain.NetworkNode;


/**
 * Instantiated for each node
 * @author Loren
 *
 */
public abstract class GeneticSequence {
	
	NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
}
