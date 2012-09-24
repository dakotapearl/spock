package networkDomain.extensions;

import networkDomain.NetworkNode;


/**
 * Instantiated once for whole network
 * @author Loren
 *
 */
public abstract class EnergyEconomics {

	NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	public abstract boolean continueFiring();
	public abstract boolean hasExceededEnergyLimit();
	public abstract double getNodeEfficiency(); // might require notify functions spread through NetworkNode and maybe other extensions
	
}
