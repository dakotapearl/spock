package networkDomain.extensions;

import networkDomain.NetworkBehaviour;
import networkDomain.NetworkNode;

/**
 * @author Loren Chorley
 */
public abstract class EnergyEconomics extends Thread implements NetworkBehaviour {

	private int energy = 0;
	protected NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") EnergyEconomics newFunction;
		public replicator(EnergyEconomics newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(EnergyEconomics newFunction) { (new replicator(newFunction)).start(); }
	public abstract EnergyEconomics replicate();
	public abstract void run();

	public void offsetEnergy(int value) {
		energy = energy + value;
	}
	
	public int checkEnergy() {
		return energy;
	}
	
	//public abstract boolean continueFiring();
	//public abstract boolean hasExceededEnergyLimit();
	//public abstract double getNodeEfficiency(); // might require notify functions spread through NetworkNode and maybe other extensions
	
}
