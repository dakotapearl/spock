package networkDomain.extensions;

import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;

public abstract class TransmissionContent extends Thread {
	
	protected NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	public abstract NetworkSignal nextSignalToFire();
	public abstract boolean signalsRemain();
	
}
