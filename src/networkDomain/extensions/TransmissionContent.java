package networkDomain.extensions;

import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;

/**
 * @author Loren Chorley
 */
public abstract class TransmissionContent implements Runnable {
	
	public Thread thread = new Thread(this);
	protected NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") TransmissionContent newFunction;
		public replicator(TransmissionContent newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(TransmissionContent newFunction) { (new replicator(newFunction)).start(); }
	public abstract TransmissionContent replicate();
	
	public abstract NetworkSignal nextSignalToFire();
	public abstract boolean signalsRemain();
	
}
