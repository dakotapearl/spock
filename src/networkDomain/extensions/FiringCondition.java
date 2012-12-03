package networkDomain.extensions;

import networkDomain.NetworkBehaviour;
import networkDomain.NetworkNode;

/**
 * @author Loren Chorley
 */
public abstract class FiringCondition extends Thread implements NetworkBehaviour {
	
	private boolean ready = false;
	protected NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") FiringCondition newFunction;
		public replicator(FiringCondition newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(FiringCondition newFunction) { (new replicator(newFunction)).start(); }
	public abstract FiringCondition replicate();
	public abstract void run();

	//public abstract boolean continueFiring();
	
	// Block until ready, return false if interrupted 
	public synchronized boolean waitUntilReadyToFire() {
		if (ready) {
			return true;
		} else {
			try {
				wait();
				return true;
			} catch (InterruptedException e) {
				return false;
			}
		}
	}
	
	protected void setNotReadyToFire() {
		ready = false;
	}
	
	protected synchronized void setReadyToFire() {
		ready = true;
		notifyAll();
	}
	
	public abstract void refresh();
	
}
