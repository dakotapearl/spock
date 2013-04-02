package spock.network.behaviours;

import spock.network.NetworkNode;

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
	public synchronized boolean waitUntilReady() {
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
	
	protected void setNotReady() {
            ready = false;
	}
	
	protected synchronized void setReady() {
            ready = true;
            notifyAll();
	}
	
	public abstract void refresh();
	
}
