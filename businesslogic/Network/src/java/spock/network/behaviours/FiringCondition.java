package spock.network.behaviours;

import spock.network.core.NetworkNode;

/**
 * @author Loren Chorley
 */
public abstract class FiringCondition extends NetworkBehaviour<FiringCondition> {
	
    protected boolean ready = false;

    @Override
    public void replaceInNode(NetworkNode node, FiringCondition behaviour) {
        node.firingCondition = behaviour;
    }
    
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
