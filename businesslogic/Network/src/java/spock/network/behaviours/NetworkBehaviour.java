package spock.network.behaviours;

import spock.network.core.NetworkNode;

/*
 * 
 */
public abstract class NetworkBehaviour<T> implements Runnable, NetworkRunnable { //Ideally T is a class of type NetworkBehaviour
    
    private boolean threadEnabled = false;
    private boolean threadWantsToStart = false; // Remembers if an attempt to start the thread was made
    private Thread thread;
    protected NetworkNode parentNode;
    
    /*
     * Starts the thread or lets it start later when it becomes enabled
     */
    @Override
    public void start() {
        if (threadEnabled) {
            thread = new Thread(this);
            thread.start();
        } else {
            threadWantsToStart = true;
        }
    }

    /*
     * 
     */
    public void setThreadEnabled(boolean threadEnabled) {
        this.threadEnabled = threadEnabled;
        if (threadEnabled && threadWantsToStart) {
            thread = new Thread(this);
            thread.start();
        }
    }

    /*
     * 
     */
    public boolean isThreadEnabled() {
        return threadEnabled;
    }
    
    // Maybe not needed
    /*
     * 
     */
    public boolean hasThreadStarted() {
        return (thread != null) ? true : false;
    }
    
    /*
     * 
     */
    public void setParentNode(NetworkNode parentNode) { 
        this.parentNode = parentNode;
    }

    /**
     * Allows to concurrent replication of the this behaviour
     */
    private class replicatorThread extends Thread {
            
        @SuppressWarnings("unused") 
        T behaviour;
        NetworkNode node;
        
        public replicatorThread(T behaviour, NetworkNode node) { 
            this.behaviour = behaviour;
            this.node = node;
        }

        @Override 
        public void run() { 
            replaceInNode(node, replicate(behaviour));
        }
        
    }
    
    /*
     * 
     */
    public void replicateFunction(T behaviour, NetworkNode node) {
        (new replicatorThread(behaviour, node)).start(); 
    }
    
    /*
     * Ideally find a way to do away with this function
     * Perhaps with a Class function
     * At the moment this must be implemented and reimplemented by every single class that extends NetworkBehaviour
     */
    public abstract T replicate(T parentBehaviour);
    
    /*
     * 
     */
    public abstract void replaceInNode(NetworkNode node, T behaviour);
    
    @Override
    public abstract void run();
    
}
