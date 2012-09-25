package networkDomain.extensions;

import networkDomain.NetworkNode;

/**
 * Instantiated for each node
 * @author Loren
 *
 */
public abstract class GeneticSequence extends Thread {
	
	protected NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") GeneticSequence newFunction;
		public replicator(GeneticSequence newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(GeneticSequence newFunction) { (new replicator(newFunction)).start(); }
	public abstract GeneticSequence replicate();
	
	//public abstract void setProperty(...);
	//public abstract ... getProperty(...);
	//public abstract void mutate(...);
	//public abstract ... splice(...);
	
}
