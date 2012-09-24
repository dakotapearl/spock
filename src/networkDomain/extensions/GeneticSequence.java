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
	//public abstract void setProperty(...);
	//public abstract ... getProperty(...);
	//public abstract void mutate(...);
	//public abstract ... splice(...);
	
}
