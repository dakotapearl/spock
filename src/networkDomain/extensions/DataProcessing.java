package networkDomain.extensions;

import networkDomain.NetworkBehaviour;
import networkDomain.NetworkNode;
import dataDomain.DataCell;

/**
 * @author Loren Chorley
 */
public abstract class DataProcessing extends Thread implements NetworkBehaviour {

	protected NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	public abstract DataCell processData(DataCell dataCell); // Make sure that it's a different object that gets returned
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") DataProcessing newFunction;
		public replicator(DataProcessing newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); /*TODO assert all went well*/ }
	}
	public void replicateFunction(DataProcessing newFunction) { (new replicator(newFunction)).start(); }
	public abstract DataProcessing replicate();
	public abstract void run();

}
