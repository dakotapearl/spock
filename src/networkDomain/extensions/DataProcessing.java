package networkDomain.extensions;

import networkDomain.NetworkNode;
import dataDomain.DataCell;

public abstract class DataProcessing extends Thread {

	protected NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	public abstract DataCell processData(DataCell dataCell); // Make sure that it's a different object that gets returned
	
}
