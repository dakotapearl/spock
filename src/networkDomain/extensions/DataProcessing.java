package networkDomain.extensions;

import networkDomain.NetworkNode;
import dataDomain.DataCell;

public abstract class DataProcessing {

	NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	public abstract DataCell processData(DataCell dataCell);
	
}
