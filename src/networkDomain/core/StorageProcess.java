package networkDomain.core;

import dataDomain.DataCell;
import networkDomain.NetworkNode;

public class StorageProcess {
	
	// Storage of data, continuous signals, and other nodes that are connected to this one
	NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }

	public StorageProcess() {
		//TODO init
	}
	
	public void storeDataCell(DataCell dataCell) {
		
	}
	
}
