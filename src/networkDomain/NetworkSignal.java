package networkDomain;

import dataDomain.DataCell;

public interface NetworkSignal {
	
	public DataCell getData(NetworkNode currentComponent);
	public void setData(DataCell dataCell, NetworkNode modifyingNode);
	
}
