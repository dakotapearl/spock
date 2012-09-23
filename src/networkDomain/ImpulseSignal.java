package networkDomain;

import dataDomain.DataCell;

public class ImpulseSignal implements NetworkSignal {

	private DataCell dataCell;
	
	@Override
	public DataCell getData(NetworkNode currentComponent) {
		return dataCell;
	}

	@Override
	public void setData(DataCell dataCell, NetworkNode modifyingNode) {
		this.dataCell = dataCell;
		
	}

}
