package networkDomain.signals;

import networkDomain.NetworkSignal;
import dataDomain.DataCell;

public class ImpulseSignal implements NetworkSignal {

	private DataCell dataCell;
	
	// Needs to be able to cope with null value for networknode
	@Override
	public DataCell getData() {
		return dataCell;
	}

	@Override
	public void setData(DataCell dataCell) {
		this.dataCell = dataCell;
		
	}

}
