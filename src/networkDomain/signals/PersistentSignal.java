package networkDomain.signals;

import dataDomain.DataCell;
import networkDomain.NetworkSignal;
import networkDomain.NetworkTargetable;
import networkDomain.NetworkTransmitter;

/**
 * @author Loren Chorley
 */
public class PersistentSignal implements NetworkSignal, NetworkTargetable {

	private int id;
	
	public PersistentSignal(int id) {
		this.id = id;
	}
	
	@Override
	public DataCell getData() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setData(DataCell dataCell) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void acceptSignal(NetworkSignal signal, NetworkTransmitter sender) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public int getID() {
		return id;
	}

}
