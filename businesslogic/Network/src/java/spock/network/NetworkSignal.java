package spock.network;

import dataDomain.DataCell;

/**
 * @author Loren Chorley
 */
public interface NetworkSignal {
	
	public DataCell getData();
	public void setData(DataCell dataCell);
	
}
