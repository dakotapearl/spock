package networkDomain.extensions;

import dataDomain.DataCell;

public interface DataProcessing {

	public void setNXE(NodeExtensionEncapsulator NXE);
	public DataCell processData(DataCell dataCell);
	
}
