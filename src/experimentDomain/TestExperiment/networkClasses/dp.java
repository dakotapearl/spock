package experimentDomain.TestExperiment.networkClasses;

import dataDomain.DataCell;
import networkDomain.extensions.DataProcessing;
import networkDomain.extensions.NodeExtensionEncapsulator;

public class dp implements DataProcessing {

	NodeExtensionEncapsulator NXE;
	
	@Override
	public void setNXE(NodeExtensionEncapsulator NXE) {
		this.NXE = NXE;
	}

	@Override
	public DataCell processData(DataCell dataCell) {
		//tools.Log.write("Test Extension: DataProcessing received processData command");
		return dataCell;
	}

}
