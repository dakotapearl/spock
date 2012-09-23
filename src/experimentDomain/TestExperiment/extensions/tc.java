package experimentsDomain.test.extensions;

import dataDomain.DataCell;
import networkDomain.extensions.NodeExtensionEncapsulator;
import networkDomain.extensions.TransmissionContent;

public class tc implements TransmissionContent {

	NodeExtensionEncapsulator NXE;
	
	@Override
	public void setNXE(NodeExtensionEncapsulator NXE) {
		this.NXE = NXE;
	}
	
	@Override
	public DataCell nextDataCellToFire() {
		//tools.Log.write("Test Extension: TransmissionContent received nextDataCellToFire command");
		
		return NXE.getNetworkNodeProperties().processedDataCells.popItem();
	}

	@Override
	public boolean dataRemains() {
		//tools.Log.write("Test Extension: TransmissionContent received dataRemains command");
		return NXE.getNetworkNodeProperties().processedDataCells.viewFirstItem() != null;
	}

}
