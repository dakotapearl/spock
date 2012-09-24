package experimentDomain.TestExperiment.networkClasses;

import dataDomain.DataCell;
import networkDomain.NetworkSignal;
import networkDomain.extensions.TransmissionContent;

public class tc extends TransmissionContent {

	@Override
	public NetworkSignal nextSignalToFire() {
		//tools.Log.write("Test Extension: TransmissionContent received nextDataCellToFire command");
		
		return null; //parent.processedDataCells.popItem();
	}

	@Override
	public boolean signalsRemain() {
		//tools.Log.write("Test Extension: TransmissionContent received dataRemains command");
		return false; //parent.processedDataCells.viewFirstItem() != null;
	}

}
