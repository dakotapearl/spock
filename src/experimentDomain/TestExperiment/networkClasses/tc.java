package experimentDomain.TestExperiment.networkClasses;

import networkDomain.NetworkSignal;
import networkDomain.extensions.TransmissionContent;

public class tc extends TransmissionContent {

	@Override
	public NetworkSignal nextSignalToFire() {
		//tools.Log.write("Test Extension: TransmissionContent received nextDataCellToFire command");
		
		// pop item and store
		//parent.processedDataCells.popItem();
		
		((fc) parent.firingCondition).update();
		
		return null; //return values
	}

	@Override
	public boolean signalsRemain() {
		//tools.Log.write("Test Extension: TransmissionContent received dataRemains command");
		return false; //parent.processedDataCells.viewFirstItem() != null;
	}

}
