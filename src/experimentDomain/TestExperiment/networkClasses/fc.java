package experimentDomain.TestExperiment.networkClasses;

import networkDomain.extensions.FiringCondition;

//TODO: implements timer
public class fc extends FiringCondition {
	

	
	@Override
	public boolean readyToFire() {
		//tools.Log.write("Test Extension: FiringCondition received readyToFire request");
		return false; //NXE.getNetworkNodeProperties().processedDataCells.viewFirstItem() != null;
	}

	@Override
	public boolean continueFiring() {
		//tools.Log.write("Test Extension: FiringCondition received continueFiring request");
		return true;
	}

	@Override
	public void notifyNodeBecameInactive() {
		//tools.Log.write("Test Extension: FiringCondition received notifyNodeBecameInactive command");
	}

}
