package experimentDomain.TestExperiment.networkClasses;

import networkDomain.extensions.FiringCondition;
import networkDomain.extensions.NodeExtensionEncapsulator;

//TODO: implements timer
public class fc implements FiringCondition {
	
	NodeExtensionEncapsulator NXE;
	
	@Override
	public void setNXE(NodeExtensionEncapsulator NXE) {
		this.NXE = NXE;
	}
	
	@Override
	public boolean readyToFire() {
		//tools.Log.write("Test Extension: FiringCondition received readyToFire request");
		return NXE.getNetworkNodeProperties().processedDataCells.viewFirstItem() != null;
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
