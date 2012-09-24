package experimentDomain.TestExperiment.networkClasses;

import networkDomain.extensions.EnergyEconomics;
import networkDomain.extensions.NodeExtensionEncapsulator;

public class ee implements EnergyEconomics {

	NodeExtensionEncapsulator NXE;
	
	@Override
	public void setNXE(NodeExtensionEncapsulator NXE) {
		this.NXE = NXE;
	}

	@Override
	public boolean continueFiring() {
		//tools.Log.write("Test Extension: EnergyEconomics received continueFiring request");
		return true;
	}

	@Override
	public double getNodeEfficiency() {
		//tools.Log.write("Test Extension: EnergyEconomics received getNodeEfficiency request");
		return 0.5;
	}

	@Override
	public boolean hasExceededEnergyLimit() {
		//tools.Log.write("Test Extension: EnergyEconomics received hasExceededEnergyLimit request");
		return false;
	}

}
