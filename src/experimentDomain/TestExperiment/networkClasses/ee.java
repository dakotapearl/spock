package experimentDomain.TestExperiment.networkClasses;

import networkDomain.extensions.EnergyEconomics;

public class ee extends EnergyEconomics {

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
