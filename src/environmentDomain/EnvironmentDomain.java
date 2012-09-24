package environmentDomain;

import networkDomain.NetworkDomain;
import dataDomain.DataDomain;
import experimentDomain.ExperimentDomain;
import tools.errorChecking.Log;

public class EnvironmentDomain {
	
	ExperimentDomain experimentDomain;
	NetworkDomain networkDomain;
	DataDomain dataDomain;
	
	public void initialise() {
		Log.write("Environment domain initialised");
	}
	
	public void setExperimentDomain(ExperimentDomain experimentDomain) {
		this.experimentDomain = experimentDomain;
	}
	
	public void setNetworkDomain(NetworkDomain networkDomain) {
		this.networkDomain = networkDomain;
	}
	
	public void setDataDomain(DataDomain dataDomain) {
		this.dataDomain = dataDomain;
	}
	
}
