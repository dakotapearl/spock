package environmentDomain;

import networkDomain.NetworkDomain;
import dataDomain.DataDomain;
import experimentDomain.ExperimentDomain;
import tools.errorChecking.Log;

public class EnvironmentDomain {
	
	public ExperimentDomain experimentDomain;
	public NetworkDomain networkDomain;
	public DataDomain dataDomain;
	
	public void initialise() {
		// check that all relevant domains are linked
		
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
