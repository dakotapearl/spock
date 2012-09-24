package experimentDomain;

import networkDomain.NetworkDomain;
import environmentDomain.EnvironmentDomain;
import tools.errorChecking.Log;

public class ExperimentDomain {

	EnvironmentDomain environmentDomain;
	NetworkDomain networkDomain;
	
	public void initialise() {
		Log.write("Experiment domain initialised");
	}
	
	public void setEnvironmentDomain(EnvironmentDomain environmentDomain) {
		this.environmentDomain = environmentDomain;
	}
	
	public void setNetworkDomain(NetworkDomain networkDomain) {
		this.networkDomain = networkDomain;
	}
	
}
